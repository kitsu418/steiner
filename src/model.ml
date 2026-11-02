(* Requests to a provider. next_tokens gives the top-k candidates for the token after a text: the conversation is sent
   with the text as the assistant's answer, max_tokens 1 and prompt_logprobs k, and vLLM returns for every position of
   the prompt the top-k tokens the model would have put there. The position where the chat template closes the answer
   holds the distribution for the next token after the text. *)

open Yojson.Safe.Util

exception Unscorable of string  (* the text cannot be scored as written; the node is marked *)

type env = {
  now : unit -> float;
  sleep : float -> unit;
  with_timeout : 'a. float -> (unit -> 'a) -> 'a;
  http_post : url:string -> headers:(string * string) list -> string -> int * string;  (* status, body *)
}

type limiter = { mutex : Eio.Mutex.t; mutable last : float }

type t = {
  env : env;
  providers : Providers.t;
  limiters : (string, limiter) Hashtbl.t;  (* by provider: one interval for all its models *)
  trims : (string * string, unit) Hashtbl.t;  (* models whose template trimmed a whitespace-ending text *)
  mutable requests : int;
  mutable note : string;  (* the last passing error, while retrying *)
}

let create env providers = { env; providers; limiters = Hashtbl.create 4; trims = Hashtbl.create 4; requests = 0; note = "" }

let probes = [ " 0"; "0" ]

let limiter t name =
  match Hashtbl.find_opt t.limiters name with
  | Some l -> l
  | None ->
      let l = { mutex = Eio.Mutex.create (); last = neg_infinity } in
      Hashtbl.replace t.limiters name l;
      l

let fail fmt = Printf.ksprintf failwith fmt

(* One request, retried over passing errors (rate limits, 5xx, timeouts), with the provider's interval between
   requests. Cancelling the fiber ends a wait at once. *)
let post t (p : Providers.provider) ?(retries = 10) ?(wait = 30.) ?(scoring = true) payload =
  let body = Yojson.Safe.to_string payload in
  let headers =
    [ ("Content-Type", "application/json"); ("Content-Length", string_of_int (String.length body)) ]
    @ match Providers.key t.providers p with Some k -> [ ("Authorization", "Bearer " ^ k.value) ] | None -> []
  in
  let rec attempt n =
    let lim = limiter t p.name in
    Eio.Mutex.use_ro lim.mutex (fun () ->
        let pause = p.min_interval -. (t.env.now () -. lim.last) in
        if pause > 0. then t.env.sleep pause;
        lim.last <- t.env.now ());
    t.requests <- t.requests + 1;
    let result =
      match t.env.with_timeout 300. (fun () -> t.env.http_post ~url:p.url ~headers body) with
      | status, reply when status >= 200 && status < 300 -> (
          match Yojson.Safe.from_string reply with
          | `Assoc fields as j when List.mem_assoc "prompt_logprobs" fields || ((not scoring) && List.mem_assoc "choices" fields) -> Ok j
          | `Assoc fields when List.mem_assoc "choices" fields ->
              fail "the reply has no prompt_logprobs: %s must be a vLLM-backed server" p.name
          | j -> Error ("unexpected reply " ^ Util.truncate 200 (Yojson.Safe.to_string j))
          | exception Yojson.Json_error _ -> Error ("unexpected reply " ^ Util.truncate 200 reply))
      | ((400 | 401 | 403 | 404) as status), reply -> fail "HTTP %d: %s" status (Util.truncate 300 reply)
      | status, reply -> Error (Printf.sprintf "HTTP %d: %s" status (Util.truncate 300 reply))
      | exception (Eio.Cancel.Cancelled _ as e) -> raise e
      | exception Eio.Time.Timeout -> Error "no reply in 300 s"
      | exception (Failure _ as e) -> raise e
      | exception e -> Error (Printexc.to_string e)
    in
    match result with
    | Ok j ->
        t.note <- "";
        j
    | Error e when n >= retries -> failwith e
    | Error e ->
        t.note <- Printf.sprintf "%s · retry %d of %d in %.0f s" e n (retries - 1) wait;
        t.env.sleep wait;
        attempt (n + 1)
  in
  attempt 1

type candidate = { token : string; id : int; logprob : float; rank : int }

(* ids a path must have; -1 is an id not known yet (a token written by fast growing), which matches any id *)
let matches expected ids = List.length expected = List.length ids && List.for_all2 (fun e a -> e < 0 || e = a) expected ids

let number = function
  | `Int n -> float_of_int n
  | `Float f -> f
  | `Intlit s -> float_of_string s
  | `String s -> float_of_string s  (* "-inf" *)
  | _ -> neg_infinity

(* [(candidates, ids)]: the candidates for the token after [text], most probable first, and the ids of [text].
   [expected] is the ids [text] must have (its root's ids, then the tokens on its path); None for a root, whose text
   may be tokenized however the server tokenizes it. *)
let next_tokens t (p : Providers.provider) (m : Providers.model) ?retries ~prompt ~text ~expected () =
  let backend = match Backend.find m.backend with Some b -> b | None -> fail "unknown backend %s" m.backend in
  let system = match m.system with Some s -> Some s | None -> backend.system () in
  let vocab =
    match m.vocab_file with
    | None -> None
    | Some f -> ( match Vocab.load f with Ok v -> Some v | Error e -> fail "vocabulary %s: %s" f e)
  in
  let bytes_of id token = if token <> "" then token else match vocab with Some v -> Option.value ~default:"" (Hashtbl.find_opt v id) | None -> "" in
  let message role content = `Assoc [ ("role", `String role); ("content", `String content) ] in
  let messages = (match system with Some s -> [ message "system" s ] | None -> []) @ [ message "user" prompt ] in
  let key = (p.name, m.name) in
  (* Some texts cannot be scored as written: trailing whitespace the template trims (Qwen's ChatML), or a split the
     tokenizer makes only before what follows (N spaces before a number are [N-1 spaces][' ']). Then the text is sent
     again with a probe appended and the distribution is read at the probe's position, which depends only on the
     tokens before it. Once a whitespace-ending text needed a probe, later ones skip the plain request. *)
  let tries = if Hashtbl.mem t.trims key && Util.ends_in_space text then probes else "" :: probes in
  let rec try_probe = function
    | [] ->
        raise
          (Unscorable
             "the server could not score this text as written: it tokenizes the path differently, or the chat template \
              changes the text")
    | probe :: rest -> (
        let sent = if text ^ probe = "" then "x" else text ^ probe in
        (* an empty assistant message is dropped: read the placeholder's position instead *)
        let data =
          post t p ?retries
            (`Assoc
               [ ("model", `String m.name);
                 ("messages", `List (messages @ [ message "assistant" sent ]));
                 ("prompt_logprobs", `Int p.width);
                 ("return_token_ids", `Bool true);
                 ("max_tokens", `Int 1);
                 ("temperature", `Int 0) ])
        in
        let ids, entries =
          try
            ( data |> member "prompt_token_ids" |> to_list |> List.map to_int |> Array.of_list,
              data |> member "prompt_logprobs" |> to_list |> Array.of_list )
          with Type_error (msg, _) -> fail "malformed reply: %s (does the server return token ids?)" msg
        in
        if Array.length ids <> Array.length entries then fail "malformed reply: token ids and logprobs differ in length";
        let decoded i =
          match entries.(i) with
          | `Assoc _ as e -> (
              match e |> member (string_of_int ids.(i)) with
              | `Assoc _ as x -> ( match x |> member "decoded_token" with `String s -> s | _ -> "")
              | _ -> "")
          | _ -> ""
        in
        let tokens = Array.init (Array.length ids) decoded in
        let end_ =
          let r = ref None in
          Array.iteri (fun i tok -> if tok = backend.end_token then r := Some i) tokens;
          match !r with
          | Some e -> e
          | None ->
              raise (Unscorable (Printf.sprintf "no %s in the scored prompt: is backend %s right for this model?" backend.end_token m.backend))
        in
        let pieces = Array.init end_ (fun i -> bytes_of ids.(i) tokens.(i)) in
        let start = ref end_ and length = ref 0 in
        while !start > 0 && !length < String.length sent do
          decr start;
          length := !length + String.length pieces.(!start)
        done;
        let start = !start in
        let span a b = String.concat "" (Array.to_list (Array.sub pieces a (b - a))) in
        if span start end_ <> sent then try_probe rest (* the template changed the text *)
        else
          let answer = if text ^ probe = "" then [] else Array.to_list (Array.sub ids start (end_ - start)) in
          let found =
            match expected with
            | None ->
                (* a root: the first ids of the answer that spell its text *)
                let n = List.length answer in
                let rec first k =
                  if k > n then None
                  else if span start (start + k) = text then Some (List.filteri (fun i _ -> i < k) answer)
                  else first (k + 1)
                in
                first 0
            | Some exp ->
                let prefix = List.filteri (fun i _ -> i < List.length exp) answer in
                if matches exp answer || (probe <> "" && matches exp prefix) then Some prefix else None
          in
          match found with
          | None -> try_probe rest
          | Some found ->
              if probe <> "" && Util.ends_in_space text then Hashtbl.replace t.trims key ();
              let at = start + List.length found in
              let cands =
                match entries.(at) with
                | `Assoc kvs ->
                    List.filter_map
                      (fun (id, e) ->
                        let rank = try e |> member "rank" |> to_int with _ -> max_int in
                        if rank > p.width then None
                        else
                          let token = match e |> member "decoded_token" with `String s -> s | _ -> "" in
                          Some { token; id = int_of_string id; logprob = Float.max (number (e |> member "logprob")) (-1e9); rank })
                      kvs
                | _ -> []
              in
              (List.sort (fun a b -> compare a.rank b.rank) cands, found))
  in
  try_probe tries

(* Fast growing: one request generates up to [steps] tokens greedily from [text], each with its top-k candidates
   (logprobs/top_logprobs), instead of one scoring request per token. It needs vLLM's continue_final_message, so that
   the model goes on with the assistant's text rather than starting a new answer, and return_tokens_as_token_ids, for
   the candidates' ids. The server's tokens for [text] must be [expected], exactly as when scoring; otherwise Fast_off is
   raised and the caller scores token by token instead. Returns the generated positions, each as (candidates, the id
   that was written), and whether the model stopped by itself. *)
exception Fast_off of string

let generate t (p : Providers.provider) (m : Providers.model) ~prompt ~text ~expected ~steps =
  let backend = match Backend.find m.backend with Some b -> b | None -> fail "unknown backend %s" m.backend in
  let system = match m.system with Some s -> Some s | None -> backend.system () in
  let message role content = `Assoc [ ("role", `String role); ("content", `String content) ] in
  let messages = (match system with Some s -> [ message "system" s ] | None -> []) @ [ message "user" prompt; message "assistant" text ] in
  if text = "" then raise (Fast_off "nothing to continue from yet");
  let data =
    try
      post t p ~retries:3 ~scoring:false
        (`Assoc
           [ ("model", `String m.name);
             ("messages", `List messages);
             ("continue_final_message", `Bool true);
             ("add_generation_prompt", `Bool false);
             ("max_tokens", `Int steps);
             ("temperature", `Int 0);
             ("logprobs", `Bool true);
             ("top_logprobs", `Int (min p.width 20));
             ("return_token_ids", `Bool true);
             ("skip_special_tokens", `Bool false) ])
    with Failure e -> raise (Fast_off e)
  in
  let off fmt = Printf.ksprintf (fun e -> raise (Fast_off e)) fmt in
  (* the text must have been read as the path's tokens; the ids the server has for it fill in unknown ones *)
  let text_ids =
    match data |> member "prompt_token_ids" with
    | `List ids ->
        let ids = List.map to_int ids and n = List.length expected in
        let tail = List.filteri (fun i _ -> i >= List.length ids - n) ids in
        if List.length ids < n || not (matches expected tail) then off "the server reads this text as other tokens";
        tail
    | _ -> off "the server does not return token ids"
  in
  let choice = match data |> member "choices" with `List (c :: _) -> c | _ -> off "no choices in the reply" in
  let entries = try choice |> member "logprobs" |> member "content" |> to_list with Type_error _ -> off "no logprobs in the reply" in
  let text_of e =
    match e |> member "bytes" with
    | `List bs ->
        let s = String.init (List.length bs) (fun i -> Char.chr (to_int (List.nth bs i))) in
        if String.is_valid_utf_8 s then s else ""  (* part of a character, as when scoring *)
    | _ -> ( match e |> member "token" with `String s -> s | _ -> "")
  in
  let lp e = Float.max (number (e |> member "logprob")) (-1e9) in
  let positions =
    List.map
      (fun e ->
        let tops = match e |> member "top_logprobs" with `List l -> l | _ -> [] in
        let cands = List.mapi (fun k c -> { token = text_of c; id = -1; logprob = lp c; rank = k + 1 }) tops in
        let chosen = text_of e in
        let cands = if List.exists (fun c -> c.token = chosen) cands then cands else cands @ [ { token = chosen; id = -1; logprob = lp e; rank = List.length cands + 1 } ] in
        (cands, chosen))
      entries
  in
  let stopped = match choice |> member "finish_reason" with `String "stop" -> true | _ -> false in
  (positions, stopped, text_ids)
