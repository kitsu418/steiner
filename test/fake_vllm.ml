(* A stand-in for a vLLM chat completions server, for testing Steiner without a real model. It tokenizes roughly like a
   real tokenizer (a word with its leading space, or one other character; special tokens whole), applies a ChatML or
   harmony template, and returns prompt_logprobs: at every position, a made-up distribution that depends only on the
   tokens before it. In the answer, the most probable token follows TARGET while the text agrees with it; elsewhere it
   is drawn from a small vocabulary of code-like tokens.

     dune exec test/fake_vllm.exe -- --port 8901 [--template harmony] [--key K] [--no-trim] [--plain] *)

let target =
  "int binary_search(const int *a, int n, int key) {\n    int lo = 0, hi = n - 1;\n    while (lo <= hi) {\n        int mid = lo + (hi - lo) / 2;\n        if (a[mid] == key) return mid;\n        if (a[mid] < key) lo = mid + 1;\n        else hi = mid - 1;\n    }\n    return -1;\n}\n"

let specials = [ "<|im_start|>"; "<|im_end|>"; "<|start|>"; "<|end|>"; "<|message|>"; "<|channel|>"; "<|return|>" ]

let is_word c = match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false

let tokenize s =
  let n = String.length s in
  let word_end i = let j = ref i in while !j < n && is_word s.[!j] do incr j done; !j in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match List.find_opt (fun sp -> let l = String.length sp in i + l <= n && String.sub s i l = sp) specials with
      | Some sp -> go (i + String.length sp) (sp :: acc)
      | None ->
          let l =
            if s.[i] = ' ' && i + 1 < n && is_word s.[i + 1] then word_end (i + 1) - i
            else if is_word s.[i] then word_end i - i
            else Uchar.utf_decode_length (String.get_utf_8_uchar s i)
          in
          go (i + l) (String.sub s i l :: acc)
  in
  go 0 []

let ids : (string, int) Hashtbl.t = Hashtbl.create 256

let id_of tok =
  match List.find_index (( = ) tok) specials with
  | Some k -> 200_000 + k
  | None -> (
      match Hashtbl.find_opt ids tok with
      | Some i -> i
      | None ->
          let i = 1000 + Hashtbl.length ids in
          Hashtbl.replace ids tok i;
          i)

(* Alternatives are whole tokens as [tokenize] makes them (a word with one leading space, or one other character), so
   that any path tokenizes back to its own tokens. *)
let vocab =
  [ " int"; " mid"; " lo"; " hi"; " key"; " n"; " a"; " return"; " if"; " while"; " for"; " size"; " left"; " right";
    " i"; "-"; "+"; "="; "<"; ";"; ","; "("; ")"; "["; "]"; "{"; "\n"; "/"; " 0"; " 1"; " const"; " unsigned";
    " long"; " found"; " result"; " break"; " else" ]

let target_tokens = Array.of_list (tokenize target)

(* The top-k after [before], and where the answer started (None outside the answer). *)
let distribution ~width ~end_token ~answer_start before =
  let so_far = match answer_start with None -> None | Some a -> Some (String.concat "" (List.filteri (fun i _ -> i >= a) before)) in
  let h = Hashtbl.hash (String.concat "\x00" before) in
  let top1 =
    match so_far with
    | None -> "x"
    | Some so_far ->
        (* follow TARGET while the text is one of its token prefixes *)
        let rec walk k acc =
          if acc = so_far then Some k
          else if k >= Array.length target_tokens || String.length acc >= String.length so_far then None
          else walk (k + 1) (acc ^ target_tokens.(k))
        in
        (match walk 0 "" with
         | Some k when k >= Array.length target_tokens -> end_token
         | Some k -> target_tokens.(k)
         | None -> List.nth vocab (h mod List.length vocab))
  in
  let rotate = h mod List.length vocab in
  let others = List.filteri (fun i _ -> i >= rotate) vocab @ List.filteri (fun i _ -> i < rotate) vocab in
  let others = List.filter (( <> ) top1) (others @ [ end_token ]) in
  let toks = List.filteri (fun i _ -> i < width) (top1 :: others) in
  let p1 = 0.35 +. 0.6 *. float_of_int (h mod 1000) /. 1000. in
  let mass = Array.init (List.length toks) (fun i -> if i = 0 then p1 else (1. -. p1) *. (0.55 ** float_of_int i)) in
  List.mapi (fun i t -> (t, log mass.(i), i + 1)) toks

let () =
  let port = ref 8901 and template = ref "qwen" and key = ref "" and trim = ref true and plain = ref false and no_continue = ref false and limit_every = ref 0 and served = ref 0 in
  Arg.parse
    [ ("--port", Arg.Set_int port, "PORT");
      ("--template", Arg.Set_string template, "qwen|harmony");
      ("--key", Arg.Set_string key, "KEY require this bearer key");
      ("--no-trim", Arg.Clear trim, " keep trailing whitespace (Qwen's template trims it)");
      ("--plain", Arg.Set plain, " answer like a non-vLLM server, without prompt_logprobs");
      ("--rate-limit-every", Arg.Set_int limit_every, "N answer every Nth request with a rate limit (HTTP 400), as GenAI Studio does");
      ("--no-continue", Arg.Set no_continue, " ignore continue_final_message: close the answer and start a new one (as GenAI Studio does for gpt-oss)") ]
    ignore "fake_vllm";
  let open Yojson.Safe.Util in
  let handle req body =
    let auth = Http.Header.get (Http.Request.headers req) "authorization" in
    incr served;
    if !key <> "" && auth <> Some ("Bearer " ^ !key) then (`Unauthorized, {|{"error":"bad key"}|})
    else if !limit_every > 0 && !served mod !limit_every = 0 then (`Bad_request, {|{"detail":"Rate limit exceeded. Please try again later."}|})
    else
      let j = Yojson.Safe.from_string (Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int) in
      let width = j |> member "prompt_logprobs" |> to_int_option |> Option.value ~default:5 in
      let msgs = j |> member "messages" |> to_list |> List.map (fun m -> (m |> member "role" |> to_string, m |> member "content" |> to_string)) in
      let rstrip s =
        let n = ref (String.length s) in
        while !n > 0 && String.contains " \n\t" s.[!n - 1] do decr n done;
        String.sub s 0 !n
      in
      let harmony = !template = "harmony" in
      let end_token = if harmony then "<|end|>" else "<|im_end|>" in
      (* the tokens, and the index where the last assistant message's content starts *)
      let toks = ref [] and answer_start = ref None in
      let add s = toks := !toks @ tokenize s in
      (* continue_final_message: the last (assistant) message is left open, and the answer goes on from it *)
      let asked = match j |> member "continue_final_message" with `Bool b -> b | _ -> false in
      let cont = asked && not !no_continue in
      let last = List.length msgs - 1 in
      List.iteri
        (fun k (role, content) ->
          let close = not (cont && k = last) in
          let content = if role = "assistant" && !trim && not harmony then rstrip content else content in
          if harmony then begin
            add ("<|start|>" ^ role ^ (if role = "assistant" then "<|channel|>final" else "") ^ "<|message|>");
            if role = "assistant" then answer_start := Some (List.length !toks);
            add (content ^ if close then "<|end|>" else "")
          end
          else begin
            add ("<|im_start|>" ^ role ^ "\n");
            if role = "assistant" then answer_start := Some (List.length !toks);
            add (content ^ if close then "<|im_end|>\n" else "")
          end)
        msgs;
      (* a server that will not continue: the answer was closed above; a new one starts *)
      if asked && !no_continue then begin
        add (if harmony then "<|start|>assistant" else "<|im_start|>assistant\n");
        answer_start := Some (List.length !toks)
      end;
      let toks = Array.of_list !toks in
      let ids = Array.map id_of toks in
      let entries =
        List.init (Array.length toks) (fun i ->
            if i = 0 then `Null
            else
              let before = Array.to_list (Array.sub toks 0 i) in
              let a = match !answer_start with Some a when i >= a -> Some a | _ -> None in
              let d = distribution ~width ~end_token ~answer_start:a before in
              let entry (t, lp, r) = (string_of_int (id_of t), `Assoc [ ("logprob", `Float lp); ("rank", `Int r); ("decoded_token", `String t) ]) in
              let actual = if List.exists (fun (t, _, _) -> t = toks.(i)) d then [] else [ entry (toks.(i), -12., width + 3) ] in
              `Assoc (List.map entry d @ actual))
      in
      if asked then begin
        (* greedy generation with top_logprobs, as vLLM's chat completions return it *)
        let top = j |> member "top_logprobs" |> to_int_option |> Option.value ~default:5 in
        let max_tokens = j |> member "max_tokens" |> to_int_option |> Option.value ~default:16 in
        let as_ids = (match j |> member "return_tokens_as_token_ids" with `Bool b -> b | _ -> false) in
        let name t = if as_ids then "token_id:" ^ string_of_int (id_of t) else t in
        let bytes t = `List (List.init (String.length t) (fun i -> `Int (Char.code t.[i]))) in
        let gen = ref (Array.to_list toks) and out = ref [] and finish = ref "length" in
        (try
           for _ = 1 to max_tokens do
             let d = distribution ~width:top ~end_token ~answer_start:!answer_start !gen in
             let t1, lp1, _ = List.hd d in
             out :=
               `Assoc
                 [ ("token", `String (name t1)); ("logprob", `Float lp1); ("bytes", bytes t1);
                   ("top_logprobs", `List (List.map (fun (t, lp, _) -> `Assoc [ ("token", `String (name t)); ("logprob", `Float lp); ("bytes", bytes t) ]) d)) ]
               :: !out;
             gen := !gen @ [ t1 ];
             if t1 = end_token then (finish := "stop"; raise Exit)
           done
         with Exit -> ());
        let choice =
          `Assoc
            [ ("message", `Assoc [ ("role", `String "assistant"); ("content", `String "") ]);
              ("logprobs", `Assoc [ ("content", `List (List.rev !out)) ]);
              ("finish_reason", `String !finish) ]
        in
        (`OK, Yojson.Safe.to_string (`Assoc [ ("id", `String "fake"); ("choices", `List [ choice ]); ("prompt_token_ids", `List (Array.to_list (Array.map (fun i -> `Int i) ids))) ]))
      end
      else
      let reply =
        [ ("id", `String "fake"); ("choices", `List [ `Assoc [ ("message", `Assoc [ ("role", `String "assistant"); ("content", `String "x") ]) ] ]) ]
        @ if !plain then [] else [ ("prompt_token_ids", `List (Array.to_list (Array.map (fun i -> `Int i) ids))); ("prompt_logprobs", `List entries) ]
      in
      (`OK, Yojson.Safe.to_string (`Assoc reply))
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.listen ~sw ~backlog:16 ~reuse_addr:true (Eio.Stdenv.net env) (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port)) in
  Printf.printf "fake vLLM (%s) on http://127.0.0.1:%d/v1/chat/completions\n%!" !template !port;
  let callback _ req body =
    let status, body = handle req body in
    Cohttp_eio.Server.respond_string ~headers:(Http.Header.of_list [ ("Content-Type", "application/json") ]) ~status ~body ()
  in
  Cohttp_eio.Server.run socket (Cohttp_eio.Server.make ~callback ()) ~on_error:(fun e -> prerr_endline (Printexc.to_string e))
