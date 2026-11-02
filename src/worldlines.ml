(* The worldlines: a forest of token trees, saved to the data file after every change. A token node is
   {p: parent, t: token, id: token id, lp: logprob, r: rank}; a root is {p: -1, prompt, prefix, origin: the node it
   branched from, or -1, ids: the prefix's token ids once scored, provider, model}. A node's text is its root's prefix
   followed by the tokens on the way down to it, and every node is scored by its root's model.

   Everything here runs in one Eio domain, so state changes between two waits are never interleaved. One grow runs at a
   time, in its own switch; Stop fails that switch, which cancels the request or the wait in progress. *)

type root = {
  prompt : string;
  prefix : string;
  origin : int;
  mutable ids : int list option;
  mutable provider : string;
  model : string;
}

type node = Root of root | Tok of { p : int; t : string; mutable tid : int; lp : float; r : int }  (* tid -1: not known yet *)

type job = {
  start : int;
  mutable head : int;
  steps : int;
  mutable added : int;
  mutable status : string;  (* running, done, ended, stopped, failed *)
  mutable error : string;
}

exception Bad_request of string
exception Stopped
exception Not_configured of string

type t = {
  path : string;
  model : Model.t;
  nodes : node Dynarray.t;
  kids : (int, int list) Hashtbl.t;
  jobs : job Dynarray.t;
  blocked : (int, string) Hashtbl.t;
  sw : Eio.Switch.t;  (* grows are forked into it *)
  mutable current : (Eio.Switch.t * unit Eio.Promise.t) option;  (* the grow in progress *)
  mutable epoch : int;  (* changes when nodes are renumbered (a deletion): a page must then read the forest again *)
}

(* JSON *)

let node_to_json = function
  | Root r ->
      `Assoc
        ([ ("p", `Int (-1));
           ("prompt", `String r.prompt);
           ("prefix", `String r.prefix);
           ("origin", `Int r.origin);
           ("provider", `String r.provider);
           ("model", `String r.model) ]
        @ match r.ids with Some ids -> [ ("ids", `List (List.map (fun i -> `Int i) ids)) ] | None -> [])
  | Tok k -> `Assoc [ ("p", `Int k.p); ("t", `String k.t); ("id", `Int k.tid); ("lp", `Float k.lp); ("r", `Int k.r) ]

let node_of_json j =
  let open Yojson.Safe.Util in
  let p = j |> member "p" |> to_int in
  if p < 0 then
    Root
      { prompt = j |> member "prompt" |> to_string;
        prefix = j |> member "prefix" |> to_string;
        origin = j |> member "origin" |> to_int_option |> Option.value ~default:(-1);
        ids = j |> member "ids" |> to_option (fun l -> to_list l |> List.map to_int);
        provider = j |> member "provider" |> to_string_option |> Option.value ~default:"";
        model = j |> member "model" |> to_string_option |> Option.value ~default:"" }
  else
    Tok
      { p;
        t = j |> member "t" |> to_string;
        tid = j |> member "id" |> to_int;
        lp = j |> member "lp" |> to_number;
        r = j |> member "r" |> to_int }

let job_to_json j =
  `Assoc
    [ ("start", `Int j.start);
      ("head", `Int j.head);
      ("steps", `Int j.steps);
      ("added", `Int j.added);
      ("status", `String j.status);
      ("error", `String j.error) ]

let job_of_json j =
  let open Yojson.Safe.Util in
  let status = j |> member "status" |> to_string in
  { start = j |> member "start" |> to_int;
    head = j |> member "head" |> to_int;
    steps = j |> member "steps" |> to_int;
    added = j |> member "added" |> to_int;
    status = (if status = "running" then "stopped" else status);
    error = j |> member "error" |> to_string_option |> Option.value ~default:"" }

let blocked_json t = `Assoc (Hashtbl.fold (fun i e acc -> (string_of_int i, `String e) :: acc) t.blocked [])

let save t =
  Util.write_atomic t.path
    (Yojson.Safe.to_string
       (`Assoc
          [ ("version", `Int 1);
            ("nodes", `List (Dynarray.to_list (Dynarray.map node_to_json t.nodes)));
            ("jobs", `List (Dynarray.to_list (Dynarray.map job_to_json t.jobs)));
            ("blocked", blocked_json t) ]))

let load ~sw ~path model =
  let t =
    { path; model; nodes = Dynarray.create (); kids = Hashtbl.create 1024; jobs = Dynarray.create ();
      blocked = Hashtbl.create 8; sw; current = None; epoch = int_of_float (Unix.time ()) }
  in
  if Sys.file_exists path then begin
    let j = try Yojson.Safe.from_file path with Yojson.Json_error e -> failwith (path ^ ": " ^ e) in
    let open Yojson.Safe.Util in
    (try
       j |> member "nodes" |> to_list |> List.iter (fun n -> Dynarray.add_last t.nodes (node_of_json n));
       j |> member "jobs" |> to_list |> List.iter (fun n -> Dynarray.add_last t.jobs (job_of_json n));
       j |> member "blocked" |> to_assoc |> List.iter (fun (k, v) -> Hashtbl.replace t.blocked (int_of_string k) (to_string v))
     with Type_error (msg, _) -> failwith (Printf.sprintf "%s is not a worldlines file: %s" t.path msg));
    (* children in the order they were added, which is by rank *)
    for i = Dynarray.length t.nodes - 1 downto 0 do
      match Dynarray.get t.nodes i with
      | Tok k -> Hashtbl.replace t.kids k.p (i :: Option.value ~default:[] (Hashtbl.find_opt t.kids k.p))
      | Root _ -> ()
    done
  end;
  t

(* The forest *)

let get t i = Dynarray.get t.nodes i
let size t = Dynarray.length t.nodes
let kids t i = Option.value ~default:[] (Hashtbl.find_opt t.kids i)

let rec root_of t i = match get t i with Root _ -> i | Tok k -> root_of t k.p
let root t i = match get t (root_of t i) with Root r -> r | Tok _ -> assert false

let text t i =
  let rec go i acc = match get t i with Root r -> r.prefix :: acc | Tok k -> go k.p (k.t :: acc) in
  String.concat "" (go i [])

(* The token ids from the root down to i, the root's own excluded. *)
let path_ids t i =
  let rec go i acc = match get t i with Root _ -> acc | Tok k -> go k.p (k.tid :: acc) in
  go i []

let running t = t.current <> None

let node_json t i =
  let r = root t i in
  (* as in the data file, but "id" is the node's index and the token's id is "token_id" *)
  let fields =
    match node_to_json (get t i) with
    | `Assoc f ->
        List.filter_map
          (function
            | "id", v -> Some ("token_id", v)
            | ("prompt" | "provider" | "model"), _ -> None  (* added below for every node *)
            | kv -> Some kv)
          f
    | _ -> []
  in
  `Assoc
    (fields
    @ [ ("id", `Int i);
        ("prompt", `String r.prompt);
        ("text", `String (text t i));
        ("provider", `String r.provider);
        ("model", `String r.model);
        ( "children",
          `List
            (List.map
               (fun c ->
                 match get t c with
                 | Tok k ->
                     `Assoc [ ("id", `Int c); ("token", `String k.t); ("p", `Float (exp k.lp)); ("rank", `Int k.r) ]
                 | Root _ -> `Null)
               (kids t i)) ) ])

let state_json t since =
  let since = max 0 (min since (size t)) in
  `Assoc
    [ ("nodes", `List (List.init (size t - since) (fun k -> node_to_json (get t (since + k)))));
      ("jobs", `List (Dynarray.to_list (Dynarray.map job_to_json t.jobs)));
      ("blocked", blocked_json t);
      ("requests", `Int t.model.requests);
      ("note", `String t.model.note);
      ("running", `Bool (running t));
      ("epoch", `Int t.epoch) ]

(* Growing *)

(* The server's ids for the text of node i (its root's ids, then the path's): fill in the ones not known yet. *)
let learn_ids t i ids =
  let r = root t i in
  let n_root = match r.ids with Some l -> List.length l | None -> 0 in
  let path = let rec go i acc = match get t i with Root _ -> acc | Tok _ -> go (match get t i with Tok k -> k.p | _ -> i) (i :: acc) in go i [] in
  List.iteri (fun k node -> match get t node with Tok tk when tk.tid < 0 -> (match List.nth_opt ids (n_root + k) with Some id -> tk.tid <- id | None -> ()) | _ -> ()) path

let lookup t (r : root) =
  match Providers.find_model t.model.providers ~provider:r.provider ~model:r.model with
  | Some pm -> pm
  | None -> raise (Not_configured (Printf.sprintf "%s / %s is not configured: add it under Providers" r.provider r.model))

(* Request node i's next tokens (one request), unless it has them. *)
let expand t i =
  if not (Hashtbl.mem t.kids i) then begin
    let ri = root_of t i in
    let r = root t i in
    let p, m = lookup t r in
    let expected = if ri = i then None else Some (Option.value ~default:[] r.ids @ path_ids t i) in
    match Model.next_tokens t.model p m ~prompt:r.prompt ~text:(text t i) ~expected () with
    | exception Model.Unscorable e ->
        Hashtbl.replace t.blocked i e;
        save t;
        failwith e
    | cands, ids ->
        if expected = None then r.ids <- Some ids else learn_ids t i ids;
        let first = size t in
        List.iter
          (fun (c : Model.candidate) -> Dynarray.add_last t.nodes (Tok { p = i; t = c.token; tid = c.id; lp = c.logprob; r = c.rank }))
          cands;
        Hashtbl.replace t.kids i (List.init (List.length cands) (fun k -> first + k));
        Hashtbl.remove t.blocked i;
        save t
  end

(* Fast: from node i, which has no candidates yet, generate up to n tokens in one request and add them, with their
   candidates, as if each had been scored. Returns (the last node added, how many, whether the model stopped by
   itself), or None when this text cannot be grown fast (the caller then scores token by token). *)
let fast_chunk t job i n =
  let r = root t i in
  match r.ids with
  | None -> None  (* a new root: its first step is scored, which fixes how its text is tokenized *)
  | Some root_ids -> (
      let p, m = lookup t r in
      match Model.generate t.model p m ~prompt:r.prompt ~text:(text t i) ~expected:(root_ids @ path_ids t i) ~steps:n with
      | exception Model.Fast_off e ->
          t.model.note <- "Fast is off here: " ^ Util.truncate 160 e ^ ". Scoring token by token.";
          None
      | [], _, _ -> None
      | positions, stopped, text_ids ->
          learn_ids t i text_ids;
          let cur = ref i and added = ref 0 in
          (try
             List.iter
               (fun ((cands : Model.candidate list), chosen) ->
                 let first = size t in
                 List.iter
                   (fun (c : Model.candidate) -> Dynarray.add_last t.nodes (Tok { p = !cur; t = c.token; tid = c.id; lp = c.logprob; r = c.rank }))
                   cands;
                 Hashtbl.replace t.kids !cur (List.init (List.length cands) (fun k -> first + k));
                 let next = List.find (fun k -> match get t k with Tok k' -> k'.t = chosen | Root _ -> false) (kids t !cur) in
                 cur := next;
                 incr added;
                 job.head <- next;
                 job.added <- job.added + 1;
                 match get t next with Tok k when not (Util.growable k.t) -> raise Exit | _ -> ())
               positions
           with Exit -> ());
          save t;
          Some (!cur, !added, stopped))

let grow t job ~fast =
  let token i = match get t i with Root _ -> "root" | Tok k -> k.t in
  if not (Util.growable (token job.start)) then
    failwith "nothing follows this token: it ends the answer or is part of a character";
  let finish i =  (* after a token was written: did it end the answer, or can the text go on after it? *)
    let tok = token i in
    if Util.is_special tok then (job.status <- "ended"; false)
    else if not (Util.growable tok) then failwith "the most probable token is part of a character: select another one to go on"
    else true
  in
  let rec step i n fast =
    if n = 0 then job.status <- "done"
    (* a new root's first step is scored (it fixes how the text is tokenized); fast takes over after it *)
    else if fast && job.steps > 0 && not (Hashtbl.mem t.kids i) && (root t i).ids <> None && text t i <> "" then
      match fast_chunk t job i n with
      | Some (j, k, stopped) when k > 0 ->
          (* when the model stopped by itself, one scored step after the last token finds how it ended *)
          if finish j then step j (n - k) (not stopped)
      | _ -> step i n false
    else begin
      expand t i;
      if job.steps = 0 then job.status <- "done"
      else
        match kids t i with
        | [] -> failwith "the server returned no candidates"
        | best :: _ ->
            job.head <- best;
            job.added <- job.added + 1;
            if finish best then step best (n - 1) fast
    end
  in
  step job.start (max job.steps 1) fast

let stop t =
  match t.current with
  | None -> ()
  | Some (jsw, finished) ->
      Eio.Switch.fail jsw Stopped;
      Eio.Promise.await finished

(* Grow [steps] tokens from [node], or, when the prompt, the text or the model differs from the node's, from a root
   with that text (an existing one with the same prompt, text and model, else a new one, drawn next to the node when
   the prompt is the same). steps 0 requests only the start's next tokens. Returns (job index, start node). *)
let start t ~node ~prompt ~text:txt ~steps ~model:sel ~fast =
  let node_model = Option.map (fun n -> let r = root t n in (r.provider, r.model)) node in
  let provider, model =
    match sel, node_model with
    | Some pm, _ | None, Some pm -> pm
    | None, None -> (
        match Providers.first_model t.model.providers with
        | Some (p, m) -> (p.name, m.name)
        | None -> raise (Bad_request "no model is configured: add a provider under Providers"))
  in
  if Providers.find_model t.model.providers ~provider ~model = None then
    raise (Bad_request (Printf.sprintf "%s / %s is not configured" provider model));
  stop t;
  let same_prompt = match node with Some n -> (root t n).prompt = prompt | None -> false in
  let begin_ =
    match node with
    | Some n when same_prompt && text t n = txt && node_model = Some (provider, model) -> n
    | _ -> (
        let existing = ref None in
        Dynarray.iteri
          (fun i -> function
            | Root r when !existing = None && r.prompt = prompt && r.prefix = txt && r.provider = provider && r.model = model ->
                existing := Some i
            | _ -> ())
          t.nodes;
        match !existing with
        | Some i -> i
        | None ->
            let origin = match node with Some n when same_prompt -> n | _ -> -1 in
            Dynarray.add_last t.nodes (Root { prompt; prefix = txt; origin; ids = None; provider; model });
            size t - 1)
  in
  let job = { start = begin_; head = begin_; steps; added = 0; status = "running"; error = "" } in
  Dynarray.add_last t.jobs job;
  save t;
  let finished, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
      (try Eio.Switch.run (fun jsw -> t.current <- Some (jsw, finished); grow t job ~fast) with
      | Stopped -> job.status <- "stopped"
      | Failure e | Not_configured e ->
          job.status <- "failed";
          job.error <- e
      | e ->
          job.status <- "failed";
          job.error <- Printexc.to_string e);
      t.current <- None;
      t.model.note <- "";
      (try save t with Sys_error _ -> ());
      Eio.Promise.resolve resolve ());
  (Dynarray.length t.jobs - 1, begin_)

(* After providers were renamed, the roots follow. *)
(* Delete the worldline at node i: everything grown after it, and the worldlines started by hand from any of that.
   A root goes with it; a token stays, as one of its parent's candidates, with nothing after it. Nodes are renumbered,
   so the epoch changes. Returns how many nodes were removed. *)
let delete t i =
  if running t then raise (Bad_request "a grow is running: stop it first");
  let n = size t in
  if i < 0 || i >= n then raise (Bad_request "unknown node");
  let is_root = match get t i with Root _ -> true | Tok _ -> false in
  let removed = Array.make n false in
  let rec mark j = if not removed.(j) then (removed.(j) <- true; List.iter mark (kids t j)) in
  if is_root then mark i else List.iter mark (kids t i);
  let gone j = removed.(j) || ((not is_root) && j = i) in  (* the kept token loses what came after it, too *)
  let again = ref true in
  while !again do
    again := false;
    Dynarray.iteri (fun k -> function Root r when (not removed.(k)) && r.origin >= 0 && gone r.origin -> mark k; again := true | _ -> ()) t.nodes
  done;
  let index = Array.make n (-1) and kept = Dynarray.create () in
  Dynarray.iteri (fun k nd -> if not removed.(k) then (index.(k) <- Dynarray.length kept; Dynarray.add_last kept nd)) t.nodes;
  let renumber = function
    | Tok k -> Tok { k with p = index.(k.p) }
    | Root r -> Root { r with origin = (if r.origin >= 0 then index.(r.origin) else -1) }
  in
  let parent = match get t i with Tok k -> k.p | Root _ -> -1 in
  let jobs =
    Dynarray.to_list t.jobs
    |> List.filter_map (fun j ->
           if removed.(j.start) || ((not is_root) && j.start = i) then None
           else
             let head = if removed.(j.head) || ((not is_root) && j.head = i) then parent else j.head in
             if head < 0 then None else Some { j with start = index.(j.start); head = index.(head) })
  in
  let blocked = Hashtbl.fold (fun k v acc -> if k < n && not removed.(k) then (index.(k), v) :: acc else acc) t.blocked [] in
  Dynarray.clear t.nodes;
  Dynarray.iter (fun nd -> Dynarray.add_last t.nodes (renumber nd)) kept;
  Dynarray.clear t.jobs;
  List.iter (Dynarray.add_last t.jobs) jobs;
  Hashtbl.reset t.blocked;
  List.iter (fun (k, v) -> Hashtbl.replace t.blocked k v) blocked;
  Hashtbl.reset t.kids;
  for k = size t - 1 downto 0 do
    match get t k with Tok tk -> Hashtbl.replace t.kids tk.p (k :: kids t tk.p) | Root _ -> ()
  done;
  t.epoch <- t.epoch + 1;
  save t;
  n - size t

(* Delete every worldline. *)
let clear t =
  if running t then raise (Bad_request "a grow is running: stop it first");
  let n = size t in
  Dynarray.clear t.nodes; Dynarray.clear t.jobs; Hashtbl.reset t.blocked; Hashtbl.reset t.kids;
  t.epoch <- t.epoch + 1;
  save t;
  n

let rename_providers t renames =
  if renames <> [] then begin
    Dynarray.iter (function Root r -> (match List.assoc_opt r.provider renames with Some n -> r.provider <- n | None -> ()) | Tok _ -> ()) t.nodes;
    save t
  end
