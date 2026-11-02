(* The providers: OpenAI-compatible chat completions URLs of vLLM-backed servers, and the models on each. They are kept
   in a JSON file that the page edits. API keys are never in that file: a provider's key is read from the environment
   variable [key_env] when it is set, else from [key_file], else from <keys dir>/<provider name>. The page can write a
   key but never read one back; it sees only whether one is set and its last four characters. *)

open Yojson.Safe.Util

type model = {
  name : string;
  backend : string;
  system : string option;  (* sent before the prompt instead of the backend's *)
  vocab_file : string option;  (* a .tiktoken file that spells byte-piece tokens *)
}

type provider = {
  name : string;
  url : string;
  key_env : string option;
  key_file : string option;
  min_interval : float;  (* seconds between requests to this provider *)
  width : int;  (* candidates per node, the top-k *)
  models : model list;
}

type t = { path : string; keys_dir : string; mutable providers : provider list }

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun s -> raise (Invalid s)) fmt

let valid_name s =
  s <> ""
  && String.length s <= 64
  && String.for_all (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' -> true | _ -> false) s
  && s.[0] <> '.'

let opt_string = function
  | `Null -> None
  | `String s when String.trim s = "" -> None
  | `String s -> Some s
  | _ -> raise (Type_error ("expected a string", `Null))

let number = function
  | `Int n -> float_of_int n
  | `Float f -> f
  | `Intlit s -> float_of_string s
  | _ -> raise (Type_error ("expected a number", `Null))

let model_of_json j =
  let name = String.trim (j |> member "name" |> to_string) in
  let backend = match j |> member "backend" |> opt_string with Some b -> b | None -> Backend.guess name in
  { name; backend; system = j |> member "system" |> opt_string; vocab_file = j |> member "vocab_file" |> opt_string }

let provider_of_json j =
  let default d f = function `Null -> d | v -> f v in
  { name = String.trim (j |> member "name" |> to_string);
    url = String.trim (j |> member "url" |> to_string);
    key_env = j |> member "key_env" |> opt_string;
    key_file = j |> member "key_file" |> opt_string;
    min_interval = j |> member "min_interval" |> default 0. number;
    width = j |> member "width" |> default 20 (fun v -> int_of_float (number v));
    models = j |> member "models" |> default [] (fun v -> List.map model_of_json (to_list v)) }

let validate providers =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (p : provider) ->
      if not (valid_name p.name) then
        invalid "provider name %S: use letters, digits, '-', '_' and '.' (at most 64)" p.name;
      if Hashtbl.mem seen p.name then invalid "two providers are named %S" p.name;
      Hashtbl.add seen p.name ();
      if not (String.starts_with ~prefix:"http://" p.url || String.starts_with ~prefix:"https://" p.url) then
        invalid "%s: the URL must start with http:// or https://" p.name;
      if p.width < 1 || p.width > 1000 then invalid "%s: candidates per node must be 1..1000" p.name;
      if p.min_interval < 0. || p.min_interval > 3600. then invalid "%s: the interval must be 0..3600 s" p.name;
      (match p.key_env with
      | Some v when not (String.for_all (function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) v) ->
          invalid "%s: %S is not an environment variable name" p.name v
      | _ -> ());
      let models = Hashtbl.create 8 in
      List.iter
        (fun (m : model) ->
          if m.name = "" then invalid "%s: a model has no name" p.name;
          if Hashtbl.mem models m.name then invalid "%s: two models are named %S" p.name m.name;
          Hashtbl.add models m.name ();
          if Backend.find m.backend = None then
            invalid "%s / %s: backend %S is not one of %s" p.name m.name m.backend (String.concat ", " Backend.names))
        p.models)
    providers

(* [{"providers": [...]}], each provider optionally with "was": its name before a rename. *)
let parse_list j =
  try
    let items = j |> member "providers" |> to_list in
    let parsed = List.map (fun i -> (provider_of_json i, i |> member "was" |> opt_string)) items in
    validate (List.map fst parsed);
    parsed
  with Type_error (msg, _) -> invalid "bad provider list: %s" msg

let model_to_json (m : model) =
  let opt = function Some s -> `String s | None -> `Null in
  `Assoc [ ("name", `String m.name); ("backend", `String m.backend); ("system", opt m.system); ("vocab_file", opt m.vocab_file) ]

let provider_to_json (p : provider) =
  let opt = function Some s -> `String s | None -> `Null in
  `Assoc
    [ ("name", `String p.name);
      ("url", `String p.url);
      ("key_env", opt p.key_env);
      ("key_file", opt p.key_file);
      ("min_interval", `Float p.min_interval);
      ("width", `Int p.width);
      ("models", `List (List.map model_to_json p.models)) ]

let save t =
  Util.mkdir_p ~perm:0o700 (Filename.dirname t.path);
  Util.write_atomic ~perm:0o600 t.path
    (Yojson.Safe.pretty_to_string (`Assoc [ ("providers", `List (List.map provider_to_json t.providers)) ]) ^ "\n")

(* An empty list is written on the first start. *)
let load ~path ~keys_dir =
  let t = { path; keys_dir; providers = [] } in
  if Sys.file_exists path then begin
    let j = try Yojson.Safe.from_file path with Yojson.Json_error e -> invalid "%s: %s" path e in
    t.providers <- List.map fst (parse_list j)
  end
  else save t;
  t

let find t name = List.find_opt (fun (p : provider) -> p.name = name) t.providers

let find_model t ~provider ~model =
  match find t provider with
  | None -> None
  | Some p -> (
      match List.find_opt (fun (m : model) -> m.name = model) p.models with Some m -> Some (p, m) | None -> None)

let first_model t =
  List.find_map (fun (p : provider) -> match p.models with m :: _ -> Some (p, m) | [] -> None) t.providers

(* Keys *)

let key_path t (p : provider) =
  match p.key_file with Some f -> Util.expand_home f | None -> Filename.concat t.keys_dir p.name

type key = { value : string; source : [ `Env of string | `File of string ] }

let key t (p : provider) =
  let from_env =
    match p.key_env with
    | Some v -> (
        match Sys.getenv_opt v with
        | Some k when String.trim k <> "" -> Some { value = String.trim k; source = `Env v }
        | _ -> None)
    | None -> None
  in
  match from_env with
  | Some k -> Some k
  | None -> (
      let path = key_path t p in
      match Util.read_file path with
      | k when String.trim k <> "" -> Some { value = String.trim k; source = `File path }
      | _ -> None
      | exception Sys_error _ -> None)

let set_key t (p : provider) value =
  let value = String.trim value in
  let path = key_path t p in
  if value = "" then (try Sys.remove path with Sys_error _ -> ())
  else begin
    if String.exists (fun c -> c = '\n' || c = '\r' || c < ' ') value || String.length value > 4096 then
      invalid "the key must be one line of at most 4096 characters";
    if p.key_file = None then Util.mkdir_p ~perm:0o700 t.keys_dir;
    Util.write_atomic ~perm:0o600 path (value ^ "\n")
  end

(* After a rename, the key in the default place follows the provider. *)
let move_key t ~was ~now =
  let src = Filename.concat t.keys_dir was and dst = Filename.concat t.keys_dir now in
  if Sys.file_exists src && not (Sys.file_exists dst) then Sys.rename src dst

(* What the page sees: everything but the key itself. *)
let view t =
  let key_json p =
    match key t p with
    | None -> `Assoc [ ("set", `Bool false); ("path", `String (key_path t p)) ]
    | Some k ->
        let n = String.length k.value in
        `Assoc
          [ ("set", `Bool true);
            ("hint", `String (if n > 8 then "…" ^ String.sub k.value (n - 4) 4 else "(short)"));
            ("source", `String (match k.source with `Env v -> "$" ^ v | `File f -> f));
            ("path", `String (key_path t p)) ]
  in
  `Assoc
    [ ("path", `String t.path);
      ("keys_dir", `String t.keys_dir);
      ("backends", `List (List.map (fun s -> `String s) Backend.names));
      ( "providers",
        `List
          (List.map
             (fun p ->
               match provider_to_json p with
               | `Assoc fields -> `Assoc (fields @ [ ("key", key_json p) ])
               | j -> j)
             t.providers) ) ]
