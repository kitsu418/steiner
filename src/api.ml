(* The HTTP API that the page drives, and that scripts can use.

   GET  /                          the page
   GET  /api/config                the starting prompt and text, and the models
   GET  /api/state?from=n          nodes from index n on, the jobs, the request count
   GET  /api/node?id=i             a node's prompt, text, model, token and children
   POST /api/grow                  {node, prompt, text, steps, provider?, model?, fast?} -> {job, start}
   POST /api/stop                  the state
   GET  /api/providers             the providers, with each key's status but never the key
   PUT  /api/providers             {providers: [...]} replaces the list
   PUT  /api/providers/<name>/key  {key} sets the key ("" removes it)
   POST /api/providers/<name>/test {model} scores an empty answer once *)

type config = {
  prompt : string;
  prefix : string;
  data : string;
  page : unit -> string;
  loopback : bool;  (* bound to a loopback address: then the Host header must name it *)
  port : int;
}

exception Http_error of Http.Status.t * string

let error status fmt = Printf.ksprintf (fun s -> raise (Http_error (status, s))) fmt

let respond ?(kind = "application/json; charset=utf-8") ?(status = `OK) body =
  let headers =
    Http.Header.of_list
      [ ("Content-Type", kind); ("Cache-Control", "no-store"); ("X-Content-Type-Options", "nosniff");
        ("X-Frame-Options", "DENY") ]
  in
  Cohttp_eio.Server.respond_string ~headers ~status ~body ()

let json ?status j = respond ?status (Yojson.Safe.to_string j)

let models_json (providers : Providers.t) =
  `List
    (List.concat_map
       (fun (p : Providers.provider) ->
         List.map
           (fun (m : Providers.model) ->
             `Assoc
               [ ("provider", `String p.name);
                 ("model", `String m.name);
                 ("backend", `String m.backend);
                 ("width", `Int p.width);
                 ("key", `Bool (Providers.key providers p <> None)) ])
           p.models)
       providers.providers)

(* A page on another site must not reach this API, through the browser of someone who has it open: requests must name
   this server in Host (against DNS rebinding), and writes must come from this page (Origin) as JSON (which a plain
   form cannot send without a preflight). *)
let check_request cfg req =
  let headers = Http.Request.headers req in
  let host = Option.value ~default:"" (Http.Header.get headers "host") in
  if cfg.loopback then begin
    let port = string_of_int cfg.port in
    let names = [ "127.0.0.1"; "localhost"; "[::1]" ] in
    if not (List.exists (fun n -> host = n ^ ":" ^ port || (cfg.port = 80 && host = n)) names) then
      error `Forbidden "unexpected Host %S" host
  end;
  match Http.Request.meth req with
  | `GET | `HEAD -> ()
  | _ ->
      (match Http.Header.get headers "origin" with
      | None -> ()
      | Some o when o = "http://" ^ host -> ()
      | Some o -> error `Forbidden "cross-origin request from %s" o);
      let kind = Option.value ~default:"" (Http.Header.get headers "content-type") in
      if not (String.starts_with ~prefix:"application/json" (String.lowercase_ascii kind)) then
        error `Unsupported_media_type "send JSON, with Content-Type: application/json"

let read_json body =
  match Eio.Buf_read.(parse_exn take_all) body ~max_size:5_000_000 with
  | "" -> `Assoc []
  | s -> ( try Yojson.Safe.from_string s with Yojson.Json_error e -> error `Bad_request "bad JSON: %s" e)
  | exception Failure _ -> error `Request_entity_too_large "the request is too large"

let field j name = match j with `Assoc kvs -> Option.value ~default:`Null (List.assoc_opt name kvs) | _ -> `Null

let grow (w : Worldlines.t) j =
  let node =
    match field j "node" with
    | `Null -> None
    | `Int n when n >= 0 && n < Worldlines.size w -> Some n
    | _ -> error `Bad_request "unknown node"
  in
  let prompt = match field j "prompt" with `String s when String.trim s <> "" -> s | _ -> error `Bad_request "the prompt is empty" in
  let text = match field j "text" with `String s -> s | `Null -> "" | _ -> error `Bad_request "the text must be a string" in
  let steps = match field j "steps" with `Int n when n >= 0 && n <= 10000 -> n | `Null -> 50 | _ -> error `Bad_request "steps must be 0..10000" in
  let model =
    match field j "provider", field j "model" with
    | `String p, `String m -> Some (p, m)
    | `Null, `Null -> None
    | _ -> error `Bad_request "give both provider and model, or neither"
  in
  let fast = match field j "fast" with `Bool b -> b | _ -> false in
  match Worldlines.start w ~node ~prompt ~text ~steps ~model ~fast with
  | job, start -> json (`Assoc [ ("job", `Int job); ("start", `Int start) ])
  | exception Worldlines.Bad_request e -> error `Bad_request "%s" e

let put_providers (w : Worldlines.t) j =
  let providers = w.model.providers in
  match Providers.parse_list j with
  | exception Providers.Invalid e -> error `Bad_request "%s" e
  | parsed ->
      let renames =
        List.filter_map
          (fun ((p : Providers.provider), was) ->
            match was with Some old when old <> p.name && Providers.find providers old <> None -> Some (old, p.name) | _ -> None)
          parsed
      in
      List.iter (fun (was, now) -> Providers.move_key providers ~was ~now) renames;
      providers.providers <- List.map fst parsed;
      Providers.save providers;
      Worldlines.rename_providers w renames;
      json (Providers.view providers)

let test (w : Worldlines.t) name j =
  let providers = w.model.providers in
  let model = match field j "model" with `String m -> m | _ -> error `Bad_request "which model?" in
  match Providers.find_model providers ~provider:name ~model with
  | None -> error `Not_found "%s / %s is not configured (save the providers first)" name model
  | Some (p, m) -> (
      let t0 = Unix.gettimeofday () in
      match Model.next_tokens w.model p m ~retries:1 ~prompt:"Say hello." ~text:"" ~expected:None () with
      | cands, _ ->
          json
            (`Assoc
               [ ("ok", `Bool true);
                 ("ms", `Int (int_of_float ((Unix.gettimeofday () -. t0) *. 1000.)));
                 ( "top",
                   `List
                     (List.filteri (fun i _ -> i < 5) cands
                     |> List.map (fun (c : Model.candidate) -> `Assoc [ ("token", `String c.token); ("p", `Float (exp c.logprob)) ])) ) ])
      | exception (Failure e | Model.Unscorable e) -> json (`Assoc [ ("ok", `Bool false); ("error", `String e) ]))

let handle cfg (w : Worldlines.t) req body =
  check_request cfg req;
  let uri = Uri.of_string (Http.Request.resource req) in
  let query name = Uri.get_query_param uri name in
  let providers = w.model.providers in
  match Http.Request.meth req, String.split_on_char '/' (Uri.path uri) with
  | `GET, [ ""; "" ] -> respond ~kind:"text/html; charset=utf-8" (cfg.page ())
  | `GET, [ ""; "api"; "config" ] ->
      json
        (`Assoc
           [ ("prompt", `String cfg.prompt); ("prefix", `String cfg.prefix); ("data", `String cfg.data);
             ("models", models_json providers) ])
  | `GET, [ ""; "api"; "state" ] ->
      let since = match query "from" with Some s -> ( try int_of_string s with _ -> 0) | None -> 0 in
      json (Worldlines.state_json w since)
  | `GET, [ ""; "api"; "node" ] -> (
      match Option.bind (query "id") int_of_string_opt with
      | Some i when i >= 0 && i < Worldlines.size w -> json (Worldlines.node_json w i)
      | _ -> error `Bad_request "bad or unknown node id")
  | `POST, [ ""; "api"; "grow" ] -> grow w (read_json body)
  | `POST, [ ""; "api"; "stop" ] ->
      Worldlines.stop w;
      json (Worldlines.state_json w (Worldlines.size w))
  | `GET, [ ""; "api"; "providers" ] -> json (Providers.view providers)
  | `PUT, [ ""; "api"; "providers" ] -> put_providers w (read_json body)
  | `PUT, [ ""; "api"; "providers"; name; "key" ] -> (
      match Providers.find providers name, field (read_json body) "key" with
      | None, _ -> error `Not_found "no provider %s (save the providers first)" name
      | Some p, `String k -> (
          match Providers.set_key providers p k with
          | () -> json (Providers.view providers)
          | exception Providers.Invalid e -> error `Bad_request "%s" e
          | exception Sys_error e -> error `Internal_server_error "%s" e)
      | Some _, _ -> error `Bad_request "send {\"key\": \"...\"}")
  | `POST, [ ""; "api"; "providers"; name; "test" ] -> test w name (read_json body)
  | _ -> error `Not_found "not found"

let callback cfg w _conn req body =
  try handle cfg w req body with
  | Http_error (status, msg) -> json ~status (`Assoc [ ("error", `String msg) ])
  | Sys_error e -> json ~status:`Internal_server_error (`Assoc [ ("error", `String e) ])
