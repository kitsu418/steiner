(* Steiner: a chat model's next-token choices as worldlines, a forest of token trees you grow from any node. *)

open Steiner

let usage =
  "steiner [options]\n\n\
   Serves the page on http://127.0.0.1:8800. Providers and models are added on the page (Providers), and kept in\n\
   --providers; API keys are kept apart from it, one file per provider in --keys.\n\nOptions:"

let config_home () =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d when d <> "" -> Filename.concat d "steiner"
  | _ -> Util.expand_home "~/.config/steiner"

let read_opt = function None -> "" | Some f -> Util.read_file f

(* An HTTPS-capable POST, with the operating system's root certificates. *)
let make_http_post ~net =
  let https =
    match Ca_certs.authenticator () with
    | Error (`Msg e) ->
        Printf.eprintf "note: no root certificates (%s): https:// providers will fail\n%!" e;
        None
    | Ok authenticator -> (
        match Tls.Config.client ~authenticator () with
        | Error (`Msg e) -> failwith e
        | Ok tls ->
            Some
              (fun uri raw ->
                let host =
                  Option.bind (Uri.host uri) (fun h ->
                      Result.to_option (Result.bind (Domain_name.of_string h) Domain_name.host))
                in
                Tls_eio.client_of_flow ?host tls raw))
  in
  let client = Cohttp_eio.Client.make ~https net in
  fun ~url ~headers body ->
    Eio.Switch.run @@ fun sw ->
    let resp, reply =
      Cohttp_eio.Client.post client ~sw ~headers:(Http.Header.of_list headers) ~body:(Cohttp_eio.Body.of_string body)
        (Uri.of_string url)
    in
    let text = Eio.Buf_read.(parse_exn take_all) reply ~max_size:(256 * 1024 * 1024) in
    (Http.Status.to_int (Http.Response.status resp), text)

let () =
  let dir = config_home () in
  let providers_file = ref (Filename.concat dir "providers.json") in
  let keys_dir = ref (Filename.concat dir "keys") in
  let data = ref "worldlines.json" in
  let prompt_file = ref None and prefix_file = ref None and page_file = ref None in
  let host = ref "127.0.0.1" and port = ref 8800 in
  Arg.parse
    (Arg.align
       [ ("--providers", Arg.Set_string providers_file, "FILE the providers and models (default: " ^ !providers_file ^ ")");
         ("--keys", Arg.Set_string keys_dir, "DIR where API keys are kept, one file per provider (default: " ^ !keys_dir ^ ")");
         ("--data", Arg.Set_string data, "FILE where the worldlines are kept (default: worldlines.json)");
         ("--prompt-file", Arg.String (fun f -> prompt_file := Some f), "FILE the prompt the page starts with");
         ("--prefix-file", Arg.String (fun f -> prefix_file := Some f), "FILE the start of the answer the page starts with");
         ("--page", Arg.String (fun f -> page_file := Some f), "FILE serve this page instead of the built-in one (for editing it)");
         ("--host", Arg.Set_string host, "ADDR where the page is served (default: 127.0.0.1)");
         ("--port", Arg.Set_int port, "PORT (default: 8800)") ])
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    usage;
  let addr = try Unix.inet_addr_of_string (if !host = "localhost" then "127.0.0.1" else !host) with Failure _ ->
    prerr_endline ("--host must be an IP address, not " ^ !host);
    exit 2
  in
  let loopback = addr = Unix.inet_addr_loopback || addr = Unix.inet6_addr_loopback in
  let providers =
    try Providers.load ~path:(Util.expand_home !providers_file) ~keys_dir:(Util.expand_home !keys_dir)
    with Providers.Invalid e ->
      prerr_endline e;
      exit 2
  in
  let cfg : Api.config =
    { prompt = read_opt !prompt_file;
      prefix = read_opt !prefix_file;
      data = !data;
      page = (match !page_file with None -> fun () -> Page.html | Some f -> fun () -> Util.read_file f);
      loopback;
      port = !port }
  in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env and net = Eio.Stdenv.net env in
  let menv : Model.env =
    { now = (fun () -> Eio.Time.now clock);
      sleep = Eio.Time.sleep clock;
      with_timeout = (fun s f -> Eio.Time.with_timeout_exn clock s f);
      http_post = make_http_post ~net }
  in
  Eio.Switch.run @@ fun sw ->
  let model = Model.create menv providers in
  let worldlines =
    try Worldlines.load ~sw ~path:(Util.expand_home !data) model
    with Failure e ->
      prerr_endline e;
      exit 2
  in
  let socket =
    Eio.Net.listen ~sw ~backlog:64 ~reuse_addr:true net (`Tcp (Eio_unix.Net.Ipaddr.of_unix addr, !port))
  in
  let server = Cohttp_eio.Server.make ~callback:(Api.callback cfg worldlines) () in
  Printf.printf "Steiner: http://%s:%d  (worldlines in %s, providers in %s)\n%!" !host !port !data providers.path;
  if providers.providers = [] then print_endline "no providers yet: add one on the page, under Providers";
  if not loopback then
    print_endline "warning: not a loopback address: anyone who can reach this port can change the providers and keys";
  Cohttp_eio.Server.run socket server ~on_error:(fun e ->
      match e with
      | Eio.Io (Eio.Net.E (Connection_reset _), _) -> ()
      | e -> Printf.eprintf "error: %s\n%!" (Printexc.to_string e))
