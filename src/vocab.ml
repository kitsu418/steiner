(* A tiktoken vocabulary file (each line: a token's bytes in base64, then its id), such as o200k_base.tiktoken. The
   server sends a token that is only part of a character with no text; its bytes are spelled from here. gpt-oss's
   o200k_harmony is o200k_base with special tokens added, so o200k_base spells its byte pieces. *)

type t = (int, string) Hashtbl.t

let cache : (string, (t, string) result) Hashtbl.t = Hashtbl.create 2

let parse path =
  let table = Hashtbl.create 200_000 in
  In_channel.with_open_bin path (fun ic ->
      let rec go n =
        match In_channel.input_line ic with
        | None -> ()
        | Some line -> (
            match String.split_on_char ' ' (String.trim line) with
            | [ b64; id ] ->
                Hashtbl.replace table (int_of_string id) (Base64.decode_exn b64);
                go (n + 1)
            | [ "" ] -> go (n + 1)
            | _ -> failwith (Printf.sprintf "%s line %d is not \"<base64> <id>\"" path (n + 1)))
      in
      go 1);
  table

let load path =
  let path = Util.expand_home path in
  match Hashtbl.find_opt cache path with
  | Some r -> r
  | None ->
      let r = try Ok (parse path) with Sys_error e | Failure e | Invalid_argument e -> Error e in
      Hashtbl.replace cache path r;
      r
