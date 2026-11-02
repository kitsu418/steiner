let expand_home path =
  if path = "~" || String.starts_with ~prefix:"~/" path then
    match Sys.getenv_opt "HOME" with
    | Some home -> home ^ String.sub path 1 (String.length path - 1)
    | None -> path
  else path

let rec mkdir_p ?(perm = 0o755) dir =
  if dir <> "" && dir <> "/" && dir <> "." && not (Sys.file_exists dir) then begin
    mkdir_p ~perm (Filename.dirname dir);
    try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* Write [contents] to [path] through a temporary file, so a crash never leaves half a file. *)
let write_atomic ?(perm = 0o644) path contents =
  let tmp = path ^ ".tmp" in
  let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_binary ] perm tmp in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc contents);
  Unix.chmod tmp perm;
  Sys.rename tmp path

let read_file path = In_channel.with_open_bin path In_channel.input_all

let truncate n s = if String.length s <= n then s else String.sub s 0 n ^ "…"

(* The Unicode replacement character: the server's text for a token that is only part of a character. *)
let replacement = "\xEF\xBF\xBD"

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

(* A special token such as <|im_end|> or <|return|>: it ends the answer. *)
let is_special t =
  let n = String.length t in
  n >= 4
  && String.sub t 0 2 = "<|"
  && String.sub t (n - 2) 2 = "|>"
  && not (String.contains (String.sub t 2 (n - 4)) '|')

(* A token the text can go on after: not a special token, not part of a character. *)
let growable t = t <> "" && (not (is_special t)) && not (contains ~sub:replacement t)

let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r' || c = '\x0b' || c = '\x0c'
let ends_in_space s = s <> "" && is_space s.[String.length s - 1]
