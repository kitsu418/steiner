(* How a model family wraps the assistant's answer. The answer must be followed by [end_token] in the scored prompt, so
   the distribution for the token after the text can be found; [system] is sent before the prompt unless a model's
   configuration gives its own. *)

type t = {
  name : string;
  system : unit -> string option;
  end_token : string;  (* closes the assistant's text in the scored prompt *)
}

let today () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let gpt_oss_system () =
  Some
    (String.concat "\n"
       [ "You are ChatGPT, a large language model trained by OpenAI.";
         "Knowledge cutoff: 2024-06";
         "Current date: " ^ today ();
         "";
         "Reasoning: low";
         "";
         "# Valid channels: analysis, commentary, final. Channel must be included for every message." ])

let all =
  [ { name = "qwen"; system = (fun () -> None); end_token = "<|im_end|>" };  (* ChatML; trims trailing whitespace *)
    { name = "gpt-oss"; system = gpt_oss_system; end_token = "<|end|>" }  (* harmony: the final channel message *) ]

let names = List.map (fun b -> b.name) all
let find name = List.find_opt (fun b -> b.name = name) all

(* The backend a model name suggests, for a model added without one. *)
let guess model = if String.length model >= 7 && String.sub model 0 7 = "gpt-oss" then "gpt-oss" else "qwen"
