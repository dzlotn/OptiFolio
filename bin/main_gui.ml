open Lwt.Syntax
open Questionnaire_types
open Optimizer

(* GUI version - separate executable with GUI support *)
let run_gui_safe () =
  try Gui.run_gui ()
  with exn ->
    Printf.eprintf
      "Error starting GUI: %s\n\
       Please ensure SDL2 and tsdl are properly installed:\n\
      \  opam install tsdl\n"
      (Printexc.to_string exn);
    exit 1

let () = run_gui_safe ()
