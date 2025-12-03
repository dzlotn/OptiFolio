open Lwt.Syntax
open Questionnaire_types
open Optimizer

(* Converts responses to questionnaire_responses type *)
let get_responses () =
  let responses = Gui.get_responses () in
  if Array.length responses < 8 then raise (Failure "Questionnaire incomplete")
  else
    let investments_str = responses.(7) in
    let investments =
      if investments_str = "" then []
      else
        String.split_on_char ',' investments_str
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
    in
    {
      name = responses.(0);
      goal = parse_goal responses.(1) |> Option.get;
      experience = parse_experience responses.(2) |> Option.get;
      risk = parse_risk responses.(3) |> Option.get;
      horizon = parse_horizon responses.(4) |> Option.get;
      portfolio_size = parse_portfolio_size responses.(5) |> Option.get;
      willing_to_lose = parse_yes_no responses.(6) |> Option.get;
      has_current_investments = investments <> [];
      current_investments =
        (if investments = [] then None else Some investments);
    }

(* Save responses to temporary file *)
let save_responses_to_file responses =
  let response_file = "quiz_responses.tmp" in
  let oc = open_out_bin response_file in
  Marshal.to_channel oc responses [];
  close_out oc

(* GUI version - separate executable with GUI support *)
let run_gui_safe () =
  try
    Gui.run_gui ();
    (* After GUI closes, get and save responses *)
    let parsed = get_responses () in
    save_responses_to_file parsed;
    exit 0
  with
  | Failure _ ->
      Printf.printf "Questionnaire not completed. Exiting.\n";
      exit 0
  | Invalid_argument msg ->
      Printf.eprintf "Invalid input: %s\n" msg;
      exit 1
  | exn ->
      Printf.eprintf
        "Error starting GUI: %s\n\
         Please ensure SDL2 and tsdl are properly installed:\n\
        \  opam install tsdl\n"
        (Printexc.to_string exn);
      exit 1

let () = run_gui_safe ()
