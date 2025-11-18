open Lwt.Syntax

(* All the questions and their potential answers the questionnaire will ask.
   Eventually, the algorithm will likely use these responses and assign values
   to them to quantify a user's investment needs. *)
type investment_goal =
  | Growth
  | Income
  | Balanced
  | Preservation

type investment_experience =
  | Beginner
  | Intermediate
  | Experienced

type risk_tolerance =
  | Conservative
  | Moderate
  | Aggressive

type time_horizon =
  | Short_term
  | Medium_term
  | Long_term

type portfolio_size =
  | Small
  | Medium
  | Large

type questionnaire_responses = {
  name : string;
  goal : investment_goal;
  experience : investment_experience;
  risk : risk_tolerance;
  horizon : time_horizon;
  portfolio_size : portfolio_size;
  willing_to_lose : bool;
  has_current_investments : bool;
  current_investments : string list option;
}

(* reading user input *)
let read_line () =
  let* line = Lwt_io.read_line Lwt_io.stdin in
  Lwt.return (String.trim line)

(* reading user input with a prompt *)
let prompt_input prompt =
  let* () = Lwt_io.printf "%s%!" prompt in
  read_line ()

(* parsing the user's investment goal *)
let parse_goal input =
  match String.lowercase_ascii input with
  | "1" | "growth" -> Some Growth
  | "2" | "income" -> Some Income
  | "3" | "balanced" -> Some Balanced
  | "4" | "preservation" -> Some Preservation
  | _ -> None

(* parsing the user's investment experience *)
let parse_experience input =
  match String.lowercase_ascii input with
  | "1" | "beginner" -> Some Beginner
  | "2" | "intermediate" -> Some Intermediate
  | "3" | "experienced" -> Some Experienced
  | _ -> None

(* parsing the user's risk tolerance *)
let parse_risk input =
  match String.lowercase_ascii input with
  | "1" | "conservative" -> Some Conservative
  | "2" | "moderate" -> Some Moderate
  | "3" | "aggressive" -> Some Aggressive
  | _ -> None

(* parsing the user's time horizon *)
let parse_horizon input =
  match String.lowercase_ascii input with
  | "1" | "short-term" | "short" -> Some Short_term
  | "2" | "medium-term" | "medium" -> Some Medium_term
  | "3" | "long-term" | "long" -> Some Long_term
  | _ -> None

(* parsing the user's portfolio size *)
let parse_portfolio_size input =
  match String.lowercase_ascii input with
  | "1" | "small" -> Some Small
  | "2" | "medium" -> Some Medium
  | "3" | "large" -> Some Large
  | _ -> None

(* parsing the user's yes/no response *)
let parse_yes_no input =
  match String.lowercase_ascii input with
  | "yes" | "y" | "true" -> Some true
  | "no" | "n" | "false" -> Some false
  | _ -> None

(* Question 1: Name *)
let rec ask_name () =
  let* () = Lwt_io.printf "\n==== Question 1 ====\n%!" in
  let* name = prompt_input "1. What is your name? " in
  if name = "" then
    let* () = Lwt_io.printf "Please enter a valid name.\n%!" in
    ask_name ()
  else Lwt.return name

(* Question 2: Investment Goal *)
let rec ask_goal name =
  let* () = Lwt_io.printf "\n==== Question 2 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, what is your primary investment goal?\n%!" name
  in
  let* () = Lwt_io.printf "  1. Growth (capital appreciation)\n%!" in
  let* () = Lwt_io.printf "  2. Income (dividend/regular income)\n%!" in
  let* () = Lwt_io.printf "  3. Balanced (growth and income)\n%!" in
  let* () = Lwt_io.printf "  4. Preservation (protect capital)\n%!" in
  let* input = prompt_input "Enter your choice (1-4): " in
  match parse_goal input with
  | Some goal -> Lwt.return goal
  | None ->
      let* () = Lwt_io.printf "Invalid choice. Please try again.\n%!" in
      ask_goal name

(* Question 3: Investment Experience *)
let rec ask_experience name =
  let* () = Lwt_io.printf "\n==== Question 3 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, how experienced are you with stock investing?\n%!" name
  in
  let* () = Lwt_io.printf "  1. Beginner\n%!" in
  let* () = Lwt_io.printf "  2. Intermediate\n%!" in
  let* () = Lwt_io.printf "  3. Experienced\n%!" in
  let* input = prompt_input "Enter your choice (1-3): " in
  match parse_experience input with
  | Some exp -> Lwt.return exp
  | None ->
      let* () = Lwt_io.printf "Invalid choice. Please try again.\n%!" in
      ask_experience name

(* Question 4: Risk Tolerance *)
let rec ask_risk name =
  let* () = Lwt_io.printf "\n==== Question 4 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, how would you describe your risk tolerance?\n%!" name
  in
  let* () = Lwt_io.printf "  1. Conservative (low risk, stable returns)\n%!" in
  let* () = Lwt_io.printf "  2. Moderate (balanced risk and return)\n%!" in
  let* () =
    Lwt_io.printf "  3. Aggressive (high risk, higher potential returns)\n%!"
  in
  let* input = prompt_input "Enter your choice (1-3): " in
  match parse_risk input with
  | Some risk -> Lwt.return risk
  | None ->
      let* () = Lwt_io.printf "Invalid choice. Please try again.\n%!" in
      ask_risk name

(* Question 5: Time Horizon *)
let rec ask_horizon name =
  let* () = Lwt_io.printf "\n==== Question 5 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, what is your investment time horizon?\n%!" name
  in
  let* () = Lwt_io.printf "  1. Short-term (1-3 years)\n%!" in
  let* () = Lwt_io.printf "  2. Medium-term (3-7 years)\n%!" in
  let* () = Lwt_io.printf "  3. Long-term (7+ years)\n%!" in
  let* input = prompt_input "Enter your choice (1-3): " in
  match parse_horizon input with
  | Some horizon -> Lwt.return horizon
  | None ->
      let* () = Lwt_io.printf "Invalid choice. Please try again.\n%!" in
      ask_horizon name

(* Question 6: Portfolio Size (Diversification) *)
let rec ask_portfolio_size name =
  let* () = Lwt_io.printf "\n==== Question 6 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, how many stocks would you like for diversification?\n%!"
      name
  in
  let* () = Lwt_io.printf "  1. Small (3-5 stocks)\n%!" in
  let* () = Lwt_io.printf "  2. Medium (5-10 stocks)\n%!" in
  let* () = Lwt_io.printf "  3. Large (10+ stocks)\n%!" in
  let* input = prompt_input "Enter your choice (1-3): " in
  match parse_portfolio_size input with
  | Some size -> Lwt.return size
  | None ->
      let* () = Lwt_io.printf "Invalid choice. Please try again.\n%!" in
      ask_portfolio_size name

(* Question 7: Willing to lose money *)
let rec ask_willing_to_lose name =
  let* () = Lwt_io.printf "\n==== Question 7 ====\n%!" in
  let* () =
    Lwt_io.printf
      "%s, are you willing to invest in assets that may temporarily lose \
       money? (yes/no)\n\
       %!"
      name
  in
  let* input = prompt_input "Enter your choice: " in
  match parse_yes_no input with
  | Some willing -> Lwt.return willing
  | None ->
      let* () =
        Lwt_io.printf "Invalid choice. Please enter 'yes' or 'no'.\n%!"
      in
      ask_willing_to_lose name

(* Question 8: Has current investments *)
let rec ask_has_current_investments name =
  let* () = Lwt_io.printf "\n==== Question 8 ====\n%!" in
  let* () =
    Lwt_io.printf "%s, do you currently have investments? (yes/no)\n%!" name
  in
  let* input = prompt_input "Enter your choice: " in
  match parse_yes_no input with
  | Some has_investments -> Lwt.return has_investments
  | None ->
      let* () =
        Lwt_io.printf "Invalid choice. Please enter 'yes' or 'no'.\n%!"
      in
      ask_has_current_investments name

(* Question 9: List current investments (only if yes to question 8) *)
let rec ask_list_investments name =
  let* () = Lwt_io.printf "\n==== Question 9 ====\n%!" in
  let* () =
    Lwt_io.printf
      "%s, please list your current investments (ticker symbols, separated by \
       commas):\n\
       %!"
      name
  in
  let* input = prompt_input "Enter your investments: " in
  if String.trim input = "" then
    let* () =
      Lwt_io.printf
        "Please enter at least one investment. You can enter ticker symbols \
         separated by commas.\n\
         %!"
    in
    ask_list_investments name
  else
    let investments =
      input |> String.split_on_char ',' |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    in
    if investments = [] then
      let* () =
        Lwt_io.printf
          "Please enter at least one valid investment ticker symbol.\n%!"
      in
      ask_list_investments name
    else Lwt.return investments

(* Helper functions to convert types to strings for display *)
let goal_to_string = function
  | Growth -> "Growth"
  | Income -> "Income"
  | Balanced -> "Balanced"
  | Preservation -> "Preservation"

let experience_to_string = function
  | Beginner -> "Beginner"
  | Intermediate -> "Intermediate"
  | Experienced -> "Experienced"

let risk_to_string = function
  | Conservative -> "Conservative"
  | Moderate -> "Moderate"
  | Aggressive -> "Aggressive"

let horizon_to_string = function
  | Short_term -> "Short-term (1-3 years)"
  | Medium_term -> "Medium-term (3-7 years)"
  | Long_term -> "Long-term (7+ years)"

let portfolio_size_to_string = function
  | Small -> "Small (3-5 stocks)"
  | Medium -> "Medium (5-10 stocks)"
  | Large -> "Large (10+ stocks)"

(* running the whole questionnaire *)
let run_questionnaire () =
  let* name = ask_name () in
  let* goal = ask_goal name in
  let* experience = ask_experience name in
  let* risk = ask_risk name in
  let* horizon = ask_horizon name in
  let* portfolio_size = ask_portfolio_size name in
  let* willing_to_lose = ask_willing_to_lose name in
  let* has_current_investments = ask_has_current_investments name in
  let* current_investments =
    if has_current_investments then
      let* investments = ask_list_investments name in
      Lwt.return (Some investments)
    else Lwt.return None
  in
  Lwt.return
    {
      name;
      goal;
      experience;
      risk;
      horizon;
      portfolio_size;
      willing_to_lose;
      has_current_investments;
      current_investments;
    }

(* Print summary of responses *)
let print_summary responses =
  let* () = Lwt_io.printf "\n=== Questionnaire Summary ===\n%!" in
  let* () = Lwt_io.printf "Name: %s\n%!" responses.name in
  let* () =
    Lwt_io.printf "Investment Goal: %s\n%!" (goal_to_string responses.goal)
  in
  let* () =
    Lwt_io.printf "Investment Experience: %s\n%!"
      (experience_to_string responses.experience)
  in
  let* () =
    Lwt_io.printf "Risk Tolerance: %s\n%!" (risk_to_string responses.risk)
  in
  let* () =
    Lwt_io.printf "Time Horizon: %s\n%!" (horizon_to_string responses.horizon)
  in
  let* () =
    Lwt_io.printf "Portfolio Size: %s\n%!"
      (portfolio_size_to_string responses.portfolio_size)
  in
  let* () =
    Lwt_io.printf "Willing to accept temporary losses: %s\n%!"
      (if responses.willing_to_lose then "Yes" else "No")
  in
  let* () =
    Lwt_io.printf "Has current investments: %s\n%!"
      (if responses.has_current_investments then "Yes" else "No")
  in
  let* () =
    match responses.current_investments with
    | Some investments ->
        Lwt_io.printf "Current investments: %s\n%!"
          (String.concat ", " investments)
    | None -> Lwt_io.printf "Current investments: None\n%!"
  in
  Lwt_io.printf "============================\n%!"

(* Main execution *)
let () =
  (* if Array.length Sys.argv > 1 && Sys.argv.(1) = "gui" then Gui.run_gui () *)
  (* else *)
    Lwt_main.run
      (let* () =
         Lwt_io.printf
           "\n\
            === Portfolio Optimizer ===\n\n\
            Welcome! Let's find the perfect portfolio for you.\n\n\
            %!"
       in
       let* responses = run_questionnaire () in
       let* () = print_summary responses in
       let* () =
         Lwt_io.printf "\nThank you, %s! Your responses have been recorded.\n%!"
           responses.name
       in
       let* () = Api.run_api () in

       Lwt.return_unit)
