open Lwt.Syntax
open Questionnaire_types
open Optimizer
open Refresh

(* All the questions and their potential answers the questionnaire will ask. *)

(* reading user input *)
let read_line () =
  let* line = Lwt_io.read_line Lwt_io.stdin in
  Lwt.return (String.trim line)

(* reading user input with a prompt *)
let prompt_input prompt =
  let* () = Lwt_io.printf "%s%!" prompt in
  read_line ()

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
  let trimmed_input = String.trim input in
  if trimmed_input = "" then
    let* () =
      Lwt_io.printf
        "Error: Empty input. Please enter 'yes' or 'no' (you can also use 'y', \
         'n', 'true', 'false', '1', or '2').\n\
         %!"
    in
    ask_willing_to_lose name
  else
    match parse_yes_no trimmed_input with
    | Some willing -> Lwt.return willing
    | None ->
        let* () =
          Lwt_io.printf
            "Error: Invalid input '%s'. Please enter one of the following:\n\
            \  For 'yes': yes, y, true, or 1\n\
            \  For 'no': no, n, false, or 2\n\
             %!"
            trimmed_input
        in
        ask_willing_to_lose name

(* Question 8: List current investments (optional) *)
let rec ask_list_investments name =
  let* () = Lwt_io.printf "\n==== Question 8 ====\n%!" in
  let* () =
    Lwt_io.printf
      "%s, list current investments tickers (comma separated, leave blank if \
       none):\n\
       %!"
      name
  in
  let* input =
    prompt_input "Enter your investments (or press Enter to skip): "
  in
  let investments =
    input |> String.split_on_char ',' |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  Lwt.return investments

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
  let* investments = ask_list_investments name in
  let has_current_investments = investments <> [] in
  let current_investments =
    if has_current_investments then Some investments else None
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
    Lwt_io.printf "Number of Stock Recommendations: %s\n%!"
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

(* Validate stock symbols and return list of invalid ones *)
let validate_stocks (stocks : string list) : string list Lwt.t =
  let cache = Stock_cache.load_cache () in
  let rec validate_loop invalid = function
    | [] -> Lwt.return invalid
    | symbol :: rest -> (
        let symbol_upper = String.uppercase_ascii symbol in
        match Stock_cache.get_stock_from_cache cache symbol_upper with
        | Some _ -> validate_loop invalid rest
        | None -> (
            (* Try to fetch from API *)
            let* result = refresh_single_stock symbol_upper in
            match result with
            | Some _ -> validate_loop invalid rest
            | None -> validate_loop (symbol_upper :: invalid) rest))
  in
  validate_loop [] stocks

(* Validate and allow correction or skipping of stocks *)
let rec validate_and_correct_stocks name (stocks : string list) :
    string list Lwt.t =
  if stocks = [] then Lwt.return []
  else
    let* invalid = validate_stocks stocks in
    if invalid = [] then Lwt.return stocks
    else
      let* () =
        Lwt_io.printf
          "\nThe following stock symbols are invalid or could not be found:\n%!"
      in
      let* () =
        Lwt_list.iter_s (fun sym -> Lwt_io.printf "  - %s\n%!" sym) invalid
      in
      let* () =
        Lwt_io.printf
          "\n\
           Options:\n\
          \  1. Enter corrected ticker symbols (comma separated)\n\
          \  2. Press Enter to skip and continue without these stocks\n\
           %!"
      in
      let* input =
        prompt_input "Enter your choice (or press Enter to skip): "
      in
      if String.trim input = "" then
        (* Skip invalid stocks, return only valid ones *)
        let valid =
          List.filter
            (fun s -> not (List.mem (String.uppercase_ascii s) invalid))
            stocks
        in
        Lwt.return valid
      else
        let corrected =
          input |> String.split_on_char ',' |> List.map String.trim
          |> List.filter (fun s -> s <> "")
        in
        validate_and_correct_stocks name corrected

let get_risk_profile responses =
  let* () = Lwt_io.printf "\n=== Calculating Your Risk Profile ===\n%!" in
  let risk_profile = Risk_profile.calculate_risk_profile responses in
  let* () =
    Lwt_io.printf "%s\n%!" (Risk_profile.risk_profile_to_string risk_profile)
  in

  (* Load cache and check current investments *)
  let cache = Stock_cache.load_cache () in
  let current_stocks =
    match responses.current_investments with
    | Some lst -> lst
    | None -> []
  in

  (* Validate and correct stocks if needed *)
  let* validated_stocks =
    if current_stocks <> [] then
      let* () =
        Lwt_io.printf "\n=== Validating Your Current Investments ===\n%!"
      in
      validate_and_correct_stocks responses.name current_stocks
    else Lwt.return []
  in

  (* Check current stocks against risk profile *)
  let* () =
    if validated_stocks <> [] then
      let* () =
        Lwt_io.printf "\n=== Analyzing Your Current Investments ===\n%!"
      in

      let rec check_stocks = function
        | [] -> Lwt.return_unit
        | symbol :: rest ->
            let symbol_upper = String.uppercase_ascii symbol in
            let* () = Lwt_io.printf "\nChecking %s...\n%!" symbol_upper in
            let* () =
              match Stock_cache.get_stock_from_cache cache symbol_upper with
              | Some stock ->
                  let matches, reason =
                    Optimizer.check_stock_against_profile risk_profile stock
                  in
                  if matches then
                    Lwt_io.printf "✓ %s: %s\n%!" symbol_upper reason
                  else Lwt_io.printf "✗ %s: %s\n%!" symbol_upper reason
              | None -> (
                  let* () =
                    Lwt_io.printf "  %s not in cache, fetching from API...\n%!"
                      symbol_upper
                  in
                  (* Stock not in cache, fetch from API *)
                  let* result = Refresh.refresh_single_stock symbol_upper in
                  match result with
                  | Some (summary, _) ->
                      let stock =
                        {
                          Stock_cache.symbol = symbol_upper;
                          summary;
                          cum_log_return = 0.0;
                          last_updated = Stock_cache.current_date_string ();
                        }
                      in
                      let matches, reason =
                        Optimizer.check_stock_against_profile risk_profile stock
                      in
                      if matches then
                        Lwt_io.printf "✓ %s: %s\n%!" symbol_upper reason
                      else Lwt_io.printf "✗ %s: %s\n%!" symbol_upper reason
                  | None ->
                      (* Error already printed by refresh_single_stock *)
                      Lwt.return_unit)
            in
            check_stocks rest
      in

      check_stocks validated_stocks
    else Lwt.return_unit
  in
  (* Get stock recommendations *)
  let* () =
    Lwt_io.printf
      "\n=== Stock Recommendations Based on Your Risk Profile ===\n%!"
  in
  let recommendations =
    Optimizer.get_recommendations risk_profile validated_stocks
  in
  if recommendations = [] then
    let* () =
      Lwt_io.printf
        "No recommendations available. Please run 'refresh' command to \
         populate the stock cache.\n\
         %!"
    in
    Lwt.return_unit
  else
    let* () =
      Lwt_io.printf "Here are %d stocks that match your risk profile:\n\n%!"
        (List.length recommendations)
    in
    let rec print_recommendations index = function
      | [] -> Lwt.return_unit
      | rec_item :: rest ->
          let* () =
            Lwt_io.printf
              "%d. %s (Score: %.2f)\n\
              \   Reason: %s\n\
              \   Volatility: %.2f%%, Sharpe: %.2f, Max Drawdown: %.2f%%\n\n\
               %!"
              index rec_item.symbol rec_item.score rec_item.reason
              (rec_item.summary.volatility *. 100.0)
              rec_item.summary.sharpe
              (rec_item.summary.max_drawdown *. 100.0)
          in
          print_recommendations (index + 1) rest
    in
    let* () = print_recommendations 1 recommendations in
    let* () =
      Lwt_io.printf "\nThank you, %s! Your portfolio analysis is complete.\n%!"
        responses.name
    in
    Lwt.return_unit

(* Helper function for reading GUI responses *)
let read_file_responses filename =
  let ic = open_in_bin filename in
  let responses = (Marshal.from_channel ic : questionnaire_responses) in
  close_in ic;
  Lwt.return responses

(* Main execution *)
let () =
  (* Check command line arguments *)
  if Array.length Sys.argv > 1 then (
    match Sys.argv.(1) with
    | "refresh" ->
        (* Refresh command - use separate executable without GUI *)
        let symbols =
          if Array.length Sys.argv > 2 then "\"" ^ Sys.argv.(2) ^ "\"" else ""
        in
        let cmd =
          Printf.sprintf "dune exec -- FinalProject-refresh %s" symbols
        in
        let exit_code = Sys.command cmd in
        exit exit_code
    | "gui" ->
        (* Run GUI version - redirect to GUI executable *)
        let exit_code = Sys.command "dune exec -- FinalProject-gui" in
        if exit_code = 0 then
          let response_file = "quiz_responses.tmp" in
          if Sys.file_exists response_file then
            (Lwt_main.run
               (let* responses = read_file_responses response_file in
                let* () =
                  Lwt.catch
                    (fun () -> Lwt.return (Sys.remove response_file))
                    (fun _ -> Lwt.return_unit)
                in
                let* () = print_summary responses in
                (* get_risk_profile will handle validation and optimization *)
                get_risk_profile responses);
             exit 0)
          else (
            Printf.printf "GUI closed without completing questionnaire.\n";
            exit 0)
        else exit exit_code
    | "cli" | "command-line" | "terminal" ->
        (* Explicitly run command-line version *)
        Lwt_main.run
          (let* () =
             Lwt_io.printf
               "\n\
                === Portfolio Optimizer (Command-Line Mode) ===\n\n\
                Welcome! Let's find the perfect portfolio for you.\n\n\
                %!"
           in
           let* responses = run_questionnaire () in
           let* () = print_summary responses in

           (* Calculate risk profile *)
           get_risk_profile responses)
    | _ ->
        (* Unknown command, show help *)
        Printf.printf
          "Usage: FinalProject [command]\n\n\
           Commands:\n\
          \  (no args)  - Run command-line questionnaire (default)\n\
          \  gui        - Run graphical user interface\n\
          \  cli         - Run command-line questionnaire (explicit)\n\
          \  refresh    - Refresh stock cache from API\n\n\
           Examples:\n\
          \  dune exec -- FinalProject           # Run CLI questionnaire\n\
          \  dune exec -- FinalProject gui       # Run GUI\n\
          \  dune exec -- FinalProject refresh AAPL,MSFT\n";
        exit 1)
  else
    (* No arguments - run command-line questionnaire by default *)
    (* Initialize random number generator for portfolio size selection *)
    Random.self_init ();
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

       (* Calculate risk profile *)
       get_risk_profile responses)
