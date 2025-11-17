open Lwt.Syntax
open FinalProject

let base_endpoint = "https://www.alphavantage.co/query"
let quote_function = "GLOBAL_QUOTE"
let daily_function = "TIME_SERIES_DAILY"
let throttle_seconds = 15.

let diversified_symbols =
  [
    "AAPL";
    "MSFT";
    "NVDA";
    "GOOGL";
    "AMZN";
    "META";
    "TSLA";
    "JPM";
    "BAC";
    "WMT";
    "COST";
    "HD";
    "V";
    "MA";
    "UNH";
    "PEP";
    "KO";
    "XOM";
    "CVX";
    "DIS";
  ]

let read_api_key () =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | Some key when String.trim key <> "" -> key
  | _ ->
      failwith
        "Set the ALPHAVANTAGE_API_KEY environment variable with your API \
         token from https://www.alphavantage.co/support/#api-key."

type quote =
  { symbol : string
  ; name : string option
  ; exchange : string option
  ; price : float option
  ; change : float option
  ; percent : float option
  ; volume : int option
  }

let string_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_string value) with _ -> None)

let float_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_float value) with _ -> None)

let int_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_int value) with _ -> None)

let parse_float_string key json =
  match string_opt key json with
  | None -> None
  | Some s -> (
      try Some (float_of_string s) with _ -> None)

let parse_int_string key json =
  match string_opt key json with
  | None -> None
  | Some s -> (
      try Some (int_of_string s) with _ -> None)

let parse_percent_string key json =
  match string_opt key json with
  | None -> None
  | Some s ->
      let trimmed = String.trim s in
      let len = String.length trimmed in
      let numeric =
        if len > 0 && trimmed.[len - 1] = '%'
        then String.sub trimmed 0 (len - 1)
        else trimmed
      in
      try Some (float_of_string numeric) with _ -> None

let decode_quote json =
  let open Yojson.Safe.Util in
  let symbol = json |> member "01. symbol" |> to_string in
  { symbol
  ; name = string_opt "name" json
  ; exchange = string_opt "exchange" json
  ; price = parse_float_string "05. price" json
  ; change = parse_float_string "09. change" json
  ; percent = parse_percent_string "10. change percent" json
  ; volume = parse_int_string "06. volume" json
  }

let fetch_quote ~api_key symbol =
  let uri =
    Uri.add_query_params' (Uri.of_string base_endpoint)
      [ ("function", quote_function)
      ; ("symbol", symbol)
      ; ("apikey", api_key)
      ]
  in
  let* response, body = Cohttp_lwt_unix.Client.get uri in
  let status = Cohttp.Response.status response in
  let* body_str = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.(is_success (code_of_status status)) then (
    try
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      match member "Global Quote" json with
      | `Null | `Assoc [] ->
          let err_msg =
            List.find_map
              (fun key ->
                match member key json with
                | `Null -> None
                | value -> (
                    try Some (to_string value) with _ -> None))
              [ "Note"; "Information"; "Error Message" ]
          in
          let msg =
            match err_msg with
            | Some msg -> msg
            | None ->
                Printf.sprintf
                  "Alpha Vantage response for %s did not contain a Global \
                   Quote payload. Raw body: %s"
                  symbol body_str
          in
          Lwt.return (Error msg)
      | quote_json ->
          let quote = decode_quote quote_json in
          Lwt.return (Ok quote)
    with ex ->
      let msg =
        Printf.sprintf "Failed to parse payload for %s: %s" symbol
          (Printexc.to_string ex)
      in
      Lwt.return (Error msg))
    else
      let msg =
        Printf.sprintf "HTTP %s: %s"
          (Cohttp.Code.string_of_status status)
          (String.trim body_str)
      in
      Lwt.return (Error msg)

let fetch_quotes ~api_key symbols =
  let rec loop acc = function
    | [] -> Lwt.return (Ok (List.rev acc))
    | symbol :: rest ->
        let* result = fetch_quote ~api_key symbol in
        (match result with
        | Error _ as e -> Lwt.return e
        | Ok quote ->
            let* () =
              if rest = [] then Lwt.return_unit
              else Lwt_unix.sleep throttle_seconds
            in
            loop (quote :: acc) rest)
  in
  if symbols = [] then Lwt.return (Error "No ticker symbols supplied.")
  else loop [] symbols

let value_or ~default = function
  | Some v -> v
  | None -> default

let format_float = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%.2f" v

let format_percent = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%.2f%%" v

let format_volume = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%d" v

let print_quote q =
  let name =
    value_or ~default:"(unknown company)" q.name
  in
  let exchange =
    value_or ~default:"n/a" q.exchange
  in
  Printf.printf
    "%-5s %-22s %-6s price=%8s change=%7s (%8s) volume=%s\n"
    q.symbol name exchange (format_float q.price)
    (format_float q.change) (format_percent q.percent)
    (format_volume q.volume)

(* Fetch daily time series data for historical analysis *)
let fetch_daily_prices ~api_key symbol =
  let uri =
    Uri.add_query_params' (Uri.of_string base_endpoint)
      [ ("function", daily_function)
      ; ("symbol", symbol)
      ; ("outputsize", "compact")
      ; ("apikey", api_key)
      ]
  in
  let* response, body = Cohttp_lwt_unix.Client.get uri in
  let status = Cohttp.Response.status response in
  let* body_str = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.(is_success (code_of_status status)) then (
    try
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      match member "Time Series (Daily)" json with
      | `Null | `Assoc [] ->
          let err_msg =
            List.find_map
              (fun key ->
                match member key json with
                | `Null -> None
                | value -> (
                    try Some (to_string value) with _ -> None))
              [ "Note"; "Information"; "Error Message" ]
          in
          let msg =
            match err_msg with
            | Some msg -> msg
            | None ->
                Printf.sprintf
                  "Alpha Vantage response for %s did not contain Time Series \
                   (Daily) data. Raw body: %s"
                  symbol body_str
          in
          Lwt.return (Error msg)
      | `Assoc time_series ->
          (* Extract closing prices and sort by date (oldest first) *)
          let prices =
            time_series
            |> List.map (fun (date, day_data) ->
                   match day_data with
                   | `Assoc fields ->
                       let close_opt =
                         List.find_map
                           (fun (key, value) ->
                             if key = "4. close" then
                               match value with
                               | `String s -> (
                                   try Some (float_of_string s) with _ -> None)
                               | _ -> None
                             else None)
                           fields
                       in
                       (date, close_opt)
                   | _ -> (date, None))
            |> List.filter_map (fun (date, price_opt) ->
                   match price_opt with
                   | Some p -> Some (date, p)
                   | None -> None)
            |> List.sort (fun (d1, _) (d2, _) -> String.compare d1 d2)
            |> List.map snd
          in
          if prices = [] then
            Lwt.return
              (Error
                 (Printf.sprintf
                    "No valid price data found in time series for %s" symbol))
          else Lwt.return (Ok prices)
      | _ ->
          Lwt.return
            (Error
               (Printf.sprintf
                  "Unexpected format in Time Series (Daily) for %s" symbol))
    with ex ->
      let msg =
        Printf.sprintf "Failed to parse daily prices for %s: %s" symbol
          (Printexc.to_string ex)
      in
      Lwt.return (Error msg))
  else
    let msg =
      Printf.sprintf "HTTP %s: %s"
        (Cohttp.Code.string_of_status status)
        (String.trim body_str)
    in
    Lwt.return (Error msg)

(* Analyze a stock using analytics.ml functions *)
let analyze_stock ~api_key symbol =
  let* prices_result = fetch_daily_prices ~api_key symbol in
  match prices_result with
  | Error err -> Lwt.return (Error err)
  | Ok prices -> (
      try
        let returns = Analytics.simple_return_ratio prices in
        let log_returns = Analytics.log_return_ratio prices in
        let volatility = Analytics.annualized_volatility returns in
        let cum_return = Analytics.cumulative_return prices in
        let avg_price = Analytics.avg prices in
        (* Cumulative log return: sum of all log returns = log(final/initial) *)
        let cum_log_return =
          match log_returns with
          | [] -> 0.0
          | _ -> List.fold_left ( +. ) 0.0 log_returns
        in
        (* Calculate max drawdown: maximum peak-to-trough decline *)
        let max_drawdown =
          let rec calc_max_dd peak max_dd = function
            | [] -> max_dd
            | p :: rest ->
                let new_peak = max peak p in
                let drawdown = (peak -. p) /. peak in
                let new_max_dd = max max_dd drawdown in
                calc_max_dd new_peak new_max_dd rest
          in
          match prices with
          | [] -> 0.0
          | p :: rest -> calc_max_dd p 0.0 rest
        in
        (* Sharpe ratio: (avg return - risk free rate) / volatility
           Using 0% risk-free rate for simplicity *)
        let avg_return =
          match returns with
          | [] -> 0.0
          | _ -> Analytics.avg returns *. 252.0
        in
        let sharpe =
          if volatility > 0.0 then avg_return /. volatility else 0.0
        in
        let summary : Analytics.summary =
          {
            avg_price
          ; cumulative_return = cum_return
          ; volatility
          ; max_drawdown
          ; sharpe
          }
        in
        Lwt.return (Ok (summary, cum_log_return))
      with Analytics.Empty_series ->
        Lwt.return (Error "Insufficient data for analysis"))

let print_analysis symbol (summary : Analytics.summary) cum_log_return =
  Printf.printf "\n=== Analysis for %s ===\n" symbol;
  Printf.printf "Average Price:        $%.2f\n" summary.avg_price;
  Printf.printf "Cumulative Return:    %.2f%%\n"
    (summary.cumulative_return *. 100.0);
  Printf.printf "Cumulative Log Return: %.2f%%\n"
    (cum_log_return *. 100.0);
  Printf.printf "Annualized Volatility: %.2f%%\n"
    (summary.volatility *. 100.0);
  Printf.printf "Max Drawdown:         %.2f%%\n"
    (summary.max_drawdown *. 100.0);
  Printf.printf "Sharpe Ratio:         %.2f\n" summary.sharpe;
  Printf.printf "========================\n"

let () =
  Lwt_main.run
    (let api_key = read_api_key () in
     let sample_symbols = [ "AAPL"; "MSFT"; "NVDA" ] in
     let* () =
       Lwt_io.printf
         "Fetching historical data and analyzing %d stocks (this will take a \
          while due to rate limits, ~%.0fs per stock)...\n%!"
         (List.length sample_symbols) throttle_seconds
     in
     let rec analyze_loop = function
       | [] -> Lwt.return_unit
       | symbol :: rest ->
           let* () =
             Lwt_io.printf "\nAnalyzing %s...\n%!" symbol
           in
           let* analysis_result = analyze_stock ~api_key symbol in
           let* () =
             match analysis_result with
             | Ok (summary, cum_log_return) ->
                 print_analysis symbol summary cum_log_return;
                 Lwt.return_unit
             | Error err ->
                 Lwt_io.eprintf "Failed to analyze %s: %s\n%!" symbol err
           in
           let* () =
             if rest = [] then Lwt.return_unit
             else Lwt_unix.sleep throttle_seconds
           in
           analyze_loop rest
     in
     analyze_loop sample_symbols)

