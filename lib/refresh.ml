open Lwt.Syntax
open Stock_cache
open Analytics

(* Default diversified stock symbols *)
let default_symbols =
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

let base_endpoint = "https://www.alphavantage.co/query"
let daily_function = "TIME_SERIES_DAILY"

(* Read API key from environment *)
let read_api_key () =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | Some key when String.trim key <> "" -> key
  | _ ->
      failwith
        "Set the ALPHAVANTAGE_API_KEY environment variable with your API token \
         from https://www.alphavantage.co/support/#api-key."

(* Fetch daily prices - duplicated from api.ml for lib access *)
let fetch_daily_prices ~api_key symbol =
  let uri =
    Uri.add_query_params'
      (Uri.of_string base_endpoint)
      [
        ("function", daily_function);
        ("symbol", symbol);
        ("outputsize", "compact");
        ("apikey", api_key);
      ]
  in
  let* response, body = Cohttp_lwt_unix.Client.get uri in
  let status = Cohttp.Response.status response in
  let* body_str = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.(is_success (code_of_status status)) then
    try
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      (* Check for error messages first *)
      let err_msg =
        List.find_map
          (fun key ->
            match member key json with
            | `Null -> None
            | value -> ( try Some (Yojson.Safe.Util.to_string value) with _ -> None))
          [ "Note"; "Information"; "Error Message" ]
      in
      (match err_msg with
      | Some msg when String.length msg > 0 ->
          Lwt.return
            (Error
               (Printf.sprintf "Alpha Vantage API error for %s: %s" symbol msg))
      | _ -> (
          match member "Time Series (Daily)" json with
          | `Null | `Assoc [] ->
              Lwt.return
                (Error
                   (Printf.sprintf
                      "Alpha Vantage response for %s did not contain Time Series \
                       (Daily) data. Stock may not exist."
                      symbol))
          | `Assoc time_series ->
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
                   (Printf.sprintf "Unexpected format in Time Series (Daily) for %s"
                      symbol))))
    with ex ->
      let msg =
        Printf.sprintf "Failed to parse daily prices for %s: %s" symbol
          (Printexc.to_string ex)
      in
      Lwt.return (Error msg)
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
      (* Validate we have enough data points for meaningful analysis *)
      if List.length prices < 30 then
        Lwt.return
          (Error
             (Printf.sprintf
                "Insufficient data: only %d price points (need at least 30)"
                (List.length prices)))
      else
        try
          let returns = Analytics.simple_return_ratio prices in
          let log_returns = Analytics.log_return_ratio prices in
          let volatility = Analytics.annualized_volatility returns in
          let cum_return = Analytics.cumulative_return prices in
          let avg_price = Analytics.avg prices in
          let cum_log_return = Analytics.cumulative_log_return log_returns in
          let max_drawdown = Analytics.max_drawdown prices in
          let sharpe = Analytics.sharpe_ratio returns volatility in

          (* Validate the results are reasonable (not all zeros or invalid) *)
          if avg_price <= 0.0 || volatility < 0.0 || volatility > 10.0 then
            Lwt.return
              (Error
                 (Printf.sprintf
                    "Invalid data: avg_price=%.2f, volatility=%.2f (stock may not exist)"
                    avg_price volatility))
          else
            let summary : Analytics.summary =
              {
                avg_price;
                cumulative_return = cum_return;
                volatility;
                max_drawdown;
                sharpe;
              }
            in
            Lwt.return (Ok (summary, cum_log_return))
        with Analytics.Empty_series ->
          Lwt.return (Error "Insufficient data for analysis"))

let throttle_seconds = 15.

(* Refresh stock cache with API data *)
let refresh_stock_cache ?(symbols : string list option = None) () : unit Lwt.t =
  let api_key = read_api_key () in
  let cache = load_cache () in

  (* Determine which symbols to refresh *)
  let symbols_to_refresh =
    match symbols with
    | Some syms -> syms
    | None -> default_symbols
  in

  let* () =
    Lwt_io.printf
      "Refreshing stock cache for %d stocks (this will take a while due to rate \
       limits, ~%.0fs per stock)...\n%!"
      (List.length symbols_to_refresh) throttle_seconds
  in

  let rec refresh_loop = function
    | [] -> Lwt.return_unit
    | symbol :: rest ->
        let symbol_upper = String.uppercase_ascii symbol in
        let* () = Lwt_io.printf "Fetching data for %s...\n%!" symbol_upper in
        let* analysis_result = analyze_stock ~api_key symbol_upper in

        let* () =
          match analysis_result with
          | Ok (summary, cum_log_return) ->
              update_cache cache symbol_upper summary cum_log_return;
              Lwt_io.printf "✓ Updated %s in cache\n%!" symbol_upper
          | Error err ->
              Lwt_io.eprintf "✗ Failed to analyze %s: %s\n%!" symbol_upper err
        in

        (* Throttle between API calls *)
        let* () =
          if rest = [] then Lwt.return_unit else Lwt_unix.sleep throttle_seconds
        in

        refresh_loop rest
  in

  let* () = refresh_loop symbols_to_refresh in
  save_cache cache;
  Lwt_io.printf "\n✓ Cache refresh complete! Saved to %s\n%!" cache_file

(* Helper function to check if a string contains a substring *)
let rec contains_substring str substr =
  if String.length substr > String.length str then false
  else if String.length substr = 0 then true
  else
    let rec check_pos pos =
      if pos + String.length substr > String.length str then false
      else if String.sub str pos (String.length substr) = substr then true
      else check_pos (pos + 1)
    in
    check_pos 0

(* Check if error message indicates invalid stock *)
let is_invalid_stock_error err =
  let err_lower = String.lowercase_ascii err in
  (* Alpha Vantage returns "Invalid API call" for invalid symbols *)
  contains_substring err_lower "invalid api call"
  || contains_substring err_lower "not exist"
  || contains_substring err_lower "stock may not exist"

(* Refresh a single stock (for on-demand API calls) *)
let refresh_single_stock (symbol : string) : (summary * float) option Lwt.t =
  let api_key = read_api_key () in
  let symbol_upper = String.uppercase_ascii symbol in
  let* analysis_result = analyze_stock ~api_key symbol_upper in
  match analysis_result with
  | Ok (summary, cum_log_return) ->
      let cache = load_cache () in
      update_cache cache symbol_upper summary cum_log_return;
      save_cache cache;
      Lwt.return (Some (summary, cum_log_return))
  | Error err ->
      (* User-friendly error message *)
      let user_message =
        if is_invalid_stock_error err then
          Printf.sprintf "\n\nStock symbol '%s' does not exist or is invalid" symbol_upper
        else if contains_substring (String.lowercase_ascii err) "insufficient data" then
          Printf.sprintf "Insufficient data available for '%s'" symbol_upper
        else
          Printf.sprintf "Unable to fetch data for '%s'" symbol_upper
      in
      let* () = Lwt_io.printf "%s\n%!" user_message in
      Lwt.return None

