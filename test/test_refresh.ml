open OUnit2
open Lwt.Syntax
open Refresh
open Stock_cache

(* Helper to get today's date string in ISO format *)
let today_date_string () =
  let time = Unix.time () in
  let tm = Unix.localtime time in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(* Test that API endpoint is accessible and returns valid data *)
let test_api_endpoint_success _ =
  (* Skip test if API key not set *)
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping API endpoint test"
  | Some api_key when String.trim api_key = "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY is empty - skipping API endpoint test"
  | Some api_key ->
      Lwt_main.run
        (let* result = fetch_daily_prices ~api_key "AAPL" in
         match result with
         | Error err ->
             assert_failure
               (Printf.sprintf "API endpoint failed for AAPL: %s" err)
         | Ok prices ->
             (* Verify we got valid price data *)
             assert_bool "Should have at least 30 price points"
               (List.length prices >= 30);
             (* Verify prices are positive *)
             assert_bool "All prices should be positive"
               (List.for_all (fun p -> p > 0.0) prices);
             Lwt.return_unit)

(* Test that analyze_stock produces valid summary *)
let test_analyze_stock_success _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping analyze_stock test"
  | Some api_key ->
      Lwt_main.run
        (let* result = analyze_stock ~api_key "MSFT" in
         match result with
         | Error err ->
             assert_failure
               (Printf.sprintf "analyze_stock failed for MSFT: %s" err)
         | Ok (summary, cum_log_return) ->
             (* Verify summary has valid values *)
             assert_bool "Average price should be positive"
               (summary.avg_price > 0.0);
             assert_bool "Volatility should be non-negative"
               (summary.volatility >= 0.0);
             assert_bool "Volatility should be reasonable (< 1.0)"
               (summary.volatility < 1.0);
             assert_bool "Max drawdown should be non-negative"
               (summary.max_drawdown >= 0.0);
             assert_bool "Max drawdown should be reasonable (< 1.0)"
               (summary.max_drawdown < 1.0);
             (* Verify cum_log_return is a valid float *)
             (* Verify cum_log_return is a valid float (not NaN or infinite) *)
             assert_bool "Cumulative log return should be finite"
               (not (Float.is_nan cum_log_return)
               && not (Float.is_infinite cum_log_return));
             Lwt.return_unit)

(* Test refresh_single_stock updates cache with today's date *)
let test_refresh_single_stock_cache _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping cache test"
  | Some api_key ->
      Lwt_main.run
         (let test_symbol = "NVDA" in
         let* result = refresh_single_stock test_symbol in
         match result with
         | None ->
             assert_failure
               (Printf.sprintf "refresh_single_stock failed for %s" test_symbol)
         | Some (summary, cum_log_return) ->
             (* Load cache after refresh *)
             let cache_after = load_cache () in
             (* Check that stock is in cache *)
             let stock_opt =
               get_stock_from_cache cache_after test_symbol
             in
             (match stock_opt with
             | None ->
                 assert_failure
                   (Printf.sprintf "%s not found in cache after refresh"
                      test_symbol)
             | Some stock ->
                 (* Verify last_updated is today's date *)
                 let today = today_date_string () in
                 assert_equal ~printer:(fun x -> x) today stock.last_updated
                   ~msg:
                     (Printf.sprintf
                        "last_updated should be %s but got %s" today
                        stock.last_updated);
                 (* Verify summary matches what we got *)
                 assert_bool "Summary avg_price should match"
                   (Float.equal stock.summary.avg_price summary.avg_price);
                 assert_bool "Summary volatility should match"
                   (Float.equal stock.summary.volatility summary.volatility);
                 (* Verify cum_log_return matches *)
                 assert_bool "cum_log_return should match"
                   (Float.equal stock.cum_log_return cum_log_return);
                 (* Verify symbol is uppercase *)
                 assert_equal ~printer:(fun x -> x) test_symbol stock.symbol
                   ~msg:"Symbol should be uppercase");
             Lwt.return_unit)

(* Test that cache file exists and is valid JSON after refresh *)
let test_cache_file_valid _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping cache file test"
  | Some api_key ->
      Lwt_main.run
        (let test_symbol = "GOOGL" in
         (* Refresh a stock to ensure cache file exists *)
         let* result = refresh_single_stock test_symbol in
         match result with
         | None ->
             assert_failure
               (Printf.sprintf "refresh_single_stock failed for %s" test_symbol)
         | Some _ ->
             (* Verify cache file exists *)
             assert_bool "Cache file should exist"
               (Sys.file_exists cache_file);
             (* Load cache and verify it's valid *)
             let cache = load_cache () in
             (* Verify test symbol is in cache *)
             let stock_opt = get_stock_from_cache cache test_symbol in
             (match stock_opt with
             | None ->
                 assert_failure
                   (Printf.sprintf "%s not in cache file" test_symbol)
             | Some stock ->
                 (* Verify all required fields are present *)
                 assert_bool "Symbol should not be empty"
                   (stock.symbol <> "");
                 assert_bool "Average price should be positive"
                   (stock.summary.avg_price > 0.0);
                 assert_bool "Last updated should not be empty"
                   (stock.last_updated <> "");
                 (* Verify last_updated format (YYYY-MM-DD) *)
                 let date_parts = String.split_on_char '-' stock.last_updated in
                 assert_equal ~printer:string_of_int 3 (List.length date_parts)
                   ~msg:"last_updated should be in YYYY-MM-DD format");
             Lwt.return_unit)

(* Test that refresh_stock_cache updates multiple stocks *)
let test_refresh_stock_cache_multiple _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping batch refresh test"
  | Some _ ->
      Lwt_main.run
        (let test_symbols = [ "AAPL"; "MSFT" ] in
         (* Refresh cache with test symbols *)
         let* () = refresh_stock_cache ~symbols:test_symbols () in
         (* Load cache and verify both stocks are present *)
         let cache = load_cache () in
         let today = today_date_string () in
         List.iter
           (fun symbol ->
             let stock_opt = get_stock_from_cache cache symbol in
             match stock_opt with
             | None ->
                 assert_failure
                   (Printf.sprintf "%s not found in cache after batch refresh"
                      symbol)
             | Some stock ->
                 (* Verify last_updated is today *)
                 assert_equal ~printer:(fun x -> x) today stock.last_updated
                   ~msg:
                     (Printf.sprintf
                        "%s last_updated should be %s but got %s" symbol today
                        stock.last_updated);
                 (* Verify stock has valid data *)
                 assert_bool
                   (Printf.sprintf "%s should have positive avg_price" symbol)
                   (stock.summary.avg_price > 0.0))
           test_symbols;
         Lwt.return_unit)

(* Test that invalid stock symbol returns appropriate error *)
let test_invalid_stock_symbol _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping invalid symbol test"
  | Some api_key ->
      Lwt_main.run
        (let invalid_symbol = "INVALID123XYZ" in
         let* result = refresh_single_stock invalid_symbol in
         (* Should return None for invalid symbol *)
         match result with
         | Some _ ->
             assert_failure
               (Printf.sprintf
                  "refresh_single_stock should return None for invalid symbol \
                   %s"
                  invalid_symbol)
         | None -> Lwt.return_unit)

let suite =
  "refresh_tests"
  >::: [
         "api_endpoint_success" >:: test_api_endpoint_success;
         "analyze_stock_success" >:: test_analyze_stock_success;
         "refresh_single_stock_cache" >:: test_refresh_single_stock_cache;
         "cache_file_valid" >:: test_cache_file_valid;
         "refresh_stock_cache_multiple" >:: test_refresh_stock_cache_multiple;
         "invalid_stock_symbol" >:: test_invalid_stock_symbol;
       ]

let _ = run_test_tt_main suite

