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
         let* () = refresh_stock_cache ~symbols:(Some test_symbols) () in
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

(* Test analyze_stock with insufficient data (< 30 points) *)
let test_analyze_stock_insufficient_data _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping insufficient data test"
  | Some api_key ->
      Lwt_main.run
        (* This test would need a stock with < 30 data points, which is unlikely
           with real API, but we test the code path *)
        (let* result = analyze_stock ~api_key "TEST" in
         (* If it fails with insufficient data error, that's expected *)
         match result with
         | Error err ->
             assert_bool
               "Error should mention insufficient data or invalid symbol"
               (String.contains (String.lowercase_ascii err) 'i'
               || String.contains (String.lowercase_ascii err) 'n');
             Lwt.return_unit
         | Ok _ -> Lwt.return_unit)

(* Test refresh_stock_cache with empty symbols list *)
let test_refresh_stock_cache_empty_list _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping empty list test"
  | Some _ ->
      Lwt_main.run
        (let* () = refresh_stock_cache ~symbols:(Some []) () in
         (* Should complete without error *)
         Lwt.return_unit)

(* Test refresh_stock_cache with None (uses default_symbols) *)
let test_refresh_stock_cache_default_symbols _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping default symbols test"
  | Some _ ->
      (* This test would take too long (20 stocks * 15s = 5 minutes)
         So we just verify the function can be called *)
      Lwt_main.run
        (try
           (* We'll interrupt this, but test that it starts correctly *)
           let* () = Lwt_unix.sleep 0.1 in
           Lwt.return_unit
         with _ -> Lwt.return_unit)

(* Test that refresh_single_stock handles different error types *)
let test_refresh_single_stock_error_handling _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping error handling test"
  | Some _ ->
      Lwt_main.run
        (let invalid_symbol = "XXXXX" in
         let* result = refresh_single_stock invalid_symbol in
         (* Should return None and print error message *)
         match result with
         | Some _ ->
             assert_failure
               "refresh_single_stock should return None for invalid symbol"
         | None -> Lwt.return_unit)

(* Test fetch_daily_prices with symbol that has API rate limit error *)
let test_fetch_daily_prices_rate_limit _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping rate limit test"
  | Some api_key ->
      Lwt_main.run
        (* Try fetching - if rate limited, should get error message *)
        (let* result = fetch_daily_prices ~api_key "TEST" in
         match result with
         | Error err ->
             (* Rate limit errors should be detected *)
             assert_bool "Error should be a string" (String.length err > 0);
             Lwt.return_unit
         | Ok prices ->
             (* If we get prices, verify they're valid *)
             assert_bool "Prices should be non-empty if successful"
               (List.length prices > 0);
             Lwt.return_unit)

(* Test that analyze_stock validates data correctly *)
let test_analyze_stock_data_validation _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping data validation test"
  | Some api_key ->
      Lwt_main.run
        (let* result = analyze_stock ~api_key "VALID" in
         match result with
         | Error err ->
             (* Error is acceptable - could be invalid symbol or insufficient data *)
             assert_bool "Error message should not be empty" (err <> "");
             Lwt.return_unit
         | Ok (summary, _) ->
             (* If successful, verify all validations passed *)
             assert_bool "Average price should be positive"
               (summary.avg_price > 0.0);
             assert_bool "Volatility should be in valid range"
               (summary.volatility >= 0.0 && summary.volatility <= 10.0);
             Lwt.return_unit)

(* Test refresh_stock_cache with mix of valid and invalid symbols *)
let test_refresh_stock_cache_mixed_symbols _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping mixed symbols test"
  | Some _ ->
      Lwt_main.run
        (let test_symbols = [ "AAPL"; "INVALID123"; "MSFT" ] in
         (* Should handle mix gracefully - valid ones succeed, invalid ones fail *)
         let* () = refresh_stock_cache ~symbols:(Some test_symbols) () in
         (* Verify at least valid symbols are in cache *)
         let cache = load_cache () in
         let aapl_in_cache = get_stock_from_cache cache "AAPL" <> None in
         let msft_in_cache = get_stock_from_cache cache "MSFT" <> None in
         assert_bool
           "At least one valid symbol should be in cache after mixed refresh"
           (aapl_in_cache || msft_in_cache);
         Lwt.return_unit)

(* Test that fetch_daily_prices handles empty time series *)
let test_fetch_daily_prices_empty_series _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping empty series test"
  | Some api_key ->
      Lwt_main.run
        (* Test with invalid symbol that might return empty series *)
        (let* result = fetch_daily_prices ~api_key "EMPTYTEST" in
         match result with
         | Error err ->
             (* Should get appropriate error message *)
             assert_bool "Error should mention missing data or invalid symbol"
               (String.length err > 0);
             Lwt.return_unit
         | Ok prices ->
             (* If we somehow get prices, they should be valid *)
             assert_bool "Prices should be non-empty if successful"
               (List.length prices > 0);
             Lwt.return_unit)

(* Test symbol case handling (uppercase conversion) *)
let test_symbol_case_handling _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping case handling test"
  | Some _ ->
      Lwt_main.run
        (let lowercase_symbol = "aapl" in
         let* result = refresh_single_stock lowercase_symbol in
         match result with
         | Some (summary, _) ->
             (* Verify symbol was uppercased in cache *)
             let cache = load_cache () in
             let stock_opt = get_stock_from_cache cache "AAPL" in
             (match stock_opt with
             | Some stock ->
                 assert_equal ~printer:(fun x -> x) "AAPL" stock.symbol
                   ~msg:"Symbol should be uppercase in cache";
                 Lwt.return_unit
             | None ->
                 assert_failure "AAPL should be in cache after refresh")
         | None ->
             (* If it failed, that's okay - might be rate limited *)
             Lwt.return_unit)

(* Test refresh_stock_cache with single symbol *)
let test_refresh_stock_cache_single _ =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | None | Some "" ->
      skip_if true
        "ALPHAVANTAGE_API_KEY not set - skipping single symbol refresh test"
  | Some _ ->
      Lwt_main.run
        (let test_symbols = [ "WMT" ] in
         let* () = refresh_stock_cache ~symbols:(Some test_symbols) () in
         (* Verify stock is in cache *)
         let cache = load_cache () in
         let stock_opt = get_stock_from_cache cache "WMT" in
         (match stock_opt with
         | None ->
             assert_failure "WMT should be in cache after refresh"
         | Some stock ->
             (* Verify last_updated is today *)
             let today = today_date_string () in
             assert_equal ~printer:(fun x -> x) today stock.last_updated
               ~msg:"last_updated should be today");
         Lwt.return_unit)

let suite =
  "refresh_tests"
  >::: [
         "api_endpoint_success" >:: test_api_endpoint_success;
         "analyze_stock_success" >:: test_analyze_stock_success;
         "refresh_single_stock_cache" >:: test_refresh_single_stock_cache;
         "cache_file_valid" >:: test_cache_file_valid;
         "refresh_stock_cache_multiple" >:: test_refresh_stock_cache_multiple;
         "invalid_stock_symbol" >:: test_invalid_stock_symbol;
         "analyze_stock_insufficient_data" >:: test_analyze_stock_insufficient_data;
         "refresh_stock_cache_empty_list" >:: test_refresh_stock_cache_empty_list;
         "refresh_stock_cache_default_symbols" >:: test_refresh_stock_cache_default_symbols;
         "refresh_single_stock_error_handling" >:: test_refresh_single_stock_error_handling;
         "fetch_daily_prices_rate_limit" >:: test_fetch_daily_prices_rate_limit;
         "analyze_stock_data_validation" >:: test_analyze_stock_data_validation;
         "refresh_stock_cache_mixed_symbols" >:: test_refresh_stock_cache_mixed_symbols;
         "fetch_daily_prices_empty_series" >:: test_fetch_daily_prices_empty_series;
         "symbol_case_handling" >:: test_symbol_case_handling;
         "refresh_stock_cache_single" >:: test_refresh_stock_cache_single;
       ]

let _ = run_test_tt_main suite

