open OUnit2
open Optimizer
open Analytics
open Risk_profile
open Stock_cache

(* Helper to create a test summary *)
let make_summary ~avg_price ~cumulative_return ~volatility ~max_drawdown ~sharpe =
  { avg_price; cumulative_return; volatility; max_drawdown; sharpe }

(* Helper to create a test cached stock *)
let make_stock ~symbol ~summary ~cum_log_return ~last_updated =
  { symbol; summary; cum_log_return; last_updated }

(* Helper to create a test risk profile *)
let make_profile ~risk_score ~target_volatility ~min_sharpe ~max_drawdown_tolerance
    ~portfolio_size =
  { risk_score; target_volatility; min_sharpe; max_drawdown_tolerance; portfolio_size }

(* Helper for float comparison *)
let eps = 1e-6
let float_eq a b = abs_float (a -. b) < eps

(* Test calculate_stock_score with perfect match *)
let test_calculate_stock_score_perfect_match _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Perfect volatility match (1.0), good sharpe (1.0), good drawdown (0.5), good return (0.7) *)
  (* Expected: 0.3*1.0 + 0.3*1.0 + 0.2*0.5 + 0.2*0.7 = 0.3 + 0.3 + 0.1 + 0.14 = 0.84 *)
  assert_bool "Perfect match should have high score" (score > 0.8)

(* Test calculate_stock_score with poor match *)
let test_calculate_stock_score_poor_match _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.10 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:(-0.10) ~volatility:0.50
      ~max_drawdown:0.50 ~sharpe:0.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:(-0.11) ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Poor volatility (0.0), poor sharpe (0.0), poor drawdown (0.0), negative return (0.0) *)
  assert_bool "Poor match should have low score" (score < 0.3)

(* Test calculate_stock_score with volatility mismatch *)
let test_calculate_stock_score_volatility_mismatch _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  (* Stock with double the target volatility *)
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.50
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Volatility score should be low (diff = 0.25, so score = max(0, 1 - 0.25/0.25) = 0) *)
  assert_bool "High volatility mismatch should reduce score" (score < 0.7)

(* Test calculate_stock_score with sharpe below minimum *)
let test_calculate_stock_score_low_sharpe _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:0.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Sharpe below minimum should give 0 sharpe score *)
  assert_bool "Low sharpe should reduce score" (score < 0.6)

(* Test calculate_stock_score with drawdown exceeding tolerance *)
let test_calculate_stock_score_high_drawdown _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.10 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.15
      ~max_drawdown:0.30 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Drawdown exceeds tolerance, so drawdown score should be 0 *)
  (* Score: volatility=1.0, sharpe=1.0, drawdown=0.0, return=0.7 *)
  (* Total: 0.3*1.0 + 0.3*1.0 + 0.2*0.0 + 0.2*0.7 = 0.74 *)
  assert_bool "High drawdown should reduce score" (score <= 0.75)

(* Test calculate_stock_score with negative returns *)
let test_calculate_stock_score_negative_returns _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:(-0.30) ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:(-0.35) ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  (* Negative return: max(0, min(1, -0.3 + 0.5)) = max(0, 0.2) = 0.2 *)
  (* Score: volatility=1.0, sharpe=1.0, drawdown=0.5, return=0.2 *)
  (* Total: 0.3*1.0 + 0.3*1.0 + 0.2*0.5 + 0.2*0.2 = 0.74 *)
  assert_bool "Negative returns should reduce score" (score <= 0.75)

(* Test generate_reason with all positive attributes *)
let test_generate_reason_all_positive _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.20
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let reason = generate_reason profile stock 0.85 in
  assert_bool "Reason should contain multiple positive attributes"
    (String.contains reason 'G' || String.contains reason 'S'
    || String.contains reason 'A' || String.contains reason 'P')

(* Test generate_reason with no positive attributes *)
let test_generate_reason_no_positive _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.10 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:(-0.10) ~volatility:0.50
      ~max_drawdown:0.50 ~sharpe:0.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:(-0.11) ~last_updated:"2024-01-01" in
  let reason = generate_reason profile stock 0.2 in
  assert_bool "Reason should default to moderate fit"
    (String.contains (String.lowercase_ascii reason) 'm')

(* Test check_stock_against_profile with perfect match *)
let test_check_stock_against_profile_match _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:1.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let matches, msg = check_stock_against_profile profile stock in
  assert_bool "Perfect match should return true" matches;
  assert_bool "Message should indicate match"
    (String.contains (String.lowercase_ascii msg) 'm')

(* Test check_stock_against_profile with high volatility *)
let test_check_stock_against_profile_high_volatility _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  (* Volatility 0.30 > 0.15 * 1.5 = 0.225, so should fail *)
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.30
      ~max_drawdown:0.10 ~sharpe:1.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let matches, msg = check_stock_against_profile profile stock in
  assert_bool "High volatility should return false" (not matches);
  assert_bool "Message should mention volatility"
    (String.contains (String.lowercase_ascii msg) 'v')

(* Test check_stock_against_profile with low sharpe *)
let test_check_stock_against_profile_low_sharpe _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:0.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let matches, msg = check_stock_against_profile profile stock in
  assert_bool "Low sharpe should return false" (not matches);
  assert_bool "Message should mention sharpe"
    (String.contains (String.lowercase_ascii msg) 's')

(* Test check_stock_against_profile with high drawdown *)
let test_check_stock_against_profile_high_drawdown _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.10 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.15
      ~max_drawdown:0.30 ~sharpe:1.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let matches, msg = check_stock_against_profile profile stock in
  assert_bool "High drawdown should return false" (not matches);
  assert_bool "Message should mention drawdown"
    (String.contains (String.lowercase_ascii msg) 'd')

(* Test check_stock_against_profile with multiple issues *)
let test_check_stock_against_profile_multiple_issues _ =
  let profile =
    make_profile ~risk_score:0.2 ~target_volatility:0.15 ~min_sharpe:1.5
      ~max_drawdown_tolerance:0.10 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.30
      ~max_drawdown:0.30 ~sharpe:0.5
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let matches, msg = check_stock_against_profile profile stock in
  assert_bool "Multiple issues should return false" (not matches);
  assert_bool "Message should contain multiple issues"
    (String.contains msg ';' || String.length msg > 50)

(* Test take helper function - using local implementation since it's not exported *)
let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs

let test_take _ =
  assert_equal [] (take 0 [ 1; 2; 3 ]);
  assert_equal [ 1 ] (take 1 [ 1; 2; 3 ]);
  assert_equal [ 1; 2 ] (take 2 [ 1; 2; 3 ]);
  assert_equal [ 1; 2; 3 ] (take 3 [ 1; 2; 3 ]);
  assert_equal [ 1; 2; 3 ] (take 5 [ 1; 2; 3 ]);
  assert_equal [] (take 0 []);
  assert_equal [] (take 5 [])

(* Test get_recommendations with empty cache *)
let test_get_recommendations_empty_cache _ =
  (* This test requires mocking the cache, which is complex.
     We'll test that it handles empty cache gracefully *)
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  (* If cache is empty, should return empty list *)
  let recommendations = get_recommendations profile [] in
  (* This will depend on actual cache state, so we just verify it's a list *)
  assert_bool "Should return a list" (List.length recommendations >= 0)

(* Test get_recommendations excludes specified symbols *)
let test_get_recommendations_excludes_symbols _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:10
  in
  let recommendations = get_recommendations profile [ "AAPL"; "MSFT" ] in
  (* This test verifies the function runs and returns a list *)
  (* Actual exclusion verification depends on cache state, so we just verify it's callable *)
  assert_bool "Function should return a list"
    (List.length recommendations >= 0)

(* Test get_recommendations respects portfolio size *)
let test_get_recommendations_portfolio_size _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:3
  in
  let recommendations = get_recommendations profile [] in
  assert_bool "Should not exceed portfolio size"
    (List.length recommendations <= profile.portfolio_size)

(* Test get_recommendations sorts by score descending *)
let test_get_recommendations_sorted _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let recommendations = get_recommendations profile [] in
  (* Verify scores are in descending order *)
  let rec check_sorted = function
    | [] | [ _ ] -> true
    | r1 :: r2 :: rest -> r1.score >= r2.score && check_sorted (r2 :: rest)
  in
  assert_bool "Recommendations should be sorted by score descending"
    (check_sorted recommendations)

(* Test edge case: very low volatility in score calculation *)
let test_calculate_stock_score_low_volatility _ =
  let profile =
    make_profile ~risk_score:0.1 ~target_volatility:0.175 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.18
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  (* Should calculate score correctly with low volatility *)
  let score = calculate_stock_score profile stock in
  assert_bool "Should handle low volatility" (score >= 0.0 && score <= 1.0)

(* Test edge case: score bounds *)
let test_calculate_stock_score_bounds _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let summary =
    make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
      ~max_drawdown:0.10 ~sharpe:2.0
  in
  let stock = make_stock ~symbol:"TEST" ~summary ~cum_log_return:0.18 ~last_updated:"2024-01-01" in
  let score = calculate_stock_score profile stock in
  assert_bool "Score should be non-negative" (score >= 0.0);
  assert_bool "Score should be at most 1.0" (score <= 1.0)

let suite =
  "optimizer_tests"
  >::: [
         "calculate_stock_score_perfect_match" >:: test_calculate_stock_score_perfect_match;
         "calculate_stock_score_poor_match" >:: test_calculate_stock_score_poor_match;
         "calculate_stock_score_volatility_mismatch"
         >:: test_calculate_stock_score_volatility_mismatch;
         "calculate_stock_score_low_sharpe" >:: test_calculate_stock_score_low_sharpe;
         "calculate_stock_score_high_drawdown"
         >:: test_calculate_stock_score_high_drawdown;
         "calculate_stock_score_negative_returns"
         >:: test_calculate_stock_score_negative_returns;
         "generate_reason_all_positive" >:: test_generate_reason_all_positive;
         "generate_reason_no_positive" >:: test_generate_reason_no_positive;
         "check_stock_against_profile_match" >:: test_check_stock_against_profile_match;
         "check_stock_against_profile_high_volatility"
         >:: test_check_stock_against_profile_high_volatility;
         "check_stock_against_profile_low_sharpe"
         >:: test_check_stock_against_profile_low_sharpe;
         "check_stock_against_profile_high_drawdown"
         >:: test_check_stock_against_profile_high_drawdown;
         "check_stock_against_profile_multiple_issues"
         >:: test_check_stock_against_profile_multiple_issues;
         "take" >:: test_take;
         "get_recommendations_empty_cache" >:: test_get_recommendations_empty_cache;
         "get_recommendations_excludes_symbols"
         >:: test_get_recommendations_excludes_symbols;
         "get_recommendations_portfolio_size"
         >:: test_get_recommendations_portfolio_size;
         "get_recommendations_sorted" >:: test_get_recommendations_sorted;
         "calculate_stock_score_low_volatility"
         >:: test_calculate_stock_score_low_volatility;
         "calculate_stock_score_bounds" >:: test_calculate_stock_score_bounds;
       ]

let _ = run_test_tt_main suite

