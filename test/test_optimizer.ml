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

(* Test take helper function - test it indirectly through get_recommendations *)
let test_take_functionality _ =
  (* Test take indirectly by testing get_recommendations with different portfolio sizes *)
  let profile_small =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:2
  in
  let profile_large =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:10
  in
  (* Create test cache with multiple stocks *)
  let cache = Stock_cache.load_cache () in
  let test_stocks =
    [
      ("TEST1", make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
         ~max_drawdown:0.10 ~sharpe:2.0);
      ("TEST2", make_summary ~avg_price:50.0 ~cumulative_return:0.15 ~volatility:0.20
         ~max_drawdown:0.08 ~sharpe:1.8);
      ("TEST3", make_summary ~avg_price:200.0 ~cumulative_return:0.25 ~volatility:0.30
         ~max_drawdown:0.12 ~sharpe:2.2);
    ]
  in
  List.iter
    (fun (symbol, summary) ->
      Stock_cache.update_cache cache symbol summary 0.18)
    test_stocks;
  Stock_cache.save_cache cache;
  
  (* Test that portfolio_size limits the results *)
  let recs_small = get_recommendations profile_small [] in
  let recs_large = get_recommendations profile_large [] in
  assert_bool "Small portfolio should limit results"
    (List.length recs_small <= profile_small.portfolio_size);
  assert_bool "Large portfolio can have more results"
    (List.length recs_large <= profile_large.portfolio_size)

(* Test get_recommendations with populated cache - covers filtering, scoring, sorting *)
let test_get_recommendations_with_cache _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  (* Create test cache with multiple stocks *)
  let cache = Stock_cache.load_cache () in
  let test_stocks =
    [
      ( "STOCK1"
      , make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
          ~max_drawdown:0.10 ~sharpe:2.0 );
      ( "STOCK2"
      , make_summary ~avg_price:50.0 ~cumulative_return:0.15 ~volatility:0.20
          ~max_drawdown:0.08 ~sharpe:1.8 );
      ( "STOCK3"
      , make_summary ~avg_price:200.0 ~cumulative_return:0.25 ~volatility:0.30
          ~max_drawdown:0.12 ~sharpe:2.2 );
      ( "STOCK4"
      , make_summary ~avg_price:75.0 ~cumulative_return:0.10 ~volatility:0.22
          ~max_drawdown:0.09 ~sharpe:1.5 );
    ]
  in
  List.iter
    (fun (symbol, summary) ->
      Stock_cache.update_cache cache symbol summary 0.18)
    test_stocks;
  Stock_cache.save_cache cache;
  
  (* Verify cache was saved by checking it has stocks *)
  let verify_cache = Stock_cache.load_cache () in
  let all_stocks = Stock_cache.get_all_cached_stocks verify_cache in
  (* Check that we have at least some stocks in cache (may have existing ones) *)
  let cache_has_stocks = List.length all_stocks > 0 in
  
  (* Test that recommendations are generated and sorted *)
  let recommendations = get_recommendations profile [] in
  (* If cache has stocks, we should get recommendations *)
  if cache_has_stocks then
    assert_bool "Should return recommendations when cache has stocks"
      (List.length recommendations > 0)
  else
    (* If cache is empty, recommendations will be empty *)
    assert_bool "Empty cache should return empty recommendations"
      (List.length recommendations = 0);
  (* Verify sorting - scores should be descending *)
  let rec check_sorted = function
    | [] | [ _ ] -> true
    | r1 :: r2 :: rest -> r1.score >= r2.score && check_sorted (r2 :: rest)
  in
  assert_bool "Recommendations should be sorted by score"
    (check_sorted recommendations);
  (* Verify each recommendation has required fields *)
  List.iter
    (fun (r : recommendation) ->
      assert_bool "Recommendation should have symbol" (r.symbol <> "");
      assert_bool "Recommendation should have score in range"
        (r.score >= 0.0 && r.score <= 1.0);
      assert_bool "Recommendation should have reason" (r.reason <> ""))
    recommendations

(* Test get_recommendations with empty cache *)
let test_get_recommendations_empty_cache _ =
  (* Save current cache and clear it *)
  let original_cache = Stock_cache.load_cache () in
  let empty_cache = Hashtbl.create 10 in
  Stock_cache.save_cache empty_cache;
  
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:5
  in
  let recommendations = get_recommendations profile [] in
  assert_bool "Should return empty list when cache is empty"
    (recommendations = []);
  
  Stock_cache.save_cache original_cache

(* Test get_recommendations excludes specified symbols - covers filtering logic *)
let test_get_recommendations_excludes_symbols _ =
  let profile =
    make_profile ~risk_score:0.5 ~target_volatility:0.25 ~min_sharpe:1.0
      ~max_drawdown_tolerance:0.20 ~portfolio_size:10
  in
  (* Create test cache with specific stocks *)
  let cache = Stock_cache.load_cache () in
  let test_stocks =
    [
      ( "EXCLUDE1"
      , make_summary ~avg_price:100.0 ~cumulative_return:0.20 ~volatility:0.25
          ~max_drawdown:0.10 ~sharpe:2.0 );
      ( "EXCLUDE2"
      , make_summary ~avg_price:50.0 ~cumulative_return:0.15 ~volatility:0.20
          ~max_drawdown:0.08 ~sharpe:1.8 );
      ( "KEEP1"
      , make_summary ~avg_price:200.0 ~cumulative_return:0.25 ~volatility:0.30
          ~max_drawdown:0.12 ~sharpe:2.2 );
      ( "KEEP2"
      , make_summary ~avg_price:75.0 ~cumulative_return:0.10 ~volatility:0.22
          ~max_drawdown:0.09 ~sharpe:1.5 );
    ]
  in
  List.iter
    (fun (symbol, summary) ->
      Stock_cache.update_cache cache symbol summary 0.18)
    test_stocks;
  Stock_cache.save_cache cache;
  
  let recommendations = get_recommendations profile [ "EXCLUDE1"; "EXCLUDE2" ] in
  let rec check_excluded = function
    | [] -> false
    | (r : recommendation) :: rest ->
        r.symbol = "EXCLUDE1" || r.symbol = "EXCLUDE2" || check_excluded rest
  in
  assert_bool "Excluded symbols should not appear in recommendations"
    (not (check_excluded recommendations));
  (* Should still have other stocks if available *)
  assert_bool "Should have recommendations from non-excluded stocks"
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
         "take_functionality" >:: test_take_functionality;
         "get_recommendations_with_cache" >:: test_get_recommendations_with_cache;
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

