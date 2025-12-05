open OUnit2
open Analytics 

(* Helper floats equality *)
let eps = 1e-6
let float_eq a b = abs_float (a -. b) < eps

let test_avg _ =
  assert_raises Empty_series (fun () -> avg []);
  assert_bool "avg simple" (float_eq (avg [ 1.0; 2.0; 3.0 ]) 2.0)

let test_sum_of_squares _ =
  assert_raises Empty_series (fun () -> sum_of_squares []);
  assert_bool "sos" (float_eq (sum_of_squares [ 1.; 2.; 3. ]) 2.)

let test_variance _ =
  assert_raises Empty_series (fun () -> variance []);
  assert_bool "variance" (float_eq (variance [ 1.; 2.; 3. ]) (2.0 /. 3.0))

let test_stddev _ =
  assert_bool "stddev"
    (float_eq (standard_deviation [ 1.; 2.; 3. ]) (sqrt (2.0 /. 3.0)))

let test_simple_return_ratio _ =
  assert_raises Empty_series (fun () -> simple_return_ratio []);
  assert_raises Empty_series (fun () -> simple_return_ratio [ 5.0 ]);

  let r = simple_return_ratio [ 100.; 110.; 121. ] in
  assert_equal 2 (List.length r);
  assert_bool "r1" (float_eq (List.nth r 0) 0.10);
  assert_bool "r2" (float_eq (List.nth r 1) 0.10)

let test_log_return_ratio _ =
  assert_raises Empty_series (fun () -> log_return_ratio []);
  assert_raises Empty_series (fun () -> log_return_ratio [ 5.0 ]);

  let r = log_return_ratio [ 100.; 110.; 121. ] in
  assert_equal 2 (List.length r);
  assert_bool "log r1" (float_eq (List.nth r 0) (log 1.1));
  assert_bool "log r2" (float_eq (List.nth r 1) (log 1.1));

  let s = cumulative_log_return r in
  assert_bool "log property" (float_eq s (log (121. /. 100.)))

let test_cumulative_return _ =
  assert_raises Empty_series (fun () -> cumulative_return []);
  assert_bool "cum return" (float_eq (cumulative_return [ 100.; 120. ]) 0.20)

let test_annualized_volatility _ =
  let sd = standard_deviation [ 1.; 2.; 3. ] in
  assert_bool "ann vol"
    (float_eq (annualized_volatility [ 1.; 2.; 3. ]) (sd *. sqrt 252.))

let test_cumulative_log_return _ =
  assert_bool "empty log returns" (float_eq (cumulative_log_return []) 0.0);
  let lr = [ log 1.1; log 1.2 ] in
  assert_bool "sum" (float_eq (cumulative_log_return lr) (log 1.1 +. log 1.2))

let test_max_drawdown _ =
  assert_bool "empty" (float_eq (max_drawdown []) 0.0);
  assert_bool "mdd" (float_eq (max_drawdown [ 100.; 90.; 80.; 70.; 85. ]) 0.30)

let test_sharpe_ratio _ =
  let returns = [ 0.01; 0.02; 0.00; 0.01 ] in
  let vol = standard_deviation returns in
  let expected = avg returns *. 252.0 /. vol in
  assert_bool "sharpe" (float_eq (sharpe_ratio returns vol) expected);

  assert_bool "zero vol" (float_eq (sharpe_ratio [ 1.0 ] 0.0) 0.0);
  
  assert_bool "empty returns" (float_eq (sharpe_ratio [] 0.1) 0.0);
  assert_bool "empty returns with zero vol" (float_eq (sharpe_ratio [] 0.0) 0.0)

let suite =
  "stock_tests"
  >::: [
         "avg" >:: test_avg;
         "sum_of_squares" >:: test_sum_of_squares;
         "variance" >:: test_variance;
         "stddev" >:: test_stddev;
         "simple_returns" >:: test_simple_return_ratio;
         "log_returns" >:: test_log_return_ratio;
         "cumulative_return" >:: test_cumulative_return;
         "annual_vol" >:: test_annualized_volatility;
         "cumulative_log" >:: test_cumulative_log_return;
         "max_drawdown" >:: test_max_drawdown;
         "sharpe" >:: test_sharpe_ratio;
       ]

let _ = run_test_tt_main suite
