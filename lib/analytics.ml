(* 
 * Representation Invariant (RI):
 * - The list is non-empty (enforced by Empty_series exception in functions)
 * - All floats are finite (not NaN or infinity)
 * - Values represent prices, so should be positive (though not strictly enforced)
 * 
 * Abstraction Function (AF):
 * - A series represents a time-ordered sequence of daily closing stock prices
 * - The list [p1; p2; ...; pn] represents prices from oldest to newest
 * - Each float represents one day's closing price in the same currency
 *)
type series = float list

(* 
 * Representation Invariant (RI):
 * - avg_price > 0 (average price must be positive)
 * - volatility >= 0 (volatility is non-negative)
 * - max_drawdown >= 0 and max_drawdown <= 1 (drawdown is a percentage 0-100%)
 * - sharpe can be any float (can be negative for poor risk-adjusted returns)
 * - cumulative_return can be any float (negative for losses)
 * 
 * Abstraction Function (AF):
 * - A summary represents aggregated financial metrics for a stock over a time period
 * - avg_price: average closing price over the period
 * - cumulative_return: total return as a decimal (e.g., 0.15 = 15% return)
 * - volatility: annualized volatility (standard deviation of returns * sqrt(252))
 * - max_drawdown: maximum peak-to-trough decline as a decimal (0.0 to 1.0)
 * - sharpe: risk-adjusted return metric (higher is better, typically -1 to 3)
 *)
type summary = {
  avg_price : float;
  cumulative_return : float;
  volatility : float;
  max_drawdown : float;
  sharpe : float;
}


(*Exception handling*)
exception Empty_series
exception Length_mismatch of string

(*Computes the avg stock price*)
let avg xs =
  match xs with
  | [] -> raise Empty_series
  | _ ->
      let sum = List.fold_left ( +. ) 0.0 xs in
      let count = float_of_int (List.length xs) in
      sum /. count

(*Computes the sum of squares of a list of stock vals*)
let sum_of_squares lst =
  match lst with
  | [] -> raise Empty_series
  | _ ->
      let m = avg lst in
      List.fold_left
        (fun acc x ->
          let d = x -. m in
          acc +. (d *. d))
        0.0 lst

(*Computes the variance of a list of stock vals*)
let variance lst =
  match lst with
  | [] -> raise Empty_series
  | _ ->
      let sos = sum_of_squares lst in
      let n = float_of_int (List.length lst) in
      sos /. n

(*Computes the standard deviation of a list of stock vals*)
let standard_deviation lst =
  let v = variance lst in
  sqrt v

(*Gets lst of prices and returns a lst of percent diffs*)
let simple_return_ratio lst =
  match lst with
  | [] | [ _ ] -> raise Empty_series
  | _ ->
      let rec aux prev xs =
        match xs with
        | [] -> []
        | p :: tl ->
            let r = (p -. prev) /. prev in
            r :: aux p tl
      in
      aux (List.hd lst) (List.tl lst)

(*Gets lst of prices and returns lst of logged percent diffs called simple
  returns*)
let log_return_ratio lst =
  match lst with
  | [] | [ _ ] -> raise Empty_series
  | _ ->
      let rec aux prev xs =
        match xs with
        | [] -> []
        | p :: tl ->
            let r = log (p /. prev) in
            r :: aux p tl
      in
      aux (List.hd lst) (List.tl lst)

(*Gets the cumulative return ratio over the entire lifetime*)
let cumulative_return lst =
  match lst with
  | [] -> raise Empty_series
  | _ ->
      let first = List.hd lst in
      let last = List.hd (List.rev lst) in
      (last -. first) /. first

(*Volatility over one-year period*)
let annualized_volatility returns =
  let sd = standard_deviation returns in
  sd *. sqrt 252.0

(*Calculate cumulative log return: sum of all log returns = log(final/initial)*)
let cumulative_log_return log_returns =
  match log_returns with
  | [] -> 0.0
  | _ -> List.fold_left ( +. ) 0.0 log_returns

(*Calculate max drawdown: maximum peak-to-trough decline*)
let max_drawdown prices =
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

(*Sharpe ratio: (avg return - risk free rate) / volatility
  Using 0% risk-free rate for simplicity*)
let sharpe_ratio returns volatility =
  let avg_return =
    match returns with
    | [] -> 0.0
    | _ -> avg returns *. 252.0
  in
  if volatility > 0.0 then avg_return /. volatility else 0.0
