(*Defines a stock price series*)
type series = float list

(*Defines a dictionary with summary values*)
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
