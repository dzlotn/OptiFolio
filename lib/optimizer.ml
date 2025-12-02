open Analytics
open Risk_profile
open Stock_cache

(* Stock recommendation with score *)
type recommendation = {
  symbol : string;
  score : float;
  summary : summary;
  reason : string;
}

(* Calculate how well a stock matches the risk profile *)
let calculate_stock_score (profile : risk_profile) (stock : cached_stock) : float =
  let volatility_score =
    (* Prefer stocks with volatility close to target *)
    let diff = abs_float (stock.summary.volatility -. profile.target_volatility) in
    max 0.0 (1.0 -. (diff /. profile.target_volatility))
  in

  let sharpe_score =
    (* Prefer stocks with higher Sharpe ratio *)
    if stock.summary.sharpe >= profile.min_sharpe then
      min 1.0 (stock.summary.sharpe /. 2.0)
    else 0.0
  in

  let drawdown_score =
    (* Prefer stocks with lower drawdown (within tolerance) *)
    if stock.summary.max_drawdown <= profile.max_drawdown_tolerance then
      1.0 -. (stock.summary.max_drawdown /. profile.max_drawdown_tolerance)
    else 0.0
  in

  let return_score =
    (* Prefer stocks with positive returns *)
    max 0.0 (min 1.0 (stock.summary.cumulative_return +. 0.5))
  in

  (* Weighted combination *)
  (volatility_score *. 0.3) +. (sharpe_score *. 0.3) +. (drawdown_score *. 0.2)
  +. (return_score *. 0.2)

(* Generate reason for recommendation *)
let generate_reason (profile : risk_profile) (stock : cached_stock) (score : float)
    : string =
  let reasons = ref [] in
  if stock.summary.volatility <= profile.target_volatility *. 1.2 then
    reasons := "Good volatility match" :: !reasons;
  if stock.summary.sharpe >= profile.min_sharpe then
    reasons := "Strong risk-adjusted returns" :: !reasons;
  if stock.summary.max_drawdown <= profile.max_drawdown_tolerance then
    reasons := "Acceptable drawdown" :: !reasons;
  if stock.summary.cumulative_return > 0.0 then
    reasons := "Positive returns" :: !reasons;
  if !reasons = [] then "Moderate fit for risk profile"
  else String.concat ", " !reasons

(* Check if current stock matches risk profile *)
let check_stock_against_profile (profile : risk_profile) (stock : cached_stock) :
    bool * string =
  let issues = ref [] in
  if stock.summary.volatility > profile.target_volatility *. 1.5 then
    issues :=
      Printf.sprintf "Volatility (%.2f%%) exceeds target (%.2f%%)"
        (stock.summary.volatility *. 100.0)
        (profile.target_volatility *. 100.0)
      :: !issues;
  if stock.summary.sharpe < profile.min_sharpe then
    issues :=
      Printf.sprintf "Sharpe ratio (%.2f) below minimum (%.2f)" stock.summary.sharpe
        profile.min_sharpe
      :: !issues;
  if stock.summary.max_drawdown > profile.max_drawdown_tolerance then
    issues :=
      Printf.sprintf "Max drawdown (%.2f%%) exceeds tolerance (%.2f%%)"
        (stock.summary.max_drawdown *. 100.0)
        (profile.max_drawdown_tolerance *. 100.0)
      :: !issues;
  match !issues with
  | [] -> (true, "Stock matches your risk profile")
  | _ -> (false, String.concat "; " !issues)

(* Helper function to take first n elements *)
let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs

(* Get recommendations based on risk profile *)
let get_recommendations (profile : risk_profile)
    (exclude_symbols : string list) : recommendation list =
  let cache = Stock_cache.load_cache () in
  let all_stocks = Stock_cache.get_all_cached_stocks cache in
  let exclude_upper =
    List.map String.uppercase_ascii exclude_symbols
  in
  let candidates =
    List.filter
      (fun stock -> not (List.mem stock.Stock_cache.symbol exclude_upper))
      all_stocks
  in
  let scored : recommendation list =
    List.map
      (fun stock ->
        let score = calculate_stock_score profile stock in
        let reason = generate_reason profile stock score in
        {
          symbol = stock.Stock_cache.symbol;
          score;
          summary = stock.Stock_cache.summary;
          reason;
        })
      candidates
  in
  (* Sort by score descending and take top N *)
  let sorted = List.sort (fun a b -> compare b.score a.score) scored in
  take profile.portfolio_size sorted

