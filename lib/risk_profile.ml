open Questionnaire_types

(* Risk profile with numeric scores *)
type risk_profile = {
  risk_score : float;
  target_volatility : float;
  min_sharpe : float;
  max_drawdown_tolerance : float;
  portfolio_size : int; 
}

(* Convert questionnaire responses to a risk profile *)
let calculate_risk_profile (responses : questionnaire_responses) : risk_profile =
  (* Base risk score from risk tolerance *)
  let base_risk =
    match responses.risk with
    | Conservative -> 0.2
    | Moderate -> 0.5
    | Aggressive -> 0.8
  in

  (* Adjust based on investment goal *)
  let goal_adjustment =
    match responses.goal with
    | Growth -> 0.15
    | Income -> -0.1
    | Balanced -> 0.0
    | Preservation -> -0.15
  in

  (* Adjust based on experience *)
  let experience_adjustment =
    match responses.experience with
    | Beginner -> -0.1
    | Intermediate -> 0.0
    | Experienced -> 0.1
  in

  (* Adjust based on time horizon *)
  let horizon_adjustment =
    match responses.horizon with
    | Short_term -> -0.15
    | Medium_term -> 0.0
    | Long_term -> 0.15
  in

  (* Adjust based on willingness to lose *)
  let loss_adjustment = if responses.willing_to_lose then 0.1 else -0.1 in

  (* Calculate final risk score (clamped between 0.0 and 1.0) *)
  let risk_score =
    base_risk +. goal_adjustment +. experience_adjustment +. horizon_adjustment
    +. loss_adjustment
    |> max 0.0 |> min 1.0
  in

  (* Target volatility: conservative = 0.15, moderate = 0.25, aggressive = 0.40 *)
  let target_volatility = 0.15 +. (risk_score *. 0.25) in

  (* Minimum Sharpe ratio: higher risk tolerance = lower minimum Sharpe *)
  let min_sharpe = 1.5 -. (risk_score *. 0.5) in

  (* Max drawdown tolerance: conservative = 0.10, aggressive = 0.30 *)
  let max_drawdown_tolerance = 0.10 +. (risk_score *. 0.20) in

  (* Portfolio size - randomly select from the specified range *)
  let portfolio_size =
    match responses.portfolio_size with
    | Small -> 3 + Random.int 3
    | Medium -> 5 + Random.int 6
    | Large -> 10 + Random.int 6
  in

  {
    risk_score;
    target_volatility;
    min_sharpe;
    max_drawdown_tolerance;
    portfolio_size;
  }

(* Convert risk profile to string for display *)
let risk_profile_to_string (profile : risk_profile) : string =
  Printf.sprintf
    "Risk Score: %.2f\nTarget Volatility: %.2f%%\nMinimum Sharpe Ratio: %.2f\nMax \
     Drawdown Tolerance: %.2f%%\nPortfolio Size: %d stocks"
    profile.risk_score (profile.target_volatility *. 100.0) profile.min_sharpe
    (profile.max_drawdown_tolerance *. 100.0)
    profile.portfolio_size

