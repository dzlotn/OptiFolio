(** Risk profile calculation from questionnaire responses *)

open Questionnaire_types

(** Risk profile with numeric scores for portfolio optimization *)
type risk_profile = {
  risk_score : float;              (** Risk score from 0.0 (conservative) to 1.0 (aggressive) *)
  target_volatility : float;       (** Target annualized volatility (0.0 to 1.0) *)
  min_sharpe : float;              (** Minimum acceptable Sharpe ratio *)
  max_drawdown_tolerance : float;  (** Maximum acceptable drawdown (0.0 to 1.0) *)
  portfolio_size : int;            (** Number of stocks desired in portfolio *)
}

(** [calculate_risk_profile responses] converts questionnaire responses into a
    numeric risk profile. Calculates risk score based on risk tolerance, investment
    goal, experience, time horizon, and willingness to accept losses. Derives
    target volatility, minimum Sharpe ratio, and drawdown tolerance from the risk score.
    @param responses The user's questionnaire responses
    @return A risk profile with numeric values for portfolio optimization *)
val calculate_risk_profile : questionnaire_responses -> risk_profile

(** [risk_profile_to_string profile] converts a risk profile to a formatted string
    for display purposes.
    @param profile The risk profile to convert
    @return A formatted string representation of the risk profile *)
val risk_profile_to_string : risk_profile -> string

