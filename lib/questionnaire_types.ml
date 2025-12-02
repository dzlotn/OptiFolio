(* Shared questionnaire types *)
type investment_goal =
  | Growth
  | Income
  | Balanced
  | Preservation

type investment_experience =
  | Beginner
  | Intermediate
  | Experienced

type risk_tolerance =
  | Conservative
  | Moderate
  | Aggressive

type time_horizon =
  | Short_term
  | Medium_term
  | Long_term

type portfolio_size =
  | Small
  | Medium
  | Large

type questionnaire_responses = {
  name : string;
  goal : investment_goal;
  experience : investment_experience;
  risk : risk_tolerance;
  horizon : time_horizon;
  portfolio_size : portfolio_size;
  willing_to_lose : bool;
  has_current_investments : bool;
  current_investments : string list option;
}

