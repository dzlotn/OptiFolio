open Analytics
open Risk_profile
open Stock_cache

(** Stock recommendation with scoring and reasoning *)
type recommendation = {
  symbol : string;      (** Stock ticker symbol *)
  score : float;       (** Match score (0.0 to 1.0) *)
  summary : summary;   (** Financial analysis summary *)
  reason : string;     (** Human-readable reason for recommendation *)
}

(** [calculate_stock_score profile stock] calculates how well a stock matches
    a risk profile. Returns a score from 0.0 to 1.0 based on:
    - Volatility match (30% weight)
    - Sharpe ratio (30% weight)
    - Drawdown tolerance (20% weight)
    - Return performance (20% weight) *)
val calculate_stock_score : risk_profile -> cached_stock -> float

(** [generate_reason profile stock score] generates a human-readable reason
    explaining why a stock was recommended. *)
val generate_reason : risk_profile -> cached_stock -> float -> string

(** [check_stock_against_profile profile stock] checks if a stock matches
    the risk profile requirements. Returns a tuple of (matches, message). *)
val check_stock_against_profile :
  risk_profile -> cached_stock -> bool * string

(** [get_recommendations profile exclude_symbols] generates stock recommendations
    based on the risk profile. Loads stocks from cache, scores them, and returns
    the top N stocks sorted by score (descending). *)
val get_recommendations :
  risk_profile -> string list -> recommendation list

