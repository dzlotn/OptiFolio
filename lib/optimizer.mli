(** Stock portfolio optimizer for generating recommendations based on risk profiles *)

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
    - Return performance (20% weight)
    @param profile The target risk profile
    @param stock The stock to score
    @return A score from 0.0 (poor match) to 1.0 (perfect match) *)
val calculate_stock_score : risk_profile -> cached_stock -> float

(** [generate_reason profile stock score] generates a human-readable reason
    explaining why a stock was recommended.
    @param profile The risk profile used for matching
    @param stock The stock being recommended
    @param score The calculated match score
    @return A string describing the recommendation reasons *)
val generate_reason : risk_profile -> cached_stock -> float -> string

(** [check_stock_against_profile profile stock] checks if a stock matches
    the risk profile requirements. Returns a tuple of (matches, message).
    @param profile The risk profile to check against
    @param stock The stock to check
    @return [(true, message)] if stock matches, [(false, issues)] if it doesn't *)
val check_stock_against_profile :
  risk_profile -> cached_stock -> bool * string

(** [get_recommendations profile exclude_symbols] generates stock recommendations
    based on the risk profile. Loads stocks from cache, scores them, and returns
    the top N stocks sorted by score (descending).
    @param profile The risk profile for matching
    @param exclude_symbols List of stock symbols to exclude from recommendations
    @return A list of recommendations, sorted by score (highest first),
            limited to [profile.portfolio_size] stocks *)
val get_recommendations :
  risk_profile -> string list -> recommendation list

