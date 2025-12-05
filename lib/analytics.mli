(** Financial analytics module for calculating stock metrics *)

(** A series of stock prices over time *)
type series = float list

(** Summary statistics for a stock *)
type summary = {
  avg_price : float;           (** Average price over the period *)
  cumulative_return : float;   (** Total return from first to last price *)
  volatility : float;          (** Annualized volatility *)
  max_drawdown : float;        (** Maximum peak-to-trough decline *)
  sharpe : float;              (** Sharpe ratio (risk-adjusted return) *)
}

(** Exception raised when attempting to compute statistics on an empty series *)
exception Empty_series

(** Exception raised when series lengths don't match *)
exception Length_mismatch of string

(** [avg xs] computes the average of a list of floats.
    @raise Empty_series if the list is empty *)
val avg : float list -> float

(** [sum_of_squares lst] computes the sum of squared deviations from the mean.
    @raise Empty_series if the list is empty *)
val sum_of_squares : float list -> float

(** [variance lst] computes the variance of a list of floats.
    @raise Empty_series if the list is empty *)
val variance : float list -> float

(** [standard_deviation lst] computes the standard deviation of a list of floats.
    @raise Empty_series if the list is empty *)
val standard_deviation : float list -> float

(** [simple_return_ratio prices] computes the simple return ratios between
    consecutive prices. Returns a list of (price[i] - price[i-1]) / price[i-1].
    @raise Empty_series if the list has fewer than 2 elements *)
val simple_return_ratio : float list -> float list

(** [log_return_ratio prices] computes the logarithmic return ratios between
    consecutive prices. Returns a list of log(price[i] / price[i-1]).
    @raise Empty_series if the list has fewer than 2 elements *)
val log_return_ratio : float list -> float list

(** [cumulative_return prices] computes the total return from the first to the
    last price: (last - first) / first.
    @raise Empty_series if the list is empty *)
val cumulative_return : float list -> float

(** [annualized_volatility returns] computes the annualized volatility by
    multiplying the standard deviation of returns by sqrt(252) (trading days per year).
    @raise Empty_series if the returns list is empty *)
val annualized_volatility : float list -> float

(** [cumulative_log_return log_returns] computes the cumulative log return by
    summing all log returns. Returns 0.0 for an empty list. *)
val cumulative_log_return : float list -> float

(** [max_drawdown prices] computes the maximum drawdown (peak-to-trough decline)
    over the price series. Returns 0.0 for an empty list. *)
val max_drawdown : float list -> float

(** [sharpe_ratio returns volatility] computes the Sharpe ratio as
    (average_return * 252) / volatility, using 0% risk-free rate.
    Returns 0.0 if volatility is 0.0. *)
val sharpe_ratio : float list -> float -> float

