open Analytics
open Stock_cache

(** Default list of diversified stock symbols for portfolio analysis *)
val default_symbols : string list

(** [read_api_key ()] reads the Alpha Vantage API key from the ALPHAVANTAGE_API_KEY
    environment variable.
    Raises: [Failure] if the API key is not set or is empty*)
val read_api_key : unit -> string

(** [fetch_daily_prices ~api_key symbol] fetches daily price data for a stock symbol
    from the Alpha Vantage API. *)
val fetch_daily_prices :
  api_key:string -> string -> (float list, string) result Lwt.t

(** [analyze_stock ~api_key symbol] fetches and analyzes a stock, computing
    all financial metrics. Requires at least 30 data points for meaningful analysis.
    @param api_key The Alpha Vantage API key *)
val analyze_stock :
  api_key:string -> string -> (summary * float, string) result Lwt.t

(** [refresh_stock_cache ?symbols ()] refreshes the stock cache by fetching
    and analyzing stocks from the Alpha Vantage API. Updates the cache file
    with the latest data. *)
val refresh_stock_cache : ?symbols:string list option -> unit -> unit Lwt.t

(** [refresh_single_stock symbol] refreshes a single stock and updates the cache.
    Useful for on-demand updates of individual stocks. *)
val refresh_single_stock : string -> (summary * float) option Lwt.t

