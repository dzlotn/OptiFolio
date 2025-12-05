(** Stock data refresh module for fetching and updating stock data from Alpha Vantage API *)

open Analytics
open Stock_cache

(** Default list of diversified stock symbols for portfolio analysis *)
val default_symbols : string list

(** [read_api_key ()] reads the Alpha Vantage API key from the ALPHAVANTAGE_API_KEY
    environment variable.
    @raise Failure if the API key is not set or is empty
    @return The API key string *)
val read_api_key : unit -> string

(** [fetch_daily_prices ~api_key symbol] fetches daily price data for a stock symbol
    from the Alpha Vantage API.
    @param api_key The Alpha Vantage API key
    @param symbol The stock ticker symbol
    @return [Ok prices] if successful, [Error message] if the request fails *)
val fetch_daily_prices :
  api_key:string -> string -> (float list, string) result Lwt.t

(** [analyze_stock ~api_key symbol] fetches and analyzes a stock, computing
    all financial metrics. Requires at least 30 data points for meaningful analysis.
    @param api_key The Alpha Vantage API key
    @param symbol The stock ticker symbol
    @return [Ok (summary, cum_log_return)] if successful,
            [Error message] if analysis fails or data is insufficient *)
val analyze_stock :
  api_key:string -> string -> (summary * float, string) result Lwt.t

(** [refresh_stock_cache ?symbols ()] refreshes the stock cache by fetching
    and analyzing stocks from the Alpha Vantage API. Updates the cache file
    with the latest data.
    @param symbols Optional list of symbols to refresh. If [None], uses [default_symbols].
    @return A promise that completes when all stocks have been processed *)
val refresh_stock_cache : ?symbols:string list option -> unit -> unit Lwt.t

(** [refresh_single_stock symbol] refreshes a single stock and updates the cache.
    Useful for on-demand updates of individual stocks.
    @param symbol The stock ticker symbol to refresh
    @return [Some (summary, cum_log_return)] if successful, [None] if the stock
            doesn't exist or analysis fails *)
val refresh_single_stock : string -> (summary * float) option Lwt.t

