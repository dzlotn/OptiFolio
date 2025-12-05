(** Stock cache management for storing and retrieving stock analysis data *)

open Analytics

(** Cached stock data with analysis summary *)
type cached_stock = {
  symbol : string;        (** Stock ticker symbol (uppercase) *)
  summary : summary;      (** Financial analysis summary *)
  cum_log_return : float; (** Cumulative log return *)
  last_updated : string; (** ISO date string (YYYY-MM-DD) *)
}

(** Path to the cache file *)
val cache_file : string

(** [load_cache ()] loads the stock cache from disk.
    Returns an empty cache if the file doesn't exist or is invalid.
    @return A hashtable mapping stock symbols to cached stock data *)
val load_cache : unit -> (string, cached_stock) Hashtbl.t

(** [save_cache cache] saves the stock cache to disk.
    @param cache The cache hashtable to save *)
val save_cache : (string, cached_stock) Hashtbl.t -> unit

(** [get_stock_from_cache cache symbol] retrieves a stock from the cache.
    Symbol is automatically uppercased for lookup.
    @param cache The cache hashtable
    @param symbol The stock ticker symbol
    @return [Some stock] if found, [None] otherwise *)
val get_stock_from_cache :
  (string, cached_stock) Hashtbl.t -> string -> cached_stock option

(** [update_cache cache symbol summary cum_log_return] adds or updates a stock
    in the cache with the current date as last_updated.
    @param cache The cache hashtable to update
    @param symbol The stock ticker symbol (will be uppercased)
    @param summary The financial analysis summary
    @param cum_log_return The cumulative log return *)
val update_cache :
  (string, cached_stock) Hashtbl.t ->
  string ->
  summary ->
  float ->
  unit

(** [get_all_cached_stocks cache] returns all stocks in the cache as a list.
    @param cache The cache hashtable
    @return A list of all cached stocks *)
val get_all_cached_stocks :
  (string, cached_stock) Hashtbl.t -> cached_stock list

(** [current_date_string ()] returns the current date as an ISO string (YYYY-MM-DD).
    @return Current date in YYYY-MM-DD format *)
val current_date_string : unit -> string

