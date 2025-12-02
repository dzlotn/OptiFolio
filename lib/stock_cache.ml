open Analytics

(* Stock analysis data stored in cache *)
type cached_stock = {
  symbol : string;
  summary : summary;
  cum_log_return : float;
  last_updated : string; (* ISO date string *)
}

(* Cache file path *)
let cache_file = "data/stock_cache.json"

(* Ensure data directory exists *)
let ensure_data_dir () =
  try
    if not (Sys.file_exists "data") then Unix.mkdir "data" 0o755
  with Unix.Unix_error _ -> ()

(* Convert cached_stock to JSON *)
let cached_stock_to_json (stock : cached_stock) : Yojson.Safe.t =
  `Assoc
    [
      ("symbol", `String stock.symbol);
      ("avg_price", `Float stock.summary.avg_price);
      ("cumulative_return", `Float stock.summary.cumulative_return);
      ("volatility", `Float stock.summary.volatility);
      ("max_drawdown", `Float stock.summary.max_drawdown);
      ("sharpe", `Float stock.summary.sharpe);
      ("cum_log_return", `Float stock.cum_log_return);
      ("last_updated", `String stock.last_updated);
    ]

(* Convert JSON to cached_stock *)
let json_to_cached_stock (json : Yojson.Safe.t) : cached_stock option =
  try
    let open Yojson.Safe.Util in
    let symbol = json |> member "symbol" |> to_string in
    let avg_price = json |> member "avg_price" |> to_float in
    let cumulative_return = json |> member "cumulative_return" |> to_float in
    let volatility = json |> member "volatility" |> to_float in
    let max_drawdown = json |> member "max_drawdown" |> to_float in
    let sharpe = json |> member "sharpe" |> to_float in
    let cum_log_return = json |> member "cum_log_return" |> to_float in
    let last_updated = json |> member "last_updated" |> to_string in
    let summary : summary =
      {
        avg_price;
        cumulative_return;
        volatility;
        max_drawdown;
        sharpe;
      }
    in
    Some { symbol; summary; cum_log_return; last_updated }
  with _ -> None

(* Load cache from file *)
let load_cache () : (string, cached_stock) Hashtbl.t =
  ensure_data_dir ();
  let cache = Hashtbl.create 100 in
  try
    let json_str = In_channel.with_open_text cache_file In_channel.input_all in
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    (match json with
    | `Assoc stocks ->
        List.iter
          (fun (symbol, stock_json) ->
            match json_to_cached_stock stock_json with
            | Some stock -> Hashtbl.add cache symbol stock
            | None -> ())
          stocks
    | _ -> ());
    cache
  with _ -> cache

(* Save cache to file *)
let save_cache (cache : (string, cached_stock) Hashtbl.t) : unit =
  ensure_data_dir ();
  let stocks_json =
    Hashtbl.fold
      (fun symbol stock acc ->
        (symbol, cached_stock_to_json stock) :: acc)
      cache []
    |> List.rev |> fun lst -> `Assoc lst
  in
  let json_str = Yojson.Safe.pretty_to_string stocks_json in
  Out_channel.with_open_text cache_file (fun oc -> Out_channel.output_string oc json_str)

(* Get current date as ISO string *)
let current_date_string () : string =
  let time = Unix.time () in
  let tm = Unix.localtime time in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(* Get stock from cache *)
let get_stock_from_cache (cache : (string, cached_stock) Hashtbl.t)
    (symbol : string) : cached_stock option =
  Hashtbl.find_opt cache (String.uppercase_ascii symbol)

(* Add or update stock in cache *)
let update_cache (cache : (string, cached_stock) Hashtbl.t) (symbol : string)
    (summary : summary) (cum_log_return : float) : unit =
  let symbol_upper = String.uppercase_ascii symbol in
  let stock : cached_stock =
    {
      symbol = symbol_upper;
      summary;
      cum_log_return;
      last_updated = current_date_string ();
    }
  in
  Hashtbl.replace cache symbol_upper stock

(* Get all stocks from cache as a list *)
let get_all_cached_stocks (cache : (string, cached_stock) Hashtbl.t) :
    cached_stock list =
  Hashtbl.fold (fun _ stock acc -> stock :: acc) cache []

