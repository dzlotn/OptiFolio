open Lwt.Syntax

let base_endpoint = "https://www.alphavantage.co/query"
let daily_function = "TIME_SERIES_DAILY"

let read_api_key () =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | Some key when String.trim key <> "" -> key
  | _ ->
      failwith
        "Set the ALPHAVANTAGE_API_KEY environment variable with your API token \
         from https://www.alphavantage.co/support/#api-key."

