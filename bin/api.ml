open Lwt.Syntax

let base_endpoint = "https://www.alphavantage.co/query"
let quote_function = "GLOBAL_QUOTE"
let throttle_seconds = 15.

let diversified_symbols =
  [
    "AAPL";
    "MSFT";
    "NVDA";
    "GOOGL";
    "AMZN";
    "META";
    "TSLA";
    "JPM";
    "BAC";
    "WMT";
    "COST";
    "HD";
    "V";
    "MA";
    "UNH";
    "PEP";
    "KO";
    "XOM";
    "CVX";
    "DIS";
  ]

let read_api_key () =
  match Sys.getenv_opt "ALPHAVANTAGE_API_KEY" with
  | Some key when String.trim key <> "" -> key
  | _ ->
      failwith
        "Set the ALPHAVANTAGE_API_KEY environment variable with your API \
         token from https://www.alphavantage.co/support/#api-key."

type quote =
  { symbol : string
  ; name : string option
  ; exchange : string option
  ; price : float option
  ; change : float option
  ; percent : float option
  ; volume : int option
  }

let string_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_string value) with _ -> None)

let float_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_float value) with _ -> None)

let int_opt field json =
  match Yojson.Safe.Util.member field json with
  | `Null -> None
  | value ->
      (try Some (Yojson.Safe.Util.to_int value) with _ -> None)

let parse_float_string key json =
  match string_opt key json with
  | None -> None
  | Some s -> (
      try Some (float_of_string s) with _ -> None)

let parse_int_string key json =
  match string_opt key json with
  | None -> None
  | Some s -> (
      try Some (int_of_string s) with _ -> None)

let parse_percent_string key json =
  match string_opt key json with
  | None -> None
  | Some s ->
      let trimmed = String.trim s in
      let len = String.length trimmed in
      let numeric =
        if len > 0 && trimmed.[len - 1] = '%'
        then String.sub trimmed 0 (len - 1)
        else trimmed
      in
      try Some (float_of_string numeric) with _ -> None

let decode_quote json =
  let open Yojson.Safe.Util in
  let symbol = json |> member "01. symbol" |> to_string in
  { symbol
  ; name = string_opt "name" json
  ; exchange = string_opt "exchange" json
  ; price = parse_float_string "05. price" json
  ; change = parse_float_string "09. change" json
  ; percent = parse_percent_string "10. change percent" json
  ; volume = parse_int_string "06. volume" json
  }

let fetch_quote ~api_key symbol =
  let uri =
    Uri.add_query_params' (Uri.of_string base_endpoint)
      [ ("function", quote_function)
      ; ("symbol", symbol)
      ; ("apikey", api_key)
      ]
  in
  let* response, body = Cohttp_lwt_unix.Client.get uri in
  let status = Cohttp.Response.status response in
  let* body_str = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.(is_success (code_of_status status)) then (
    try
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      match member "Global Quote" json with
      | `Null | `Assoc [] ->
          let err_msg =
            List.find_map
              (fun key ->
                match member key json with
                | `Null -> None
                | value -> (
                    try Some (to_string value) with _ -> None))
              [ "Note"; "Information"; "Error Message" ]
          in
          let msg =
            match err_msg with
            | Some msg -> msg
            | None ->
                Printf.sprintf
                  "Alpha Vantage response for %s did not contain a Global \
                   Quote payload. Raw body: %s"
                  symbol body_str
          in
          Lwt.return (Error msg)
      | quote_json ->
          let quote = decode_quote quote_json in
          Lwt.return (Ok quote)
    with ex ->
      let msg =
        Printf.sprintf "Failed to parse payload for %s: %s" symbol
          (Printexc.to_string ex)
      in
      Lwt.return (Error msg))
    else
      let msg =
        Printf.sprintf "HTTP %s: %s"
          (Cohttp.Code.string_of_status status)
          (String.trim body_str)
      in
      Lwt.return (Error msg)

let fetch_quotes ~api_key symbols =
  let rec loop acc = function
    | [] -> Lwt.return (Ok (List.rev acc))
    | symbol :: rest ->
        let* result = fetch_quote ~api_key symbol in
        (match result with
        | Error _ as e -> Lwt.return e
        | Ok quote ->
            let* () =
              if rest = [] then Lwt.return_unit
              else Lwt_unix.sleep throttle_seconds
            in
            loop (quote :: acc) rest)
  in
  if symbols = [] then Lwt.return (Error "No ticker symbols supplied.")
  else loop [] symbols

let value_or ~default = function
  | Some v -> v
  | None -> default

let format_float = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%.2f" v

let format_percent = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%.2f%%" v

let format_volume = function
  | None -> "n/a"
  | Some v -> Printf.sprintf "%d" v

let print_quote q =
  let name =
    value_or ~default:"(unknown company)" q.name
  in
  let exchange =
    value_or ~default:"n/a" q.exchange
  in
  Printf.printf
    "%-5s %-22s %-6s price=%8s change=%7s (%8s) volume=%s\n"
    q.symbol name exchange (format_float q.price)
    (format_float q.change) (format_percent q.percent)
    (format_volume q.volume)

let () =
  Lwt_main.run
    (let api_key = read_api_key () in
     let symbols = diversified_symbols in
     let* () =
       Lwt_io.printf
         "Fetching %d symbols from Alpha Vantage (function=%s, %.0fs \
          throttle)...\n%!"
         (List.length symbols) quote_function throttle_seconds
     in
     match%lwt fetch_quotes ~api_key symbols with
     | Ok quotes ->
         List.iter print_quote quotes;
         Lwt.return_unit
     | Error err ->
         Lwt_io.eprintf "Failed to retrieve quotes: %s\n%!" err)

