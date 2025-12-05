open Lwt.Syntax

(* Refresh command - separate executable without GUI dependency *)
let () =
  let symbols =
    if Array.length Sys.argv > 1 then
      let symbols_str = Sys.argv.(1) in
      let symbols_list =
        symbols_str |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      if symbols_list = [] then None else Some symbols_list
    else None
  in
  Lwt_main.run (Refresh.refresh_stock_cache ~symbols ())
