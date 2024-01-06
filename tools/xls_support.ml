(*open! Core*)
(*open Eio.Std*)
(*open Eio.Fs*)
(*open SZXX*)

(*open! Core
open Eio.Std
open SZXX*)

let to_string x = Yojson.Basic.to_string x
let extract xlsx_path env =
  let ( / ) = Eio.Path.( / ) in
  let print_xlsx xlsx_path =
  (* The Switch receives any parsing errors *)
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_in ~sw (Eio.Stdenv.fs env / xlsx_path) in

  let seq = SZXX.Xlsx.stream_rows_double_pass ~sw file SZXX.Xlsx.yojson_cell_parser in

  (* Extract each row *)
  let rec aux seq acc =
    match Base.Sequence.next seq with
      | None -> List.rev acc
      | Some (row,tail) -> aux tail (List.rev_map to_string (List.rev row.SZXX.Xlsx.data)::acc)
  in
  aux seq []
  in print_xlsx xlsx_path

let extract_xlsx xlsx_path =
  let () = Format.printf "BEGIN@." in
  let l =
    Eio_main.run @@ (extract xlsx_path) in
  l
