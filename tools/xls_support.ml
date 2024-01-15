(*open! Core*)
(*open Eio.Std*)
(*open Eio.Fs*)
(*open SZXX*)

(*open! Core
open Eio.Std
open SZXX*)

let ( let* ) = Lwt.Syntax.( let* )
let ( >|= ) = Lwt.Infix.( >|= )

let feed_string ic =
  SZXX.Zip.String
    (fun () ->
      Lwt_io.read ~count:4096 ic >|= function
      | "" -> None (* EOF *)
      | chunk -> Some chunk)

  let print_rows_as_json xlsx_path =
        Lwt_io.with_file ~flags:[Unix.O_RDONLY; Unix.O_NONBLOCK] ~mode:Lwt_io.Input xlsx_path (fun ic ->
            (* yojson_cell_parser is an easy way to quickly inspect a file by mapping XLSX's data types to JSON *)
            let stream, success =
              SZXX.Xlsx.stream_rows_buffer ~feed:(feed_string ic) SZXX.Xlsx.yojson_cell_parser
            in
            (* We create promise called `processed` but we don't await it until later *)
            let processed =
              Lwt_stream.fold (fun row acc_lwt ->
                  let open SZXX.Xlsx in
                  (* `row.data` has type `Yojson.Basic.t array` because of our choice of `cell_parser` *)
                  let all_columns_as_json = Array.to_list row.data in
                  (* Print to stdout *)
                  (List.rev_map (fun x ->
                                  match x with `Null -> "" | `Float a when Float.is_integer a ->
                                  string_of_int (int_of_float a)
                                  | _ -> Yojson.Basic.to_string x) (List.rev all_columns_as_json))::acc_lwt)
                stream []
            in
            (* Bind to/await the `success` promise to catch any error that may have terminated the stream early *)
            let* () = success in
            (* Make sure to bind to/await `processed` AFTER `success` to avoid deadlocks on broken files *)
            processed)

let open_xlsx xlsx_path =
(*  let content = print_rows_as_json xlsx_path in
  match Lwt.state content with
    | Lwt.Return () -> Format.printf "RETURN"
    | Lwt.Fail _ -> Format.printf "FAIL"
    | Lwt.Sleep  -> Format.printf "SLEEP"*)
 List.rev (    Lwt_main.run begin
      Lwt.bind (print_rows_as_json xlsx_path)
          (fun data ->
      Lwt.return data)
end)

(*
let to_string x = Yojson.Basic.to_string x
let extract xlsx_path env =
  (*let ( / ) = Eio.Path.( / ) in*)
  let print_xlsx xlsx_path =
  (* The Switch receives any parsing errors *)
  Lwt.Switch.run @@ fun sw ->
  let file = Lwt.Path.open_in ~sw (Eio.Stdenv.fs env / xlsx_path) in

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
*)
