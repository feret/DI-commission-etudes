type orientation = Landscape | Normal
type encoding =
  | HTML | HTML_Tabular | HTML_encapsulated
  | TXT | CSV | XLS | Json | Latex of orientation
  | Latex_encapsulated

module type FormatMap =
sig
  type 'a t
  val add : encoding -> 'a  -> 'a t -> 'a t
  val find : encoding -> 'a t -> 'a
  val empty : 'a t
end

module FormatMap:FormatMap

type t

val devnul: t
val get_encoding_format: t -> encoding
val fprintf: t -> ('a, Format.formatter, unit) format -> 'a
val log: ?backgroundcolor:Color.color -> ?textcolor:Color.color ->
  ?lineproportion:float -> t -> ('a, Format.formatter, unit) format -> 'a

val print_newline: t -> unit
val print_cell: t -> string -> unit
val print_optional_cell: t -> string -> unit
val print_as_logger: t -> (Format.formatter -> unit) -> unit
val flush_logger: t -> unit
val close_logger: t -> unit

val open_infinite_buffer: ?headerextralength:int -> ?mode:encoding -> unit -> t
val open_circular_buffer: ?mode:encoding -> ?size:int -> unit -> t
val open_logger_from_formatter: ?headerextralength:int -> ?mode:encoding -> Format.formatter -> t
val open_logger_from_channel: ?headerextralength:int -> ?mode:encoding -> out_channel -> t

val open_array: ?size: float option list -> ?color: Color.color option list -> ?bgcolor: Color.color option list -> ?align: char option list -> title:string list -> t -> bool
val close_array: t -> unit
val open_row: ?macro:string -> t -> unit
val close_row: t -> unit


val print_breakable_space: t -> unit
val print_breakable_hint: t -> unit
val breakpage: t -> unit

val dummy_txt_logger: t
val dummy_html_logger: t

val redirect: t -> Format.formatter -> t
val formatter_of_logger: t -> Format.formatter option
val channel_of_logger: t -> out_channel option

val flush_buffer: t -> Format.formatter -> unit
val flush_and_clean: t -> Format.formatter -> unit

val dump_json: t -> Yojson.Basic.t -> unit
val to_json: t -> Yojson.Basic.t
val of_json: Yojson.Basic.t -> string list

val with_lines: t -> t
val without_lines: t -> t

val encapsulate: encoding -> encoding

val print_headers: t -> int -> string -> unit

val maketitle: t -> string -> unit
val setheadpage: t -> string -> unit
val setsignature: t -> string -> unit
val setpreamble: t -> string -> unit
val setfootpage: t -> string -> unit 
