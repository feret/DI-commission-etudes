val unsome: 'a option -> 'a -> 'a
val unsome_string: string option -> string

val remove_space_from_string:
  string -> string
val array_map_of_list : ('a -> 'b) -> 'a list -> 'b array

val asso_list_map2:
  'a list -> 'b list ->
  ('a -> 'c) -> ('b -> 'c) ->
  ('a -> 'd) -> ('b -> 'd) ->
  ('a -> 'b -> 'd) ->
  ('d list)

val date: unit -> string

val is_fully_capitalised: string -> bool

val map_opt:  ('a -> 'b) -> 'a option -> 'b option
val map_opt_state:  ('a -> 'b -> 'a * 'c) -> 'a -> 'b option -> 'a * ('c option)

val basename: string -> string

val space_only: string -> bool

val substring: string -> string -> bool

val float_to_string:
  ?force_dec_sep_to_dot:bool ->
  ('remanent_state -> 'remanent_state * char) ->
  'remanent_state -> float -> 'remanent_state * string

val float_of_string:
  (string -> 'remanent_state -> 'remanent_state)
  -> 'remanent_state -> string -> 'remanent_state * float option

val int_of_string:
  (string -> 'remanent_state -> 'remanent_state) ->
  'remanent_state -> string -> 'remanent_state * int option

val fun_ignore:
  'remanent_state -> 'data -> 'acc -> 'remanent_state * 'acc

val collect_conv:
(string -> string, unit, string) format ->
((string -> 'remanent_state -> 'remanent_state) -> 'remanent_state -> string  -> 'remanent_state * 'target option) ->
(string -> 'remanent_state -> 'remanent_state) ->
('target option -> 'accumulator -> 'accumulator) -> 'remanent_state -> string option -> 'accumulator -> 'remanent_state * 'accumulator

val collect_string:
  (string option -> 'accumulator -> 'accumulator) ->
  'remanent_state -> string option -> 'accumulator ->
  'remanent_state * 'accumulator

val collect_int:
  (string -> 'remanent_state -> 'remanent_state) ->
  (int option -> 'accumulator -> 'accumulator) ->
  'remanent_state -> string option -> 'accumulator ->
  'remanent_state * 'accumulator

val collect_float:
  (string -> 'remanent_state -> 'remanent_state) ->
  (float option -> 'accumulator -> 'accumulator) ->
  'remanent_state -> string option -> 'accumulator ->
  'remanent_state * 'accumulator

val collect_bool:
  (string -> 'remanent_state -> 'remanent_state) ->
  (bool option -> 'accumulator -> 'accumulator) ->
  'remanent_state -> string option -> 'accumulator ->
  'remanent_state * 'accumulator

val sort: ('a -> 'b) -> (('b * 'a) -> ('b * 'a) -> int) -> 'a list -> 'a list
val valide_sans_note: string

val prepare_report:
  cmp:('a -> 'a -> int) list ->
  headers:('a -> 'b) list ->
  'a list ->
  ('b list * 'a) list

val dump_report:
  print_header:
    (int -> string  -> unit) ->
  open_array:(title:string list -> bool) ->
  open_row:(unit -> unit) ->
  close_row:(unit -> unit) ->
  print_cell:(string -> unit) ->
  close_array:(unit -> unit) ->
  string_of_headers:(string * ('a -> string)) list ->
  string_of_column:(string * ('b -> string)) list ->
  settitle:(string -> unit) ->
  setpreamble:(string -> unit) ->
  setheadpage:(?color:Color.color -> string -> unit) ->
  setfootpage:(?color:Color.color -> string -> unit) ->
  setsignature:(string -> unit) ->
  ?title:string ->
  ?headpage:(int -> string) ->
  ?headcolor:Color.color ->
  ?footpage:string ->
  ?footcolor:Color.color ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) -> ('a list * 'b) list -> unit

val build_output:
  get_repository:('a -> 'a * string) ->
  get_store_according_promotion:('a -> 'a * bool) ->
  get_indicate_promotions_in_file_names:('a -> 'a * bool) ->
  rec_mk_when_necessary:(string * int * int * int ->
                                'a -> string -> 'c * 'd) ->
  f_firstname:(string -> string)->
  f_lastname:(string -> string) ->
  firstname:string ->
  lastname:string ->
  promotion:string option ->
  ?prefix:string ->
  ?output_repository:string ->
  ?output_file_name:string ->
  extension:string -> 'a -> 'c * 'd * string
