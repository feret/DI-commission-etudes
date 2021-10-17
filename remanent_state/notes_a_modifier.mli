type t =
  Public_data.note_a_modifier 
    Public_data.YearMap.t
    Public_data.CodeMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

val empty: t
val get_note_a_modifier:
  firstname:string -> lastname:string -> code:string -> year:string
  ->
  t -> Public_data.note_a_modifier option

val add_note_a_modifier:
  (string * int * int * int -> string -> exn -> 'state -> 'state)
  ->
  ((string * int * int * int) ->
   'state ->
   Public_data.note_a_modifier ->
   Public_data.note_a_modifier -> 'state * Public_data.note_a_modifier) ->
  (string * int * int * int) ->
  'state ->
  Public_data.note_a_modifier -> t ->
  'state * t
