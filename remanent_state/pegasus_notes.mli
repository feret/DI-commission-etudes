type t
val empty: t
val get_pegasus_note:
  code:string ->
  year:string ->
  firstname: string ->
  lastname: string ->
   t -> Public_data.note_pegasus option

val get_pegasus_notes:
  firstname: string ->
  lastname: string ->
  t -> Public_data.note_pegasus list

val add_pegasus_note:
((string * int * int * int) ->
 'state ->
 Public_data.note_pegasus ->
 Public_data.note_pegasus -> 'state * Public_data.note_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.note_pegasus->
t ->
'state * t
