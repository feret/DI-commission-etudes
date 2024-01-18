type t
val empty: t
val get_pegasus_administrative_status:
  firstname:string ->
  lastname:string ->
  year:string -> t -> Public_data.student_pegasus list

val add_pegasus_administrative_status:
((string * int * int * int) ->
 'state ->
 Public_data.student_pegasus ->
 Public_data.student_pegasus -> 'state * Public_data.student_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.student_pegasus->
t ->
'state * t

val dump: t -> unit 
