type t
val empty: t
val get_pegasus_course:
  code:string ->
  year:string ->
   t -> Public_data.course_pegasus option

val add_pegasus_course:
((string * int * int * int) ->
 'state ->
 Public_data.course_pegasus ->
 Public_data.course_pegasus -> 'state * Public_data.course_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.course_pegasus->
t ->
'state * t
