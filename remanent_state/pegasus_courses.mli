type t
val empty: t
val get_pegasus_course:
  code:string ->
  year:string ->
   t -> Public_data.course_pegasus option

val get_pegasus_course_by_libelle:
    libelle:string ->
    year:string ->
    semester:string option ->
    t -> Public_data.course_pegasus list

val add_pegasus_course:
((string * int * int * int) ->
 'state ->
 Public_data.course_pegasus ->
 Public_data.course_pegasus -> 'state * Public_data.course_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.course_pegasus->
t ->
'state * t

val dump: t -> unit
