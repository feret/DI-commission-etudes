type t
val empty: t
val get_course_exception:
  year:string ->
  codegps:string ->
  t -> Public_data.course_exception  option
val add_course_exception:
  ((string * int * int * int ) ->
   'state ->
   Public_data.course_exception ->
   Public_data.course_exception -> 'state * Public_data.course_exception) ->
  (string * int * int * int) -> 'state -> Public_data.course_exception -> t -> 'state * t
