type t
val empty: t
val get_course_name_translation:
  year:string ->
  codegps:string ->
  t -> Public_data.course_name_translation option
val add_course_name_translation:
  ((string * int * int * int ) ->
   'state ->
   Public_data.course_name_translation ->
   Public_data.course_name_translation -> 'state * Public_data.course_name_translation) ->
  (string * int * int * int) -> 'state -> Public_data.course_name_translation -> t -> 'state * t

type tentry
val empty_course_entry: tentry
val get_course_entry:
  string -> tentry -> Public_data.course_entry option

val add_course_entry:
  ((string * int * int * int) -> 'state -> Public_data.course_entry -> Public_data.course_entry -> 'state * Public_data.course_entry) ->
  (string * int * int * int) -> 'state -> Public_data.course_entry -> tentry -> 'state * tentry

val to_list: tentry -> Public_data.course_entry list 
