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
