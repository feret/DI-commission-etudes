type t =
  Public_data.cours_a_ajouter list
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

val empty: t
val get_additional_courses:
  firstname:string -> lastname:string ->
  t -> Public_data.cours_a_ajouter list

val add_additional_course:
  (string * int * int * int) ->
  'state ->
  Public_data.cours_a_ajouter -> t ->
  'state * t
