type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

type 'elt filter

val dump_elts:
  ?dpt:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?promo:string ->
  ?ninscription:int ->
  ?niveau:string ->
  ?recu:bool ->
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  ?headpage:(int -> string) ->
  ?title:string ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) ->
  ?headerextralength:int -> 
  get:(Remanent_state.t -> Remanent_state.t * 'a list) ->
  filter:'a filter ->
  get_repository:(Remanent_state.t -> Remanent_state.t * string) ->
  default_file_name:string ->
  cmp:('a -> 'a -> int) list ->
  headers:(string * ('b -> string) * ('a -> 'b)) list ->
  columns:(string * ('a -> string)) list ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

val lift_cmp:
  ('a -> 'b) -> 'a -> 'a -> int
val op_cmp: ('a -> 'a -> int) -> 'a -> 'a -> int

val filter_grade:
  Public_data.missing_grade filter
val filter_internship_description:
  Public_data.missing_internship_description filter
val filter_mentoring:
  Public_data.missing_mentor filter
val filter_mentoring_list:
  Public_data.mentor filter
val filter_dens:
  Public_data.dens filter
val filter_national_diploma:
  Public_data.diplome_national filter



module type Interface =
sig
  type elt
  val default_file_name: string
  val get:(Remanent_state.t -> Remanent_state.t * elt list)
  val get_repository:(Remanent_state.t -> Remanent_state.t * string)
end
