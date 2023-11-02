type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

type 'elt filter

val dump_elts:
  ?commission:bool ->
  ?dpt:Public_data.main_dpt ->
  ?universite:Public_data.universite ->
  ?dpt_gps_code: string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?attributionyear:string ->
  ?promo:string ->
  ?ninscription:int ->
  ?niveau:string ->
  ?recu:bool ->
  ?libelle:string ->
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  ?headpage:(int ->
             ((Loggers.t ->
               (string -> unit, Format.formatter, unit) format ->
               string -> unit) *
              string)
               list) ->
  ?footpage:((Loggers.t ->
              (string -> unit, Format.formatter, unit) format ->
              string -> unit) *
             string)
      list ->
  ?title:((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list ->
  ?preamble:(int ->
             ((Loggers.t ->
               (string -> unit, Format.formatter, unit) format ->
               string -> unit) *
              string)
               list) ->
  ?signature:(int ->
              ((Loggers.t ->
                (string -> unit, Format.formatter, unit) format ->
                string -> unit) *
               string)
                list) ->
  ?headerextralength:int ->
  ?headcolor:Color.color ->
  ?footcolor:Color.color ->
  get:(Remanent_state.t -> Remanent_state.t * 'a list) ->
  filter:'a filter ->
  get_repository:(Remanent_state.t -> Remanent_state.t * string) ->
  default_file_name:string ->
  cmp:('a -> 'a -> int) list ->
  headers:(string list * ('b -> string) * ('a -> 'b)) list ->
  columns:(string list * ('a -> string)) list ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

val filter:
  ?commission:bool ->
  ?dpt:Public_data.main_dpt ->
  ?universite:Public_data.universite ->
  ?dpt_gps_code:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?attributionyear:string ->
  ?promo:string ->
  ?ninscription:int ->
  ?niveau:string ->
  ?recu:bool ->
  ?libelle:string ->
  'a filter ->
  Remanent_state.t ->
  'a list -> Remanent_state.t * 'a list

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
  ?nb_inscription_list:int list -> Public_data.dens filter
val filter_national_diploma:
  Public_data.diplome_national filter
val filter_student_list:
  Public_data.student filter
val filter_course_name_translation:
  Public_data.course_name_translation filter
val filter_course_entry:
  Public_data.course_entry filter
val filter_mineures_majeures:
  Public_data.mineure_majeure filter
val filter_dens_candidate:
  Public_data.dens_candidate filter
val filter_coursat:
  Public_data.cours_a_trier filter

module type Interface =
sig
  type elt
  val default_file_name: string
  val get:(Remanent_state.t -> Remanent_state.t * elt list)
  val get_repository:(Remanent_state.t -> Remanent_state.t * string)
end
