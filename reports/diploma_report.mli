type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?recu:bool ->
  ?academicyear:string ->
  ?headpage:(int -> string) ->
  ?footpage:string ->
  ?footcolor:Color.color ->
  ?title:string ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) ->
  Gen.dump

module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
  val dump_per_student: dump
end

module DiplomaReport: DiplomaReport

val dump_pv:
  ?output_repository:string ->
  ?prefix:string ->
  ?output_file_name:string ->
  Public_data.diplome_national ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

val dump_pvs:
  ?recu:bool ->
  ?academicyear:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?output_repository:string ->
  ?prefix:string -> Remanent_state.t -> Remanent_state.t
