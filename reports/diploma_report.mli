type dump =
  ?commission:bool ->
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:Public_data.main_dpt ->
  ?universite:Public_data.universite ->
  ?recu:bool ->
  ?academicyear:string ->
  ?headpage:(int -> ((Loggers.t ->
                      (string -> unit, Format.formatter, unit) format ->
                      string -> unit) *
                     string)
               list)
  ->
  ?footpage:((Loggers.t ->
                       (string -> unit, Format.formatter, unit) format ->
                       string -> unit) *
                      string)
                     list ->
  ?footcolor:Color.color ->
  ?title:((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list ->
  ?preamble:(int -> ((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list) ->
  ?signature:(int -> ((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list) ->
  Gen.dump

module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
  val dump_per_student: dump
  val dump_stats: dump
end

module DiplomaReport: DiplomaReport

val dump_attestation:
  ?output_repository:string ->
  ?prefix:string ->
  ?output_file_name:string ->
  ?date:string ->
  signataire:string  ->
  Public_data.diplome_national ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

val dump_attestations:
  signataires:string list ->
  ?recu:bool ->
  ?academicyear:string ->
  ?niveau:string ->
  ?dpt:Public_data.main_dpt ->
  ?output_repository:string ->
  ?prefix:string -> Remanent_state.t -> Remanent_state.t
