type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int ->
  ?promo:string ->
  ?headpage:(int -> ((Loggers.t ->
                      (string -> unit, Format.formatter, unit) format ->
                      string -> unit) *
                     string)
               list ) ->
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
               list ) ->
  ?signature:(int -> ((Loggers.t ->
                       (string -> unit, Format.formatter, unit) format ->
                       string -> unit) *
                      string)
                list ) ->
  ?nb_inscription_list:int list ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
end

module DensReport: DensReport
