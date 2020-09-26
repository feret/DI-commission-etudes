type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int ->
  ?promo:string ->
  ?headpage:(int -> string) ->
  ?footpage:string ->
  ?footcolor:Color.color ->
  ?title:string ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) ->
  ?nb_inscription_list:int list ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
end

module DensReport: DensReport
