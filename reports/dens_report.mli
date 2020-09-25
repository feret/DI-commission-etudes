type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int ->
  ?promo:string ->
  ?headpage:(int -> string) ->
  ?footpage:string -> 
  ?title:string ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
end

module DensReport: DensReport
