type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int -> 
  ?promo:string ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
end

module DensReport: DensReport
