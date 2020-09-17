type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?recu:bool ->
  ?academicyear:string ->
  ?headpage:(int -> string) ->
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
