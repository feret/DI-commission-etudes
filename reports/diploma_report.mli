type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?recu:bool ->
  ?academicyear:string -> 
  Gen.dump

module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
end

module DiplomaReport: DiplomaReport
