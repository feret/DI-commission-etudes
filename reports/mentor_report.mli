type dump =
  ?studentfirstname:string ->
  ?studentlastname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?academicyear:string ->
  ?attributionyear:string ->
  ?promo:string ->
  ?title:string ->
  ?dpt:string ->
  correct_email:(string -> string) -> 
  Gen.dump

module type ReportMentors =
sig
  val dump_per_year_mentor_student: dump
  val dump_per_year_student_mentor: dump
  val dump_per_promo_mentor_student: dump
  val dump_per_promo_student_mentor: dump
  val dump_per_mentor_year_promo_student: dump
  val dump_per_student: dump


end

module ReportListMentors: ReportMentors