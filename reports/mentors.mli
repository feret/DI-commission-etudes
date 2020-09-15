type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?mentorname:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportMentors =
sig
  val dump_per_year: dump
  val dump_per_promotion: dump
  val dump_per_student: dump
end

module ReportMissingMentors: ReportMentors
