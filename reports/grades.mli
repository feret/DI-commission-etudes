type dump =
  ?dpt_gps_code:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportGrades =
sig
  val dump_per_year_dpt_student: dump
  val dump_per_dpt_student_year: dump
  val dump_per_dpt_year_student: dump
  val dump_per_dpt_class_year: dump
  val dump_per_dpt_year_class: dump
  val dump_per_student: dump
  val dump_per_promotion: dump
end

module MissingGrades: ReportGrades
module MissingECTSAttributions: ReportGrades
module NonAcceptedGrades: ReportGrades
module Validated_twice: ReportGrades 
