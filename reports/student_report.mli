type dump =
  ?studentfirstname:string ->
  ?studentlastname:string ->
  ?promo:string ->
  Gen.dump

module type ReportStudents =
sig
  val dump_per_student: dump
  val dump_per_promo: dump
end

module ReportMissingPictures: ReportStudents 
module ReportGpsServerFaillures: ReportStudents
