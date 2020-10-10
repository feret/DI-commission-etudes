type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportInternshipDescriptions =
sig
  val dump_per_year: dump
  val dump_per_promotion: dump
  val dump_per_student: dump
end

module MissingInternshipDescriptions: ReportInternshipDescriptions
module AmbiguousInternshipDescriptions: ReportInternshipDescriptions
module NonValidatedInternships: ReportInternshipDescriptions
