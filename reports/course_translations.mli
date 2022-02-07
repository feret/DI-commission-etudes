type dump =
  ?codegps:string ->
  ?academicyear:string ->
  Gen.dump

module type ReportCourseTranslations =
sig
  val dump_per_year: dump
  val dump_per_code: dump
end

module MissingCourseTranslations: ReportCourseTranslations

module type ReportCourseEntries =
sig
  val dump: Gen.dump
end

module MissingCourseEntries: ReportCourseEntries 
module CourseEntriesReport: ReportCourseEntries
