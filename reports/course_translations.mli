type dump =
  ?codegps:string ->
  ?academicyear:string ->
  Gen.dump

module type ReportCourseEntries =
sig
  val dump: Gen.dump
end

module MissingCourseEntries: ReportCourseEntries
module CourseEntriesReport: ReportCourseEntries
