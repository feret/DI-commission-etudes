type dump =
  Gen.dump

module type ReportCoursesToBeSorted =
sig
  val dump: Gen.dump
end

module CoursesToBeSorted: ReportCoursesToBeSorted
