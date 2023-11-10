type dump =
  Gen.dump

module type ReportInternshipsToBeSorted =
sig
  val dump: Gen.dump
end

module InternshipsToBeSorted: ReportInternshipsToBeSorted
