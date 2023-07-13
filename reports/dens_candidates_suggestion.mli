type dump =
  Gen.dump

module type ReportDensCandidates =
sig
  val dump: Gen.dump
end

module SuggestionsDensCandidates: ReportDensCandidates
