type dump =
  Gen.dump

module type ReportMineuresMajeures =
sig
  val dump: Gen.dump
end

module SuggestionsMineures: ReportMineuresMajeures
module SuggestionsMajeures: ReportMineuresMajeures
