module type OrdSimplified =
sig
  module Ord:Map.OrderedType
  val simplify : Ord.t -> Ord.t
end

module MakeSimplified (O:OrdSimplified) :
  Map.S with type key = O.Ord.t 
