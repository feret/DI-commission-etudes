module type OrdSimplified =
sig
  module Ord:Map.OrderedType
  val simplify : Ord.t -> Ord.t
end

module type Collect =
sig
  type key
  type 'a t
  val collect : key option -> 'a t -> 'a list -> 'a list
end

module MakeSimplified (O:OrdSimplified) :
  Map.S with type key = O.Ord.t

module MakeSetSimplified (O:OrdSimplified) :
    Set.S with type elt = O.Ord.t

module Collect (M:Map.S) :
  Collect with type key = M.key and type 'a t = 'a M.t
