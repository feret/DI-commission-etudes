module type OrdSimplified =
sig
  module Ord:Map.OrderedType
  val simplify : Ord.t -> Ord.t
end

module MakeSimplified (O:OrdSimplified) =
struct
  module M = Map.Make (O.Ord)
  include M

  let gen f key =
    f (O.simplify key)
  let gen_pred f p =
    f (fun x -> p (O.simplify x))

  let add key data map = gen add key data map
  let find key map = gen find key map
  let find_opt key map = gen find_opt key map
  let find_first p map = gen_pred find_first p map
  let find_last p map = gen_pred find_last p map
  let remove key map = gen remove key map
  let find_first_opt p map =
    gen_pred find_first_opt p map
  let find_last_opt p map =
    gen_pred find_last_opt p map

end
