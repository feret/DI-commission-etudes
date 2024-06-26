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

module MakeSetSimplified (O:OrdSimplified) =
struct
  module S = Set.Make (O.Ord)
  include S

  let gen f key =
    f (O.simplify key)

  let add key map = gen add key map
  let mem key map = gen mem key map
  let remove key map = gen remove key map
end

module Collect (M:Map.S) =
  (struct
    type key = M.key
    type 'a t = 'a M.t
    let collect arg map acc =
      match arg with
      | None ->
        M.fold (fun _ -> List.cons) map acc
      | Some arg ->
        begin
          match M.find_opt arg map with
          | None -> acc
          | Some data -> data::acc
        end
  end: Collect with type key = M.key and type 'a t = 'a M.t)
