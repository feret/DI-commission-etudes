let unsome a_opt a =
  match a_opt with
  | Some a -> a
  | None -> a

let unsome_string s_opt = unsome s_opt ""

let remove_space_from_string s =
  let seq = String.to_seq s in
  let seq = Seq.filter (fun x -> not (x=' ')) seq in
  String.of_seq seq

let array_map_of_list =
  let rec fill f i v = function
    | [] -> ()
    | x :: l ->
      Array.unsafe_set v i (f x);
      fill f (succ i) v l in
  fun f -> function
    | [] -> [||]
    | x :: l ->
      let len = succ (List.length l) in
      let ans = Array.make len (f x) in
      let () = fill f 1 ans l in
      ans

let asso_list_map2
    list1
    list2
    key1
    key2
    fun1
    fun2
    fun3
  =
  let list1 = List.sort (fun a b -> compare (key1 a) (key1 b)) list1 in
  let list2 = List.sort (fun a b -> compare (key2 a) (key2 b)) list2 in
  let rec aux list1 list2 output =
    match list1,list2 with
    | [],[] -> List.rev output
    | _, [] ->
      List.fold_left
        (fun output elt -> (fun1 elt)::output)
        output list1
    | [],_ ->
      List.fold_left
        (fun output elt -> (fun2 elt)::output)
        output list2
    | h1::t1, h2::t2 ->
      let k1 = key1 h1 in
      let k2 = key2 h2 in
      let cmp = compare k1 k2 in
      if cmp = 0
      then
        aux t1 t2 ((fun3 h1 h2)::output)
      else if cmp < 0
      then
        aux t1 list2 ((fun1 h1)::output)
else
  aux list1 t2 ((fun2 h2)::output)
in
aux list1 list2 []

let date () =
  let date_string_of_tm tm =
    Printf.sprintf "%0*d%0*d%0*d"
      4 (1900 + tm.Unix.tm_year)
      2 (1 + tm.Unix.tm_mon)
      2 tm.Unix.tm_mday
  in
  date_string_of_tm (Unix.gmtime (Unix.time ()))

let is_fully_capitalised s =
  let n = String.length s in
  let rec aux k =
    if k=n then true
    else
      let c = String.get s k in
      if Char.equal c (Char.uppercase_ascii c)
      then
        aux (k+1)
      else
        false
  in
  aux 0

let map_opt f a_opt =
  match a_opt with
  | None -> None
  | Some a -> Some (f a)

let map_opt_state f state a_opt =
  match a_opt with
  | None -> state, None
  | Some a ->
    let state, output = f state a in
    state, Some output

let basename x =
  match String.rindex_opt x '.' with
  | Some i -> String.sub x 0 i
  | None -> x 
