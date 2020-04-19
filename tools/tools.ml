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
