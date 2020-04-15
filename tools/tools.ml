let remove_space_from_string s =
  let seq = String.to_seq s in
  let seq = Seq.filter (fun x -> not (x=' ')) seq in
  String.of_seq seq
