module CharMap =
  Map.Make
    (struct
      type t = char
      let compare = compare
    end)

let special_char =
  [
    '\136', 'e';
    '\137', 'e';
    '\169', 'e';
    '\167', 'c';
    '\168', 'e';
    '\180', 'o';
    '\160', 'a'
  ]

let special_char_txt =
  [
    '\195', "";
    '\136', "e";
    '\137', "e";
    '\169', "e";
    '\167', "c";
    '\168', "e";
    '\180', "o";
    '\160', "a"
  ]


let special_char_latex =
  [
    '\195', "";
    '\137', "{\\'e}";
    '\169', "{\\'e}";
    '\167', "{\\c c}";
    '\136', "{\\`e}";
    '\168', "{\\`e}";
    '\180', "{\\^o}";
    '\160', "a"
  ]

let special_char_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char

let special_char_latex_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_latex

let special_char_txt_map =
      List.fold_left
        (fun map (x,y) ->
           CharMap.add x y map)
        CharMap.empty
        special_char_txt

let correct_cute_in_char c =
match
  CharMap.find_opt c special_char_map
with
| Some x -> x
| None -> c

let correct_char c =
  Char.lowercase_ascii (correct_cute_in_char c)

let expand_cute_in_char c =
  match
    CharMap.find_opt c special_char_map
  with
  | Some x -> [c;x]
  | None -> [c]

let expand_char_upper_lower c =
  let c1 = Char.uppercase_ascii c in
  let c2 = Char.lowercase_ascii c in
  if c1=c2
  then [c1]
  else [c1;c2]

let expand_char c =
  List.flatten
    (List.rev_map
       expand_cute_in_char
       (expand_char_upper_lower c))

let correct_string s =
  String.map correct_char s

let string_to_list s =
  let n = String.length s in
  let rec aux k out =
    if k<0 then out
    else
      aux (k-1) ((s.[k])::out)
  in
  aux (n-1) []

let cartesian_product l =
  List.fold_left
    (fun suffix_list elt_list ->
       List.fold_left
         (fun output elt ->
            List.fold_left
              (fun output suffix -> (elt::suffix)::output)
              output suffix_list
         )
         [] elt_list
    )
    [[]]
    (List.rev l)

let list_to_string l =
  let n = List.length l in
  let s = Bytes.create n in
  let _ =
    List.fold_left
      (fun i elt -> let () = Bytes.set s i elt in i+1)
      0 l
  in
  Bytes.to_string s


let expand_string s =
  let list = string_to_list s in
  let (list:char list list) = List.rev_map expand_char (List.rev list) in
  let (list:char list list) = cartesian_product list in
  List.rev_map list_to_string (List.rev list)

let correct_string_gen map s =
  let list = string_to_list s in
  let list =
    List.rev_map
      (fun c ->
         match CharMap.find_opt c map with
         | Some a -> a
         | None ->
           let s = String.make 1 c in s)
      (List.rev list)
  in
  String.concat "" list

let correct_string_latex s =
  correct_string_gen special_char_latex_map s

  let correct_string_txt s =
    correct_string_gen special_char_txt_map s
