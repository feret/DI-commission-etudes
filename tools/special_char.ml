module CharMap =
  Map.Make
    (struct
      type t = char
      let compare = compare
    end)

let special_char_latex =
  [
      '\128', "{\\`A}";
      '\129', "{\\'A}";
      '\130', "{\\^A}";
      '\131', "{\\~A}";
      '\132', "{\\\"A}";
      '\135', "{\\c C}";
      '\136', "{\\`E}";
      '\137', "{\\'E}";
      '\138', "{\\^E}";
      '\139', "{\\\"E}";
      '\140', "{\\`I}";
      '\141', "{\\'I}";
      '\142', "{\\^I}";
      '\143', "{\\\"I}";
      '\146', "{\\`O}";
      '\147', "{\\'O}";
      '\148', "{\\^O}";
      '\149', "{\\~O}";
      '\150', "{\\\"O}";
      '\152', "{\\c E}";
      '\153', "{\\`U}";
      '\155', "{\\^U}";
      '\156', "{\\\"U}";
      '\160', "{\\`a}";
      '\161', "{\\'a}";
      '\162', "{\\^a}";
      '\163', "{\\~a}";
      '\164', "{\\\"a}";
      '\167', "{\\c c}";
      '\168', "{\\`e}";
      '\169', "{\\'e}";
      '\170', "{\\^e}";
      '\171', "{\\\"e}";
      '\172', "{\\`i}";
      '\173', "{\\'i}";
      '\174', "{\\^i}";
      '\175', "{\\\"i}";
      '\178', "{\\`o}";
      '\179', "{\\'o}";
      '\180', "{\\^o}";
      '\181', "{\\~o}";
      '\182', "{\\\"o}";
      '\185', "{\\`u}";
      '\187', "{\\^u}";
      '\195', "";
    ]

let special_char_html =
  [
    '\128', "&Agrave;";
    '\129', "&Aacute;";
    '\130', "&Acirc;";
    '\131', "&Atilde;";
    '\132', "&Auml;";
    '\135', "&Ccedil;";
    '\136', "&Egrave;";
    '\137', "&Eacute;";
    '\138', "&Ecirc;";
    '\139', "&Euml;";
    '\140', "&Igrave;";
    '\141', "&Iacute;";
    '\142', "&Icirc;";
    '\143', "&Iuml;";
    '\146', "&Ograve;";
    '\147', "&Oacute;";
    '\148', "&Ocirc;";
    '\149', "&Otilde;";
    '\150', "&Ouml;";
    '\152', "&Ecedil;";
    '\153', "&Ugrave;";
    '\155', "&Ucirc;";
    '\156', "&Uuml;";
    '\160', "&agrave;";
    '\161', "&aacute;";
    '\162', "&acirc;";
    '\163', "&atilde;";
    '\164', "&auml;";
    '\167', "&ccedil;";
    '\168', "&egrave;";
    '\169', "&eacute;";
    '\170', "&ecirc;";
    '\171', "&euml";
    '\172', "&igrave;";
    '\173', "&iacute;";
    '\174', "&icirc;";
    '\175', "&iuml;";
    '\178', "&ograve;";
    '\179', "&oacute;";
    '\180', "&ocirc;";
    '\181', "&otilde;";
    '\182', "&ouml;";
    '\185', "&ugrave;";
    '\187', "&ucirc";
    '\195', "";
  ]

let special_char_url =
  [
    ' ', "+";
    '\128', "%C0";
    '\129', "%C1";
    '\130', "%C2";
    '\131', "%C3";
    '\132', "%C4";
    '\135', "%C7";
    '\136', "%C8";
    '\137', "%C9";
    '\138', "%CA";
    '\139', "%CB";
    '\140', "%CC";
    '\141', "%CD";
    '\142', "%CE";
    '\143', "%CF";
    '\146', "%D2";
    '\147', "%D3";
    '\148', "%D4";
    '\149', "%D5";
    '\150', "%D6";
    (*'\152', "{\\c E}";*)
    '\153', "%D9";
    '\155', "%DB";
    '\156', "%DC";
    '\160', "%E0";
    '\161', "%E1";
    '\162', "%E2";
    '\163', "%E3";
    '\164', "%E4";
    '\167', "%E7";
    '\168', "%E8";
    '\169', "%E9";
    '\170', "%EA";
    '\171', "%EB";
    '\172', "%EC";
    '\173', "%ED";
    '\174', "%EE";
    '\175', "%EF";
    '\178', "%F2";
    '\179', "%F3";
    '\180', "%F4";
    '\181', "%F5";
    '\182', "%F6";
    '\185', "%F9";
    '\187', "%FA";
    '\195', "";
  ]

let special_char_email_latex =
  ['_',"\\_"]

let lowercase_char =
  [
    '\128', '\160';
    '\129', '\161';
    '\130', '\162';
    '\131', '\163';
    '\132', '\164';
    '\135', '\167';
    '\136', '\168';
    '\137', '\169';
    '\138', '\170';
    '\139', '\171';
    '\140', '\172';
    '\141', '\173';
    '\142', '\174';
    '\143', '\175';
    '\146', '\178';
    '\147', '\179';
    '\148', '\180';
    '\149', '\181';
    '\150', '\182';
    '\152', '\184';
    '\153', '\185';
    '\155', '\187';
    '\156', '\188'
]

let uppercase_char =
  List.rev_map
    (fun (a,b) -> (b,a))
    (List.rev lowercase_char)

let special_char =
  [
    '\128', 'A';
    '\129', 'A';
    '\130', 'A';
    '\131', 'A';
    '\132', 'A';
    '\135', 'C';
    '\136', 'E';
    '\137', 'E';
    '\138', 'E';
    '\139', 'E';
    '\140', 'I';
    '\141', 'I';
    '\142', 'I';
    '\143', 'I';
    '\146', 'O';
    '\147', 'O';
    '\148', 'O';
    '\149', 'O';
    '\150', 'O';
    '\151', 'e';
    '\152', 'E';
    '\153', 'U';
    '\155', 'U';
    '\156', 'U';
    '\160', 'a';
    '\161', 'a';
    '\162', 'a';
    '\163', 'a';
    '\164', 'a';
    '\167', 'c';
    '\168', 'e';
    '\169', 'e';
    '\170', 'e';
    '\171', 'e';
    '\172', 'i';
    '\173', 'i';
    '\174', 'i';
    '\175', 'i';
    '\178', 'o';
    '\179', 'o';
    '\180', 'o';
    '\181', 'o';
    '\182', 'o';
    '\185', 'u';
    '\187', 'u';
    '\188', 'u'
  ]

let special_char_txt =
  ('\195',"")::(List.rev_map (fun (a,b) -> a,String.make 1 b) (List.rev special_char))

let special_char_file_name =
  (' ', "")::special_char_txt

let special_char_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char

let special_char_url_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_url

let special_char_email_latex =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_email_latex

let special_char_latex_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_latex

let special_char_html_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_html

let special_char_txt_map =
      List.fold_left
        (fun map (x,y) ->
           CharMap.add x y map)
        CharMap.empty
        special_char_txt

let special_char_file_name_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    special_char_file_name

let lowercase_char_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    lowercase_char

let uppercase_char_map =
  List.fold_left
    (fun map (x,y) ->
       CharMap.add x y map)
    CharMap.empty
    uppercase_char

let correct_acute_in_char c =
match
  CharMap.find_opt c special_char_map
with
| Some x -> x
| None -> c

let correct_char c =
  Char.lowercase_ascii (correct_acute_in_char c)

let expand_acute_in_char c =
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
       expand_acute_in_char
       (expand_char_upper_lower c))

let remove_acute s =
  String.map correct_acute_in_char s

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

let correct_string_url s =
  correct_string_gen special_char_url_map s

let correct_string_email_latex s =
  correct_string_gen special_char_email_latex s

let correct_string_latex s =
  correct_string_gen special_char_latex_map s

let correct_string_html s =
  correct_string_gen special_char_html_map s

let correct_string_txt s =
    correct_string_gen special_char_txt_map s

let correct_string_filename s =
  correct_string_gen special_char_file_name_map s

let uppercase_char c =
  match
    CharMap.find_opt c uppercase_char_map
  with
  | Some a -> a
  | None -> Char.uppercase_ascii c
let lowercase_char c =
  match
    CharMap.find_opt c lowercase_char_map
  with
  | Some a -> a
  | None -> Char.lowercase_ascii c


let uppercase s =
  String.map uppercase_char s
let lowercase s =
  String.map lowercase_char s

let capitalize s =
  let n = String.length s in
  let a = Array.make n ' ' in
  let rec aux k bool =
    if k=n
    then ()
    else
      let () =
        a.(k)<-(if bool then uppercase_char s.[k] else s.[k])
      in
      aux (k+1) (s.[k]=' ' || s.[k]='-' || (bool && s.[k]='\195'))
  in
  let () = aux 0 true in
  String.init n (fun i -> a.(i))

let delimiter =
  [' ';'-';
   '\"';'\'';
   '\\';',';'.';'?';':';'!';'{';'}';'_';'[';']';'#']
let clean_spurious_uppercase_letters string =
  let n = String.length string in
  let string = Bytes.of_string string in
  let buggy i =
    let c = Bytes.get string i in
    Char.lowercase_ascii c <> lowercase_char c
  in
  let lower l =
    List.iter
      (fun i -> Bytes.set string i (lowercase_char (Bytes.get string i)))
      l
  in
  let rec aux k has_lowercase acc =
    if k>=n then
      let () =
        if has_lowercase then
          lower acc
      in
      Bytes.to_string string
    else
      let c = Bytes.get string k in
      if List.mem c delimiter
      then
        let () =
          if has_lowercase
          then
            lower acc
        in
        f (k+1)
      else
        let has_lowercase =
          has_lowercase
          || ((c = lowercase_char c)
              && (c <> uppercase_char c))
        in
        let acc =
          if buggy k
          then
            k::acc
          else acc
        in
          aux (k+1) has_lowercase acc
  and
    f k =
    if k>=n then Bytes.to_string string
    else
      let c = Bytes.get string k in
      if c = '\195'
      then f (k+1)
      else aux (k+1) false []
  in f 0
