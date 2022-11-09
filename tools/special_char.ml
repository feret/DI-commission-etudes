module CharMap =
  Map.Make
    (struct
      type t = char
      let compare = compare
    end)

module Char2Map =
  Map.Make
    (struct
      type t = char * char
      let compare = compare
    end)

let get_main (a,_,_,_,_,_,_) = a
let get_latex (_,a,_,_,_,_,_) = a
let get_html (_,_,a,_,_,_,_) = a
let get_url (_,_,_,a,_,_,_) =  a
let get_lowercase (_,_,_,_,a,_,_) = a
let get_uppercase  (_,_,_,_,_,a,_) = a
let get_txt (_,_,_,_,_,_,a) = a
let special_char_tab  =
  [
      "À", "{\\`A}", "&Agrave;","%C0","à","À","A";
      "Â", "{\\^A}", "&Acirc;","%C2","â","Â","A";
      "Æ", "{\\AE}", "&AElig","Æ","æ","Æ","AE";
      "Á", "{\\'A}", "&Aacute;","%C1","á","Á","A";
      "Ä", "{\\\"A}", "&Auml;","%C4","ä","Ä","A";
      "Ã", "{\\~A}", "&Atilde;","%C3","ã","Ã","A";
      (*"Å",*)
      (*"Ā",*)
      "Ç","{\\c C}", "&Ccedil;","%C7","ç","Ç","C";
      "É", "{\\'E}", "&Eacute;","%C9","é","É","E";
      "È", "{\\`E}", "&Egrave;","%C8","è","È","E";
      "Ê", "{\\^E}", "&Ecirc;","%CA","ê","Ê","E";
      "Ë", "{\\\"E}", "&Euml;","%CB","ë","Ë","E";
      "Ę", "{\\c E}", "&Ecedil;","Ę","ę","Ę","E";
      (*
        "Ė",
        "Ē", *)
      "Î", "{\\^I}", "&Icirc;","%CE","î","Î","I";
      "Ï", "{\\\"I}", "&Iuml;","%CF","ï","Ï","I";
      "Ì", "{\\`I}", "&Igrace;","%CC","ì","Ì","I";
      "Í", "{\\'I}", "&Iacute;","%CD","í","Í","I";
    (*  "Į",
      "Ī", *)
      "Ô", "{\\^O}", "&Ocirc;","%D4","ô","Ô","O";
      "Œ", "{\\OE}", "&OElig;","%D2","œ","Œ","OE";
      "Ö", "{\\\"O}", "&Ouml;","%D6","ö","Ö","O";
      "Ò", "{\\`O}", "&Ograve;","Ò","ò","Ò","O";
      "Ó", "{\\'O}", "&Oacute;","Ó","ó","Ó","O";
      "Õ", "{\\~O}", "&Otilde;","%D5","õ","Õ","O";
      (*"Ø",
      "Ō",*)
      "Û", "{\\^U}", "&Ucirc;","%DB","û","Û","U";
      "Ù", "{\\`U}", "&Ugrave;","%D9","ù","Ù","U";
      "Ü", "{\\\"U}", "&Uuml;","%DC","ü","Ü","U";
      "Ú", "{\\\'U}", "&Uacute;","Ú","ú","Ú","U";
      (*"Ū",*)
      "à", "{\\`a}", "&agrave;","%E0","à","À","a";
      "â", "{\\^a}", "&acirc;","%E2","â","Â","a";
      "æ", "{\\ae}", "&aelig;","%E6","æ","Æ","ae";
      "á", "{\\'a}", "&aacute;","%E1","á","Á","a";
      "ä", "{\\\"a}", "&auml;","%E4","ä","Ä","a";
      "ã", "{\\~a}", "&atilde;","%E3","ã","Ã","a";
      (*"å",
      "ā",*)
      "ç", "{\\c c}", "&ccedil;","%E7","ç","Ç","c";
      "é", "{\\'e}", "&eacute;","%E9","é","É","e";
      "è", "{\\`e}", "&egrave;","%E8","è","È","e";
      "ê", "{\\^e}", "&ecirc;","%EA","ê","Ê","e";
      "ë", "{\\\"e}", "&euml;","%EB","ë","Ë","e";
    (*  "ę",
      "ė",
      "ē",*)
      "î", "{\\^i}", "&icirc;","%EE","î","Î","i";
      "ï", "{\\\"i}", "&iuml;","%EF","ï","Ï","i";
      "ì", "{\\`i}", "&iagrave;","%EC","ì","Ì","i";
      "í", "{\\\'i}", "&iacute;","%ED","í","Í","i";
      (*"į",
      "ī"*)
      "ô", "{\\^o}", "&ocirc;","%F4","ô","Ô","o";
      "œ", "{\\oe}", "&oelig;","œ","œ","Œ","oe";
      "ö", "{\\\"o}", "&ouml;","%F6","ö","Ö","o";
      "ò", "{\\`o}", "&ograve;","%F2","ò","Ò","o";
      "ó", "{\\\'o}", "&oacute;","%F3","ó","Ó","o";
      "õ", "{\\~o}", "&otilde;","%F5","õ","Õ","o";
      (*"ø", "ō"; *)
      "û","{\\^u}", "&ucirc;","%FA","û","Û","u";
      "ù","{\\`u}", "&ugrave;","%F9","ù","Ù","u";
      "ü","{\\¨u}", "&uuml;","%F6","ü","Ü","u";
      "ú","{\\'u}", "&uacute;","ú","ú","Ú","u";
      (*"ū"*)
      "\191", "'", "'","'","'","'","\'";
      "_", "\\_" , "_","_","_","_","_";
      " ", " ", " ", "+", " ", " ", " ";
    ]

let get_gen proj =
  List.rev_map
      (fun k -> get_main k, proj k)
      (List.rev special_char_tab)

let special_char_latex = get_gen get_latex
let special_char_html = get_gen get_html
let special_char_url = get_gen get_url
let lowercase_char = get_gen get_lowercase
let uppercase_char = get_gen get_uppercase
let special_char = get_gen get_txt

let special_char_csv =
  ['"',"\"\"";
   '{',"\"";
   '}',"\""]


let special_char_txt =
  ("\194","")::("\195","")::special_char

let special_char_utf8 =
  ['\233',"e"]

let special_char_file_name =
  (" ", "")::special_char_txt

let map_from_char_list l =
  List.fold_left
    (fun map (x,y) -> CharMap.add x y map)
    CharMap.empty l, Char2Map.empty

let map_from_string_list =
  List.fold_left
    (fun (map1,map2) (x,y) ->
        if String.length x = 1 then
          CharMap.add (String.get x 0) y map1, map2
        else
        if String.length x = 2 then
          map1,
          Char2Map.add
            (String.get x 0, String.get x 1)
            y
          map2
        else failwith "BAD STRING in Special_char tabs")
    (CharMap.empty, Char2Map.empty)

let special_char_map =
    map_from_string_list special_char

let special_char_url_map =
    map_from_string_list special_char_url

let special_char_utf8_map =
    map_from_char_list special_char_utf8

let special_char_email_latex =
    CharMap.empty, Char2Map.empty

let special_char_latex_map =
    map_from_string_list special_char_latex

let special_char_html_map =
    map_from_string_list special_char_html

let special_char_txt_map =
    map_from_string_list special_char_txt

let special_char_file_name_map =
    map_from_string_list special_char_file_name

let special_char_csv_map =
    map_from_char_list special_char_csv

let lowercase_char_map =
    map_from_string_list lowercase_char

let uppercase_char_map =
    map_from_string_list uppercase_char

(*let correct_acute_in_char c =
match
  CharMap.find_opt c special_char_map
with
| Some x -> x
| None -> c

let correct_char c =
  Char.lowercase_ascii (correct_acute_in_char c)*)

(*let expand_acute_in_char c =
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
       (expand_char_upper_lower c))*)

let string_map ?dft (map1,map2) s =
  let dft =
    match dft with
      | None -> (fun x -> x)
      | Some f -> f
  in
  let size = String.length s in
  let rec aux k accu =
      if k=size
      then String.concat "" (List.rev accu)
      else
        if k=size-1
        then
          let c1 = String.get s k in
          match CharMap.find_opt c1 map1 with
          | None -> aux (k+1) (dft (String.make 1 c1)::accu)
          | Some c -> aux (k+1) (c::accu)
        else
          let c1 = String.get s k in
          let c2 = String.get s (k+1) in
          match Char2Map.find_opt (c1,c2) map2 with
          | Some c -> aux (k+2) (c::accu)
          | None ->
            begin
              match CharMap.find_opt c1 map1 with
                | None -> aux (k+1) (dft (String.make 1 c1)::accu)
                | Some c -> aux (k+1) (c::accu)
            end
  in
  aux 0 []

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

let both a b = [a;b]
let expand_string (map1,map2) s =
  let size = String.length s in
  let rec aux k accu =
      if k=size
      then accu
      else
        if k=size-1
        then
          let c1 = String.get s k in
          let s1 = String.make 1 c1 in
          match CharMap.find_opt c1 map1 with
          | None -> aux (k+1) ([s1]::accu)
          | Some c -> aux (k+1) ((both s1 c)::accu)
        else
            let c1 = String.get s k in
            let s1 = String.make 1 c1 in
            let c2 = String.get s (k+1) in
            let s2 = String.sub s k 2 in
            match Char2Map.find_opt (c1,c2) map2 with
              | None ->
                begin
                  match CharMap.find_opt c1 map1 with
                  | None -> aux (k+1) ([s1]::accu)
                  | Some c -> aux (k+1) ((both s1 c)::accu)
                end
              | Some c -> aux (k+2) ((both s2 c)::accu)
    in
    let l =  aux 0 [] in
    let l = cartesian_product l in
    List.rev_map
      (fun x ->
          String.concat "" (List.rev x))
      l

let expand_string s =
  let l1 = expand_string lowercase_char_map s in
  let l2 =
      List.flatten
          (List.rev_map
              (expand_string uppercase_char_map)
              l1)
  in
  List.flatten
    (List.rev_map (expand_string special_char_map) l2)

let remove_acute s =
  string_map special_char_map s

let lowercase s =
  string_map ~dft:String.lowercase_ascii lowercase_char_map s

let correct_string s =
  lowercase (remove_acute s)

let correct_string_utf8 s =
  string_map special_char_utf8_map s

let correct_string_url s =
  string_map special_char_url_map s

let correct_string_email_latex s =
  string_map special_char_email_latex s

let correct_string_latex s =
  string_map special_char_latex_map s

let correct_string_html s =
  string_map special_char_html_map s

let correct_string_txt s =
  string_map special_char_txt_map s

let correct_string_filename s =
  string_map special_char_file_name_map s

let correct_string_csv s =
  string_map special_char_csv_map s

let gen_char ?dft map  c =
  let dft =
    match dft with
      | None -> (fun x->x)
      | Some f -> f
  in
  match c with
    | [a] ->
      begin
        match
          CharMap.find_opt a (fst map)
        with
          | None -> dft (String.make 1 a)
          | Some a -> a
      end
    | [a;b] ->
    begin
      match
        Char2Map.find_opt (a,b) (snd map)
      with
        | None -> dft ((String.make 1 a)^(String.make 1 b))
        | Some a -> a
    end
    | [] | _::_::_::_ -> failwith "BAD STRING in Special_char tabs"

let uppercase_char = gen_char ~dft:String.uppercase_ascii uppercase_char_map
let lowercase_char = gen_char ~dft:String.lowercase_ascii lowercase_char_map
let lowercase_spe = gen_char lowercase_char_map

let uppercase s =
  string_map
      ~dft:String.uppercase_ascii
      uppercase_char_map s
let lowercase s =
  string_map
      ~dft:String.lowercase_ascii
      lowercase_char_map s

let capitalize s =
  let n = String.length s in
  let rec aux k bool accu =
    if k=n
    then String.concat "" (List.rev accu)
    else
      if k = n-1
      then
        let c1 = String.get s k in
        let elt =
          if bool then uppercase_char [c1] else (String.make 1 c1)
        in
        aux (k+1) (s.[k]=' ' || s.[k]='-') (elt::accu)
      else
        let c1 = String.get s k in
        let c2 = String.get s (k+1) in
        match Char2Map.find_opt (c1,c2) (snd special_char_map) with
        | Some c -> aux (k+2) false (if bool then c::accu else ((String.make 1 c2)::(String.make 1 c1)::accu))
        | None ->
          begin
            let elt =
              if bool then uppercase_char [c1] else (String.make 1 c1)
            in
            aux (k+1) (c1=' ' || c1='-') (elt::accu)
         end
  in aux 0 true []

let delimiter =
  [' ';'-';
   '\"';'\'';
   '\\';',';'.';'?';':';'!';'{';'}';'_';'[';']';'#']

let correct_buggy s =
let n = String.length s in
let rec aux k accu =
  if k=n
  then String.concat "" (List.rev accu)
  else
    if k = n-1
    then
      let c1 = String.get s k in
      let elt =
        if k=0
        then String.make 1 c1
        else lowercase_spe [c1]
      in
      aux (k+1) (elt::accu)
    else
      let c1 = String.get s k in
      let c2 = String.get s (k+1) in
      match Char2Map.find_opt (c1,c2) (snd special_char_map) with
      | Some _ ->
          let elt =
            if k=0
            then String.sub s k 2
            else lowercase_spe [c1;c2]
          in
          aux (k+2) (elt::accu)
      | None ->
        begin
          let elt =
            if k=0
            then String.make 1 c1
            else lowercase_spe [c1]
          in
          aux (k+1)  (elt::accu)
       end
in aux 0 []


let update s i j has_buggy has_lowercase accu =
    if has_lowercase && has_buggy then
      (correct_buggy (String.sub s i (j-i+1)))::accu
    else
      (String.sub s i (j-i+1))::accu

let clean_spurious_uppercase_letters s =
    let n = String.length s in
    let rec aux k has_buggy has_lowercase start accu =
      if k=n
      then
        let accu =
          if start = n then accu
          else update s start (n-1) has_buggy has_lowercase accu
        in
        String.concat "" (List.rev accu)
      else
      if k = n-1
      then
        let c1 = String.get s k in
        let sc1 = String.sub s k 1 in
        if List.mem c1 delimiter
        then
           let accu = update s start k has_buggy has_lowercase accu in
           aux (k+1) false false (k+1) accu
        else
          let has_lowercase =
            has_lowercase ||
            (sc1 = lowercase_char [c1] && sc1 <> uppercase_char [c1])
          in
          let has_buggy =
            has_buggy ||
            (String.lowercase_ascii sc1 <> lowercase_char [c1])
          in
          aux (k+1) has_buggy has_lowercase start accu
      else
        let c1 = String.get s k in
        let c2 = String.get s (k+1) in
        let sc1 = String.sub s k 1 in
        let sc12 = String.sub s k 2 in
        match Char2Map.find_opt (c1,c2) (snd special_char_map) with
          | Some _ ->
            let has_lowercase =
              has_lowercase ||
              (sc12 = lowercase_char [c1;c2]
              && sc12 <> uppercase_char [c1;c2])
            in
            let has_buggy =
              has_buggy ||
              String.lowercase_ascii sc12 <> lowercase_char [c1;c2]
            in
            aux (k+2) has_buggy has_lowercase start accu
          | None ->
            begin
              if List.mem c1 delimiter
              then
                let accu = update s start k has_buggy has_lowercase accu in
                aux (k+1) false false (k+1) accu
              else
                let has_lowercase =
                  has_lowercase ||
                  (sc1 = lowercase_char [c1] && sc1 <> uppercase_char [c1])
                in
                let has_buggy =
                  has_buggy ||
                  String.lowercase_ascii sc1 <> lowercase_char [c1]
                in
                aux (k+1) has_buggy has_lowercase start accu
            end
    in aux 0 false false 0 []

let clean_mlle_gen char s =
  let a = String.split_on_char char s in
  let a =
    List.rev_map
      (fun x ->
         let n = String.length x in
         if n<4 then x
         else
         if String.sub x (n-4) 4 = "Mlle"
         then
           (String.sub x 0 (n-4))^"Mme"
         else x)
      (List.rev a )
  in
  let a = String.concat (String.make 1 char) a in
  a

let clean_mlle s =
  List.fold_left
    (fun s char -> clean_mlle_gen char s)
    s [' ';'}']

let split_name s =
  let a = String.split_on_char ' ' s in
  let rec aux to_do gender firstname lastname =
    match to_do with
    | [] ->
      String.concat " " gender,
      String.concat " " firstname,
      String.concat " " lastname
    | h::t ->
      begin
        if
          List.mem (lowercase h)
            ["m";"mr";"monsieur";"m.";"mr.";
             "mlle";"mme";"mlle.";"mme.";"madame";"mademoiselle"]
        then
          aux t (h::gender) firstname lastname
        else
        if capitalize h = h
        then aux t gender (h::firstname) lastname
        else
          aux t gender firstname (h::lastname)
      end
  in
  aux (List.rev a ) [] [] []
