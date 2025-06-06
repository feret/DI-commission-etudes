let unsome a_opt a =
  match a_opt with
  | Some a -> a
  | None -> a

let unsome_string s_opt = unsome s_opt ""
let unsome_float f_opt =
    match f_opt with
      | None -> ""
      | Some f -> (Format.sprintf "%f" f)

let remove_space_from_string s =
  let seq = String.to_seq s in
  let seq = Seq.filter (fun x -> not (x=' ')) seq in
  String.of_seq seq

let remove_single_quote_from_string s =
    let seq = String.to_seq s in
    let seq = Seq.filter (fun x -> not (x='\'')) seq in
    String.of_seq seq

let remove_space_and_single_quote_from_string s =
    remove_single_quote_from_string (remove_space_from_string s)

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

let split_rep_filename s =
  match String.rindex_opt s '/' with
  | Some i -> String.sub s 0 i, String.sub s (i+1) (String.length s - (i+1))
  | None -> "",s

let basename x =
  match String.rindex_opt x '.' with
  | Some i -> String.sub x 0 i
  | None -> x

let extension x =
  match String.rindex_opt x '.' with
  | Some i -> String.sub x (i+1) (String.length x - (i+1))
  | None -> ""

let space_only s =
    let size = String.length s in
    let rec aux k =
      if k>=size then true
      else
        (String.get s k) =' ' && aux (k+1)
    in
    aux 0

let substring s s' =
  let n = String.length s in
  let n' = String.length s' in
  let rec aux k =
    if k+n > n' then false
    else
    if s = String.sub s' k n
    then
      true
    else
      aux (k+1)
  in
  aux 0

let fun_ignore = (fun state _ x -> state, x)

let collect_string set state data x =
  state,
  let data =
    match data with
    | Some x when String.trim x = "" -> None
    | _ -> data
  in
  set data x

let collect_conv string from warn set state data x =
    let state, data =
      match data with
      | Some x when String.trim x = "" -> state, None
      | Some x ->
        begin
          try
            let state, int_opt = from warn state x in
            begin
              match int_opt with
              | None ->
                let msg = Format.sprintf string x in
                let state = warn msg state
                in state, None
              |Some _ -> state, int_opt
            end
          with
          | _ ->
            let msg = Format.sprintf string x in
            let state = warn msg state in
            state, None
        end
      | None -> state, None
    in
    state, set data x

let correct_comma ?force_dec_sep_to_dot get_comma_symbol state t =
  let corrected_size = (String.length t)-1 in
  match t.[corrected_size] with
  | '.' | ',' -> state, String.sub t 0 corrected_size
  | _ ->
    let state, comma =
      match force_dec_sep_to_dot with
      | Some true -> state, '.'
      | _ -> get_comma_symbol state
    in
    state,
    if comma = '.'
    then t
    else
      String.map
        (fun x ->
           if x = '.'
           then comma
           else x)
        t

let remove_comma =
  String.map
    (fun x ->
       if x = ','
       then '.'
       else x)

let float_of_string warn state t =
  if space_only t
  then state, None
  else
    let t = remove_comma t in
    try
      state, Some (float_of_string t)
    with
    | _ ->
      try
        state, Some (float_of_int (int_of_string t))
      with
      | _ ->
        let msg =
          Format.sprintf "Undefined String (%s)-> float conversion" t
        in
        let state =
          warn
            msg
            state
        in state, None



let float_to_string ?force_dec_sep_to_dot get_comma_symbol state f =
  let s = string_of_float f in
  correct_comma ?force_dec_sep_to_dot get_comma_symbol state s


let collect_float warn =
  collect_conv "string %s cannot be converted into a float" float_of_string warn

let bool_of_string warn state s =
  let s = Special_char.lowercase (String.trim s) in
  if List.mem s ["o";"oui";"y";"yes";"ok";"true"]
  then state, Some true
  else if List.mem s ["n";"no";"non";"false"]
  then state, Some false
  else
    let msg =
      Format.sprintf "Undefined String (%s)-> Boolean conversion" s
    in
    warn msg state, None

let collect_bool warn  =
  collect_conv "string %s cannot be converted into a Boolean"
    bool_of_string warn

let sort f p l =
  let l =
    List.rev_map
      (fun a -> f a,a)
      l
  in
  let cmp a b = p b a in
  let l = List.sort cmp l in
  let l =
    List.rev_map
      snd
      l
  in
  l

let valide_sans_note = "validé \\newline (sans note)"
let valide_sans_note_en = "valided \\newline (without grade)"

let prepare_report
    ~cmp ~headers list =
  let rec p cmp_list a b =
    match cmp_list with
    | [] -> compare a b
    | h::t ->
      let cmp = h a b in
      if cmp = 0
      then p t a b
      else
        cmp
  in
  let list =
    List.sort (p cmp) list
  in
  let rec clean list old acc =
    match list with
    | h::t when h=old -> clean t old acc
    | h::t -> clean t h (h::acc)
    | [] -> List.rev acc
  in
  let list =
    match list with
    | [] -> []
    | h::t -> clean t h [h]
  in
  let rev_headers = List.rev headers in
  let tag a =
    List.rev_map (fun f -> f a) rev_headers
  in
  let list =
    List.rev_map
      (fun a -> tag a,a)
      (List.rev list)
  in
  list

  let dump_report
    ~print_header
    ~open_array
    ~open_row
    ~close_row
    ~print_cell
    ~close_array
    ~string_of_headers
    ~string_of_column
    ~(settitle:(('logger -> ('c, Format.formatter, unit) format -> 'c) * string)
          list -> unit)
    ~(setpreamble:(('logger -> ('d, Format.formatter, unit) format -> 'd) * string)
          list -> unit)
    ~(setheadpage:
    ?color:Color.color ->
      (('logger -> ('e, Format.formatter, unit) format -> 'e) * string) list  -> unit)
    ~(setfootpage:
      ?color:Color.color ->
        (('logger -> ('f, Format.formatter, unit) format -> 'f) * string) list  -> unit)
    ~(setsignature:(('logger -> ('g, Format.formatter, unit) format -> 'g) * string)
         list -> unit)
    ?title
    ?headpage
    ?headcolor
    ?footpage
    ?footcolor
    ?preamble
    ?signature
    list =
    let n = List.length list in
    let () =
      match title with
      | None -> ()
      | Some t -> settitle t
    in
    let _ =
      match headpage with
      | None ->
        setheadpage []
      | Some f ->
        let color = headcolor in
        setheadpage ?color (f n)
    in
    let _ =
      match footpage with
      | None -> setfootpage []
      | Some f ->
        let color = footcolor in
        setfootpage ?color f
    in
    let _ =
      match preamble with
      | None -> ()
      | Some f -> setpreamble (f n)
    in
    let array_of_headers =
      Array.of_list string_of_headers
    in
    let compute_headers (a,b) c =
      let a = String.trim (String.concat " " a) in
      if a = ""
      then
        Format.sprintf "%s" (b c)
      else
        Format.sprintf "%s : %s" a (b c)
    in
    let compare_headers a b_opt =
      match b_opt
      with
      | None -> Some (1,a)
      | Some b ->
        let rec aux i a b =
          match a,b with
          | h::t, h'::t' when h=h' ->
            aux (i+1) t t'
          | [], _ -> None
          | _::_, _ -> Some (i,a)
        in
        aux 1 a b
    in
    let dump_row a =
      let () = open_row () in
      let () =
        List.iter
          (fun (_,f) -> print_cell (f a))
          string_of_column
      in
      let () = close_row () in
      ()
    in
    let rec aux list old =
      match list with
      | [] -> old
      | (headers,a)::t ->
        let old =
          begin
            match compare_headers headers old with
            | None ->
              let () = dump_row a in old
            | Some (i, residue) ->
              begin
                let () =
                  match old with
                  | None -> ()
                  | Some _ -> close_array ()
                in
                let rec aux i residue =
                  match residue with
                  | [] -> ()
                  | h::t ->
                    let () =
                      print_header i
                        (compute_headers (array_of_headers.(i-1)) h)
                    in
                    aux (i+1) t
                in
                let () = aux i residue in
                let (_:bool) =
                  open_array
                    ~title:(List.rev_map
                              fst
                              (List.rev string_of_column))
                in
                let () = dump_row a in
                Some (headers)
            end
          end
        in
        aux t old
    in
    let () =
      match aux list None with
      | None -> ()
      | Some _ -> close_array ()
    in
    let () =
      match signature with
      | None -> ()
      | Some f -> setsignature (f n)
    in
    ()


let build_output
    pos
    ~has_promo
    ~get_repository
    ~get_store_according_promotion
    ~get_indicate_promotions_in_file_names
    ~rec_mk_when_necessary
    ~f_firstname ~f_lastname ~firstname ~lastname ~promotion ?prefix ?output_repository ?output_file_name ~extension state =
  let firstname = f_firstname firstname in
  let lastname = f_lastname lastname in
  let state, output_repository =
    match output_repository with
    | None -> get_repository state
    | Some rep -> state, rep
  in
  let promotion =
    match promotion with
    | None -> ""
    | Some x -> x
  in
  let state, prefix =
    match prefix with
    | None ->
      let state, bool =
        get_store_according_promotion state
      in
      state, if bool && not has_promo then promotion else ""
    | Some prefix -> state, prefix
  in
  let state, output_file_name =
    match output_file_name with
    | None ->
      let state,bool =
        get_indicate_promotions_in_file_names state
      in
      state, (if promotion = "" && not bool
              then ""
              else promotion^"_")^lastname^"_"^firstname^extension
    | Some file_name -> state, file_name
  in
  let output_file_name =
    remove_space_and_single_quote_from_string output_file_name
  in
  let output_repository =
    match output_repository,prefix  with
    | ".",prefix | "",prefix -> prefix
    | x,"" -> x
    | x1,x2 ->
      Printf.sprintf "%s/%s" x1 x2
  in
  let state, output_repository =
    rec_mk_when_necessary
      pos
      state output_repository
  in
  state, output_repository, output_file_name

let find_starting_with
    ~file_exists ~warn
    ~prefix ~between state file_name =
  let state, b =
    file_exists
      __POS__
      state
      file_name
  in
  if not b then
    warn __POS__
      (Printf.sprintf
         "While picture extraction, file %s has not been produced" file_name)
      Exit
      state,
    None
  else
  let channel =
    open_in file_name
  in
  let n = String.length prefix in
  let rec aux acc =
    let a =
      try
        Some (input_line channel)
      with
      | End_of_file -> None
    in
    match a with
    | None -> acc
    | Some s ->
      begin
        let ls = String.split_on_char between s in
        let acc =
          List.fold_left
            (fun acc elt ->
               if String.length elt > n && String.sub elt 0 n = prefix
               then elt::acc
               else acc)
            acc ls
        in
        aux acc
      end
  in
  let state, output =
    match aux [] with
    | [] ->
      warn
        __POS__
        (Format.sprintf "no url starting with %s found in %s" prefix file_name)
        Exit
        state, None
    | a::_::_ ->
      warn
        __POS__
        (Format.sprintf "several urls starting with %s found in %s" prefix file_name)
        Exit
        state, Some a
    | [a] -> state, Some a
  in
  let () = close_in channel in
  state, output

let get_option state get opt =
  match opt with
  | None -> get state
  | Some x -> state, x

let include_latex_list ?prefix f state list
  =
  let prefix =
      match prefix with
        | None -> ""
        | Some a -> a
  in
  let b = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer b in
  let k =
    List.fold_left
      (fun n x ->
         let () =
           Format.fprintf fmt
             "\\IfFileExists{%s}%%\n\ {%s%s}%%\n\ {"
             x prefix (f x)
         in
         n+1)
      0
      list
  in
  let () =
    Format.fprintf
      fmt
      "%s%%\n\ "
      (String.init k (fun _ -> '}'))
  in
  let s = Buffer.contents b in
  let () = Buffer.reset b in
  state, s

let correct_year year =
    if String.length year = 4 then year
    else
    if String.length year = 2 then
      if int_of_string year > 70 then Format.sprintf "19%s" year
      else Format.sprintf "20%s" year
    else
      year

let date_to_string_fr s =
  match String.split_on_char '/' s with
  | [day;month;year] ->
    let correct_month =
      match month with
      | "1" | "01" -> "janvier"
      | "2" | "02" -> "février"
      | "3" | "03" -> "mars"
      | "4" | "04" -> "avril"
      | "5" | "05" -> "mai"
      | "6" | "06" -> "juin"
      | "7" | "07" -> "juillet"
      | "8" | "08" -> "août"
      | "9" | "09" -> "septembre"
      | "10" -> "octobre"
      | "11" -> "novembre"
      | "12" -> "décembre"
      | _ -> month
    in
    let correct_year = correct_year year in
    Format.sprintf
      "le %s %s %s" day correct_month correct_year
  | _ -> s

let date_to_string_en s =
  match String.split_on_char '/' s with
  | [day;month;year] ->
    let correct_day =
      let shorten_day =
        if String.get day 0 = '0' then String.sub day 1 1  else day
      in
      match shorten_day with
      | "1" | "21" | "31" -> Format.sprintf "%s1st" shorten_day
      | "2" | "22" -> Format.sprintf "%snd" shorten_day
      | "3" | "23" -> Format.sprintf "%srd" shorten_day
      | _ -> Format.sprintf "%sth" shorten_day
    in
    let correct_month =
      match month with
      | "1" | "01" -> "January"
      | "2" | "02" -> "February"
      | "3" | "03" -> "March"
      | "4" | "04" -> "April"
      | "5" | "05" -> "May"
      | "6" | "06" -> "June"
      | "7" | "07" -> "July"
      | "8" | "08" -> "August"
      | "9" | "09" -> "September"
      | "10" -> "October"
      | "11" -> "November"
      | "12" -> "December"
      | _ -> month
    in
    let correct_year = correct_year year in
    Format.sprintf
      "the %s of %s %s" correct_day correct_month correct_year
  | _ -> s

let int_of_string warn state t =
  try
    state, Some (int_of_string t)
  with
  | _ ->
    begin
      let t = remove_comma t in
      let state, a = float_of_string warn state t in
      match a with
      | None -> state, None
      | Some a ->
        if float_of_int (int_of_float a) = a
        then
          state, Some (int_of_float a)
        else
          let msg =
            Format.sprintf "Undefined String (%s)-> int conversion" t
          in
          let state =
            warn
              msg
              state
          in
          state, None
    end

let collect_int warn =
  collect_conv
    "string %s cannot be converted into an int"
    int_of_string warn

let translate_et a =
  let l = String.split_on_char ' ' a in
  let l = List.rev_map
            (fun x -> if x = "et" then "and" else x)
            (List.rev l)
  in
  String.concat " " l

let split_on_backslash_n s =
  let n = String.length s in
  let rec aux d k state acc =
      let e = d+k-1 in
      if e >= n then
         (d,k-1)::acc
      else
        let c = Char.code (String.get s e) in
        match c with
          | 92 -> aux d (k+1) true acc
          | 110 when state -> aux (e+1) 0 false ((d,k-2)::acc)
          | _ -> aux d (k+1) false acc
  in
  let l = aux 0 1 false [] in
  List.fold_left
      (fun acc (d,k) -> (String.sub s d k )::acc)
      [] l

let replace_backslash_n_with_spaces s =
  let l = split_on_backslash_n s in
  String.concat " " l

let decompose_name l =
    let rec aux l lastname =
      match l with
        | [] -> List.rev lastname, l
        | h::t ->
          if Special_char.uppercase h = h
          then
            aux t  (h::lastname)
          else
            List.rev lastname, l
   in
   let lastname, firstname = aux l [] in
   let firstname, lastname =
    match firstname with
      | [] ->
        begin
          match List.rev lastname with
              | h::q -> String.capitalize_ascii h, String.concat " " (List.rev q)
              | _ -> "", String.concat " " (List.rev lastname)
        end
      | _ ->
      let firstname = String.concat " " firstname in
      let lastname = String.concat " " lastname in
      firstname, lastname
   in
   lastname, firstname

let get_teachers list =
    match list with None -> []
      | Some list ->
    let n = String.length list in
    let rec split deb k acc =
        if k=n then ((String.sub list deb (k-deb))::acc) else
        if (k+1<n && String.sub list k 2 = "et") then
           split (k+2) (k+2) ((String.sub list deb (k-deb+1))::acc)
        else if
           (k+2<n && String.sub list k 3 = "and") then
           split (k+3) (k+3) ((String.sub list deb (k-deb+1))::acc)
        else split deb (k+1) acc
    in
    let l = split 0 0 [] in
      List.rev_map
        (fun a -> decompose_name (String.split_on_char ' ' a))
        l

let simplify_spaces s =
  let l = String.split_on_char ' ' s in
  let l = List.filter (fun x -> x<>"") l in
  String.concat " " l

let remove_ending_dash libelle =  
    if String.length libelle = 0 
    then libelle 
    else 
       if String.get libelle ((String.length libelle)-1)  = '-'
       then 
          String.trim (String.sub libelle 0 ((String.length libelle)-1))
       else 
          libelle 

let normalise_and s = 
    let l = String.split_on_char '&' s in 
    String.concat " & " (List.rev_map String.trim (List.rev l))

let simplify_libelle s = normalise_and (remove_ending_dash (simplify_spaces s))

let hash_libelle s = 
  let l = String.split_on_char ' ' s in 
  String.lowercase_ascii (String.concat "" l)  

let remove_end ~suffix s = 
  let n = String.length suffix in 
  let m = String.length s in 
  if m >= n then 
    if String.sub s (m-n) n = suffix 
    then String.sub s 0 (m-n)
    else s  
  else 
    s 