type t = Public_data.note

let space_only s =
  let size = String.length s in
  let rec aux k =
    if k>=size then true
    else
      (String.get s k) =' ' && aux (k+1)
  in
  aux 0

let correct_comma ?force_dec_sep_to_dot state t =
  let corrected_size = (String.length t)-1 in
  match t.[corrected_size] with
    | '.' | ',' -> state, String.sub t 0 corrected_size
    | _ ->
      let state, comma =
        match force_dec_sep_to_dot with
        | Some true -> state, '.'
        | _ -> Remanent_state.get_comma_symbol state
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

let float_of_string pos state t =
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
        Remanent_state.warn_dft
        pos
        msg
        Exit
        None
        state

let int_of_string pos state t =
  try
    state, Some (int_of_string t)
  with
  | _ ->
    begin
      let t = remove_comma t in
      let state, a = float_of_string pos state t in
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
          Remanent_state.warn_dft
            pos
            msg
            Exit
            None
            state
    end

let float_to_string ?force_dec_sep_to_dot state f =
  let s = string_of_float f in
  correct_comma ?force_dec_sep_to_dot state s


let to_string _pos ?force_dec_sep_to_dot state t =
  match t with
  | Public_data.Float f ->
    float_to_string ?force_dec_sep_to_dot state f
  | Public_data.Absent -> state, "abs"
  | Public_data.En_cours -> state, "en cours"
  | Public_data.Abandon -> state, "abandon"
  | Public_data.Valide_sans_note ->
    state, "validé (sans note)"

let of_string pos state s =
  if space_only s then
    state, Some Public_data.En_cours
  else
    match
      String.lowercase_ascii s
    with
    | "abandon" ->
      state,
      Some Public_data.Abandon
    | "validé (sans note)" ->
      state,
      Some Public_data.Valide_sans_note
    | "en cours" ->
      state,
      Some Public_data.En_cours
    | f ->
      begin
        match
          float_of_string pos state f
        with
        | state, None -> state, None
        | state, Some f -> state, Some (Public_data.Float f)
      end

let valide f =
  match f with
  | Public_data.Float f -> Some (f >= 10.)
  | Public_data.Valide_sans_note
    -> Some true
  | Public_data.Abandon
  | Public_data.Absent -> Some false
  | Public_data.En_cours -> None

let a_compter f =
  match valide f,f with
  | None,_ -> None
  | Some true, Public_data.Float f -> Some (f>=10.)
  | Some true, (Public_data.Valide_sans_note

               | Public_data.Abandon
               | Public_data.En_cours
               | Public_data.Absent)
  | Some false,_
    -> Some false

let compare a b =
  match a,b
  with
  | Public_data.Float a,
    Public_data.Float b -> compare a b
  | _, Public_data.Float _ -> 1
  | Public_data.Float _, _ -> (-1)
  | Public_data.Valide_sans_note, Public_data.Valide_sans_note -> 0
  | _, Public_data.Valide_sans_note -> 1
  | Public_data.Valide_sans_note,_ -> -1
  | Public_data.En_cours, Public_data.En_cours -> 0
  | _, Public_data.En_cours -> 1
  | Public_data.En_cours,_ -> (-1)
  | Public_data.Absent,Public_data.Absent -> 0
  | _, Public_data.Absent -> 1
  | Public_data.Absent, _ -> (-1)
  | Public_data.Abandon, Public_data.Abandon -> 0

let string_of_ects f_opt =
  match f_opt with
  | None -> ""
  | Some f ->
    if float_of_int (int_of_float f) = f
    then
      string_of_int (int_of_float f)
    else
      string_of_float f
