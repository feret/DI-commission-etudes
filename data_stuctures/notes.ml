type t = Public_data.note

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

let correct_comma_easy  t =
  let corrected_size = (String.length t)-1 in
  match t.[corrected_size] with
  | '.' | ',' -> String.sub t 0 corrected_size
  | _ ->
    let comma = ',' in
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

let float_to_string_easy f =
  let s = Format.sprintf "%.2f" f in
  let f' = float_of_string s in
  let s =
    if f'+.0.005 = f
    then
      Format.sprintf "%.2f" (f'+.0.01)
    else s
  in
  correct_comma_easy  s

let float_of_string pos state t =
  if Tools.space_only t
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



let valide_sans_note = Tools.valide_sans_note
let valide_sans_note_en = Tools.valide_sans_note_en

let to_string _pos ?force_dec_sep_to_dot state t =
  match t with
  | Public_data.String s -> state, s
  | Public_data.Float f ->
    float_to_string ?force_dec_sep_to_dot state f
  | Public_data.Temporary f ->
    let state, f = float_to_string ?force_dec_sep_to_dot state f in
    Remanent_state.bilingual_string
      ~french:(Format.sprintf "%s (partiel)" f)
      ~english:(Format.sprintf "%s (partial)" f)
      state
  | Public_data.Absent -> state, "abs"
  | Public_data.En_cours ->
    Remanent_state.bilingual_string
      ~french:"en cours"
      ~english:"in progress"
      state
  | Public_data.Abandon ->
  Remanent_state.bilingual_string
    ~french:"abandon"
    ~english:"quitted"
    state
  | Public_data.Valide_sans_note ->
    Remanent_state.bilingual_string
      ~french:valide_sans_note
      ~english:valide_sans_note_en
      state

let valid_string f =
  List.mem
      (String.trim f)
      ["A";"A+";"A-";"B";"B+";"B-";"C";"C+";"C-";"D";"D+";"D-";"E";"E+";"E-";"P"]

let valide_string f =
List.mem
    (String.trim f)
    ["A";"A+";"A-";"B";"B+";"B-";"C";"C+";"P"]

let of_string pos state s v =
  if Tools.space_only s then
    state, Some Public_data.En_cours
  else
    match
      String.lowercase_ascii s
    with
    | "abandon" ->
      state,
      Some Public_data.Abandon
    | s when s=valide_sans_note ->
      state,
      Some Public_data.Valide_sans_note
    | "en cours" ->
      state,
      Some Public_data.En_cours
    | f ->
      begin
        if valid_string f
        then
            state, Some (Public_data.String f)
        else
          match
            float_of_string pos state f
          with
          | state, None ->
            Remanent_state.warn_dft
              pos
              "Wrong format of note"
              Exit
              None
              state
          | state, Some f -> state, Some (
              match v with Some _ -> Public_data.Float f
                        | None -> Public_data.Temporary f)
        end

let valide f =
  match f with
  | Public_data.String s -> Some (valide_string s)
  | Public_data.Float f -> Some (f >= 10.)
  | Public_data.Valide_sans_note
    -> Some true
  | Public_data.Abandon
  | Public_data.Absent -> Some false
  | Public_data.Temporary _
  | Public_data.En_cours -> None

let temporary f =
  match f with
  | Public_data.Temporary _
  | Public_data.En_cours -> Some true
  | Public_data.Float _
  | Public_data.Valide_sans_note
  | Public_data.Abandon
  | Public_data.String _
  | Public_data.Absent -> Some false

let a_compter f =
  match valide f,f with
  | None,_ -> None
  | Some true, Public_data.Float f -> Some (f>=10.)
  | Some true, (Public_data.Valide_sans_note
               | Public_data.Temporary _
               | Public_data.Abandon
               | Public_data.En_cours
               | Public_data.Absent
               | Public_data.String _)
  | Some false,_
    -> Some false

let en_cours f =
  match f with (Public_data.En_cours | Public_data.Temporary _) -> true
             | Public_data.String _
             | Public_data.Float _
             | Public_data.Abandon
             | Public_data.Absent
             | Public_data.Valide_sans_note -> false


let compensable f =
  match valide f,f with
  |(Some true | None),_ -> false
  | Some false , Public_data.Float f -> f < 10.
  | Some false, (Public_data.Valide_sans_note
                | Public_data.String _
                | Public_data.Abandon
                | Public_data.Temporary _
                | Public_data.En_cours
                | Public_data.Absent) -> false


let compare a b =
  match a,b
  with
  | Public_data.Float a,
    Public_data.Float b -> compare a b
  | _, Public_data.Float _ -> 1
  | Public_data.Float _, _ -> (-1)
  | Public_data.String a,
    Public_data.String b -> compare a b
  | _, Public_data.String _ -> 1
  | Public_data.String _, _ -> (-1)
  | Public_data.Valide_sans_note, Public_data.Valide_sans_note -> 0
  | _, Public_data.Valide_sans_note -> 1
  | Public_data.Valide_sans_note,_ -> -1
  | Public_data.Temporary a,
    Public_data.Temporary b -> compare a b
  | _, Public_data.Temporary _ -> 1
  | Public_data.Temporary _, _ -> (-1)
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
