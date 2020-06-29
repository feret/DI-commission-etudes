type t = Public_data.dpt Public_data.AcronymMap.t

let empty = Public_data.AcronymMap.empty

let unify ~safe_mode logger prefix pos error dpt dpt' =
  if
    Special_char.correct_string dpt.Public_data.dpt_nom
    =
    Special_char.correct_string dpt'.Public_data.dpt_nom
    &&
    Special_char.correct_string dpt.Public_data.dpt_gerundif
    =
    Special_char.correct_string dpt'.Public_data.dpt_gerundif
  then
      let error,dpt =
        match dpt.Public_data.dpt_font_color,
              dpt'.Public_data.dpt_font_color
        with
        | _, None -> error,dpt
        | None, _ -> error,
                     {dpt
                      with Public_data.dpt_font_color = dpt'.Public_data.dpt_font_color}
        | Some a, Some a' when a = a' -> error,dpt
        | Some _, Some _ ->
            let message =
              Format.sprintf
                "Incompatible font colors for %s and %s"
                dpt.Public_data.dpt_acronyme
                dpt'.Public_data.dpt_acronyme
            in
            Exception.warn
              logger
              ~safe_mode
              ~message
              prefix
              error
              pos
              Exit
              dpt
      in
      let error,dpt =
        match dpt.Public_data.dpt_bg_color,
              dpt'.Public_data.dpt_bg_color
        with
        | _, None -> error,dpt
        | None, _ -> error,
                     {dpt
                      with Public_data.dpt_bg_color = dpt'.Public_data.dpt_bg_color}
        | Some a, Some a' when a = a' -> error,dpt
        | Some _, Some _ ->
            let message =
              Format.sprintf
                "Incompatible background colors for %s and %s"
                dpt.Public_data.dpt_acronyme
                dpt'.Public_data.dpt_acronyme
            in
            Exception.warn
              logger
              ~safe_mode
              ~message
              prefix
              error
              pos
              Exit
              dpt
      in
      error, dpt
  else
  let message =
    Format.sprintf
      "Cannot unify dpt data with different names, acronymes, and gerundif  %s %s %s VS %s %s %s"
      dpt.Public_data.dpt_nom
      dpt.Public_data.dpt_acronyme
      dpt.Public_data.dpt_gerundif
      dpt'.Public_data.dpt_nom
      dpt'.Public_data.dpt_acronyme
      dpt'.Public_data.dpt_gerundif
  in
  Exception.warn
    logger
    ~safe_mode
    ~message
    prefix
    error
    pos
    Exit
    dpt

let get_dpt  ~acronym dpt =
  let acronym =
    String.lowercase_ascii acronym
  in
    Public_data.AcronymMap.find_opt
      acronym
      dpt


let add_dpt
    ~safe_mode logger prefix pos error
    dpt dpts =
  let acronym = dpt.Public_data.dpt_acronyme in
  let dpt' =
    get_dpt ~acronym dpts
  in
  let error, dpt =
    match dpt' with
    | None -> error, dpt
    | Some b ->
      unify ~safe_mode logger prefix pos error
        dpt b
  in
  let dpts =
    Public_data.AcronymMap.add
      acronym
      dpt
      dpts
  in
  error, dpts
