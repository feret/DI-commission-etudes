type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:Public_data.main_dpt ->
  ?recu:bool ->
  ?academicyear:string ->
  ?headpage:(int -> ((Loggers.t -> (string -> unit, Format.formatter, unit) format -> string -> unit) *
                     string)
               list)
  ->
  ?footpage:((Loggers.t ->
                       (string -> unit, Format.formatter, unit) format ->
                       string -> unit) *
                      string)
                     list ->
  ?footcolor:Color.color ->
  ?title:(((Loggers.t -> (string -> unit, Format.formatter, unit) format -> string -> unit) *
           string)
            list)  ->
  ?preamble:(int -> ((Loggers.t -> (string -> unit, Format.formatter, unit) format -> string -> unit) *
                     string)
               list)  ->
  ?signature:(int -> ((Loggers.t -> (string -> unit, Format.formatter, unit) format -> string -> unit) *
                      string)
                list) ->
  Gen.dump



module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
  val dump_per_student: dump
end

module Build
    (I:Gen.Interface
     with type elt = Public_data.diplome_national) =
struct

  let dump_national_diploma_list
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_national_diploma_list
    in
    let filter = Gen.filter_national_diploma in
    let get = I.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.get_repository in
    Gen.dump_elts
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name ?event_opt
      ~headerextralength:5
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state


  let nom_etudiant =
    ["NOM"],
    (fun a -> a.Public_data.diplome_lastname)
  let prenom_etudiant =
    ["PRENOM"],
    (fun a -> a.Public_data.diplome_firstname)
  let promotion =
    ["PROMOTION"],
    (fun a -> a.Public_data.diplome_promotion
    )
  let moyenne =
    ["Moyenne"],
    (fun a ->
       match a.Public_data.diplome_moyenne with
       | None -> ""
       | Some f ->
         Notes.float_to_string_easy f)
  let mention =
    ["Mention"],
    (fun a ->
       match a.Public_data.diplome_mention with
       | None -> ""
       | Some a -> a)
  let resultat =
    ["Résultat"],
    (fun a ->
       Format.sprintf
         "%s%s"
         (if a.Public_data.diplome_recu then "Reçu" else "Ajourné")
         (match a.Public_data.diplome_gender with
          | Public_data.Masculin -> ""
          | Public_data.Feminin -> "e"
          | Public_data.Unknown -> "(e)")
    )
  let departement =
    ["Département"],
    (fun a -> a.Public_data.diplome_dpt)
  let level =
    ["Niveau"],
    (fun a -> a.Public_data.diplome_niveau)
  let ects =
    ["Nbects"],(fun a -> string_of_float (a.Public_data.diplome_nb_ects))
  let year =
    ["Année"],(fun a -> a.Public_data.diplome_year)

  let lresultat =
    [],(fun x -> x),
    (fun a ->
       if a.Public_data.diplome_recu then "Reçu(e)s" else "Ajourné(e)s"
    )
  let dump_per_result_per_student
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> if a.Public_data.diplome_recu then "0" else "1");
        Gen.lift_cmp (fun a -> a.Public_data.diplome_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;moyenne; mention] in
    let headers =
      [
        lresultat;
      ]
    in
    dump_national_diploma_list
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_student
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.diplome_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;resultat;moyenne; mention] in
    let headers =
      []
    in
    dump_national_diploma_list
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  end


module DiplomaReport =
  Build
    (struct
      type elt = Public_data.diplome_national

      let default_file_name = "diploma.html"
      let get = Remanent_state.get_national_diplomas
      let get_repository =
        Remanent_state.get_repository_to_dump_national_diplomas
    end)

let next_year year =
  try
    string_of_int (1+(int_of_string year))
  with
  | _ -> year

let direction_etude =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.di_list

let direction_etude_dma =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.dma_list

let direction_etude_ibens =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.ibens_list

let direction_etude_phys =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.phys_list

let direction_etude_dri =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.dri_list

let dump_attestation
  ?output_repository
  ?prefix
  ?output_file_name
  ?date
  ~signataire
  diplome
  state =
  let date =
    match date with
    | None -> "\\today"
    | Some x -> x
  in
  let state, main_dpt =
    Remanent_state.get_main_dpt state
  in
  let firstname = diplome.Public_data.diplome_firstname in
  let lastname = diplome.Public_data.diplome_lastname in
  let get_repository =
    Remanent_state.get_repository_to_dump_attestations
  in
  let get_store_according_promotion =
    Remanent_state.get_store_output_according_to_their_promotions
  in
  let get_indicate_promotions_in_file_names =
    Remanent_state.get_indicate_promotions_in_attestation_file_names
  in
  let rec_mk_when_necessary =
    Safe_sys.rec_mk_when_necessary
  in
  let dpt = diplome.Public_data.diplome_dpt in
  let promotion =
    Some (diplome.Public_data.diplome_promotion)
  in
  let level = diplome.Public_data.diplome_niveau in
  let f_firstname = (fun x -> x) in
  let f_lastname = (fun x -> x) in
  let state, y =
    try
       let y = int_of_string
           diplome.Public_data.diplome_year
       in
       state, Printf.sprintf "%i-%i." y (y+1)
     with _ ->
       Remanent_state.warn
         __POS__
         (Format.sprintf
            "illegal string for academic year (%s)"
            diplome.Public_data.diplome_year)
         Exit
         state, ""
  in
  let state, output_repository, output_file_name =
    Tools.build_output
      __POS__
      ~has_promo:false
      ~get_repository
      ~get_store_according_promotion
      ~get_indicate_promotions_in_file_names
      ~rec_mk_when_necessary
      ~f_firstname
      ~f_lastname
      ~firstname
      ~lastname
      ~promotion
      ~extension:(Format.sprintf ".attestation.%s.%s%s.tex"
                    begin
                      match
                        level
                      with
                      | "l" -> "L3"
                      | "m" -> "M1"
                      | x -> x
                    end y signataire)
      ?prefix
      ?output_repository
      ?output_file_name
      state
  in
  let file =
    if output_repository = ""
    then output_file_name
    else
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let extension_opt =
    Safe_sys.get_extension output_file_name
  in
  let state, output_channel_opt =
    try
      state, Some (open_out file)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@."
          file
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file)
        Exit
        state ,
      None
  in
  match output_channel_opt with
  | None -> state, None
  | Some out ->
    let state, mode =
      match extension_opt with
      | Some "html" -> state, Loggers.HTML
      | Some "tex" ->
        state, Loggers.Latex Loggers.Normal
      | Some _ ->
        Remanent_state.warn
          __POS__
          (Printf.sprintf
             "Extension of file %s is invalid"
             output_file_name)
          Exit
          state, Loggers.HTML
      | None ->
        Remanent_state.warn
          __POS__
          (Printf.sprintf
             "File %s has no extension"
             output_file_name)
          Exit
          state, Loggers.HTML
    in
    let logger =
      Loggers.open_logger_from_channel
        ~headerextralength:5 ~mode
        out in
    let state, enspsl =
      Remanent_state.get_ENSPSL_logo state
    in
    let () =
      Loggers.setheadpage logger
        [
          Loggers.fprintf_verbatim,
         (Format.sprintf
           "\\IfFileExists{%s}{\\includegraphics{%s}}{}"
           enspsl enspsl)]
    in
    let (),dir_list,dir_dpt =
      match main_dpt with
      | Public_data.DRI ->
      let color = Color.orange in
      Loggers.setfootpage logger ~color
      [
        Loggers.fprintf,
        People.footpage_string
      ],direction_etude_dri,"relations internationales"
      | Public_data.DI | Public_data.ENS ->
        let color = Color.digreen in
        Loggers.setfootpage logger ~color
        [
          Loggers.fprintf,
          People.footpage_string
        ],direction_etude, "informatique"
      | Public_data.ECO ->
          let color = Color.pink in
          Loggers.setfootpage logger ~color
            [
              Loggers.fprintf,
              People.footpage_string_dma
            ], direction_etude_dma, "économie"

      | Public_data.DMA ->
        let color = Color.duckblue in
        Loggers.setfootpage logger ~color
          [
            Loggers.fprintf,
            People.footpage_string_dma
          ], direction_etude_dma, "mathématiques"
      | Public_data.IBENS ->
        let color = Color.green in
        Loggers.setfootpage logger ~color
          [Loggers.fprintf,
           People.footpage_string_ibens], direction_etude_ibens, "biologie"
      | Public_data.PHYS ->
        let color = Color.blue in
        Loggers.setfootpage logger ~color
          [Loggers.fprintf,
           People.footpage_string_phys], direction_etude_phys, "physique"

    in
    let dir =  Public_data.StringMap.find_opt signataire dir_list in
    match dir with
    | None ->
      Remanent_state.warn
        __POS__
        (Format.sprintf "Unknown director")
        Exit
        state, None
    | Some dir ->
      let dir_e = People.e_of_direction dir in
      let dir_name = dir.Public_data.direction_nom_complet in
      let dir_fonction = dir.Public_data.direction_titre in
      let dir_du_dpt = dir.Public_data.direction_departement in
      let s  = dir.Public_data.direction_signature in
      let state, sign =
        match s
        with
        | Some s  ->
          let state, s = s state in
          state, Format.sprintf
          "\\IfFileExists{%s}%%\n\ {\ {\\includegraphics{%s}}}%%\n\ {}\\end{center}\\vfill"
          s s
        | None -> state, "\\end{center}\\vfill"
      in
      let year = diplome.Public_data.diplome_year in
      let level = diplome.Public_data.diplome_niveau in
      let state, cursus_opt =
        Remanent_state.get_cursus
          __POS__
          ~level
          ?dpt:(match level, dpt with
              | "dens",_
              | _,Public_data.DRI
              | _,Public_data.ENS -> None
              | _,(Public_data.DI | Public_data.ECO |
                   Public_data.DMA | Public_data.IBENS | Public_data.PHYS) ->
                Some dpt)
          ~year
          state
      in
      let state, libelle =
        match cursus_opt with
        | None ->
          Remanent_state.warn
            __POS__
            (Format.sprintf
               "unknown cursus %s %s"
               level (Public_data.string_of_dpt dpt))
            Exit
            state, ""
        | Some cursus ->
          match
            cursus.Public_data.inscription
          with
          | None ->
            Remanent_state.warn
              __POS__
              (Format.sprintf
                 "The field inscritpion of cursus %s %s is missing"
                 level (Public_data.string_of_dpt dpt))
              Exit
              state, ""
          | Some a -> state, a
      in
      let body =
        Format.sprintf
          "\\vfill\n\n\\begin{center}\\underline{ATTESTATION}\\end{center}\n\ \\vfill\n\n\  Je soussigné%s, \\textbf{%s}, %s du département %s de l'École Normale Supérieure,\\bigskip\\\\CERTIFIE que,\\bigskip\\\\conformément aux dispositions générales de la scolarité au sein de la formation universitaire en %s de l'ENS et aux décisions de la commission des études du département %s de l'ENS, \\bigskip\\\\\\textbf{%s %s}, a obtenu en %s-%s\\\\\\textbf{%s}\\\\Parcours : \\textbf{Formation interuniversitaire en %s -- ENS Paris}.\\\\ %s \n\n\\vfill\n\\begin{center}Fait à Paris le %s\\smallskip\n\nPour valoir et servir ce que de droit \n\n\n\n"
          dir_e
          dir_name
          dir_fonction
          dir_du_dpt
          dir_dpt
          dir_du_dpt
          lastname
          firstname
          year
          (next_year year)
          libelle
          dir_dpt
          "VALIDÉE"
          date, sign
      in
      let _ = Loggers.fprintf logger  "%s" (fst body) in
      let _ = Loggers.fprintf_verbatim logger  "%s" (snd body) in
      let () = Loggers.close_logger logger in
      state, Some (output_repository,output_file_name)

let dump_attestations
    ~signataires
    ?recu ?academicyear ?niveau ?dpt
    ?output_repository
    ?prefix
    state =
  let state, diplome_list =
    Remanent_state.get_national_diplomas state
  in
  let state, diplome_list =
    Gen.filter
      Gen.filter_national_diploma
      ?recu ?academicyear ?niveau ?dpt
      state
      diplome_list
  in
  List.fold_left
    (fun state diplome ->
       let state, output_rep, date =
         match
           Remanent_state.get_commission_rep_from_key
             (match diplome.Public_data.diplome_niveau with
              | "l" -> "L3"
              | "m" -> "M1"
              | x -> x)
             state

         with
         | state, (attestation_rep,_ , _) ->
           match Remanent_state.get_commission state with
           | state, None -> state, attestation_rep, None
           | state, Some (com,_) -> state, attestation_rep, Some com
       in
       List.fold_left
         (fun state signataire ->
            let
              state, input =
              dump_attestation
                ~signataire
                ?date
                ?output_repository
                ?prefix
                diplome
                state
            in
            let state =
              match input with
              | None->
                Remanent_state.warn
                  __POS__
                  "MISSING Info input"
                  Exit
                  state
              | Some (input_rep, file_name) ->
                let file_name = Copy.pdf_file file_name in
                let state =
                  Remanent_state.push_copy
                    ~input_rep ~output_rep ~file_name state
                in
                state
            in
            Latex_engine.latex_opt_to_pdf state ~input)
         state
         signataires)
    state
    diplome_list
