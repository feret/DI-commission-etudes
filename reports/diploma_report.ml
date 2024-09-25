type dump =
  ?commission:bool ->
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:Public_data.main_dpt ->
  ?universite:Public_data.universite ->
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
  val dump_stats: dump
  val dump_situation: string -> dump
end

module Build
  (I:Gen.Interface
    with type Missing_entry.entry = Public_data.diplome_national
    and type Missing_entry.collector = Public_data.diplome_national list)
=
struct

  let dump_national_diploma_list
      ?commission
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?universite
      ?recu
      ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_national_diploma_list
    in
    let state = if lastname = "CLERGUE" && niveau = "m" then
      match recu with
      | None -> Remanent_state.warn __POS__ "NONE" Exit state
      | Some true -> Remanent_state.warn __POS__ "TRUE" Exit state
      | Some false -> Remanent_state.warn __POS__ "FALSE" Exit state
else state
    in
    let filter = Gen.filter_national_diploma in
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    Gen.dump_elts
      ?commission
      ?firstname ?lastname ?promo ?niveau ?dpt ?universite ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name ?event_opt
      ~headerextralength:5
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state


  let nom_etudiant =
    ["Nom"],
    (fun a -> a.Public_data.diplome_lastname)
  let prenom_etudiant =
    ["Prénom"],
    (fun a -> a.Public_data.diplome_firstname)
  let commentaire =
    ["Commentaire"], (fun _ -> "")
  let moyenne =
    ["Moyenne"],
    (fun a ->
       match a.Public_data.diplome_moyenne with
       | None -> ""
       | Some f ->
         Notes.float_to_string_easy f)
  let promotion =
    ["Promotion"],
    (fun a -> a.Public_data.diplome_promotion)
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

  let simplify_niveau a =
    match a with
    | "l" -> "L3"
    | "m" -> "M1"
    | x -> Printf.sprintf "M2 %s" (String.uppercase_ascii x)

  let diplome =
    ["Diplôme"],
    (fun a -> simplify_niveau a.Public_data.diplome_niveau)
  let year =
    ["Année d'obtention"],(fun a -> a.Public_data.diplome_year)
  let ranking =
    ["rang"],(fun a -> match a.Public_data.diplome_ranking, a.Public_data.diplome_effectif with
        | None, _ -> ""
        | Some a, None -> string_of_int a
        | Some a, Some b -> Format.sprintf "%i/%i" a b)
  let origine =
    ["Concours d'origine"],(fun a ->
        match
          Public_data.string_of_origin_opt a.Public_data.diplome_origine
        with
        | "concours universitaire informatique" -> "CNE Info"
        | "CPGE Informatique" -> "CPGE Info"
        | "sélection Internationale" -> "SI"
        | "CPGE Physique-Sciences de l'Ingénieur" -> "CPGE PSI"
        | "CPGE Math-Physique-Info" -> "CPGE MPI"
        | x -> x )

  let lresultat =
    [],(fun x -> x),
    (fun a ->
       if a.Public_data.diplome_recu then "Reçu(e)s" else "Ajourné(e)s"
    )

  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_result_per_student
      ?commission
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?universite
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
         lresultat;  ]
    in
    dump_national_diploma_list
      ?commission
      ?firstname ?lastname ?promo ?niveau ?dpt ?universite ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_student
      ?commission
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?universite
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
    let state =
if lastname = "CLERGUE" && niveau = "m" then
        match recu with
        | None -> Remanent_state.warn __POS__ "NONE" Exit state
        | Some true -> Remanent_state.warn __POS__ "TRUE" Exit state
        | Some false -> Remanent_state.warn __POS__ "FALSE" Exit state
  else state 
      in
    dump_national_diploma_list
      ?commission ?firstname ?lastname ?promo ?niveau ?dpt ?universite
      ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_situation
          situation
          ?commission
          ?firstname
          ?lastname
          ?promo
          ?niveau
          ?dpt
          ?universite
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
        let columns = [prenom_etudiant;nom_etudiant;["Situation"],(fun _ -> situation);commentaire] in
        let headers =
          []
        in
        dump_national_diploma_list
          ?commission ?firstname ?lastname ?promo ?niveau ?dpt ?universite
          ?recu ?academicyear
          ?headpage ?footpage ?footcolor
          ?title ?preamble ?signature
          ?output_repository ?prefix ?file_name cmp headers columns state


  let dump_stats
      ?commission
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?universite
      ?recu
      ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a ->
            simplify_niveau a.Public_data.diplome_niveau);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_year);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_origine);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_promotion);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;promotion;moyenne; mention; ranking] in
    let headers =
      [
        lift_id diplome;
        lift_id year;
        (*lift_id promotion;*)
        lift_id origine;
        (*lift_id statut;*)
      ]
    in
    dump_national_diploma_list
      ?commission
      ?firstname ?lastname ?promo ?niveau ?dpt ?universite
      ?recu ?academicyear
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  end


module DiplomaReport =
  Build
    (struct

      let default_file_name = "diploma.html"
      module Missing_entry = Remanent_state.Collector_national_diplomas
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

    let direction_etude_chimie =
      List.fold_left
        (fun map elt ->
           Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
        Public_data.StringMap.empty
        People.chimie_list

let direction_etude_dri =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.dri_list

let direction_etude_arts =
List.fold_left
  (fun map elt ->
     Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
  Public_data.StringMap.empty
  People.arts_list

let direction_etude_lila =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.lila_list

let direction_etude_eco =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.eco_list

    let direction_etude_gsc=
      List.fold_left
        (fun map elt ->
           Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
        Public_data.StringMap.empty
        People.gsc_list
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
  let univ = diplome.Public_data.diplome_univ_key in
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
      ~extension:(Format.sprintf ".attestation.%s.%s.%s%s.tex"
                    begin
                      match
                        level
                      with
                      | "l" -> "L3"
                      | "m" -> "M1"
                      | x -> x
                    end
                    (Public_data.file_suffix_of_univ univ)
                    y signataire)
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
    with exn ->
      let msg = Printexc.to_string exn in
      let () =
        Format.printf
          "Cannot open file %s (%s)@."
          file
          msg
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s (%s)"  file msg)
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
        state, Loggers.Latex Loggers.latex_normal
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
    let f x =
      Printf.sprintf
        "\\includegraphics{%s}"
        x
    in
    let state, s  =
      Tools.include_latex_list
        f
        state
        enspsl
    in
      let () =
      Loggers.setheadpage logger
        [
          Loggers.fprintf_verbatim,
         (Format.sprintf
           "%s"
           s)]
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
      | Public_data.ARTS ->
        let color = Color.brown in
        Loggers.setfootpage logger ~color
        [
          Loggers.fprintf,
          People.footpage_string
        ],direction_etude_arts,"arts"
      | Public_data.DEC  ->
          let color = Color.white in
          Loggers.setfootpage logger ~color
          [
            Loggers.fprintf,
            People.footpage_string
          ],direction_etude, "études cognitives"
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
              People.footpage_string_eco
            ], direction_etude_eco, "économie"
      | Public_data.LILA ->
        let color = Color.pink in
        Loggers.setfootpage logger ~color
          [
            Loggers.fprintf,
            People.footpage_string
          ], direction_etude_lila, "littératures et langage"


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
           | Public_data.CHIMIE ->
             let color = Color.blue in
             Loggers.setfootpage logger ~color
               [Loggers.fprintf,
                People.footpage_string_chimie], direction_etude_chimie, "chimie"
                | Public_data.GEOSCIENCES ->
                  let color = Color.green in
                  Loggers.setfootpage logger ~color
                    [Loggers.fprintf,
                     People.footpage_string_gsc], direction_etude_gsc, "géosciences"


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
      let state, l =
        match s with
        | None -> state, []
        | Some l -> l state
      in
      let year = diplome.Public_data.diplome_year in
      let level = diplome.Public_data.diplome_niveau in
      let state, libelle =
        match
          diplome.Public_data.diplome_cursus.Public_data.inscription
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
      let f x =
        Format.sprintf
          "\\includegraphics{%s}"
          x
      in
      let state, sign =
        Tools.include_latex_list
          f
          state
          l
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
          date
      in
      let () = Loggers.fprintf logger  "%s" body in
      let () = Loggers.fprintf_verbatim logger "%s" sign  in
      let () = Loggers.fprintf_verbatim logger "\\end{center}\\vfill" in
      let () = Loggers.close_logger logger in
      state, Some (output_repository,output_file_name)

let dump_attestations
    ~signataires
    ?recu ?academicyear ?niveau ?dpt
    ?output_repository
    ?prefix
    state =
  let state, diplome_list =
    Remanent_state.Collector_national_diplomas.get state
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
