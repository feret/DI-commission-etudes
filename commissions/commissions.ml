type direction_key = string
type sous_commission_key = string

let e_of_direction dir =
  match dir.Public_data.direction_genre with
  | Public_data.Masculin | Public_data.Unknown -> ""
  | Public_data.Feminin -> "e"

let mp =
  {
    Public_data.direction_initiales = "MP";
    Public_data.direction_nom_complet = "Marc Pouzet";
    Public_data.direction_genre = Public_data.Masculin ;
    Public_data.direction_signature = None ;
    Public_data.direction_titre = "Directeur des études";
    Public_data.direction_departement = "d'informatique";
  }

let jf =
  {
    Public_data.direction_initiales = "JF";
    Public_data.direction_nom_complet = "Jérôme Feret";
    Public_data.direction_genre = Public_data.Masculin ;
    Public_data.direction_signature =
      Some Remanent_state.get_signature;
    Public_data.direction_titre = "Directeur des études";
    Public_data.direction_departement = "d'informatique";

  }

let am =
  {
    Public_data.direction_initiales = "AM";
    Public_data.direction_nom_complet = "Ariane Mézard";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature =
      Some Remanent_state.get_signature;
    Public_data.direction_titre = "Directeur de la formation";
    Public_data.direction_departement = "de mathématiques";

  }

let lb =
  {
    Public_data.direction_initiales = "LB";
    Public_data.direction_nom_complet = "Linda Boulevart";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature =
      None;
    Public_data.direction_titre = "Secrétaire pédagogique";
    Public_data.direction_departement = "d'informatique";

  }

let dens_two =
  Public_data.Diplome_ENS
    {
      Public_data.dens_key = "dens_two";
      Public_data.nb_inscription_list=[1;2];
      Public_data.dens_short= "DENS";
      Public_data.which_year_string="première et deuxième année";
  }

let l =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_long="Licence L3 d'informatique";
      Public_data.dn_universite="Paris 7 - Denis Diderot";
      Public_data.dn_niveau="l";
      Public_data.dn_departement="informatique";
    }

let m =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 d'informatique";
      Public_data.dn_universite="Master M1 d'informatique";
      Public_data.dn_niveau="m";
      Public_data.dn_departement="informatique";
    }

let l_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="Licence L3 de mathématiques";
      Public_data.dn_niveau="l";
      Public_data.dn_departement="mathématiques";
    }

let m_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="Master M1 de mathématiques";
      Public_data.dn_niveau="m";
      Public_data.dn_departement="mathématiques";
    }

type todo =
  | TODO_Nat of Diploma_report.dump * string
  | TODO_DENS of Dens_report.dump * string

let todo =
  [
    TODO_Nat
      (Diploma_report.DiplomaReport.dump_per_result_per_student,"_par_resultat") ;
    TODO_Nat
      (Diploma_report.DiplomaReport.dump_per_student,"_alphabetic");
    TODO_DENS
      (Dens_report.DensReport.dump_per_promo,"_par_promotion");
    TODO_DENS
      (Dens_report.DensReport.dump_per_n_inscription,"_par_nb_inscriptions")
  ]

let direction_etude =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    [mp;jf;lb]

let direction_etude_dma =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    [am]

let dpt_di = "informatique"
let dpt_dma = "mathématiques"
let diplomes =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key
          | Public_data.Diplome_National elt ->
              elt.Public_data.dn_key)
          elt map)
    Public_data.StringMap.empty
    [l;m;dens_two]

let diplomes_dma =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key)
         elt map)
    Public_data.StringMap.empty
    [l_dma;m_dma;dens_two]

let footpage_string = "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 32 20 45 --  Fax : + 33 (0) 1 44 32 20 75 -- direction.etudes@di.ens.fr}"
let footpage_string_dma =
  "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 31 72 45 --  Fax : + 33 (0) 1 44 32 20 69 -- education@math.ens.fr}"

let print_sous_commission
    commission_year
    commission_date
    direction_key
    sous_commission_key
    todo
    state
  =
  let state, dpt =
    Remanent_state.get_main_dpt state
  in
  let dpt,direction_etude,diplomes,footpage_string,footcolor  =
    match dpt with
    | Public_data.DI -> dpt_di,direction_etude,diplomes,footpage_string,Color.digreen
    | Public_data.DMA -> dpt_dma,direction_etude_dma,diplomes_dma,footpage_string_dma,Color.duckblue
  in
  let state, full_year =
    try
      let year_int = int_of_string commission_year in
      state, Format.sprintf "%i-%i" year_int (year_int+1)
    with
    | _ ->
      Remanent_state.warn
        __POS__
        (Format.sprintf "Bad string for a year (%s)" commission_year)
        Exit
        state,
      commission_year
  in
  match
    Public_data.StringMap.find_opt direction_key direction_etude,
    Public_data.StringMap.find_opt sous_commission_key diplomes
  with
  | None, None ->
  Remanent_state.warn
    __POS__
    (Format.sprintf
       "Cannot find direction_key (%s), not diploma key (%s)"
       direction_key sous_commission_key)
    Exit
    state
  | None,_ ->
    Remanent_state.warn
      __POS__
      (Format.sprintf
         "Cannot find direction_key (%s)"
         direction_key)
      Exit state
  | _, None  ->
    Remanent_state.warn
      __POS__
      (Format.sprintf
         "Cannot find diploma key (%s)"
         sous_commission_key)
      Exit state
  | Some direction, Some sous_commission ->
    begin
      let state, enspsl = Remanent_state.get_ENSPSL_logo state in
      let headpage s _ =
        [Loggers.fprintf_verbatim,
         Format.sprintf "\\IfFileExists{%s}{\\includegraphics{%s} \\\\}{}"
           enspsl enspsl;
         Loggers.fprintf,
           Format.sprintf
             "Résultats %s\\\\%s\\\\Page \\thepage/\\pageref{LastPage}\\\\"
           full_year s]
      in
      let footpage =
        [Loggers.fprintf, footpage_string]
      in
      match todo, sous_commission with
      | TODO_DENS (f,lbl), Public_data.Diplome_ENS dip ->
        let state,_ =
          f
            ~file_name:(Format.sprintf
                        "PV_%s%s.html" dip.Public_data.dens_short lbl)
            ~nb_inscription_list:dip.Public_data.nb_inscription_list
            state
        in
        let headpage = headpage dip.Public_data.dens_short in
        let state =
          match direction.Public_data.direction_signature with
          | None -> state
          | Some s ->
            let preamble i =
              [Loggers.fprintf,
               Format.sprintf
                 "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en %s à l'ENS et aux décisions de la commission des études du %s,} je soussigné%s \\textbf{%s}, %s du département %s de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, %s du diplôme de l'École Normale Supérieure, ont obtenu les résultats suivants"
                 dpt
                 commission_date
                 (e_of_direction direction)
                direction.Public_data.direction_nom_complet
                direction.Public_data.direction_titre
                direction.Public_data.direction_departement
                i
                full_year
                dip.Public_data.which_year_string]
            in
            let state, s =
              s state
            in
            let signature _ =
              [
                Loggers.fprintf,
                Format.sprintf
                 "Certifié exact à Paris \\\\ le %s \\\\"
                 commission_date;
                Loggers.fprintf_verbatim,
                Format.sprintf  "\\IfFileExists{%s}{\\includegraphics{%s}}{}"
                 s s
              ]
            in
            let state,input =
              f
                ~file_name:(Format.sprintf "PV_%s%s.tex"
                              dip.Public_data.dens_short lbl)
                state
                ~signature ~preamble ~headpage:headpage
                ~footpage ~footcolor
                ~nb_inscription_list:dip.Public_data.nb_inscription_list

            in
            let state =
              Latex_engine.latex_opt_to_pdf state ~times:2 ~input
            in
            state
        in
        state
      | TODO_Nat (f,lbl), Public_data.Diplome_National dip ->
        let headpage = headpage dip.Public_data.dn_long in
        let state,_ =
          f
            ~file_name:(Format.sprintf
                          "PV_%s%s.html" dip.Public_data.dn_short lbl)
            ~academicyear:commission_year
            ~niveau:dip.Public_data.dn_niveau
            ~dpt:dip.Public_data.dn_departement
            state
        in
        let preamble i =
          [Loggers.fprintf,
           Format.sprintf
             "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en %s à l'ENS et aux décisions de la commission des études du %s,} je soussigné%s \\textbf{%s}, %s du département %s de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, à l'université %s, \\textbf{en %s - parcours : Formation interuniversitaire en informatique de l'ENS Paris, ont obtenu les résultats suivants}"
             dpt
             commission_date
             (e_of_direction direction)
            direction.Public_data.direction_nom_complet
            direction.Public_data.direction_titre
            direction.Public_data.direction_departement
            i full_year
            dip.Public_data.dn_universite
            dip.Public_data.dn_long]
        in
        let state,input =
          f
            ~file_name:(Format.sprintf "PV_%s%s_sans_signature_%s.tex"
                          dip.Public_data.dn_short lbl direction.Public_data.direction_initiales)
            ~academicyear:commission_year
            ~niveau:dip.Public_data.dn_niveau ~dpt:dip.Public_data.dn_departement
            ~headpage:headpage
            ~preamble:preamble
            ~footpage ~footcolor
            state
        in
        let state =
          Latex_engine.latex_opt_to_pdf ~times:2 state ~input
        in
        let state =
          match direction.Public_data.direction_signature with
          | None -> state
          | Some s ->
          let state, s =
            s state
          in
          let signature _ =
            [
              Loggers.fprintf,
              Format.sprintf
                "Certifié exact à Paris \\\\ le %s \\\\"
                commission_date ;
              Loggers.fprintf_verbatim,
              Format.sprintf
                "\\IfFileExists{%s}{\\includegraphics{%s}}{}"
                s s
            ]
          in
          let state,input =
            f
              ~file_name:(Format.sprintf "PV_%s%s_signe_%s.tex"
                            dip.Public_data.dn_short lbl direction.Public_data.direction_initiales)
              ~academicyear:commission_year
              ~niveau:dip.Public_data.dn_niveau ~dpt:dip.Public_data.dn_departement
              ~headpage
              ~preamble
              ~footpage ~footcolor ~signature
              state
          in
          let state =
            Latex_engine.latex_opt_to_pdf ~times:2 state ~input
          in
          state
        in state
        | TODO_Nat _, Public_data.Diplome_ENS _
        | TODO_DENS _, Public_data.Diplome_National _
          -> state
    end

let prepare_commission
    ~annee
    ~date_complete
    ?signataires:(persons=["MP";"JF";"LB"])
    ?diplomes:(sous_commissions=["dens_two";"l";"m"])
    state =
    let state =
      Remanent_state.warn
        __POS__
        "PREPARE COMMISSION"
        Exit
        state
    in
  List.fold_left
    (fun state direction ->
       List.fold_left
         (fun state sous_commission ->
            List.fold_left
              (fun state todo ->
                 let state =
                   Remanent_state.warn
                     __POS__
                     "SOUS COMMISSION: TODO"
                     Exit
                     state
                 in
                 print_sous_commission
                   annee
                   date_complete
                   direction
                   sous_commission
                   todo
                   state
              ) state todo)
         state sous_commissions)
    state persons
