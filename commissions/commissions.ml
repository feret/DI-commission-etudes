type direction_key = string
type sous_commission_key = string

let dens_two =
  Public_data.Diplome_ENS
    {
      Public_data.dens_key = "dens";
      Public_data.nb_inscription_list=[1;2];
      Public_data.dens_short= "DENS";
      Public_data.which_year_string="première et deuxième années";
    }


let _ = dens_two

let dens =
  Public_data.Diplome_ENS
    {
      Public_data.dens_key = "dens";
      Public_data.nb_inscription_list=[1;2;3;4;5;6;7;8;9;10];
      Public_data.dens_short= "DENS";
      Public_data.which_year_string="toutes les années";
    }
let l =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_long="Licence L3 d'informatique";
      Public_data.dn_universite="à l'université Paris 7 - Denis Diderot";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DI;
    }

let m =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 d'informatique";
      Public_data.dn_universite="dans une université partenaire";
      Public_data.dn_niveau="m";
      Public_data.dn_departement=Public_data.DI;
    }

let l_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="dans une université partenaire";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DMA;
    }

let m_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="dans une université partenaires";
      Public_data.dn_niveau="m";
      Public_data.dn_departement=Public_data.DMA;
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
      (Dens_report.DensReport.dump_per_n_inscription,"_par_nb_inscriptions");
    TODO_DENS
      (Dens_report.DensReport.dump_per_alphabetic_order,"_par_ordre_alphabetic");
  ]

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

let direction_etude_eco =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.eco_list

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
    [l;m;dens]

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
    [l_dma;m_dma;dens]


let diplomes_ibens =
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
    [dens]

let diplomes_phys =
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
    [dens]

let diplomes_eco =
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
    [dens]

let print_sous_commission
    commission_rep
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
    | Public_data.DI ->
      People.dpt_di,direction_etude,diplomes,People.footpage_string,Color.digreen
    | Public_data.DMA ->
      People.dpt_dma,direction_etude_dma,diplomes_dma,People.footpage_string_dma,Color.duckblue
    | Public_data.ENS ->
      People.dpt_di,Public_data.StringMap.empty,Public_data.StringMap.empty,"",Color.digreen
    | Public_data.IBENS ->
      People.dpt_ibens, direction_etude_ibens,diplomes_ibens,People.footpage_string_ibens,Color.green
    | Public_data.PHYS ->
      People.dpt_phys, direction_etude_phys,diplomes_phys,People.footpage_string_phys,Color.blue
    | Public_data.ECO ->
      People.dpt_eco,
      direction_etude_eco,diplomes_eco,People.footpage_string_eco,Color.pink
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
      let state, (_,output_rep,_) =
          Remanent_state.get_commission_rep
            ~commission_rep ~sous_commission state
      in
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
          let state, signature =
            match direction.Public_data.direction_signature with
            | None -> state,
                      (fun _ -> [
                           Loggers.fprintf,
                           Format.sprintf
                             "Certifié exact à Paris \\\\ le %s \\\\"
                             commission_date])
            | Some s ->
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
              state, signature
          in
          let preamble i =
            [Loggers.fprintf,
             Format.sprintf
               "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en %s à l'ENS et aux décisions de la commission des études du %s,} je soussigné%s \\textbf{%s}, %s du département %s de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, %s du diplôme de l'École Normale Supérieure, ont obtenu les résultats suivants"
               dpt
               commission_date
               (People.e_of_direction direction)
               direction.Public_data.direction_nom_complet
               direction.Public_data.direction_titre
               direction.Public_data.direction_departement
               i
               full_year
               dip.Public_data.which_year_string]
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
            match input with
            | None -> state
            | Some (input_rep,file_name) ->
              let file_name = Copy.pdf_file file_name in
              Remanent_state.push_copy
                ~input_rep ~output_rep ~file_name state
          in
          let state =
            Latex_engine.latex_opt_to_pdf
              state  ~times:2 ~input
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
             "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en %s à l'ENS et aux décisions de la commission des études du %s,} je soussigné%s \\textbf{%s}, %s du département %s de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, %s, \\textbf{en %s - parcours : Formation interuniversitaire en %s de l'ENS Paris, ont obtenu les résultats suivants}"
             dpt
             commission_date
             (People.e_of_direction direction)
            direction.Public_data.direction_nom_complet
            direction.Public_data.direction_titre
            direction.Public_data.direction_departement
            i full_year
            dip.Public_data.dn_universite
            dip.Public_data.dn_long
            dpt]
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
          Latex_engine.latex_opt_to_pdf
            ~times:2 state ~input
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
            match input with
            | None -> state
            | Some (input_rep,file_name) ->
              let file_name = Copy.pdf_file file_name in
              Remanent_state.push_copy
                ~input_rep ~output_rep ~file_name state
          in
          let state =
            Latex_engine.latex_opt_to_pdf
              ~times:2 state ~input
          in
          state
        in state
        | TODO_Nat _, Public_data.Diplome_ENS _
        | TODO_DENS _, Public_data.Diplome_National _
          -> state
    end

let prepare_commission
    ~commission_rep
    ~annee
    ~date_complete
    ?signataires:(persons=["MP";"JF";"LB"])
    ?diplomes:(sous_commissions=["dens";"l";"m"])
    state =
  List.fold_left
    (fun state direction ->
       List.fold_left
         (fun state sous_commission ->
            List.fold_left
              (fun state todo ->
                 print_sous_commission
                   commission_rep
                   annee
                   date_complete
                   direction
                   sous_commission
                   todo
                   state
              ) state todo)
         state sous_commissions)
    state persons
