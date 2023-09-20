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
      Public_data.dn_universite="À l'université Paris 7 - Denis Diderot";
      Public_data.dn_univ_key=Public_data.UPC;
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DI;
    }

let m =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 d'informatique";
      Public_data.dn_universite="à l'Université Paris Sciences et Lettres ";
      Public_data.dn_niveau="m";
      Public_data.dn_univ_key=Public_data.PSL;
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
      Public_data.dn_univ_key=Public_data.Upartenaire;
      Public_data.dn_departement=Public_data.DMA;
    }

let m_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="dans une université partenaires";
      Public_data.dn_niveau="m";        Public_data.dn_univ_key=Public_data.Upartenaire;
      Public_data.dn_departement=Public_data.DMA;
    }


let l_upc_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_univ_key = Public_data.UPC;
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="à l'Université Paris Cité";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DMA;
    }

let m_upc_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="à l'Université Paris Cité";
      Public_data.dn_niveau="m";
      Public_data.dn_univ_key = Public_data.UPC;
      Public_data.dn_departement=Public_data.DMA;
    }

let l_ups_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_univ_key = Public_data.UPS;
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="à l'Université Paris-Saclay";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DMA;
    }

    let l_uspn_dma =
      Public_data.Diplome_National
        {
          Public_data.dn_key="l";
          Public_data.dn_short="L3";
          Public_data.dn_univ_key = Public_data.USPN;
          Public_data.dn_long="Licence L3 de mathématiques";
          Public_data.dn_universite="à l'Université Sorbonne Paris Nord";
          Public_data.dn_niveau="l";
          Public_data.dn_departement=Public_data.DMA;
        }
let m_ups_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="à l'Université Paris-Saclay";
      Public_data.dn_niveau="m";
      Public_data.dn_univ_key = Public_data.UPS;
      Public_data.dn_departement=Public_data.DMA;
    }

let l_su_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_univ_key = Public_data.SU;
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="à Sorbonne université";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DMA;
    }

let m_su_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="à Sorbonne université";
      Public_data.dn_niveau="m";
      Public_data.dn_univ_key = Public_data.SU;
      Public_data.dn_departement=Public_data.DMA;
    }

let l_psl_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="l";
      Public_data.dn_short="L3";
      Public_data.dn_univ_key = Public_data.PSL;
      Public_data.dn_long="Licence L3 de mathématiques";
      Public_data.dn_universite="à l'Université Paris Sciences et Lettres";
      Public_data.dn_niveau="l";
      Public_data.dn_departement=Public_data.DMA;
    }

let m_psl_dma =
  Public_data.Diplome_National
    {
      Public_data.dn_key="m";
      Public_data.dn_short="M1";
      Public_data.dn_long="Master M1 de mathématiques";
      Public_data.dn_universite="à l'Université Paris Sciences et Lettres";
      Public_data.dn_niveau="m";
      Public_data.dn_univ_key = Public_data.PSL;
      Public_data.dn_departement=Public_data.DMA;
    }

let m_uspn_dma =
Public_data.Diplome_National
  {
    Public_data.dn_key="m";
    Public_data.dn_short="M1";
    Public_data.dn_long="Master M1 de mathématiques";
    Public_data.dn_universite="à l'Université Sorbonne Paris Nord";
    Public_data.dn_niveau="m";
    Public_data.dn_univ_key = Public_data.USPN;
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

let direction_etude_chimie=
      List.fold_left
        (fun map elt ->
           Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
        Public_data.StringMap.empty
        People.chimie_list

let direction_etude_gsc=
              List.fold_left
                (fun map elt ->
                   Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
                Public_data.StringMap.empty
                People.gsc_list

let direction_etude_eco =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.eco_list

let direction_etude_lila =
  List.fold_left
    (fun map elt ->
       Public_data.StringMap.add elt.Public_data.direction_initiales elt map)
    Public_data.StringMap.empty
    People.lila_list

let diplomes =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key,Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,
             elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [l;m;dens]

let diplomes_dma =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key, Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [l_dma;l_su_dma;l_upc_dma;l_ups_dma;l_psl_dma;l_uspn_dma;
     m_dma;m_uspn_dma;m_su_dma;m_upc_dma;m_ups_dma;m_psl_dma;
     dens]


let diplomes_ibens =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key, Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [dens]

let diplomes_phys =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key, Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [dens]

    let diplomes_chimie =
      List.fold_left
        (fun map elt ->
           Public_data.StringUnivMap.add
             (match elt with
              | Public_data.Diplome_ENS elt ->
                elt.Public_data.dens_key, Public_data.UENS
              | Public_data.Diplome_National elt ->
                elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
             elt map)
        Public_data.StringUnivMap.empty
        [dens]


        let diplomes_gsc =
          List.fold_left
            (fun map elt ->
               Public_data.StringUnivMap.add
                 (match elt with
                  | Public_data.Diplome_ENS elt ->
                    elt.Public_data.dens_key, Public_data.UENS
                  | Public_data.Diplome_National elt ->
                    elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
                 elt map)
            Public_data.StringUnivMap.empty
            [dens]


let diplomes_eco =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key,Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [dens]


let diplomes_lila =
  List.fold_left
    (fun map elt ->
       Public_data.StringUnivMap.add
         (match elt with
          | Public_data.Diplome_ENS elt ->
            elt.Public_data.dens_key,Public_data.UENS
          | Public_data.Diplome_National elt ->
            elt.Public_data.dn_key,elt.Public_data.dn_univ_key)
         elt map)
    Public_data.StringUnivMap.empty
    [dens]

let print_sous_commission
    commission_rep
    ?commission_year
    ?commission_date
    direction_key
    (sous_commission_key,sous_commission_key_next_opt)
    universite_key
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
    | Public_data.DRI ->
      People.dpt_dri,direction_etude_dri,Public_data.StringUnivMap.empty ,People.footpage_string_dri,Color.orange
    | Public_data.ARTS ->
      People.dpt_arts,direction_etude_arts,Public_data.StringUnivMap.empty,People.footpage_string_arts,Color.brown
    | Public_data.ENS ->
      People.dpt_di,Public_data.StringMap.empty,Public_data.StringUnivMap.empty,"",Color.digreen
    | Public_data.IBENS ->
      People.dpt_ibens, direction_etude_ibens,diplomes_ibens,People.footpage_string_ibens,Color.green
    | Public_data.PHYS ->
      People.dpt_phys, direction_etude_phys,diplomes_phys,People.footpage_string_phys,Color.blue
    | Public_data.CHIMIE ->
      People.dpt_chimie, direction_etude_chimie,diplomes_chimie,People.footpage_string_chimie,Color.blue
      | Public_data.GEOSCIENCES ->
        People.dpt_gsc, direction_etude_gsc,diplomes_gsc,People.footpage_string_gsc,Color.blue
  | Public_data.ECO ->
      People.dpt_eco,
      direction_etude_eco,diplomes_eco,People.footpage_string_eco,Color.pink
    | Public_data.LILA ->
      People.dpt_lila,
      direction_etude_lila,diplomes_lila,People.footpage_string_lila,Color.white
  in
  let state, full_year =
    match
      commission_year
    with
    | None -> state, ""
    | Some commission_year ->
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
    Public_data.StringUnivMap.find_opt (sous_commission_key,universite_key) diplomes
  with
  | None, None ->
  Remanent_state.warn
    __POS__
    (Format.sprintf
       "Can find neither the direction_key (%s), nor the diploma key (%s,%s)"
       direction_key sous_commission_key (Public_data.string_of_universite universite_key))
    Exit
    state
  | None,_ ->
    Remanent_state.warn
      __POS__
      (Format.sprintf
         "Cannot find direction_key (%s)"
         direction_key)
      Exit state
  | _, None  -> state
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
      match todo, sous_commission, universite_key with
      | TODO_DENS (f,lbl), Public_data.Diplome_ENS dip, Public_data.UENS ->
        let state,_ =
          f
            ~file_name:(Format.sprintf
                        "PV_%s%s.html" dip.Public_data.dens_short lbl )
            ~nb_inscription_list:dip.Public_data.nb_inscription_list
            state
        in
        let headpage = headpage dip.Public_data.dens_short in
        let state =
          let state, signature =
            match commission_date with
            | None ->
                      begin
                      match direction.Public_data.direction_signature with
                        | None -> state, (fun _ -> [])
                        | Some s ->
                        let state, s =
                          s state
                        in
                        let f x =
                          Format.sprintf
                            "\\includegraphics{%s}"
                            x
                        in
                        let state, s =
                          Tools.include_latex_list
                            f
                            state
                            s
                        in
                        let signature _ =
                          [
                            Loggers.fprintf,
                            Format.sprintf "Rapport intermédiaire du \\today.\\\\";
                            Loggers.fprintf_verbatim, s
                          ]
                        in
                        state, signature
                    end

            | Some commission_date
              ->
              match direction.Public_data.direction_signature with
              | None ->
                let signature _ =
                  [
                    Loggers.fprintf,
                    Format.sprintf
                      "Certifié exact à Paris \\\\ le %s \\\\"
                      commission_date
                  ]
                in
                state, signature
              | Some s ->
                let state, s =
                  s state
                in
                let f x =
                  Format.sprintf
                    "\\includegraphics{%s}"
                    x
                in
                let state, s =
                  Tools.include_latex_list
                    f
                    state
                    s
                in
                let signature _ =
                  [
                    Loggers.fprintf,
                    Format.sprintf
                      "Certifié exact à Paris \\\\ le %s \\\\"
                      commission_date;
                    Loggers.fprintf_verbatim,
                    s
                  ]
                in
                state, signature
          in
          let preamble i =
            match commission_date with
            | None -> []
            | Some commission_date ->
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
              ~file_name:(Format.sprintf "PV_%s%s_sans_signature_%s.tex"
                            dip.Public_data.dens_short lbl
                         direction.Public_data.direction_initiales)
              state
              ~preamble ~headpage:headpage
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
          let state,input =
            f
              ~file_name:(Format.sprintf "PV_%s%s_signe_%s.tex"
                            dip.Public_data.dens_short lbl
                         direction.Public_data.direction_initiales)
              ~signature ~preamble ~headpage:headpage
              ~footpage ~footcolor
              ~nb_inscription_list:dip.Public_data.nb_inscription_list
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
              state  ~times:2 ~input
            in
          state
        in
        state

      | TODO_Nat (f,lbl), Public_data.Diplome_National dip, univ ->
        let headpage = headpage dip.Public_data.dn_long in
        let academicyear=commission_year in
        let state,_ =
          f
            ~file_name:(Format.sprintf
                          "PV_%s%s%s.html" dip.Public_data.dn_short lbl (Public_data.file_suffix_of_univ univ))
            ?academicyear
            ~niveau:dip.Public_data.dn_niveau
            ~commission:true
            ~universite:univ
            ~dpt:dip.Public_data.dn_departement
            state
        in
        let preamble i =
          match commission_date with
          | None -> []
          | Some commission_date ->
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
            ~file_name:(Format.sprintf "PV_%s%s_sans_signature_%s%s.tex"
                          dip.Public_data.dn_short lbl direction.Public_data.direction_initiales
                          (Public_data.file_suffix_of_univ dip.Public_data.dn_univ_key))
            ?academicyear
            ~niveau:dip.Public_data.dn_niveau ~dpt:dip.Public_data.dn_departement
            ~headpage:headpage
            ~preamble:preamble
            ~commission:true
            ~universite:univ
            ~footpage ~footcolor
            state
        in
        let state =
          match direction.Public_data.direction_signature with
          | None ->
            begin
              match input with
              | None -> state
              | Some (input_rep,file_name) ->
                let file_name = Copy.pdf_file file_name in
                Remanent_state.push_copy
                  ~input_rep ~output_rep ~file_name state
            end
          | Some _ -> state
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
          let fa x =
            Format.sprintf
              "\\includegraphics{%s}"
              x
          in
          let state, s =
            Tools.include_latex_list
              fa
              state
              s
          in
          let signature _ =
            match commission_date with
            | None -> []
            | Some commission_date ->
              [
                Loggers.fprintf,
                Format.sprintf
                  "Certifié exact à Paris \\\\ le %s \\\\"
                  commission_date ;
                Loggers.fprintf_verbatim,
                s
              ]
          in
          let state,input =
            f
              ~file_name:(Format.sprintf "PV_%s%s_signe_%s%s.tex"
                            dip.Public_data.dn_short lbl direction.Public_data.direction_initiales
                            (Public_data.file_suffix_of_univ dip.Public_data.dn_univ_key))
              ?academicyear
              ~niveau:dip.Public_data.dn_niveau ~dpt:dip.Public_data.dn_departement
              ~commission:true
              ~universite:univ
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
        in
        let state =
          match sous_commission_key_next_opt with
          | None -> state
          | Some x ->
          begin
          let state = Remanent_state.warn __POS__ "S_NEXT" Exit state in
          match
            Public_data.StringUnivMap.find_opt (x,Public_data.PSL) diplomes
          with None -> state
          | Some Public_data.Diplome_ENS _ -> state
          | Some Public_data.Diplome_National dip' ->
          let state = Remanent_state.warn __POS__ "S_NEXT 2" Exit state in

          let preamble _ =
            match commission_date with
            | None -> []
              | Some commission_date ->
              [Loggers.fprintf,
               Format.sprintf
                 "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en %s à l'ENS et aux décisions de la commission des études du %s,} je soussigné%s \\textbf{%s}, %s du département %s de l'École Normale Supérieure, certifie que les étudiants suivants sont \\textbf{admis en %s - parcours : Formation interuniversitaire en %s de l'ENS Paris.}"
                 dpt
                 commission_date
                 (People.e_of_direction direction)
                 direction.Public_data.direction_nom_complet
                 direction.Public_data.direction_titre
                 direction.Public_data.direction_departement
                 (* full_year *)
                 (*dip.Public_data.dn_universite*)
                 dip'.Public_data.dn_long
                 dpt]
          in
          let state,input =
            f
              ~file_name:(Format.sprintf "PV_admission_%s%s_sans_signature_%s%s.tex"
                            dip'.Public_data.dn_short lbl direction.Public_data.direction_initiales
                            (Public_data.file_suffix_of_univ dip'.Public_data.dn_univ_key))
              ?academicyear
              ~niveau:dip.Public_data.dn_niveau ~dpt:dip.Public_data.dn_departement
              ~headpage:headpage
              ~preamble:preamble
              ~commission:true
              ~universite:univ
              ~footpage ~footcolor
              state
          in
          let state =
            match direction.Public_data.direction_signature with
            | None ->
              begin
                match input with
                | None -> state
                | Some (input_rep,file_name) ->
                  let file_name = Copy.pdf_file file_name in
                  Remanent_state.push_copy
                    ~input_rep ~output_rep ~file_name state
              end
            | Some _ -> state
          in
          let state =
            Latex_engine.latex_opt_to_pdf
              ~times:2 state ~input
          in
          let state =
            match direction.Public_data.direction_signature with
            | None -> state
            | Some _ ->
          (*  let state, s =
              s state
            in*)
          (*  let fa x =
              Format.sprintf
                "\\includegraphics{%s}"
                x
            in*)
            (*let state, s =
              Tools.include_latex_list
                fa
                state
                s
            in*)
            (*let signature _ =
              match commission_date with
              | None -> []
              | Some commission_date ->
                [
                  Loggers.fprintf,
                  Format.sprintf
                    "Certifié exact à Paris \\\\ le %s \\\\"
                    commission_date ;
                  Loggers.fprintf_verbatim,
                  s
                ]
            in*) state
        in
        state
        end in state
      | TODO_DENS _, Public_data.Diplome_ENS _, (Public_data.UPC | Public_data.PSL | Public_data.UP | Public_data.SU | Public_data.UPSud | Public_data.UPantheonSorbonne | Public_data.UPS | Public_data.Upartenaire | Public_data.UDiderot | Public_data.UDauphine | Public_data.USPN | Public_data.UPNord ) ->
        let state =
          Remanent_state.warn
            __POS__
            (Format.sprintf "WRONG DENS: %s"
               (Public_data.string_of_universite universite_key))
            Exit
            state
        in
        state
      | TODO_Nat _, Public_data.Diplome_ENS _, _
      | TODO_DENS _, Public_data.Diplome_National _, _
          -> state
    end

let prepare_commission
    ~commission_rep
    ?annee
    ?date_complete
    ?universites:(universites=[Public_data.UPC;Public_data.UPS;Public_data.PSL;Public_data.Upartenaire;Public_data.UENS;Public_data.SU;Public_data.UPantheonSorbonne;Public_data.USPN])
    ?signataires:(persons=["MP";"JF";"LB"])
    ?diplomes:(sous_commissions=[("dens",None);("l",Some "m");("m",None)])
    state =
  let commission_year = annee in
  let commission_date = date_complete in
  List.fold_left
    (fun state direction ->
       List.fold_left
         (fun state sous_commission ->
            List.fold_left
              (fun state universite ->
                 List.fold_left
                   (fun state todo ->
                      print_sous_commission
                        commission_rep
                        ?commission_year
                        ?commission_date
                        direction
                        sous_commission
                        universite
                        todo
                        state
                   ) state todo)
              state universites)
         state sous_commissions)
    state persons
