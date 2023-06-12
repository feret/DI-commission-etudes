module StringSet =
  Set.Make
    (struct type t = string let compare = compare end)
let state = Remanent_state.init ()
let state =
  Cloud_interaction.safe_synchronize_shared_repository
    state
let state =
  Get_gps_files.get_students_list state
let students_list =
  Remanent_state.get_students state
let state, students_list =
  match
    Remanent_state.get_target state
  with
  | state, None -> state, students_list
  | state, Some target ->
    let target = Special_char.lowercase target in
    state, List.filter
      (fun a ->
         (Special_char.lowercase
            (a.Public_data.lastname))=target ||
         a.Public_data.promotion= Some target)
      students_list
let state =
  Collect_cours_a_ajouter.get_additional_courses state
let state =
  Collect_notes_a_modifier.get_updated_grades state
let state =
  Collect_course_exceptions.get_course_exceptions state
let state =
  Collect_scholarships.get_scholarships state
let state =
  Collect_mentoring.get_mentoring state
let state, current_year =
  Remanent_state.get_current_academic_year state
let state, l =
  Remanent_state.get_mentoring_list
    ~year:current_year
    state
let s string =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (String.trim string))
let state, main_dpt =
  Remanent_state.get_main_dpt state
let state =
  List.fold_left
    (fun state elt ->
       if List.exists
           (fun student ->
              s student.Public_data.firstname
              = s elt.Public_data.prenom_de_l_etudiant &&
              s student.Public_data.lastname
              =
              s elt.Public_data.nom_de_l_etudiant
           )
           students_list
       then state
       else
       Remanent_state.add_mentor
         state
         {Public_data.mentor_attribution_year =
            elt.Public_data.annee_academique ;
           Public_data.mentor_gender =
             (match
               elt.Public_data.genre_du_tuteur
             with
               None -> Public_data.Unknown
             | Some x -> x);
           Public_data.mentor_lastname =
             Tools.unsome_string
               elt.Public_data.nom_du_tuteur;
           Public_data.mentor_firstname =
             Tools.unsome_string
               elt.Public_data.prenom_du_tuteur;
          Public_data.mentor_email =
            Tools.unsome_string
              elt.Public_data.courriel_du_tuteur;
          Public_data.mentor_academic_year =
             elt.Public_data.annee_academique;
           Public_data.mentor_student_promo = current_year ;
           Public_data.mentor_student_gender =
             Public_data.Unknown ;
           Public_data.mentor_student_lastname = elt.Public_data.nom_de_l_etudiant ;
           Public_data.mentor_student_firstname = elt.Public_data.prenom_de_l_etudiant ;
          Public_data.mentor_student_dpt = main_dpt ;
          Public_data.mentor_secondary = elt.Public_data.secondaire;
         })
    state
    l
let state =
  Collect_programs.get_cursus state
let state =
  Collect_programs.get_dpt state
let state =
  Collect_programs.get_programs state
let state =
  Collect_inscriptions.get_inscriptions state
let state =
  Collect_programs.get_cursus_exceptions state
let state =
  Collect_compensations.get_compensations state
let state =
  Collect_decisions.get_decisions state
let state =
  Collect_admissions.get_admissions state
let state =
  Collect_dispenses.get_dispenses state
let state =
  Collect_course_entries.get_course_entries state
let state, output =
  Cloud_interaction.get_dated_repository state
let state, output_repository =
  match
    Remanent_state.get_target state
  with
  | state, None -> state, fst output
  | state, Some _ -> state, snd output
let state, output_repository_gps =
  Remanent_state.get_repository_to_dump_gps_files
    ~output_repository
    state
let state, is_di = Remanent_state.is_main_dpt_di state
let state =
  List.fold_left
    (fun state id ->
       let state, output =
         Get_gps_files.get_student_file
           ~output_repository:output_repository_gps
           id state
       in
       match output with
       | None -> state
       | Some output ->
         let firstname =  id.Public_data.firstname in
         let lastname = id.Public_data.lastname in
         let promo = id.Public_data.promotion in
         let state,patched_file_opt =
           Get_gps_files.patch_student_file
             state
             ~firstname
             ~lastname
             ~input:output
             ~output:output
         in
         let state, gps =
           match patched_file_opt with
           | None -> state, None
           | Some input ->
             Transcripts.get_gps_file
               ~input state
         in
         let output0 = output in
         let output =
           (fst output0,
            (Tools.basename (snd
                               output0))^".validated_and_in_progress_only.tex")
         in
         let state, is_dma = Remanent_state.is_main_dpt_dma state  in
         let state, is_di = Remanent_state.is_main_dpt_di state in
         let state, is_phys = Remanent_state.is_main_dpt_phys state in
         let state, is_chimie = Remanent_state.is_main_dpt_chimie state in
         let state =
           match gps with
           | None -> state
           | Some gps ->
             (*let report = true in*)
             let state, input =
               Transcripts.export_transcript
                 ~output ~keep_success:true (*~report*) state gps
             in
             let state =
               match input, is_dma || is_phys || is_chimie with
               | Some (input_rep,file_name), true ->
                 let state,rep  =
                   Remanent_state.get_student_personnal_repository
                     ~firstname ~lastname ?promo state
                 in
                 let output_rep = Printf.sprintf "%s/" rep in
                 let file_name = Copy.pdf_file file_name in
                 let state =
                   Remanent_state.push_copy
                     ~input_rep ~output_rep ~file_name state
                 in
                 state
               | _, false | None, _ ->
                 state
             in
             Latex_engine.latex_opt_to_pdf ~rev:true state ~input
         in
         let state =
           if is_dma || is_phys || is_chimie then
             let output =
               (fst output0,
                (Tools.basename (snd
                                   output0))^".validated_and_in_progress_only.en.tex")
             in
             match gps with
             | None -> state
             | Some gps ->
               let state, input =
                 Transcripts.export_transcript
                   ~language:Public_data.English
                   ~output state gps
               in
               let state =
                 match input, is_dma || is_phys || is_chimie with
                 | Some (input_rep,file_name), true ->
                   let state,rep  =
                     Remanent_state.get_student_personnal_repository
                       ~firstname ~lastname ?promo state
                   in
                   let output_rep = Printf.sprintf "%s/" rep in
                   let file_name = Copy.pdf_file file_name in
                   let state =
                     Remanent_state.push_copy
                       ~input_rep ~output_rep ~file_name state
                   in
                   state
                 | _, false | None, _ ->
                   state
             in
             Latex_engine.latex_opt_to_pdf ~rev:true state ~input
           else state
         in
         let state =
           match is_di
           with
           | true ->
             let output =
               (fst output0,
                (Tools.basename (snd
                                   output0))^".signe_JF.tex")
             in
             let state, signature =
               Remanent_state.get_signature state
             in
             let state =
               match gps with
               | None -> state
               | Some gps ->
                 let state, input =
                   Transcripts.export_transcript
                     ~signature ~output state gps
                 in
                 let state =
                   match input with
                   | None -> state
                   | Some (input_rep,file_name) ->
                     let file_name = Copy.pdf_file file_name in
                     let state,rep  =
                       Remanent_state.get_student_personnal_repository
                         ~firstname ~lastname ?promo state
                     in
                     let output_rep = Printf.sprintf "%s/" rep in
                     Remanent_state.push_copy
                       ~input_rep ~output_rep ~file_name state
                 in
                 Latex_engine.latex_opt_to_pdf
                   ~rev:true state ~input
             in
             let output =
               (fst output0,
                (Tools.basename (snd
                                   output0))^".signe_JF.en.tex")
             in
             let state, signature =
               Remanent_state.get_signature state
             in
             let state =
               match gps with
               | None -> state
               | Some gps ->
                 let state, input =
                   Transcripts.export_transcript
                     ~language:Public_data.English ~signature ~output state gps
                 in
                 let state =
                   match input with
                   | None -> state
                   | Some (input_rep,file_name) ->
                     let file_name = Copy.pdf_file file_name in
                     let state,rep  =
                       Remanent_state.get_student_personnal_repository
                         ~firstname ~lastname ?promo state
                     in
                     let output_rep = Printf.sprintf "%s/" rep in
                     Remanent_state.push_copy
                       ~input_rep ~output_rep ~file_name state
                 in
                 Latex_engine.latex_opt_to_pdf
                   ~rev:true state ~input
             in
             state
           | false -> state
         in
         let state =
           match is_di
           with
           | true ->
             let output =
               (fst output0,
                (Tools.basename (snd
                                   output0))^".1_by_1.signe_JF.tex")
             in
             let state, signature =
               Remanent_state.get_signature state
             in
             let state =
               match gps with
               | None -> state
               | Some gps ->
                 let state, input =
                   Transcripts.export_transcript
                     ~number_of_diploma_per_page:1
                     ~signature ~output state gps
                 in
                 (*let state =
                   match input with
                   | None -> state
                   | Some (input_rep,file_name) ->
                     let file_name = Copy.pdf_file file_name in
                     let state,rep  =
                       Remanent_state.get_student_personnal_repository
                         ~firstname ~lastname ?promo state
                     in
                     let output_rep = Printf.sprintf "%s/" rep in
                     Remanent_state.push_copy
                       ~input_rep ~output_rep ~file_name state
                 in*)
                 Latex_engine.latex_opt_to_pdf
                   ~rev:true state ~input
             in
             let output =
               (fst output0,
                (Tools.basename (snd
                                   output0))^".1_by_1.signe_JF.en.tex")
             in
             let state, signature =
               Remanent_state.get_signature state
             in
             let state =
               match gps with
               | None -> state
               | Some gps ->
                 let state, input =
                   Transcripts.export_transcript
                      ~number_of_diploma_per_page:1
                     ~language:Public_data.English ~signature ~output state gps
                 in
                (* let state =
                   match input with
                   | None -> state
                   | Some (input_rep,file_name) ->
                     let file_name = Copy.pdf_file file_name in
                     let state,rep  =
                       Remanent_state.get_student_personnal_repository
                         ~firstname ~lastname ?promo state
                     in
                     let output_rep = Printf.sprintf "%s/" rep in
                     Remanent_state.push_copy
                       ~input_rep ~output_rep ~file_name state
                 in*)
                 Latex_engine.latex_opt_to_pdf
                   ~rev:true state ~input
             in
             state
           | false -> state
         in

         let output =
           (fst output0, (Tools.basename (snd output0))^".all.tex")
         in
         let state =
           match gps with
           | None -> state
           | Some gps ->
             let filter = Public_data.All in
             let report = true in
             let state, output_opt =
               Transcripts.export_transcript
                 ~filter ~keep_faillure:true ~output ~report state gps
             in
             match output_opt with
             | None -> state
             | Some input ->
               Latex_engine.latex_to_pdf ~rev:true state ~input
         in state
    )
    state
    students_list
let years =
  List.fold_left
    (fun set id ->
       match id.Public_data.promotion with
       | None -> set
       | Some promo ->
         StringSet.add promo set)
    StringSet.empty students_list
let title =
  [Loggers.fprintf,
   Format.sprintf
    "TUTEURS NOUVELLEMENT ATTRIBUÉS en %s"
    current_year]
let state =
  Mentor_report.ReportListMentors.dump
    ~attributionyear:current_year
    ~academicyear:current_year
    ~file_name:(fun s ext ->  Format.sprintf "tuteurs_nouvelles_affectations_%s%s.%s" current_year s ext)
    ~title
    state
let title =
  [Loggers.fprintf,
   Format.sprintf
     "LISTE DES TUTEURS EN %s"
     current_year]
let state =
  Mentor_report.ReportListMentors.dump
    ~academicyear:current_year ~dpt:main_dpt
    ~title
    ~file_name:(fun s ext -> Format.sprintf "tuteurs_%s%s.%s" current_year s ext)
    state
let title =
  [Loggers.fprintf,
   "HISTORIQUE DES TUTEURS"]
let state =
    Mentor_report.ReportListMentors.dump
      ~dpt:main_dpt
      ~file_name:(fun s ext ->
          Format.sprintf "tutorat_all%s.%s" s ext)
      ~title
      state
let state, commission_rep =
  Remanent_state.get_main_commission_rep state
let state, _dpt, signataires =
  match
    Remanent_state.get_main_dpt state
  with
  | state, Public_data.DI -> state, Public_data.DI, ["JF";"MP";"LB"]
  | state, Public_data.DMA -> state, Public_data.DMA, ["DC"]
  | state, Public_data.CHIMIE -> state, Public_data.CHIMIE, ["JD"]
  | state, Public_data.ARTS -> state, Public_data.ARTS, []
  | state, Public_data.ENS -> state, Public_data.ENS, []
  | state, Public_data.PHYS -> state, Public_data.PHYS, []
  | state, Public_data.IBENS -> state, Public_data.IBENS, []
  | state, Public_data.ECO -> state, Public_data.ECO, []
  | state, Public_data.DRI -> state, Public_data.DRI, []
  | state, Public_data.LILA -> state, Public_data.LILA, []
let state =
  match
    Remanent_state.get_commission state
  with
  | state, None ->
    let state =
      Commissions.prepare_commission
        ~commission_rep
        ~signataires
        state
    in
    state

  | state, Some (commission_date,commission_year) ->
    begin
      (*let state =
        Diploma_report.dump_attestations
          ~signataires
          ~recu:true
          ~academicyear:commission_year
          ~niveau:"m"
          ~dpt
          state
      in
      let state =
        Diploma_report.dump_attestations
          ~signataires
          ~recu:true
          ~academicyear:commission_year
          ~niveau:"l"
          ~dpt
          state
      in*)
      let state =
        Commissions.prepare_commission
          ~commission_rep
          ~signataires
          ~annee:commission_year
          ~date_complete:commission_date
          state
      in
      state
    end
let state = Remanent_state.empty_copy ~copy:(Copy.copy) state
let state =
  StringSet.fold
    (fun promo state ->
       let state,pattern  =
         Remanent_state.get_students_personnal_files
            ~promo state
       in
       let state, output_rep =
         Remanent_state.get_promos_personnal_repository
            state
       in
       let output = (output_rep,Format.sprintf "promo%s.pdf" promo)
       in
       let state =
         Latex_engine.concat_pdf ~pattern ~exclude:".en.pdf" ~output
           state
       in
       let output = (output_rep,Format.sprintf "promo%s_en.pdf" promo)
       in
       let state, pattern =
         Remanent_state.get_students_personnal_files
           ~language:Public_data.English
           ~promo state
       in
       let state =
         Latex_engine.concat_pdf ~pattern ~output
           state
       in

       state)
    years
    state
let state =
  if is_di then
    let state,input =
      Diploma_report.DiplomaReport.dump_stats
        ~file_name:"stats.tex"
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf
        ~times:2 state ~input
    in
    state
  else
    state
let state = Report.dump_issues state
let state = Report.warn state
let state =
  Cloud_interaction.make_current_repository state
let state =
    Remanent_state.print_errors "" state
let state =
    Remanent_state.print_errors ~logger:(Remanent_state.std_logger) "" state
let state =
  Remanent_state.store_errors_and_profiling_info
    Safe_sys.rec_mk_when_necessary
    Safe_sys.cp
    state
let state =
  Cloud_interaction.make_current_repository state
let state =
  Cloud_interaction.synchronize_shared_repository
    state
