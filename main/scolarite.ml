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
  Collect_course_exceptions.get_course_exceptions state
let state =
  Collect_scholarships.get_scholarships state
let state =
  Collect_mentoring.get_mentoring state
let state, l =
  Remanent_state.get_mentoring_list
    ~year:"2020"
    state
let s string =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (String.trim string))
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
           Public_data.mentor_student_promo = "2020" ;
           Public_data.mentor_student_gender =
             Public_data.Unknown ;
           Public_data.mentor_student_lastname = elt.Public_data.nom_de_l_etudiant ;
           Public_data.mentor_student_firstname = elt.Public_data.prenom_de_l_etudiant ;
           Public_data.mentor_student_dpt = "informatique" ;
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
  Collect_programs.get_cursus_exceptions state
let state =
  Collect_compensations.get_compensations state
let state =
  Collect_decisions.get_decisions state
let state =
  Collect_admissions.get_admissions state
let state =
  Collect_dispenses.get_dispenses state
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
         let state,patched_file_opt =
           Get_gps_files.patch_student_file
             state
             ~firstname:id.Public_data.firstname
             ~lastname:id.Public_data.lastname
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
         let state =
           match gps with
           | None -> state
           | Some gps ->
             let state, input =
               Transcripts.export_transcript
                 ~output state gps
             in
               Latex_engine.latex_opt_to_pdf ~rev:true state ~input
         in
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
               Latex_engine.latex_opt_to_pdf ~rev:true state ~input
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
                 ~filter ~output ~report state gps
             in
             match output_opt with
             | None -> state
             | Some input ->
               Latex_engine.latex_to_pdf ~rev:true state ~input
         in state
    )
    state
    students_list
let state, academicyear =
  Remanent_state.get_current_academic_year state
let title = "TUTEURS NOUVELLEMENT ATTRIBUÉS en 2020"
let correct_email = fun x -> x
let state,_ =
  Mentor_report.ReportListMentors.dump_per_mentor_year_promo_student
    ~attributionyear:"2020"
    ~academicyear:"2020"  ~file_name:"tuteurs_nouvelles_affectations_2020_par_tuteur.html"
    ~title
    ~correct_email
    state
let correct_email =
  Special_char.correct_string_email_latex
let state, input =
  Mentor_report.ReportListMentors.dump_per_mentor_year_promo_student
    ~attributionyear:"2020"
    ~academicyear:"2020"  ~file_name:"tuteurs_nouvelles_affectations_2020_par_tuteur.tex"
    ~title ~correct_email
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let correct_email = fun x -> x
let state,_ =
  Mentor_report.ReportListMentors.dump_per_student
    ~attributionyear:"2020"
    ~academicyear:"2020"
    ~file_name:"tuteurs_nouvelles_affectations_2020_par_étudiants.html" ~title ~correct_email
    state
let correct_email =
  Special_char.correct_string_email_latex
let state, input =
  Mentor_report.ReportListMentors.dump_per_student
        ~attributionyear:"2020"
        ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tuteurs_nouvelles_affectations_2020_par_étudiants.tex"
        ~title ~correct_email
        state
let state =
  Latex_engine.latex_opt_to_pdf state ~input

let title = "LISTE DES TUTEURS 2020"
let correct_email = fun x -> x
let state,_ =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_tuteur.html" ~title ~correct_email
    state
let correct_email =
      Special_char.correct_string_email_latex
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_tuteur.tex"
    ~title ~correct_email
    state
let correct_email = fun x -> x
let state =
  Latex_engine.latex_opt_to_pdf state ~input
  let state,_ =
    Mentor_report.ReportListMentors.dump_per_year_mentor_student
      ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_tuteur.html" ~title ~correct_email
      state
let correct_email =
  Special_char.correct_string_email_latex
let state, input =
    Mentor_report.ReportListMentors.dump_per_year_student_mentor
      ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_etudiant.tex" ~title ~correct_email
      state
  let state =
    Latex_engine.latex_opt_to_pdf state ~input
let title = "LISTE DES TUTEURS 2019"
let correct_email = fun x -> x
let state,_ =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear ~dpt:"informatique" ~file_name:"tutorat_2019_par_tuteur.html" ~title ~correct_email
    state
let correct_email =
  Special_char.correct_string_email_latex
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear ~dpt:"informatique" ~file_name:"tutorat_2019_par_tuteur.tex"
    ~title ~correct_email
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let title = "HISTORIQUE DES TUTEURS "
let correct_email = fun x -> x
let state,_ =
    Mentor_report.ReportListMentors.dump_per_year_mentor_student
       ~dpt:"informatique" ~file_name:"tutorat_all_par_tuteur.html" ~title ~correct_email
      state
let correct_email =
  Special_char.correct_string_email_latex
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~dpt:"informatique" ~file_name:"tutorat_all_par_tuteur.tex"
    ~title ~correct_email
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  match
    Remanent_state.get_commission state
  with
  | state, None -> state
  | state, Some (commission_date,commission_year) ->
    begin
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
      let state =
        Diploma_report.dump_pvs
          ~recu:true
          ~academicyear:commission_year
          ~niveau:"m"
          ~dpt:"informatique"
          state
      in
      let state =
        Diploma_report.dump_pvs
          ~recu:true
          ~academicyear:commission_year
          ~niveau:"l"
          ~dpt:"informatique"
          state
      in
      let state, enspsl = Remanent_state.get_ENSPSL_logo state in
      let headpage s _ =
        Format.sprintf
          "\\IfFileExists{%s}{\\includegraphics{%s} \\\\}{} Résultats %s\\\\%s\\\\Page \\thepage/\\pageref{LastPage}\\\\"
          enspsl enspsl full_year s
      in
      let footpage =
        "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 32 20 45 --  Fax : + 33 (0) 1 44 32 20 75 - direction.etudes@di.ens.fr}"
      in
      let footcolor = Color.digreen in
      let state, s =
        Remanent_state.get_signature state
      in
      let signature _ =
        Format.sprintf
          "Certifié exact à Paris \\\\ le %s \\\\ \\IfFileExists{%s}{\\includegraphics{%s}}{}"
          commission_date s s
      in
      let nb_inscription_list = [1;2] in
      let state,_ =
        Dens_report.DensReport.dump_per_promo
          ~file_name:"PV_DENS_par_promotion.html"
          ~nb_inscription_list
          state
      in
      let state,_ =
        Dens_report.DensReport.dump_per_n_inscription
          ~file_name:"PV_DENS_par_nb_inscriptions.html"
          ~nb_inscription_list
          state
      in
      let state,_ =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_M1_par_resultat.html"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          state
      in
      let state,_ =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_L3_par_resultat.html"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique" state
      in
      let state,_ =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_M1_alphabetic.html"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          state
      in
      let state,_ =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_L3_alphabetic.html"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique" state
      in
      let preamble i =
        Format.sprintf
          "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en informatique à l'ENS et aux décisions de la commission des études du %s,} je soussigné \\textbf{Jérôme Feret}, directeur des études du département d'informatique de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, en première et deuxième année du diplôme de l'École Normale Supérieure, ont obtenu les résultats suivants" commission_date i full_year
      in
      let state,input =
        Dens_report.DensReport.dump_per_promo
          ~file_name:"PV_DENS_par_promotion.tex" state
          ~signature ~preamble ~headpage:(headpage "DENS")
          ~footpage ~footcolor ~nb_inscription_list
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state,input =
        Dens_report.DensReport.dump_per_n_inscription
          ~file_name:"PV_DENS_par_nb_inscriptions.tex" state
          ~signature ~preamble ~headpage:(headpage "DENS")
          ~footpage ~footcolor ~nb_inscription_list
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let preamble who s u i =
        Format.sprintf
          "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en informatique à l'ENS et aux décisions de la commission des études du %s,} je soussigné \\textbf{%s}, directeur des études du département d'informatique de l'École Normale Supérieure, certifie que les \\underline{\\textbf{%i étudiants inscrits en %s}}, à l'université %s, \\textbf{en %s - parcours : Formation interuniversitaire en informatique de l'ENS Paris, ont obtenu les résultats suivants}"
          commission_date who i full_year u s
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_M1_par_resultat_signe_JF.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique")
          ~preamble:(preamble "Jérôme Feret" "Master M1 d'informatique" "Paris Dauphine")
          ~footpage ~footcolor
          ~signature
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~times:2 state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_M1_par_resultat_sans_signature_JF.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique")
          ~preamble:(preamble "Jérôme Feret" "Master M1 d'informatique" "Paris Dauphine")
          ~footpage ~footcolor
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~times:2 state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_M1_par_resultat_sans_signature_MP.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique")
          ~preamble:(preamble "Marc Pouzet" "Master M1 d'informatique" "Paris Dauphine")
          ~footpage ~footcolor
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~times:2 state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_M1_alphabetic_signe_JF.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique fondamental")
          ~footpage ~footcolor
          ~preamble:(preamble "Jérôme Feret" "Master M1 d'informatique fondamentale" "PSL")
          ~signature
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~rev:false ~times:2 state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_M1_alphabetic_signe_JF.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique fondamental")
          ~footpage ~footcolor
          ~preamble:(preamble "Jérôme Feret" "Master M1 d'informatique fondamentale" "PSL")
          ~signature
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~rev:false ~times:2
          state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_M1_alphabetic_sans_signature_JF.tex"
          ~academicyear:commission_year ~niveau:"m"
          ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique fondamental")
          ~footpage ~footcolor
          ~preamble:(preamble "Jérôme Feret" "Master M1 d'informatique fondamentale" "PSL")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~rev:false ~times:2 state ~input
      in
      let state,input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_M1_alphabetic_sans_signature_MP.tex"
          ~academicyear:commission_year
          ~niveau:"m" ~dpt:"informatique"
          ~headpage:(headpage "Master M1 d'informatique fondamental")
          ~footpage ~footcolor
          ~preamble:(preamble "Marc Pouzet" "Master M1 d'informatique fondamentale" "PSL")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf ~rev:false ~times:2 state ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_L3_par_résultat_signe_JF.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~headpage:(headpage "Licence L3 d'informatique")
          ~footpage ~footcolor
          ~preamble:(preamble "Jérôme Feret" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          ~signature
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_L3_par_résultat_sans_signature_JF.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~headpage:(headpage "Licence L3 d'informatique")
          ~footpage ~footcolor
          ~preamble:(preamble "Jérôme Feret" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_result_per_student
          ~file_name:"PV_L3_par_résultat_sans_signature_MP.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~headpage:(headpage "Licence L3 d'informatique")
          ~footpage ~footcolor
          ~preamble:(preamble "Marc Pouzet" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_L3_alphabetic_signe_JF.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~footpage ~footcolor
          ~headpage:(headpage "Licence L3 d'informatique")
          ~preamble:(preamble "Jérôme Feret" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          ~signature
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_L3_alphabetic_sans_signature_MP.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~footpage ~footcolor
          ~headpage:(headpage "Licence L3 d'informatique")
          ~preamble:(preamble "Marc Pouzet" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      let state, input =
        Diploma_report.DiplomaReport.dump_per_student
          ~file_name:"PV_L3_alphabetic_sans_signature_JF.tex"
          ~academicyear:commission_year
          ~niveau:"l" ~dpt:"informatique"
          ~footpage ~footcolor
          ~headpage:(headpage "Licence L3 d'informatique")
          ~preamble:(preamble "Jérôme Feret" "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
          state
      in
      let state =
        Latex_engine.latex_opt_to_pdf state ~times:2 ~input
      in
      state
    end

let state,_ =
        Student_report.ReportGpsServerFaillures.dump_per_student
    ~file_name:"échecs_extraction_gps_par_étudiant.html"
    state
let state,_ =
  Student_report.ReportGpsServerFaillures.dump_per_promo
    ~file_name:"échecs_extraction_gps_par_promotion.html"
    state
let state,_ =
  Student_report.ReportMissingPictures.dump_per_student
    ~file_name:"photos_manquantes_par_étudiant.html"
    state
let state,_ =
  Student_report.ReportMissingPictures.dump_per_promo
    ~file_name:"photos_manquantes_par_promotion.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_student_year
    ~file_name:"notes_manquantes_par_dpt_et_cours.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_class_year
    ~file_name:"notes_manquantes_par_dpt_et_étudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_year_student
    ~file_name:"notes_manquantes_par_dpt_et_année_et_étudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_year_class
    ~file_name:"notes_manquantes_par_dpt_et_année_et_cours.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_student
    ~file_name:"notes_manquantes_par_étudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_promotion
    ~file_name:"notes_manquantes_par_promotion.html"
    state

let state,_ =
  Grades.NonAcceptedGrades.dump_per_dpt_student_year
    ~file_name:"cours_non_acceptes_par_dpt_et_cours.html"
    state
let state,_ =
  Grades.NonAcceptedGrades.dump_per_dpt_class_year
    ~file_name:"cours_non_acceptes_par_dpt_et_étudiant.html"
    state
let state,_ =
  Grades.NonAcceptedGrades.dump_per_dpt_year_student
    ~file_name:"cours_non_accepted_par_dpt_et_année_et_étudiant.html"
    state
let state,_ =
  Grades.NonAcceptedGrades.dump_per_dpt_year_class
    ~file_name:"cours_non_accepted_par_dpt_et_année_et_cours.html"
    state
let state,_ =
  Grades.NonAcceptedGrades.dump_per_student
    ~file_name:"cours_non_accepted_par_étudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_promotion
    ~file_name:"cours_non_accepted_par_promotion.html"
    state

let state,_ =
  Grades.MissingECTSAttributions.dump_per_dpt_student_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_cours.html"
        state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_dpt_class_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_étudiant.html"
    state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_student
    ~file_name:"attributions_de_notes_manquantes_par_étudiant.html"
    state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_promotion
    ~file_name:"attributions_de_notes_manquantes_par_promotion.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_student
    ~file_name:"tuteurs_manquants_par_étudiant.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_year
    ~file_name:"tuteurs_manquants_par_année.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_promotion
    ~file_name:"tuteurs_manquants_par_promotion.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_manquantes_par_année.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_student
    ~file_name:"descriptions_de_stage_manquantes_par_étudiant.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_promotion
    ~file_name:"descriptions_de_stage_manquantes_par_promotion.html"
    state
let state,_ =
  Internship_descriptions.NonValidatedInternships.dump_per_year
    ~file_name:"stages_non_validés_par_année.html"
    state
let state,_ =
  Internship_descriptions.NonValidatedInternships.dump_per_student
    ~file_name:"stages_non_validés_par_étudiant.html"
    state
let state,_ =
  Internship_descriptions.NonValidatedInternships.dump_per_promotion
    ~file_name:"stages_non_validés_par_promotion.html"
        state
let state,_ =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_ambigues_par_année.html"
    state
let state,_ =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_promotion
    ~file_name:"descriptions_de_stage_ambigues_par_promotion.html"
    state
let state,_ =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_student
    ~file_name:"descriptions_de_stage_ambigues_par_etudiant.html"
    state
let state =
  match Remanent_state.get_missing_ects_attributions state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Some ects are not attributed"
      Exit
      state
let state =
  match Remanent_state.get_missing_grades state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Some grades are missing"
      Exit
      state
let state =
  match Remanent_state.get_non_accepted_grades state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Some courses should be validated by the department"
      Exit
      state
let state =
  match Remanent_state.get_missing_mentors state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Some mentors are missing"
      Exit
      state
let state =
  match Remanent_state.get_missing_internship_descriptions state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "The description of some internships is missing"
      Exit
      state
let state =
  match Remanent_state.get_ambiguous_internship_descriptions state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Several descriptions may apply to a single internship"
      Exit
      state
let state =
  match Remanent_state.get_non_validated_internships state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "The statis of several internships is not consistent"
      Exit
      state
let state =
        match Remanent_state.get_missing_pictures state
        with
        | state, [] -> state
        | state, _::_ ->
          Remanent_state.warn
            __POS__
            "Some pictures are missing"
            Exit
            state
let state =
  match Remanent_state.get_gps_server_faillures state
  with
  | state, [] -> state
  | state, _::_ ->
    Remanent_state.warn
      __POS__
      "Some gps extractions failed"
      Exit
      state
let state =
  Cloud_interaction.make_current_repository state
let state =
  Cloud_interaction.synchronize_shared_repository
    state
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
