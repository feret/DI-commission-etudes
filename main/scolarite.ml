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
let state =
  List.fold_left
    (fun state elt ->
       Remanent_state.add_mentor
         state
         {
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
let () = Remanent_state.list_all_cursus state
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
let state =
  List.fold_left
    (fun state id ->
       let state, output =
         Get_gps_files.get_student_file
           ~output_repository
           id state
       in
       match output with
       | None -> state
       | Some output ->
         let state,patched_file_opt =
           Get_gps_files.patch_student_file
             state
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
                               output0))^".signed.tex")
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
let title = "LISTE DES TUTEURS 2020"
let state,_ =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_tuteur.html" ~title
    state
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear:"2020" ~dpt:"informatique" ~file_name:"tutorat_2020_par_tuteur.tex" ~title
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let title = "LISTE DES TUTEURS 2019"
let state,_ =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear ~dpt:"informatique" ~file_name:"tutorat_2019_par_tuteur.html" ~title
    state
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear ~dpt:"informatique" ~file_name:"tutorat_2019_par_tuteur.tex" ~title
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let title = "HISTORIQUE DES TUTEURS "
let state,_ =
    Mentor_report.ReportListMentors.dump_per_year_mentor_student
       ~dpt:"informatique" ~file_name:"tutorat_all_par_tuteur.html" ~title
      state
let state, input =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~dpt:"informatique" ~file_name:"tutorat_all_par_tuteur.tex" ~title
    state
let state =
    Latex_engine.latex_opt_to_pdf state ~input
let headpage s _ =
  Format.sprintf
    "Département Informatique \\\\Résultats 2019-2020\\\\%s\\\\Page \\thepage/\\pageref{LastPage}\\\\" s
let signature _ =
  "Paris \\ le 18 septembre 2020"
let state,_ =
  Dens_report.DensReport.dump_per_promo
    ~file_name:"dens_par_promo.html" state
let state,_ =
  Dens_report.DensReport.dump_per_n_inscription
    ~file_name:"dens_par_nb_inscriptions.html" state
let state, academicyear =
  Remanent_state.get_current_academic_year state
let state,_ =
  Diploma_report.DiplomaReport.dump_per_result_per_student
    ~file_name:"M1result.html"
    ~academicyear ~niveau:"m" ~dpt:"informatique"
    state
let state,_ =
  Diploma_report.DiplomaReport.dump_per_result_per_student
    ~file_name:"L3result.html"
    ~academicyear ~niveau:"l" ~dpt:"informatique" state
let state,_ =
  Diploma_report.DiplomaReport.dump_per_student
    ~file_name:"M1student.html"
    ~academicyear ~niveau:"m" ~dpt:"informatique"
    state
let state,_ =
  Diploma_report.DiplomaReport.dump_per_student
    ~file_name:"L3student.html"
    ~academicyear ~niveau:"l" ~dpt:"informatique" state
let preamble i =
  Format.sprintf
    "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en informatique à l'ENS et aux décisions de la commission des études du 18 septembre 2020,} \\\\ Je soussigné \\textbf{Marc Pouzet}, directeur des études du département d'informatique de l'École Normale Supérieure, certifie que les \\\\ \\underline{\\textbf{%i étudiants inscrits en 2019-2020}}, en première et deuxième année du diplôme de l'École Normale Supérieure, ont obtenu les résultats suivant" i
let state,input =
  Dens_report.DensReport.dump_per_promo
    ~file_name:"dens_par_promo.tex" state
    ~signature ~preamble ~headpage:(headpage "DENS")
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state,input =
  Dens_report.DensReport.dump_per_n_inscription
    ~file_name:"dens_par_nb_inscriptions.tex" state
    ~signature ~preamble ~headpage:(headpage "DENS")
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state, academicyear =
  Remanent_state.get_current_academic_year state
let preamble s u i =
  Format.sprintf
    "\\textbf{Conformément aux dispositions générales de la scolarité au sein des Études pré-doctorales en informatique à l'ENS et aux décisions de la commission des études du 18 septembre 2020,} \\\\ Je soussigné \\textbf{Marc Pouzet}, directeur des études du département d'informatique de l'École Normale Supérieure, certifie que les \\\\ \\underline{\\textbf{%i étudiants inscrits en 2019-2020}}, à l'université %s, \\\\ \\textbf{en %s - parcours : Formation interuniversitaire en informatique de l'ENS Paris, ont obtenu les résultats suivant}" i u s

let state,input =
  Diploma_report.DiplomaReport.dump_per_result_per_student
    ~file_name:"M1result.tex"
    ~academicyear ~niveau:"m" ~dpt:"informatique"
    ~headpage:(headpage "Master M1 d'informatique")
    ~preamble:(preamble "Master M1 d'informatique" "Paris Dauphine")
    ~signature
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state,input =
  Diploma_report.DiplomaReport.dump_per_student
    ~file_name:"M1student.tex"
    ~academicyear ~niveau:"m" ~dpt:"informatique"
    ~headpage:(headpage "Master M1 d'informatique fondamental")
    ~preamble:(preamble "Master M1 d'informatique fondamentale" "PSL")
    ~signature
    state
let state =
  Latex_engine.latex_opt_to_pdf ~rev:false state ~input
let state =
  Latex_engine.latex_opt_to_pdf ~rev:false state ~input
let state, input =
  Diploma_report.DiplomaReport.dump_per_result_per_student
    ~file_name:"L3result.tex"
    ~academicyear ~niveau:"l" ~dpt:"informatique"
    ~headpage:(headpage "Licence L3 d'informatique")
    ~preamble:(preamble "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
    ~signature
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state, input =
  Diploma_report.DiplomaReport.dump_per_student
    ~file_name:"L3student.tex"
    ~academicyear ~niveau:"l" ~dpt:"informatique"
    ~headpage:(headpage "Licence L3 d'informatique")
    ~preamble:(preamble "Licence L3 d'informatique" "Paris 7 - Denis Diderot")
    ~signature
    state
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state =
  Latex_engine.latex_opt_to_pdf state ~input
let state,_ =
  Grades.MissingGrades.dump_per_dpt_student_year
    ~file_name:"notes_manquantes_par_dpt_et_cours.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_class_year
    ~file_name:"notes_manquantes_par_dpt_et_etudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_year_student
    ~file_name:"notes_manquantes_par_dpt_et_annee_et_etudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_dpt_year_class
    ~file_name:"notes_manquantes_par_dpt_et_annee_et_cours.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_student
    ~file_name:"notes_manquantes_par_etudiant.html"
    state
let state,_ =
  Grades.MissingGrades.dump_per_promotion
    ~file_name:"notes_manquantes_par_promotion.html"
    state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_dpt_student_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_cours.html"
        state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_dpt_class_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_etudiant.html"
    state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_student
    ~file_name:"attributions_de_notes_manquantes_par_etudiant.html"
    state
let state,_ =
  Grades.MissingECTSAttributions.dump_per_promotion
    ~file_name:"attributions_de_notes_manquantes_par_promotion.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_student
    ~file_name:"mentors_manquants_par_etudiant.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_year
    ~file_name:"mentors_manquants_par_annee.html"
    state
let state,_ =
  Mentors.ReportMissingMentors.dump_per_promotion
    ~file_name:"mentors_manquants_par_promotion.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_manquantes_par_annee.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_student
    ~file_name:"descriptions_de_stage_manquantes_par_etudiant.html"
    state
let state,_ =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_promotion
    ~file_name:"descriptions_de_stage_manquantes_par_promotion.html"
    state

let state,_ =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_ambigues_par_annee.html"
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
  Cloud_interaction.make_current_repository state
let state =
  Cloud_interaction.synchronize_shared_repository
    state
let state =
    Remanent_state.print_errors "" state
let state =
    Remanent_state.print_errors ~logger:(Remanent_state.std_logger) "" state
