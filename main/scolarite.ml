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
  Collect_course_exceptions.get_course_exceptions state
let state =
  Collect_scholarships.get_scholarships state
let state =
  Collect_mentoring.get_mentoring state
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
         (fst output0, (Tools.basename (snd output0))^".validated_and_in_progress_only.tex")
       in
       let state =
         match gps with
         | None -> state
         | Some gps ->
           let state, output_opt =
             Transcripts.export_transcript
               ~output state gps
           in
           match output_opt with
           | None -> state
           | Some input -> Latex_engine.latex_to_pdf ~rev:true state ~input
       in
       let output =
         (fst output0, (Tools.basename (snd output0))^".all.tex")
       in
       let state =
         match gps with
         | None -> state
         | Some gps ->
           let filter = Public_data.All in
           let state, output_opt =
             Transcripts.export_transcript
               ~filter ~output state gps
           in
           match output_opt with
           | None -> state
           | Some input -> Latex_engine.latex_to_pdf ~rev:true state ~input
       in state
    )
    state
    students_list
let state, academicyear =
  Remanent_state.get_current_academic_year state
let state =
  Mentor_report.ReportListMentors.dump_per_year_mentor_student
    ~academicyear ~file_name:"tuteurs.html"
    state
let state =
  Grades.MissingGrades.dump_per_dpt_student_year
    ~file_name:"notes_manquantes_par_dpt_et_cours.html"
    state
let state =
  Grades.MissingGrades.dump_per_dpt_class_year
    ~file_name:"notes_manquantes_par_dpt_et_etudiant.html"
    state
let state =
  Grades.MissingGrades.dump_per_dpt_year_student
    ~file_name:"notes_manquantes_par_dpt_et_annee_et_etudiant.html"
    state
let state =
  Grades.MissingGrades.dump_per_dpt_year_class
    ~file_name:"notes_manquantes_par_dpt_et_annee_et_cours.html"
    state
let state =
  Grades.MissingGrades.dump_per_student
    ~file_name:"notes_manquantes_par_etudiant.html"
    state
let state =
  Grades.MissingGrades.dump_per_promotion
    ~file_name:"notes_manquantes_par_promotion.html"
    state
let state =
  Grades.MissingECTSAttributions.dump_per_dpt_student_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_cours.html"
        state
let state =
  Grades.MissingECTSAttributions.dump_per_dpt_class_year
    ~file_name:"attributions_de_notes_manquantes_par_dpt_et_etudiant.html"
    state
let state =
  Grades.MissingECTSAttributions.dump_per_student
    ~file_name:"attributions_de_notes_manquantes_par_etudiant.html"
    state
let state =
  Grades.MissingECTSAttributions.dump_per_promotion
    ~file_name:"attributions_de_notes_manquantes_par_promotion.html"
    state
let state =
  Mentors.ReportMissingMentors.dump_per_student
    ~file_name:"mentors_manquants_par_etudiant.html"
    state
let state =
  Mentors.ReportMissingMentors.dump_per_year
    ~file_name:"mentors_manquants_par_annee.html"
    state
let state =
  Mentors.ReportMissingMentors.dump_per_promotion
    ~file_name:"mentors_manquants_par_promotion.html"
    state
let state =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_manquantes_par_annee.html"
    state
let state =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_student
    ~file_name:"descriptions_de_stage_manquantes_par_etudiant.html"
    state
let state =
  Internship_descriptions.MissingInternshipDescriptions.dump_per_promotion
    ~file_name:"descriptions_de_stage_manquantes_par_promotion.html"
    state

let state =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_year
    ~file_name:"descriptions_de_stage_ambigues_par_annee.html"
    state
let state =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_promotion
    ~file_name:"descriptions_de_stage_ambigues_par_promotion.html"
    state
let state =
  Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_student
    ~file_name:"descriptions_de_stage_ambigues_par_etudiant.html"
    state
let state =
  Cloud_interaction.make_current_repository state
let state =
  Cloud_interaction.synchronize_shared_repository
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
    Remanent_state.print_errors "" state
let state =
    Remanent_state.print_errors ~logger:(Remanent_state.std_logger) "" state
