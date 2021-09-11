let dump_issues state =
  let state,_ =
    Student_report.ReportGpsServerFaillures.dump_per_student
      ~file_name:"échecs_extraction_gps_par_étudiant.html"
      state
  in
  let state,_ =
    Student_report.ReportGpsServerFaillures.dump_per_promo
      ~file_name:"échecs_extraction_gps_par_promotion.html"
      state
  in
  let state,_ =
    Student_report.ReportMissingPictures.dump_per_student
      ~file_name:"photos_manquantes_par_étudiant.html"
      state
  in
  let state,_ =
    Student_report.ReportMissingPictures.dump_per_promo
      ~file_name:"photos_manquantes_par_promotion.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_dpt_student_year
      ~file_name:"notes_manquantes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_year_dpt_student
      ~file_name:"notes_manquantes_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_dpt_class_year
      ~file_name:"notes_manquantes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_dpt_year_student
      ~file_name:"notes_manquantes_par_dpt_et_année_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_dpt_year_class
      ~file_name:"notes_manquantes_par_dpt_et_année_et_cours.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_student
      ~file_name:"notes_manquantes_par_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_promotion
      ~file_name:"notes_manquantes_par_promotion.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_dpt_student_year
      ~file_name:"cours_non_acceptes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_year_dpt_student
      ~file_name:"cours_non_acceptes_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_dpt_class_year
      ~file_name:"cours_non_acceptes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_dpt_year_student
      ~file_name:"cours_non_acceptes_par_dpt_et_année_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_dpt_year_class
      ~file_name:"cours_non_acceptes_par_dpt_et_année_et_cours.html"
      state
  in
  let state,_ =
    Grades.NonAcceptedGrades.dump_per_student
      ~file_name:"cours_non_acceptes_par_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingGrades.dump_per_promotion
      ~file_name:"cours_non_acceptes_par_promotion.html"
      state
  in
  let state,_ =
    Grades.MissingECTSAttributions.dump_per_dpt_student_year
      ~file_name:"attributions_de_notes_manquantes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingECTSAttributions.dump_per_year_dpt_student
      ~file_name:"attributions_de_notes_manquantes_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingECTSAttributions.dump_per_dpt_class_year
      ~file_name:"attributions_de_notes_manquantes_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.MissingECTSAttributions.dump_per_student
    ~file_name:"attributions_de_notes_manquantes_par_étudiant.html"
    state
  in
  let state,_ =
    Grades.MissingECTSAttributions.dump_per_promotion
    ~file_name:"attributions_de_notes_manquantes_par_promotion.html"
    state
  in
  let state,_ =
    Mentors.ReportMissingMentors.dump_per_student
      ~file_name:"tuteurs_manquants_par_étudiant.html"
      state
  in
  let state,_ =
    Mentors.ReportMissingMentors.dump_per_year
      ~file_name:"tuteurs_manquants_par_année.html"
      state
  in
  let state,_ =
    Mentors.ReportMissingMentors.dump_per_promotion
      ~file_name:"tuteurs_manquants_par_promotion.html"
      state
  in
  let state,_ =
    Internship_descriptions.MissingInternshipDescriptions.dump_per_year
      ~file_name:"descriptions_de_stage_manquantes_par_année.html"
      state
  in
  let state,_ =
    Internship_descriptions.MissingInternshipDescriptions.dump_per_student
      ~file_name:"descriptions_de_stage_manquantes_par_étudiant.html"
      state
  in
  let state,_ =
    Internship_descriptions.MissingInternshipDescriptions.dump_per_promotion
      ~file_name:"descriptions_de_stage_manquantes_par_promotion.html"
      state
  in
  let state,_ =
    Internship_descriptions.NonValidatedInternships.dump_per_year
      ~file_name:"stages_non_validés_par_année.html"
      state
  in
  let state,_ =
    Internship_descriptions.NonValidatedInternships.dump_per_student
      ~file_name:"stages_non_validés_par_étudiant.html"
      state
  in
  let state,_ =
    Internship_descriptions.NonValidatedInternships.dump_per_promotion
      ~file_name:"stages_non_validés_par_promotion.html"
      state
  in
  let state,_ =
    Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_year
      ~file_name:"descriptions_de_stage_ambigues_par_année.html"
      state
  in
  let state,_ =
    Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_promotion
      ~file_name:"descriptions_de_stage_ambigues_par_promotion.html"
      state
  in
  let state,_ =
    Internship_descriptions.AmbiguousInternshipDescriptions.dump_per_student
      ~file_name:"descriptions_de_stage_ambigues_par_etudiant.html"
      state
  in
  state

let warn state =
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
  in
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
  in
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
  in
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
  in
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
  in
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
  in
  let state =
    match Remanent_state.get_non_validated_internships state
    with
    | state, [] -> state
    | state, _::_ ->
      Remanent_state.warn
        __POS__
        "The status of several internships is not consistent"
        Exit
        state
  in
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
  in
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
  in state
