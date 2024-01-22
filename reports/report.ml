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
    Grades.Underaveragevalidated.dump_per_dpt_student_year
      ~file_name:"cours_valides_sans_la_moyenne_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Underaveragevalidated.dump_per_year_dpt_student
      ~file_name:"cours_valides_sans_la_moyenne_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Underaveragevalidated.dump_per_dpt_class_year
      ~file_name:"cours_valides_sans_la_moyenne_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Underaveragevalidated.dump_per_dpt_year_student
      ~file_name:"cours_valides_sans_la_moyenne_par_dpt_et_année_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Underaveragevalidated.dump_per_dpt_year_class
      ~file_name:"cours_valides_sans_la_moyenne_par_dpt_et_année_et_cours.html"
      state
  in
  let state,_ =
    Grades.Underaveragevalidated.dump_per_student
      ~file_name:"cours_valides_sans_la_moyenne_par_étudiant.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_dpt_student_year
      ~file_name:"cours_hors_scolarite_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_year_dpt_student
      ~file_name:"cours_hors_scolarite_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_dpt_class_year
      ~file_name:"cours_hors_scolarite_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_dpt_year_student
      ~file_name:"cours_hors_scolarite_par_dpt_et_année_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_dpt_year_class
      ~file_name:"cours_hors_scolarite_par_dpt_et_année_et_cours.html"
      state
  in
  let state,_ =
    Grades.Outofschoolingyears.dump_per_student
      ~file_name:"cours_hors_scolarite_par_étudiant.html"
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
    Grades.Validated_twice.dump_per_dpt_student_year
      ~file_name:"cours_en_double_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Validated_twice.dump_per_year_dpt_student
      ~file_name:"cours_en_double_par_année_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Validated_twice.dump_per_dpt_class_year
      ~file_name:"cours_en_double_par_dpt_et_étudiant.html"
      state
  in
  let state,_ =
    Grades.Validated_twice.dump_per_student
    ~file_name:"cours_en_double_par_étudiant.html"
    state
  in
  let state,_ =
    Grades.Validated_twice.dump_per_promotion
    ~file_name:"cours_en_double_par_promotion.html"
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
  let state,_ =
    Course_translations.MissingCourseEntries.dump
      ~file_name:"entrees_de_cours_incompletes.csv"
      state
  in
  let state,_ =
    Course_translations.CourseEntriesReport.dump
      ~file_name:"course_entries.csv"
      state
  in
  let state,_ =
    Mineures_majeures_suggestions.SuggestionsMineures.dump
      ~file_name:"suggestion_de_mineures.csv"
      state
  in
  let state,_ =
    Mineures_majeures_suggestions.SuggestionsMajeures.dump
      ~file_name:"suggestion_de_majeures.csv"
      state
  in
  let state,_ =
    Dens_candidates_suggestion.SuggestionsDensCandidates.dump
      ~file_name:"suggestion_de_diplomants.csv"
      state
  in
  let state,_ =
    Courses_to_be_sorted.CoursesToBeSorted.dump
      ~file_name:"courses_to_be_attributed.csv"
      state
  in
  let state,_ =
    Internships_to_be_sorted.InternshipsToBeSorted.dump
      ~file_name:"internships_to_be_attributed.csv"
      state
  in
  state

let warn state =
  let state =
    let state, a = Remanent_state.Missing_course_entries.get state in
    match a
    with
    | []-> state
    | _ ->
      Remanent_state.warn
        __POS__
        "Some course name translations are missing"
        Exit
        state
  in
  let state =
    match Remanent_state.Missing_ects_attributions.get state
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
    match Remanent_state.Courses_validated_twice.get state
    with
    | state, [] -> state
    | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some courses are validated several times"
        Exit
        state
  in
  let state =
    match Remanent_state.Missing_grades.get state
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
    match Remanent_state.Non_accepted_grades.get state
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
    match Remanent_state.Under_average_validated_grades.get state
    with
    | state, [] -> state
    | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some courses are validated with a grade below average"
        Exit
        state
  in
  let state =
    match Remanent_state.Grade_out_of_schooling_years.get state
    with
    | state, [] -> state
    | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some courses does not match with some schooling years"
        Exit
        state
  in
  let state =
    match Remanent_state.Missing_mentors.get state
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
    match Remanent_state.Missing_internship_descriptions.get state
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
    match Remanent_state.Ambiguous_internship_descriptions.get state
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
    match Remanent_state.Non_validated_internships.get state
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
          match Remanent_state.Missing_pictures.get state
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
    match Remanent_state.Gps_server_faillures.get state
    with
    | state, [] -> state
    | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some gps extractions failed"
        Exit
        state
  in
  let state =
    match Remanent_state.Course_to_be_sorted.get state
    with
      | state, [] -> state
      | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some courses should be attributed to some departments"
        Exit
        state
  in
  let state =
    match Remanent_state.Internships_to_be_sorted.get state
    with
      | state, [] -> state
      | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some internships should be attributed to some departments"
        Exit
        state
  in
  let state =
    match Remanent_state.Dens_candidate_missing_minors.get state
    with
      | state, [] -> state
      | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some minor should be requested"
        Exit
        state
  in
  let state =
    match Remanent_state.Dens_candidate_missing_majors.get state
    with
      | state, [] -> state
      | state, _::_ ->
      Remanent_state.warn
        __POS__
        "Some students are elligible for the DENS"
        Exit
        state
  in
  state
