type dump =
  ?dpt:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportGrades =
sig
  val dump_per_year_dpt_student: dump
  val dump_per_dpt_student_year: dump
  val dump_per_dpt_year_student: dump
  val dump_per_dpt_class_year: dump
  val dump_per_dpt_year_class: dump
  val dump_per_student: dump
  val dump_per_promotion: dump
end

module Build
    (I:Gen.Interface with type elt = Public_data.missing_grade) =
  struct

    let dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        cmp headers columns state  =
      let event_opt =
        Some Profiling.Dump_missing_grades
      in
      let filter = Gen.filter_grade in
      let default_file_name = I.default_file_name in
      let get_repository = I.get_repository in
      let get = I.get in
      Gen.dump_elts
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name ?event_opt
        ~filter ~cmp ~headers ~columns ~get
        ~default_file_name ~get_repository
        state

    let dump_per_dpt_student_year
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> int_of_string
                a.Public_data.missing_grade_dpt_indice);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_grade_lastname);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_grade_firstname);
          Gen.op_cmp
            (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year))]
      in
      let headers =
        [
          ["Département"],(fun x -> x),
          (fun a ->
             a.Public_data.missing_grade_dpt);
          ["Etudiant"],(fun x -> x),
          (fun a ->
             Printf.sprintf "%s %s (PROMO %s)"
               a.Public_data.missing_grade_lastname
               a.Public_data.missing_grade_firstname
               a.Public_data.missing_grade_promotion);
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year)
        ] in
      let columns =
        [
          ["CODE GPS"],(fun a -> a.Public_data.missing_grade_code_gps);
          ["COURS"],(fun a -> a.Public_data.missing_grade_intitule);
          ["ENSEIGNANT(E)"],(fun a -> a.Public_data.missing_grade_teacher)
        ] in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state

    let dump_per_year_dpt_student
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.op_cmp
            (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
          Gen.lift_cmp
            (fun a -> int_of_string
                a.Public_data.missing_grade_dpt_indice);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_grade_lastname);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_grade_firstname);
        ]
      in
      let headers =
        [
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year);
          ["Département"],(fun x -> x),
          (fun a ->
             a.Public_data.missing_grade_dpt);
          ["Etudiant"],(fun x -> x),
          (fun a ->
             Printf.sprintf "%s %s (PROMO %s)"
               a.Public_data.missing_grade_lastname
               a.Public_data.missing_grade_firstname
               a.Public_data.missing_grade_promotion);
          ] in
      let columns =
        [
          ["CODE GPS"],(fun a -> a.Public_data.missing_grade_code_gps);
          ["COURS"],(fun a -> a.Public_data.missing_grade_intitule);
          ["ENSEIGNANT(E)"],(fun a -> a.Public_data.missing_grade_teacher)
        ] in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state

    let dump_per_dpt_year_student
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository
        ?prefix
        ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> int_of_string
                a.Public_data.missing_grade_dpt_indice);
          Gen.op_cmp
            (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_lastname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_firstname);
        ]
      in
      let headers =
        [
          ["Département"],(fun x -> x),
          (fun a ->
             a.Public_data.missing_grade_dpt);
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year);
          ["Étudiant"],(fun x -> x),
          (fun a ->
             Printf.sprintf "%s %s (PROMO %s)"
               a.Public_data.missing_grade_lastname
               a.Public_data.missing_grade_firstname
               a.Public_data.missing_grade_promotion);
        ] in
      let columns =
        [
          ["CODE GPS"],(fun a -> a.Public_data.missing_grade_code_gps);
          ["COURS"],(fun a -> a.Public_data.missing_grade_intitule);
          ["ENSEIGNANT(E)"],(fun a -> a.Public_data.missing_grade_teacher)
        ] in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state


    let dump_per_dpt_class_year
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [Gen.lift_cmp
           (fun a -> int_of_string
               a.Public_data.missing_grade_dpt_indice);
         Gen.lift_cmp (fun a -> a.Public_data.missing_grade_intitule);
         Gen.op_cmp
           (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
         Gen.lift_cmp (fun a -> a.Public_data.missing_grade_lastname);
         Gen.lift_cmp (fun a -> a.Public_data.missing_grade_firstname);
         Gen.lift_cmp (fun a -> a.Public_data.missing_grade_promotion)]
      in
      let headers =
        [
          ["Département"],(fun x -> x),
          (fun a ->
             a.Public_data.missing_grade_dpt);
          ["Cours"],(fun x -> x),
          (fun a ->
             match
               String.trim a.Public_data.missing_grade_teacher,
               String.trim a.Public_data.missing_grade_code_gps
             with
             | "","" -> a.Public_data.missing_grade_intitule
             | s,"" | "",s ->
               Format.sprintf
                 "%s (%s)"
                 a.Public_data.missing_grade_intitule s
             | s,s' ->
               Format.sprintf
                 "%s (%s / %s)"
                 a.Public_data.missing_grade_intitule s s'
          );
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year)
        ] in
      let columns =
        [
          ["NOM"],(fun a -> a.Public_data.missing_grade_lastname);
          ["PRÉNOM"],(fun a -> a.Public_data.missing_grade_firstname);
          ["PROMOTION"],(fun a -> a.Public_data.missing_grade_promotion);
        ]
      in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository
        ?prefix
        ?file_name
        cmp headers columns state

    let dump_per_dpt_year_class
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> int_of_string
                a.Public_data.missing_grade_dpt_indice);
          Gen.op_cmp
            (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_intitule);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_lastname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_firstname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_promotion)]
      in
      let headers =
        [
          ["Département"],(fun x -> x),
          (fun a ->
             a.Public_data.missing_grade_dpt);
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year);
          ["Cours"],(fun x -> x),
          (fun a ->
             match
               String.trim a.Public_data.missing_grade_teacher,
               String.trim a.Public_data.missing_grade_code_gps
             with
             | "","" -> a.Public_data.missing_grade_intitule
             | s,"" | "",s ->
               Format.sprintf
                 "%s (%s)"
                 a.Public_data.missing_grade_intitule s
             | s,s' ->
               Format.sprintf
                 "%s (%s / %s)"
                 a.Public_data.missing_grade_intitule s s'
          );
        ] in
      let columns =
        [
          ["NOM"],(fun a -> a.Public_data.missing_grade_lastname);
          ["PRÉNOM"],(fun a -> a.Public_data.missing_grade_firstname);
          ["PROMOTION"],(fun a -> a.Public_data.missing_grade_promotion);
        ]
      in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        cmp headers columns state

    let dump_per_student
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_lastname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_firstname);
          Gen.op_cmp
            (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
          Gen.lift_cmp
            (fun a ->
               int_of_string
                 a.Public_data.missing_grade_dpt_indice);
          Gen.lift_cmp (fun a -> a.Public_data.missing_grade_intitule)
        ]
      in
      let headers =
        [
          ["Étudiant"],(fun x -> x),
          (fun a ->
             Printf.sprintf "%s %s (PROMO %s)"
               a.Public_data.missing_grade_lastname
               a.Public_data.missing_grade_firstname
               a.Public_data.missing_grade_promotion);
          ["Année académique"],
          (fun x -> x),
          (fun a ->
             let year = a.Public_data.missing_grade_year in
             try
               let year =
                 int_of_string year
               in
               Printf.sprintf "%i -- %i" year (year+1)
             with
               _ -> year)
        ]
      in
      let columns =
        [
          ["DÉPARTEMENT"],(fun a -> a.Public_data.missing_grade_dpt);
          ["CODE GPS"],(fun a -> a.Public_data.missing_grade_code_gps);
          ["COURS"],(fun a -> a.Public_data.missing_grade_intitule);
          ["ENSEIGNANT"],(fun a -> a.Public_data.missing_grade_teacher);
        ]
      in
      dump_missing_grades
        ?dpt ?firstname ?lastname ?codegps ?teachername
        ?academicyear ?promo
        ?output_repository ?prefix?file_name
    cmp headers columns state

let dump_per_promotion
    ?dpt ?firstname ?lastname ?codegps ?teachername
    ?academicyear ?promo
    ?output_repository ?prefix ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp (fun a -> a.Public_data.missing_grade_promotion);
      Gen.lift_cmp (fun a -> a.Public_data.missing_grade_lastname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_grade_firstname);
      Gen.op_cmp
        (Gen.lift_cmp (fun a -> a.Public_data.missing_grade_year));
      Gen.lift_cmp (fun a -> int_of_string  a.Public_data.missing_grade_dpt_indice);
      Gen.lift_cmp (fun a -> a.Public_data.missing_grade_intitule)
    ]
  in
  let headers =
    [
      ["Promotion"],(fun x ->x),(fun a -> a.Public_data.missing_grade_promotion);
      ["Étudiant"],(fun x -> x),
      (fun a ->
         Printf.sprintf "%s %s"
           a.Public_data.missing_grade_lastname
           a.Public_data.missing_grade_firstname
           );
      ["Année académique"],
      (fun x -> x),
      (fun a ->
         let year = a.Public_data.missing_grade_year in
         try
           let year =
             int_of_string year
           in
           Printf.sprintf "%i -- %i" year (year+1)
         with
           _ -> year)
    ]
  in
  let columns =
    [
      ["DÉPARTEMENT"],(fun a -> a.Public_data.missing_grade_dpt);
      ["CODE GPS"],(fun a -> a.Public_data.missing_grade_code_gps);
      ["COURS"],(fun a -> a.Public_data.missing_grade_intitule);
      ["ENSEIGNANT"],(fun a -> a.Public_data.missing_grade_teacher);
    ]
  in
  dump_missing_grades
    ?dpt ?firstname ?lastname ?codegps ?teachername ?academicyear ?promo
    ?output_repository
    ?prefix
    ?file_name
    cmp headers columns state
end

module MissingGrades =
  Build
    (struct
      type elt = Public_data.missing_grade

      let default_file_name = "missing_grade.html"
      let get = Remanent_state.get_missing_grades
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_grades
    end)



module NonAcceptedGrades =
  Build
    (struct
      type elt = Public_data.missing_grade

      let default_file_name = "non_accedpted_grades.html"
      let get = Remanent_state.get_non_accepted_grades
      let get_repository =
        Remanent_state.get_repository_to_dump_non_accepted_grades
    end)

module MissingECTSAttributions =
  Build
    (struct
      type elt = Public_data.missing_grade

      let default_file_name = "missing_ects_attributions.html"
      let get = Remanent_state.get_missing_ects_attributions
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_ects_attributions
    end)
