type dump =
  ?codegps:string ->
  ?academicyear:string ->
  Gen.dump

module type ReportCourseTranslations =
sig
  val dump_per_year: dump
  val dump_per_code: dump
end

let string_of_stringopt x =
  match x with
  | None -> ""
  | Some x -> x

module Build
    (I:Gen.Interface with type elt = Public_data.course_name_translation) =
  struct


  let headers =
    []

  let columns =
    [
      ["CODE"],
      (fun a ->
          a.Public_data.code);
      ["ANNEE"],
      (fun a -> a.Public_data.year);
      ["COURS"],
      (fun a ->
         string_of_stringopt a.Public_data.name);
      ["COURS(ANGLAIS)"],
      (fun a -> string_of_stringopt (a.Public_data.name_en))
    ]

    let dump_missing_course_name_translation
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name
        cmp headers columns state  =
      let event_opt =
        Some Profiling.Dump_missing_grades
      in
      let filter = Gen.filter_course_name_translation in
      let default_file_name = I.default_file_name in
      let get_repository = I.get_repository in
      let get = I.get in
      Gen.dump_elts
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name ?event_opt
        ~filter ~cmp ~headers ~columns ~get
        ~default_file_name ~get_repository
        state

    let dump_per_year
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> int_of_string
                a.Public_data.year);
          Gen.lift_cmp
            (fun a -> a.Public_data.code);
          ]
      in
      dump_missing_course_name_translation
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name cmp headers columns state

      (*let filter_student_list
        ?dpt ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
        ?niveau
        ?recu state student =
      let _ =
        dpt, dpt_gps_code, niveau, recu, academicyear, codegps, mentorname, mentorfirstname, mentorlastname, teachername, academicyear, ninscription, attributionyear
      in
      state,
      check firstname student.Public_data.student_firstname_report
      &&
      check lastname student.Public_data.student_lastname_report
      &&
      check promo student.Public_data.student_promo_report
      *)

    let dump_per_code
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> a.Public_data.code);
          Gen.lift_cmp
            (fun a -> a.Public_data.year);
        ]
      in
      dump_missing_course_name_translation
        ?codegps
        ?academicyear
        ?output_repository ?prefix ?file_name cmp headers columns state


end

module MissingCourseTranslations =
  Build
    (struct
      type elt = Public_data.course_name_translation

      let default_file_name = "missing_course_name_translation.csv"
      let get = Remanent_state.get_missing_course_name_translations
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_course_name_translations
    end)
