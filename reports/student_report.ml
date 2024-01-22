type dump =
?studentfirstname:string ->
?studentlastname:string ->
?promo:string ->
  Gen.dump

module type ReportStudents =
sig
  val dump_per_student: dump
  val dump_per_promo: dump
end

module Build
    (I:Gen.Interface
     with type Missing_entry.entry = Public_data.student) =
struct

  let dump_student_list
      ?studentfirstname
      ?studentlastname
      ?promo
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_student_list
    in
    let filter = Gen.filter_student_list in
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    let firstname = studentfirstname in
    let lastname = studentlastname in
    Gen.dump_elts
      ?firstname ?lastname ?promo
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state

  let nom =
    ["NOM"],
    (fun a -> a.Public_data.student_lastname_report)

  let prenom =
    ["PRÉNOM"],
    (fun a -> a.Public_data.student_firstname_report)

  let promotion =
    ["PROMOTION"],
    (fun a -> a.Public_data.student_promo_report)

  let dump_per_student
      ?studentfirstname ?studentlastname  ?promo
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [ Gen.lift_cmp (fun a ->
            a.Public_data.student_lastname_report);
        Gen.lift_cmp (fun a ->
            a.Public_data.student_firstname_report);
        Gen.lift_cmp (fun a ->
            a.Public_data.student_promo_report);
      ]
    in
    let columns = [nom;prenom;promotion] in
    let headers = [] in
    dump_student_list
      ?studentfirstname ?studentlastname ?promo
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump_per_promo
      ?studentfirstname ?studentlastname  ?promo
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a ->
            a.Public_data.student_promo_report);
        Gen.lift_cmp (fun a ->
            a.Public_data.student_lastname_report);
        Gen.lift_cmp (fun a ->
            a.Public_data.student_firstname_report);

      ]
    in
    let columns = [nom;prenom;promotion] in
    let headers = [] in
    dump_student_list
      ?studentfirstname ?studentlastname ?promo
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  end

module ReportGpsServerFaillures =
  Build
    (struct
      module Missing_entry = Remanent_state.Gps_server_faillures
      let default_file_name = "échecs_extraction_gps.html"
    end)

module ReportMissingPictures =
  Build
    (struct
      module Missing_entry = Remanent_state.Missing_pictures
      let default_file_name = "photos_manquantes.html"
    end)
