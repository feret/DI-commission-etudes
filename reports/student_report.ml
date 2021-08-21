type dump =
?studentfirstname:string ->
?studentlastname:string ->
?promo:string ->
  Gen.dump

let save _ a =  a

module type ReportStudents =
sig
  val dump_per_student: dump
  val dump_per_promo: dump
end

module Build
    (I:Gen.Interface
     with type elt = Public_data.student) =
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
    let get = I.get in
    let save = I.save in
    let default_file_name = I.default_file_name in
    let get_repository = I.get_repository in
    let firstname = studentfirstname in
    let lastname = studentlastname in
    Gen.dump_elts
      ?firstname ?lastname ?promo
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository ~save
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

  let lift_id (a,b) = (a,(fun x -> x),b)

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
      type elt = Public_data.student
      let default_file_name = "échecs_extraction_gps.html"
      let get = Remanent_state.get_gps_server_faillures
      let get_repository =
        Remanent_state.get_repository_to_dump_gps_server_faillures
      let save = save

    end)

module ReportMissingPictures =
  Build
    (struct
      type elt = Public_data.student
      let default_file_name = "photos_manquantes.html"
      let get = Remanent_state.get_missing_pictures
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_pictures
      let save = save
    end)
