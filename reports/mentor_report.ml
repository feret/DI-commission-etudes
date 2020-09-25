type dump =
?studentfirstname:string ->
?studentlastname:string ->
?mentorfirstname:string ->
?mentorlastname:string ->
?academicyear:string ->
?attributionyear:string ->
?promo:string ->
?title:string ->
?dpt:string ->
  Gen.dump

module type ReportMentors =
sig
  val dump_per_year_mentor_student: dump
  val dump_per_year_student_mentor: dump
  val dump_per_promo_mentor_student: dump
  val dump_per_promo_student_mentor: dump
  val dump_per_mentor_year_promo_student: dump
  val dump_per_student: dump
end

module Build
    (I:Gen.Interface
     with type elt = Public_data.mentor) =
struct

  let dump_mentor_list
      ?studentfirstname
      ?studentlastname
      ?mentorfirstname
      ?mentorlastname
      ?academicyear
      ?attributionyear
      ?promo
      ?title
      ?dpt
      ?output_repository ?prefix ?file_name
    cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_mentor_list
    in
    let filter = Gen.filter_mentoring_list in
    let get = I.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.get_repository in
    let firstname = studentfirstname in
    let lastname = studentlastname in
    Gen.dump_elts
      ?firstname ?lastname ?mentorfirstname ?mentorlastname ?academicyear ?attributionyear
      ?promo ?dpt
      ?output_repository ?prefix ?file_name ?event_opt ?title
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state

  let string_of_gender m =
    match m with
    | Public_data.Masculin -> "M. "
    | Public_data.Feminin -> "Mme. "
    | Public_data.Unknown -> ""
  let whoshort first last =
    Printf.sprintf "%s %s"
      (Special_char.capitalize first)
      (Special_char.uppercase last)

  let who genre first last =
    Printf.sprintf "%s%s"
      (string_of_gender genre)
      (whoshort
         (Special_char.capitalize first)
         (Special_char.uppercase last))


  let nom_etudiant =
    "ÉTUDIANT",
    (fun a ->
       whoshort
         a.Public_data.mentor_student_firstname
         a.Public_data.mentor_student_lastname)
  let nom_etudiant_long =
    let l,f = nom_etudiant in
    l, (fun a ->
        Printf.sprintf "%s (%s)"
          (f a)
          (a.Public_data.mentor_student_promo))

  let nom_tuteur =
    "TUTEUR",
    (fun a ->
       who
         a.Public_data.mentor_gender
         a.Public_data.mentor_firstname
         a.Public_data.mentor_lastname)
  let promotion =
    "PROMOTION",
    (fun a -> a.Public_data.mentor_student_promo)

  let annee =
    "ANNÉE ACADÉMIQUE",
    (fun a ->
       let a = a.Public_data.mentor_academic_year in
       try
         let i = int_of_string a in
         Printf.sprintf "%i -- %i" i (i+1)
       with _ -> a
    )

  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_year_mentor_student
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo
      ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [ Gen.lift_cmp (fun a ->
          a.Public_data.mentor_student_dpt);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_academic_year);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname)
      ]
    in
    let columns = [nom_etudiant; promotion] in
    let headers =
      match academicyear with
      | None ->
      [
        lift_id annee ;
        lift_id nom_tuteur
      ]
      | Some _ -> [lift_id nom_tuteur]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_year_student_mentor
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_academic_year);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
      ]
    in
    let columns = [nom_tuteur] in
    let headers =
      match academicyear with
      | None ->
      [
        lift_id annee ;
        lift_id nom_tuteur
      ]
      | Some _ -> [lift_id nom_tuteur]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_promo_mentor_student
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_promo);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname)
      ]
    in
    let columns =
      match academicyear with
      | None ->
      [
        annee; nom_etudiant
      ]
      | Some _ -> [nom_etudiant]
    in
    let headers =
      match promo with
      | None ->
      [
        lift_id promotion ;
        lift_id nom_tuteur
      ]
      | Some _ -> [lift_id nom_tuteur]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_promo_student_mentor
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_promo);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
      ]
    in
    let columns =
      match academicyear with
      | None -> [ annee ; nom_tuteur ]
      | Some _ -> [nom_tuteur]
    in
    let headers =
      match promo with
      | None ->
      [
        lift_id promotion ;
        lift_id nom_etudiant_long
      ]
      | Some _ -> [lift_id nom_etudiant_long]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state


  let dump_per_student
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_promo);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
      ]
    in
    let columns = [nom_etudiant;nom_tuteur] in
    let columns =
      match academicyear
      with
      | None -> annee::columns
      | Some _ -> columns
    in
    let headers =
      match promo with
      | None ->
      [
        lift_id promotion ;
        lift_id nom_etudiant_long
      ]
      | Some _ -> [lift_id nom_etudiant_long]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_mentor_year_promo_student
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_academic_year);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_promo);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);

      ]
    in
    let columns =
      match promo with
      | None -> [promotion;nom_etudiant]
      | Some _ -> [nom_etudiant]
    in
    let headers =
      match academicyear with
      | None ->
        [
          lift_id nom_tuteur ;
          lift_id annee
        ]
      | Some _ ->
        [lift_id nom_tuteur]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name cmp headers columns state
  end


module ReportListMentors =
  Build
    (struct
      type elt = Public_data.mentor
      let default_file_name = "tuteurs.html"
      let get = Remanent_state.get_mentors
      let get_repository =
        Remanent_state.get_repository_to_dump_mentors
    end)
