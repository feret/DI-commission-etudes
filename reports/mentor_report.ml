type dump =
?studentfirstname:string ->
?studentlastname:string ->
?mentorfirstname:string ->
?mentorlastname:string ->
?academicyear:string ->
?attributionyear:string ->
?promo:string ->
?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
?dpt:Public_data.main_dpt ->
Gen.dump

module type ReportMentors =
sig
  val dump_per_year_mentor_student: correct_email:(string -> string) -> dump
  val dump_per_year_student_mentor: correct_email:(string -> string) -> dump
  val dump_per_promo_mentor_student: correct_email:(string -> string) -> dump
  val dump_per_promo_student_mentor: correct_email:(string -> string) -> dump
  val dump_per_mentor_year_promo_student: correct_email:(string -> string) -> dump
  val dump_per_student: correct_email:(string -> string) -> dump
  val dump_per_student_without_promo: correct_email:(string -> string) -> dump

  val dump:
    ?studentfirstname:string ->
    ?studentlastname:string ->
    ?mentorfirstname:string ->
    ?mentorlastname:string ->
    ?academicyear:string ->
    ?attributionyear:string ->
    ?promo:string ->
    ?title:((Sco_remanent_state.Loggers.t ->
                        (string -> unit, Format.formatter, unit) format ->
                        string -> unit) *
                       string)
                      list ->
    ?dpt:Public_data.main_dpt ->
    ?output_repository:string ->
    ?prefix:string ->
    ?file_name:(string -> string -> string) ->
    Remanent_state.t -> Remanent_state.t

end

module Build
    (I:Gen.Interface
     with type Missing_entry.entry = Public_data.mentor) =
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
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
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

  let whoshort ?with_secondaire first last main_dpt tut_dpt =
    let tut =
      match with_secondaire, main_dpt, tut_dpt with
      | (None | Some false), _, _ | _, _, None -> ""
      | _, a, Some a' when a = a' -> ""
      | _, _, Some _ -> " (tuteur secondaire)"
    in
    Printf.sprintf "%s %s%s"
      (Special_char.capitalize first)
      (Special_char.uppercase last)
      tut

  let who ?with_secondaire genre first last main_dpt tut_dpt =
    Printf.sprintf "%s%s"
      (string_of_gender genre)
      (whoshort
         ?with_secondaire
         (Special_char.capitalize first)
         (Special_char.uppercase last)
         main_dpt tut_dpt
      )

  let wholong ?with_secondaire correct_email genre first last email main_dpt tut_dpt =
    if String.trim email = ""
    then who ?with_secondaire genre first last main_dpt tut_dpt
    else
      Printf.sprintf "%s (%s)"
        (who ?with_secondaire genre first last main_dpt tut_dpt)
        (correct_email email)

  let nom_etudiant =
    ["ÉTUDIANT"],
    (fun a ->
       whoshort
         ~with_secondaire:true
         a.Public_data.mentor_student_firstname
         a.Public_data.mentor_student_lastname
         a.Public_data.mentor_student_dpt
         a.Public_data.mentor_secondary
    )
  let nom_etudiant_long =
    let l,f = nom_etudiant in
    l, (fun a ->
        Printf.sprintf "%s (%s)"
          (f a)
          (a.Public_data.mentor_student_promo))

  let nom_tuteur correct_email =
    ["TUTEUR"],
    (fun a ->
       wholong correct_email
         a.Public_data.mentor_gender
         a.Public_data.mentor_firstname
         a.Public_data.mentor_lastname
         a.Public_data.mentor_email
         a.Public_data.mentor_student_dpt
         a.Public_data.mentor_secondary)

  let promotion =
    ["PROMOTION"],
    (fun a -> a.Public_data.mentor_student_promo)

  let annee =
    ["ANNÉE ACADÉMIQUE"],
    (fun a ->
       let a = a.Public_data.mentor_academic_year in
       try
         let i = int_of_string a in
         Printf.sprintf "%i -- %i" i (i+1)
       with _ -> a
    )

  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_year_mentor_student
      ~correct_email
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo
      ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.op_cmp
          (Gen.lift_cmp
             (fun a ->
                a.Public_data.mentor_academic_year));
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
        lift_id (nom_tuteur correct_email)
      ]
      | Some _ ->
        [lift_id (nom_tuteur correct_email)]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump_per_year_student_mentor
      ~correct_email
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.op_cmp
          (Gen.lift_cmp (fun a -> a.Public_data.mentor_academic_year));
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
      ]
    in
    let columns =
      [
        nom_etudiant_long;
        nom_tuteur correct_email
      ]
    in
    let headers =
      match academicyear with
      | None ->
      [
        lift_id annee ;
          ]
      | Some _ -> []
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump_per_promo_mentor_student
      ~correct_email
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
        lift_id (nom_tuteur correct_email)
      ]
      | Some _ ->
        [
          lift_id
            (nom_tuteur correct_email)
        ]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump_per_promo_student_mentor
      ~correct_email
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
      | None -> [ annee ; nom_tuteur correct_email  ]
      | Some _ -> [nom_tuteur correct_email]
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
      ?output_repository ?prefix ?file_name
      cmp headers columns state


  let dump_per_student
      ~correct_email
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
    let columns =
      [
        nom_etudiant;
        nom_tuteur correct_email
      ]
    in
    let columns =
      match academicyear
      with
      | None -> annee::columns
      | Some _ ->
        [nom_etudiant;
         promotion;
         nom_tuteur correct_email ]
    in
    let headers =
      match academicyear, promo with
      | Some _, _ -> []
      | _, None ->
      [
        lift_id promotion ;
        lift_id nom_etudiant_long
      ]
      | _,Some _ -> [lift_id nom_etudiant_long]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump_per_student_without_promo
      ~correct_email
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_student_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.mentor_firstname);
      ]
    in
    let columns =
      [
        nom_etudiant;
        nom_tuteur correct_email
      ]
    in

    let headers =
      match academicyear, promo with
      | Some _, _ -> []
      | _, None ->
        [
          lift_id nom_etudiant_long
        ]
      | _,Some _ -> [lift_id nom_etudiant_long]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state


  let dump_per_mentor_year_promo_student
      ~correct_email
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
          lift_id (nom_tuteur correct_email) ;
          lift_id annee
        ]
      | Some _ ->
        [lift_id (nom_tuteur correct_email)]
    in
    dump_mentor_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state

  let dump
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname
      ?academicyear ?attributionyear ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name state
    =
    let file_name =
      match file_name with
      | Some f -> f
      | None ->
        begin
          let new_or_not =
            match academicyear,attributionyear
            with
            | Some y, Some y' when y=y' -> "_nouvelles_affectations"
            | _, Some y ->
              Format.sprintf "_affectés_en_%s" y
            | _, None -> ""
          in
          let year =
            match academicyear with
            | None -> "all"
            | Some y -> y
          in
          (fun s ext ->
          Format.sprintf "tuteurs%s_%s%s.%s" new_or_not year s ext)
        end
    in
    let correct_email = fun x -> x in
    let state,_ =
      dump_per_year_student_mentor
        ?studentfirstname ?studentlastname ?mentorfirstname
        ?mentorlastname
        ?academicyear ?attributionyear ?promo ?title ?dpt
        ?output_repository ?prefix
        ~file_name:(file_name "_par_étudiant" "html")
        ~correct_email
        state
    in
    let state,_ =
      dump_per_year_mentor_student
        ?studentfirstname ?studentlastname
        ?mentorfirstname ?mentorlastname
        ?academicyear ?attributionyear ?promo ?title ?dpt
        ?output_repository ?prefix
        ~file_name:(file_name "_par_tuteur" "html")
        ~correct_email
        state
    in
    let correct_email =
          Special_char.correct_string_email_latex
    in
    let state, input =
      dump_per_year_mentor_student
        ?studentfirstname ?studentlastname
        ?mentorfirstname ?mentorlastname
        ?academicyear ?attributionyear ?promo ?title ?dpt
        ?output_repository ?prefix
        ~file_name:(file_name "_par_tuteur" "tex")
        ~correct_email
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf state ~input
    in
    let state, input =
      dump_per_year_student_mentor
        ?studentfirstname ?studentlastname
        ?mentorfirstname ?mentorlastname
        ?academicyear ?attributionyear ?promo ?title ?dpt
        ?output_repository ?prefix
        ~file_name:(file_name "_par_étudiant" "tex")
        ~correct_email
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf state ~input
    in
    let state, input =
      dump_per_student_without_promo
        ?studentfirstname ?studentlastname
        ?mentorfirstname ?mentorlastname
        ?academicyear ?attributionyear ?promo ?title ?dpt
        ?output_repository ?prefix
        ~file_name:(file_name "_par_étudiant_sans_promo" "tex")
        ~correct_email
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf state ~input
    in
    state
  end


module ReportListMentors =
  Build
    (struct
      module Missing_entry =
        struct
          type entry = Public_data.mentor
          let get = Remanent_state.get_mentors
          let get_repository =
            Remanent_state.get_repository_to_dump_mentors
          let add a _ =  a
        end
      let default_file_name = "tuteurs.html"
          end)
