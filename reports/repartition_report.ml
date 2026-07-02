type dump =
?firstname:string ->
    ?lastname:string ->
    ?academicyear:string ->
    ?attributionyear:string -> 
    ?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
Gen.dump

    module type ReportRepartition =
sig
  val dump_per_year_course: correct_email:(string -> string) ->  dump
  val dump_per_year_teacher: correct_email:(string -> string) ->dump
 (* val dump_per_course_teacher: correct_email:(string -> string) ->dump
  val dump_per_course_year: correct_email:(string -> string) ->dump
  val dump_per_teacher_course: correct_email:(string -> string) ->dump
  val dump_per_teacher_year: correct_email:(string -> string) ->dump*)
  
  val dump:
    ?firstname:string ->
    ?lastname:string ->
    ?academicyear:string ->
    ?attributionyear:string -> 
    ?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
    ?output_repository:string ->
    ?prefix:string ->
    ?file_name:(string -> string -> string) ->
    Remanent_state.t -> Remanent_state.t

end
module Build
(I:Gen.Interface
  with type Missing_entry.entry = Public_data.pedagogical_charge
  and type Missing_entry.collector = Public_data.pedagogical_charge list)
=
struct

  let dump_repartition_list
    ?firstname
    ?lastname 
    ?academicyear 
    ?attributionyear 
    ?title
    ?output_repository
    ?prefix
    ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_repartition_list
    in
    let filter = Gen.filter_pedagogical_charge in
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    let firstname = firstname in
    let lastname = lastname in
    Gen.dump_elts
      ?firstname ?lastname ?academicyear ?attributionyear 
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

  let _wholong ?with_secondaire correct_email genre first last email main_dpt tut_dpt =
    if String.trim email = ""
    then who ?with_secondaire genre first last main_dpt tut_dpt
    else
      Printf.sprintf "%s (%s)"
        (who ?with_secondaire genre first last main_dpt tut_dpt)
        (correct_email email)

  
  let nom_enseignant =
    ["ENSEIGNANT"],
    (fun a ->  Printf.sprintf "%s %s"
      (Special_char.capitalize a.Public_data.charge_firstname) 
      (Special_char.uppercase a.Public_data.charge_lastname))

  let short (_,b) = [""],b
    let _ = short 

  let annee =
    ["ANNÉE ACADÉMIQUE"],
    (fun a ->
       let a = a.Public_data.charge_attribution_year in
       try
         let i = int_of_string a in
         Printf.sprintf "%i -- %i" i (i+1)
       with _ -> a
    )

  let cours = 
      ["COURS"], 
      (fun a -> 
      Tools.option_to_string (fun a -> a) a.Public_data.charge_course_title)
      
  

  let td = 
      (["Heures de TD"]), 
      (fun a -> 
        Tools.option_to_string (Public_data.string_of_or_unknown string_of_float) 
        a.Public_data.charge_td) 

  let tp = 
      (["Heures de TP"]), 
      (fun a -> 
        Tools.option_to_string (Public_data.string_of_or_unknown string_of_float) 
         a.Public_data.charge_tp) 

  let cm = 
      (["Heures de CM"]), 
      (fun a -> 
        Tools.option_to_string (Public_data.string_of_or_unknown string_of_float) 
         a.Public_data.charge_cm) 

  let type_de_contrat = 
    ["Mode de rémunération"], 
    (fun a -> 
      Tools.option_to_string (Public_data.string_of_or_unknown Public_data.string_of_contract) a.Public_data.charge_remuneration) 
  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_year_course
      ~correct_email
      ?firstname ?lastname 
      ?academicyear 
      ?attributionyear 
      ?title 
      ?output_repository ?prefix ?file_name
      state =
    let _ = correct_email in 
    let cmp =
      [
        Gen.op_cmp
          (Gen.lift_cmp
             (fun a ->
                a.Public_data.charge_attribution_year));
        Gen.lift_cmp (fun a -> a.Public_data.charge_course_title);
        Gen.lift_cmp (fun a -> a.Public_data.charge_lastname); 
        Gen.lift_cmp (fun a -> a.Public_data.charge_firstname);
        ]
    in
    let columns = [nom_enseignant ; cm; td ; tp ; type_de_contrat ] in
    let headers =
      match attributionyear with
      | None ->
      [
        lift_id annee ;
        lift_id cours  
      ]
      | Some _ ->
        [lift_id cours]
    in
    dump_repartition_list
      ?firstname ?lastname  ?academicyear ?attributionyear ?title 
      ?output_repository ?prefix ?file_name
      cmp headers columns state


 let dump_per_year_teacher
      ~correct_email
      ?firstname ?lastname 
      ?academicyear 
      ?attributionyear 
      ?title 
      ?output_repository ?prefix ?file_name
      state =
    let _ = correct_email in 
    let cmp =
      [
        Gen.op_cmp
          (Gen.lift_cmp
             (fun a ->
                a.Public_data.charge_attribution_year));
        Gen.lift_cmp (fun a -> a.Public_data.charge_course_title);
        Gen.lift_cmp (fun a -> a.Public_data.charge_firstname);
        Gen.lift_cmp (fun a -> a.Public_data.charge_lastname); 
         ]
    in
    let columns = [cours; cm; td ; tp ; type_de_contrat ] in
    let headers =
      match attributionyear with
      | None ->
      [
        lift_id annee ;
        lift_id nom_enseignant  
      ]
      | Some _ ->
        [lift_id cours]
    in
    dump_repartition_list
      ?firstname ?lastname  ?academicyear ?attributionyear ?title 
      ?output_repository ?prefix ?file_name
      cmp headers columns state   
      
      
      
(*
  let dump_per_year_teacher 
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
    dump_repartition_list
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
    dump_repartition_list
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
    dump_repartition_list
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
    dump_repartition_list
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
    dump_repartition_list
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
    dump_repartition_list
      ?studentfirstname ?studentlastname ?mentorfirstname
      ?mentorlastname ?academicyear ?attributionyear
      ?promo ?title ?dpt
      ?output_repository ?prefix ?file_name
      cmp headers columns state*) 

  let dump
  ?firstname ?lastname 
      ?academicyear 
      ?attributionyear  ?title 
      ?output_repository ?prefix ?file_name
       state
    =
    let file_name =
      match file_name with
      | Some f -> f
      | None ->
        begin
          let new_or_not =
            match academicyear,attributionyear
            with
            | Some y, Some y' when y=y' -> "_nouvelles_charges_pedagogiques"
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
          Format.sprintf "charges_pedagogiques_%s_%s%s.%s" new_or_not year s ext)
        end
    in
    let correct_email = fun x -> x in
    let state, _ = 
      dump_per_year_course ~correct_email
      ?firstname ?lastname 
      ?academicyear ?attributionyear
      ?title 
      ?output_repository ?prefix ~file_name:(file_name "_par_cours" "html")
      state
  in 
  let state, input =
      dump_per_year_course 
      ~correct_email
      ?firstname ?lastname 
      ?academicyear ?attributionyear
      ?title 
      ?output_repository ?prefix ~file_name:(file_name "_par_cours" "tex")
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf state ~input
    in
 let state, _ = 
      dump_per_year_teacher ~correct_email
      ?firstname ?lastname 
      ?academicyear ?attributionyear
      ?title 
      ?output_repository ?prefix ~file_name:(file_name "_par_enseignant" "html")
      state
  in 
  let state, input =
      dump_per_year_teacher
      ~correct_email
      ?firstname ?lastname 
      ?academicyear ?attributionyear
      ?title 
      ?output_repository ?prefix ~file_name:(file_name "_par_enseignant" "tex")
        state
    in
    let state =
      Latex_engine.latex_opt_to_pdf state ~input
    in
   (* let state,_ =
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
    in*)
    state
  end


module ReportListRepartition =
  Build
    (struct
      module Missing_entry = Remanent_state.Collector_charges 
      let default_file_name = "pedagogical_charges.html"
     end)
