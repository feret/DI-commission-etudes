type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) option

type 'elt filter =
  ?commission:bool ->
  ?dpt:Public_data.main_dpt ->
  ?universite:Public_data.universite ->
  ?dpt_gps_code:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?attributionyear:string ->
  ?promo:string ->
  ?ninscription:int ->
  ?niveau:string ->
  ?recu:bool ->
  Remanent_state.t -> 'elt -> Remanent_state.t * bool

let dump_elts
    ?commission
    ?dpt
    ?universite
    ?dpt_gps_code
    ?firstname
    ?lastname
    ?codegps
    ?mentorname
    ?mentorfirstname
    ?mentorlastname
    ?teachername
    ?academicyear
    ?attributionyear
    ?promo
    ?ninscription
    ?niveau
    ?recu
    ?output_repository ?prefix ?file_name
    ?event_opt
    ?headpage
    ?footpage
    ?title
    ?preamble
    ?signature
    ?headerextralength:(headerextralength=0)
    ?headcolor
    ?footcolor
    ~get ~filter ~get_repository ~default_file_name
    ~cmp ~headers ~columns state  =
  let state =
    Remanent_state.open_event_opt event_opt state
  in
  let state, elts =
    get state
  in
  let state, filtered_elts =
    List.fold_left
      (fun (state, l) elt ->
         let state, b =
           filter
             ?commission
             ?dpt
             ?universite
             ?dpt_gps_code
             ?firstname
             ?lastname
             ?codegps
             ?mentorname
             ?mentorfirstname
             ?mentorlastname
             ?teachername
             ?academicyear
             ?attributionyear
             ?promo
             ?ninscription
             ?niveau
             ?recu
             state elt
         in
         if b then
           state, elt::l
         else
           state, l
      )
      (state,[]) (List.rev elts)
  in
  match filtered_elts with
  | [] -> state, None
  | _ ->
    let state, prefix =
      match prefix with
      | None -> state, ""
      | Some prefix -> state, prefix
    in
    let state, output_repository =
      match output_repository with
      | None ->
        get_repository state
      | Some rep ->
          state, rep
    in
    let state, output_file_name =
      match file_name with
      | None -> state, default_file_name
      | Some file_name -> state, file_name
    in
    let output_repository =
      match output_repository,prefix  with
      | ".",prefix | "",prefix -> prefix
      | x,"" -> x
      | x1,x2 ->
        Printf.sprintf "%s/%s" x1 x2
    in
    let state, output_repository =
      Safe_sys.rec_mk_when_necessary
        __POS__
        state output_repository
    in
    let file =
      if output_repository = ""
      then output_file_name
      else
        Printf.sprintf "%s/%s"
          output_repository output_file_name
    in
    let extension_opt =
      Safe_sys.get_extension output_file_name
    in
    let state, output_channel_opt =
      try
        state, Some (open_out file)
      with exn ->
        let msg = Printexc.to_string exn in
        let () =
          Format.printf
            "Cannot open file %s (%s)@."
            file
            msg
        in
        Remanent_state.warn
          __POS__
          (Format.sprintf "Cannot open file %s (%s)"  file msg)
          Exit
          state,
        None
    in
    let state =
      match output_channel_opt with
      | None -> state
      | Some out ->
        let state, mode =
          match extension_opt with
          | Some "html" -> state, Loggers.HTML
          | Some "tex" -> state, Loggers.Latex Loggers.latex_normal
          | Some "csv" -> state, Loggers.CSV
          | Some _ ->
            Remanent_state.warn
              __POS__
              (Printf.sprintf
                 "Extension of file %s is invalid"
                 output_file_name)
              Exit
              state, Loggers.HTML
          | None ->
          Remanent_state.warn
            __POS__
            (Printf.sprintf
               "File %s has no extension"
               output_file_name)
            Exit
            state, Loggers.HTML
        in
        let logger =
          Loggers.open_logger_from_channel
            ~mode  ~headerextralength out
        in
        let logger = Loggers.with_lines logger in
        let extended_elts =
          Tools.prepare_report
            ~cmp
            ~headers:(List.rev_map (fun (_,_,a) -> a)
                        (List.rev headers))
            filtered_elts
        in
        let headpage =
          match headpage with None -> (fun _ -> [])
                            | Some a -> a
        in
        let () =
          Tools.dump_report
            ~print_header:(Loggers.print_headers logger)
            ~open_row:(fun () -> Loggers.open_row logger)
            ~close_row:(fun () -> Loggers.close_row logger)
            ~print_cell:(Loggers.print_cell logger)
            ~close_array:(fun () -> Loggers.close_array logger)
            ~string_of_headers:(List.rev_map (fun (a,b,_) ->
                (a:string list),b) (List.rev headers))
            ~string_of_column:columns
            ~settitle:(Loggers.maketitle logger)
            ~setheadpage:(fun ?color s ->
                Loggers.setheadpage logger ?color s)
            ~setfootpage:(Loggers.setfootpage logger)
            ~setsignature:(Loggers.setsignature logger)
            ~setpreamble:(Loggers.setpreamble logger)
            ~open_array:(Loggers.open_array logger)
            ?title
            ~headpage
            ?footpage
            ?headcolor
            ?footcolor
            ?preamble
            ?signature
            extended_elts in
        let () = Loggers.flush_logger logger in
        let () = Loggers.close_logger logger in
        state
    in
    let () =
      match output_channel_opt with
      | Some chan -> close_out chan
      | None -> ()
    in
    let state =
      Remanent_state.close_event_opt event_opt state
    in
    state, Some (output_repository,output_file_name)

let lift_cmp f a b =
  compare (f a) (f b)
let op_cmp cmp a b = cmp b a

let check elt_opt elt =
  match elt_opt with
  | None -> true
  | Some elt' ->
    elt = elt'

let filter_grade
    ?commission
    ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau ?recu state grade =
  let _  =
    commission, dpt, niveau, recu, mentorname, mentorfirstname, mentorlastname, ninscription, attributionyear, universite
  in
  state, check dpt_gps_code grade.Public_data.missing_grade_dpt
  &&
  check firstname grade.Public_data.missing_grade_firstname
  &&
  check lastname grade.Public_data.missing_grade_lastname
  &&
  check codegps grade.Public_data.missing_grade_code_gps
  &&
  check teachername grade.Public_data.missing_grade_teacher
  &&
  check academicyear grade.Public_data.missing_grade_year
  &&
  check promo grade.Public_data.missing_grade_promotion

let filter_internship_description
    ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname  ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau ?recu state internship =
  let _ =
    commission, dpt, universite, dpt_gps_code, mentorname, teachername, mentorfirstname, mentorlastname, ninscription, niveau, recu, attributionyear
  in
  state,
  check
    firstname internship.Public_data.missing_internship_firstname
  &&
  check
    lastname internship.Public_data.missing_internship_lastname
  &&
  check codegps internship.Public_data.missing_internship_code_gps
  &&
  check academicyear internship.Public_data.missing_internship_year
  &&
  check promo internship.Public_data.missing_internship_promotion

let filter_mentoring
    ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau
    ?recu state mentoring =
  let _ =
    commission, dpt, universite, dpt_gps_code, mentorname, mentorfirstname, mentorlastname, teachername, codegps, ninscription, niveau, recu, attributionyear
  in
  state,
  check firstname mentoring.Public_data.missing_mentor_firstname
  &&
  check lastname mentoring.Public_data.missing_mentor_lastname
  &&
  check academicyear mentoring.Public_data.missing_mentor_year
  &&
  check promo mentoring.Public_data.missing_mentor_promotion

let filter_mentoring_list
    ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau
    ?recu state mentoring =
  let _ =
    commission, universite, dpt_gps_code, teachername, mentorname, codegps, ninscription, niveau, recu
  in
  state,
  (check dpt
     mentoring.Public_data.mentor_student_dpt
   || check (Some dpt) mentoring.Public_data.mentor_secondary)
  &&
  check firstname mentoring.Public_data.mentor_student_firstname
  &&
  check lastname mentoring.Public_data.mentor_student_lastname
  &&
  check academicyear mentoring.Public_data.mentor_academic_year
  &&
  check promo mentoring.Public_data.mentor_student_promo
  &&
  check mentorfirstname mentoring.Public_data.mentor_firstname
  &&
  check mentorlastname mentoring.Public_data.mentor_lastname
  &&
  check attributionyear
    mentoring.Public_data.mentor_attribution_year

let filter_dens
    ?nb_inscription_list
    ?commission
    ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear
    ?promo ?ninscription ?niveau
    ?recu
    state dens =
  let _ =
    commission, universite, dpt, dpt_gps_code, codegps, mentorname, mentorfirstname, mentorlastname, teachername, academicyear, niveau, recu, attributionyear
  in
  state,
  begin
    match nb_inscription_list with
    | None -> true
    | Some l ->
      List.mem
        dens.Public_data.dens_nb_inscriptions
        l
  end
  &&

  check firstname dens.Public_data.dens_firstname
  &&
  check lastname dens.Public_data.dens_lastname
  &&
  check promo dens.Public_data.dens_promotion
  &&
  check ninscription dens.Public_data.dens_nb_inscriptions

  let filter_national_diploma
        ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
        ?niveau
        ?recu state dens =
    let _ =
      dpt, dpt_gps_code, codegps, mentorname, mentorfirstname, mentorlastname, teachername, academicyear, ninscription, attributionyear
    in
    state,
    check commission dens.Public_data.diplome_commission
    &&
    check firstname dens.Public_data.diplome_firstname
    &&
    check lastname dens.Public_data.diplome_lastname
    &&
    check promo dens.Public_data.diplome_promotion
    &&
    check dpt dens.Public_data.diplome_dpt
    &&
    check universite dens.Public_data.diplome_univ_key
    &&
    check niveau dens.Public_data.diplome_niveau
    &&
    check recu dens.Public_data.diplome_recu
    &&
    check academicyear dens.Public_data.diplome_year

let filter_student_list
    ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau
    ?recu state student =
  let _ =
    commission, dpt, universite, dpt_gps_code, niveau, recu, academicyear, codegps, mentorname, mentorfirstname, mentorlastname, teachername, academicyear, ninscription, attributionyear
  in
  state,
  check firstname student.Public_data.student_firstname_report
  &&
  check lastname student.Public_data.student_lastname_report
  &&
  check promo student.Public_data.student_promo_report

let filter_course_name_translation
    ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
    ?niveau
    ?recu state course =
  let _ =
    commission, dpt, universite, dpt_gps_code, niveau, recu, mentorname, mentorfirstname, mentorlastname, teachername, ninscription, attributionyear, promo, lastname, firstname
  in
  state,
  check codegps course.Public_data.code
  &&
  check academicyear course.Public_data.year

  let filter_course_entry
      ?commission ?dpt ?universite ?dpt_gps_code ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?attributionyear ?promo ?ninscription
      ?niveau
      ?recu state course =
    let _ =
      commission, dpt, universite, dpt_gps_code, niveau, recu, mentorname, mentorfirstname, mentorlastname, teachername, ninscription, attributionyear, promo, lastname, firstname, course, academicyear, codegps
    in
    state, true

let filter
    ?commission
    ?dpt ?universite
    ?dpt_gps_code
?firstname
?lastname
?codegps
?mentorname
?mentorfirstname
?mentorlastname
?teachername
?academicyear
?attributionyear
?promo
?ninscription
?niveau
?recu p state list =
  List.fold_left
    (fun (state,list) elt ->
       let state,b =
         p
           ?commission
           ?dpt
           ?universite
           ?dpt_gps_code
           ?firstname
           ?lastname
           ?codegps
           ?mentorname
           ?mentorfirstname
           ?mentorlastname
           ?teachername
           ?academicyear
           ?attributionyear
           ?promo
           ?ninscription
           ?niveau
           ?recu
           state elt in
      if b then
        state, elt::list
      else
        state, list)
    (state,[]) (List.rev list)

module type Interface =
sig
  type elt
  val default_file_name: string
  val get:(Remanent_state.t -> Remanent_state.t * elt list)
  val get_repository:(Remanent_state.t -> Remanent_state.t * string)
end
