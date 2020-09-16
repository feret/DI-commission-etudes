type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

type 'elt filter =
  ?dpt:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?teachername:string ->
  ?academicyear:string ->
  ?promo:string ->
  ?ninscription:int ->
  Remanent_state.t -> 'elt -> Remanent_state.t * bool

let dump_elts
    ?dpt
    ?firstname
    ?lastname
    ?codegps
    ?mentorname
    ?mentorfirstname
    ?mentorlastname
    ?teachername
    ?academicyear
    ?promo
    ?ninscription
    ?output_repository ?prefix ?file_name
    ?event_opt
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
           ?dpt
           ?firstname
           ?lastname
           ?codegps
           ?mentorname
           ?mentorfirstname
           ?mentorlastname
           ?teachername
           ?academicyear
           ?promo
           ?ninscription
           state elt in
         if b then
           state, elt::l
         else
           state, l
      )
      (state,[]) (List.rev elts)
  in
  match filtered_elts with
  | [] -> state
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
      | Some rep -> state, rep
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
    let state, output_channel_opt =
      try
        state, Some (open_out file)
      with _ ->
        let () =
          Format.printf
            "Cannot open file %s@."
            file
        in
        Remanent_state.warn
          __POS__
          (Format.sprintf "Cannot open file %s"  file)
          Exit
          state ,
        None
    in
    let state =
      match output_channel_opt with
      | None -> state
      | Some out ->
        let mode = Loggers.HTML in
        let logger = Loggers.open_logger_from_channel ~mode
            out in
        let extended_elts =
          Tools.prepare_report
            ~cmp
            ~headers:(List.rev_map (fun (_,_,a) -> a)
                        (List.rev headers))
            elts
        in
        let () =
          Tools.dump_report
            ~print_header:(Loggers.print_headers logger)
            ~open_row:(fun () -> Loggers.open_row logger)
            ~close_row:(fun () -> Loggers.close_row logger)
            ~print_cell:(Loggers.print_cell logger)
            ~close_array:(fun () -> Loggers.close_array logger)
            ~string_of_headers:(List.rev_map (fun (a,b,_) ->
                a,b) (List.rev headers))
            ~string_of_column:columns
            ~open_array:(Loggers.open_array logger)
            extended_elts in
        let () = Loggers.flush_logger logger in
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
    state

let lift_cmp f a b =
  compare (f a) (f b)
let op_cmp cmp a b = cmp b a

let check elt_opt elt =
  match elt_opt with
  | None -> true
  | Some elt' -> elt = elt'

let filter_grade
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?promo ?ninscription
    state grade =
  let _  =
    mentorname, mentorfirstname, mentorlastname, ninscription
  in
  state, check dpt grade.Public_data.missing_grade_dpt
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
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname  ?teachername ?academicyear ?promo ?ninscription
    state internship =
  let _ =
    dpt, mentorname, teachername, mentorfirstname, mentorlastname, ninscription
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
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?promo ?ninscription
    state mentoring =
  let _ =
    dpt, mentorname, mentorfirstname, mentorlastname, teachername, codegps, ninscription
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
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?promo ?ninscription
    state mentoring =
  let _ =
    teachername, mentorname, codegps, dpt, ninscription
  in
  state,
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

let filter_dens
      ?dpt ?firstname ?lastname ?codegps ?mentorname ?mentorfirstname ?mentorlastname ?teachername ?academicyear ?promo ?ninscription
      state dens =
  let _ =
    dpt, codegps, mentorname, mentorfirstname, mentorlastname, teachername, academicyear
  in
  state,
  check firstname dens.Public_data.dens_firstname
  &&
  check lastname dens.Public_data.dens_lastname
  &&
  check promo dens.Public_data.dens_promotion
  &&
  check ninscription dens.Public_data.dens_nb_inscriptions

module type Interface =
sig
  type elt
  val default_file_name: string
  val get:(Remanent_state.t -> Remanent_state.t * elt list)
  val get_repository:(Remanent_state.t -> Remanent_state.t * string)
end
