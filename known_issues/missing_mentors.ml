let dump_missing_mentors
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository ?prefix ?file_name
    cmp headers columns state  =
  let event_opt =
    Some Profiling.Dump_missing_mentors
  in
  let filter ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo state _ =
    let _ = dpt, firstname, lastname, codegps, mentorname, academicyear, promo in
    state, true in
  let get = Remanent_state.get_missing_mentors in
  let default_file_name = "missing_mentors.html" in
  let get_repository = Remanent_state.get_repository_to_dump_missing_mentors
  in
  Gen.dump_elts
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository ?prefix ?file_name ?event_opt
    ~cmp ~filter ~headers ~columns ~get ~default_file_name
    ~get_repository
    state

let columns =
  [
    "NOM",(fun a -> a.Public_data.missing_mentor_lastname);
    "PRÉNOM",(fun a -> a.Public_data.missing_mentor_firstname);
    "PROMOTION",(fun a -> a.Public_data.missing_mentor_promotion);
    "ANNÉE ACADÉMIQUE",
    (fun a ->
       let year = a.Public_data.missing_mentor_year in
       try
         let year =
           int_of_string year
         in
         Printf.sprintf "%i -- %i" year (year+1)
       with
         _ -> year)
  ]

let dump_per_student
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_lastname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_firstname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_promotion);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_year)
    ]
  in
  let headers =
    []
  in
  dump_missing_mentors
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository ?prefix ?file_name cmp headers columns state

let dump_per_promotion
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_promotion);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_lastname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_firstname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_year)
    ]
  in
  let headers =
    []
  in
  dump_missing_mentors
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository ?prefix ?file_name cmp headers columns state

let dump_per_year
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_year);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_lastname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_firstname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_mentor_promotion)
    ]
  in
  let headers =
    []
  in
  dump_missing_mentors
    ?dpt ?firstname ?lastname ?codegps ?mentorname ?academicyear ?promo
    ?output_repository
    ?prefix
    ?file_name
    cmp headers columns state
