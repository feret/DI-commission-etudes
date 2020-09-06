let dump_ambiguous_internship_descriptions
    ?output_repository ?prefix ?file_name cmp headers columns state  =
  let event_opt =
    Some Profiling.Dump_ambiguous_internship_descriptions
  in
  let get = Remanent_state.get_ambiguous_internship_descriptions in
  let default_file_name = "ambiguous_internship_descriptions.html" in
  let get_repository = Remanent_state.get_repository_to_dump_ambiguous_internship_descriptions
  in
  Gen.dump_elts
    ?output_repository ?prefix ?file_name ?event_opt
    ~cmp ~headers ~columns ~get ~default_file_name
    ~get_repository
    state

let columns =
  [
    "PRÉNOM",(fun a -> a.Public_data.missing_internship_firstname) ;
    "NOM",(fun a -> a.Public_data.missing_internship_lastname);
    "PROMOTION",(fun a -> a.Public_data.missing_internship_promotion);
    "ANNÉE ACADÉMIQUE",
    (fun a ->
       let year = a.Public_data.missing_internship_year in
       try
         let year =
           int_of_string year
         in
         Printf.sprintf "%i -- %i" year (year+1)
       with
         _ -> year);
    "CODE GPS",(fun a -> a.Public_data.missing_internship_code_gps);
    "INTITULÉ",(fun a -> a.Public_data.missing_internship_intitule);
  ]

let dump_per_student
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp
        (fun a -> a.Public_data.missing_internship_lastname);
      Gen.lift_cmp
        (fun a -> a.Public_data.missing_internship_firstname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_internship_year);
      Gen.lift_cmp (fun a -> a.Public_data.missing_internship_intitule);
    ]
  in
  let headers =
    []
  in
  dump_ambiguous_internship_descriptions ?output_repository ?prefix ?file_name cmp headers columns state

let dump_per_year
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      Gen.lift_cmp (fun a -> a.Public_data.missing_internship_year);
      Gen.lift_cmp
        (fun a -> a.Public_data.missing_internship_lastname);
      Gen.lift_cmp
        (fun a -> a.Public_data.missing_internship_firstname);
      Gen.lift_cmp (fun a -> a.Public_data.missing_internship_intitule);
    ]  in
  let headers = [] in
  dump_ambiguous_internship_descriptions
    ?output_repository
    ?prefix
    ?file_name
    cmp headers columns state

let dump_per_promotion
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
        [
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_promotion);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_lastname);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_firstname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_year);
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_intitule);
        ]  in
      let headers = [] in
      dump_ambiguous_internship_descriptions
        ?output_repository
        ?prefix
        ?file_name
        cmp headers columns state
