type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportInternshipDescriptions =
sig
  val dump_per_year: dump
  val dump_per_promotion: dump
  val dump_per_student: dump
end

module Build
     (I:Gen.Interface with type Missing_entry.entry = Public_data.missing_internship_description) =
  struct
    let dump_missing_internship_descriptions
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state  =
      let event_opt =
        Some Profiling.Dump_missing_internship_descriptions
      in
      let filter = Gen.filter_internship_description in
      let get = I.Missing_entry.get in
      let default_file_name = I.default_file_name in
      let get_repository = I.Missing_entry.get_repository in
      Gen.dump_elts
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository ?prefix ?file_name ?event_opt
        ~cmp ~filter ~headers ~columns ~get ~default_file_name
        ~get_repository
        state

    let columns =
      [
        ["PRÉNOM"],(fun a -> a.Public_data.missing_internship_firstname) ;
        ["NOM"],(fun a -> a.Public_data.missing_internship_lastname);
        ["PROMOTION"],(fun a -> a.Public_data.missing_internship_promotion);
        ["ANNÉE ACADÉMIQUE"],
        (fun a ->
           let year = a.Public_data.missing_internship_year in
           try
             let year =
               int_of_string year
             in
             Printf.sprintf "%i -- %i" year (year+1)
           with
             _ -> year);
        ["CODE GPS"],(fun a -> a.Public_data.missing_internship_code_gps);
        ["INTITULÉ"],(fun a -> a.Public_data.missing_internship_intitule);
      ]

    let dump_per_student
        ?firstname ?lastname ?codegps ?academicyear ?promo
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
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_promotion);
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_year);
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_intitule);
        ]
      in
      let headers =
        []
      in
      dump_missing_internship_descriptions
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state

    let dump_per_promotion
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository
        ?prefix
        ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_promotion);
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
      dump_missing_internship_descriptions
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository ?prefix ?file_name cmp headers columns state

    let dump_per_year
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository
        ?prefix
        ?file_name
        state =
      let cmp =
        [
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_year);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_promotion);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_lastname);
          Gen.lift_cmp
            (fun a -> a.Public_data.missing_internship_firstname);
          Gen.lift_cmp (fun a -> a.Public_data.missing_internship_intitule);
        ]  in
      let headers = [] in
      dump_missing_internship_descriptions
        ?firstname ?lastname ?codegps ?academicyear ?promo
        ?output_repository
        ?prefix
        ?file_name
        cmp headers columns state




  end

module MissingInternshipDescriptions =
  Build
    (struct
      let default_file_name = "missing_internship_description.html"
      module Missing_entry = Remanent_state.Missing_internship_descriptions
    end)

module AmbiguousInternshipDescriptions =
  Build
    (struct
      let default_file_name = "ambiguous_internship_descriptions.html"
      module Missing_entry = Remanent_state.Ambiguous_internship_descriptions
    end)

module NonValidatedInternships =
  Build
    (struct
      let default_file_name = "non_validated_internships.html"
      module Missing_entry = Remanent_state.Non_validated_internships
    end)
