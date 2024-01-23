type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?mentorname:string ->
  ?academicyear:string ->
  ?promo:string ->
  Gen.dump

module type ReportMentors =
sig
  val dump_per_year: dump
  val dump_per_promotion: dump
  val dump_per_student: dump
end

module Build
  (I:Gen.Interface
    with type Missing_entry.entry = Public_data.missing_mentor
    and type Missing_entry.collector = Public_data.missing_mentor list) =
struct

  let dump_missing_mentors
    ?firstname ?lastname ?mentorname ?academicyear ?promo
    ?output_repository ?prefix ?file_name
    cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_missing_mentors
    in
    let filter = Gen.filter_mentoring in
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    Gen.dump_elts
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state

  let columns =
    [
      ["NOM"],(fun a -> a.Public_data.missing_mentor_lastname);
      ["PRÉNOM"],(fun a -> a.Public_data.missing_mentor_firstname);
      ["PROMOTION"],(fun a -> a.Public_data.missing_mentor_promotion);
      ["ANNÉE ACADÉMIQUE"],
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
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name
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
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_promotion
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name
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
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_year
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name
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
      ?firstname ?lastname ?mentorname ?academicyear ?promo
      ?output_repository ?prefix ?file_name
      cmp headers columns state
  end

module ReportMissingMentors =
  Build
    (struct
  (*    type elt = Public_data.missing_mentor*)
      module Missing_entry = Remanent_state.Missing_mentors
      let default_file_name = "missing_mentoring.html"
(*      let get = Remanent_state.get_missing_mentors
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_mentors*)
    end)
