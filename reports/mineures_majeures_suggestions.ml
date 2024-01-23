type dump =
  Gen.dump

module type ReportMineuresMajeures =
sig
  val dump: Gen.dump
end


module Build
  (I:Gen.Interface
    with type Missing_entry.entry = Public_data.mineure_majeure
    and type Missing_entry.collector = Public_data.mineure_majeure list)
  =
    struct

  let headers =
    []

  let columns =
    [
      ["NOM"],
      (fun a -> a.Public_data.secondary_student_lastname);
      ["PRENOM"],
      (fun a -> a.Public_data.secondary_student_firstname);
      ["PROMO"],
      (fun a -> a.Public_data.secondary_student_promo);
      ["DPT"],
      (fun a -> Public_data.string_of_dpt (a.Public_data.secondary_dpt));
      ["ANNEE"],
      (fun a -> a.Public_data.secondary_diplomation_year);
      ["ACCEPTE"],
      (fun a ->
          match a.Public_data.secondary_accepted with
          | None -> ""
          | Some true -> "O"
          | Some false -> "N")
    ]


  let dump_suggestion
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_mineures_suggestions
    in
    let filter = Gen.filter_mineures_majeures in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    let get = I.Missing_entry.get in
    Gen.dump_elts
      ?output_repository ?prefix ?file_name ?event_opt
      ~filter ~cmp ~headers ~columns ~get
      ~default_file_name ~get_repository
      state

  let dump
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp
          (fun a -> a.Public_data.secondary_student_lastname);
        Gen.lift_cmp
            (fun a -> a.Public_data.secondary_student_firstname);
        Gen.lift_cmp
            (fun a -> a.Public_data.secondary_dpt);
        Gen.lift_cmp
                (fun a -> a.Public_data.secondary_student_promo);
        Gen.lift_cmp
                (fun a -> a.Public_data.secondary_diplomation_year)

      ]
    in
    dump_suggestion
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module SuggestionsMineures =
  Build
    (struct
        let default_file_name = "suggestions_mineures.csv"
        module Missing_entry = Remanent_state.Dens_candidate_missing_minors
    end)

module SuggestionsMajeures =
  Build
    (struct
      let default_file_name = "suggestions_doubles_majeures.csv"
      module Missing_entry = Remanent_state.Dens_candidate_missing_majors
    end)
