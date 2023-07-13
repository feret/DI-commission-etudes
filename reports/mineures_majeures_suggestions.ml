type dump =
  Gen.dump

module type ReportMineuresMajeures =
sig
  val dump: Gen.dump
end


module Build
    (I:Gen.Interface with type elt = Public_data.mineure_majeure) =
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
      (fun a -> a.Public_data.secondary_dpt);
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
    let get_repository = I.get_repository in
    let get = I.get in
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

      ]
    in
    dump_suggestion
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module SuggestionsMineures =
  Build
    (struct
      type elt = Public_data.mineure_majeure

      let default_file_name = "suggestions_mineures.csv"
      let get = Remanent_state.get_minor_suggestion_list
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_minor_major
    end)

module SuggestionsMajeures =
  Build
    (struct
      type elt = Public_data.mineure_majeure

      let default_file_name = "suggestions_doubles_majeures.csv"
      let get = Remanent_state.get_major_suggestion_list
      let get_repository =
        Remanent_state.get_repository_to_dump_missing_minor_major
    end)
