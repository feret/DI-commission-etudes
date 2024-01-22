type dump =
  Gen.dump

module type ReportDensCandidates =
sig
  val dump: Gen.dump
end


module Build
   (I:Gen.Interface with type Missing_entry.entry = Public_data.dens_candidate) =
  struct


  let headers =
    []


  let columns =
    [
      ["NOM"],
      (fun a -> a.Public_data.dens_candidate_lastname);
      ["PRENOM"],
      (fun a -> a.Public_data.dens_candidate_firstname);
      ["PROMO"],
      (fun a -> a.Public_data.dens_candidate_promotion);
      ["DPT"],
      (fun a -> Public_data.string_of_dpt (a.Public_data.dens_candidate_main_dpt));
      ["ANNEE"],
      (fun a -> a.Public_data.dens_candidate_diplomation_year);
      ["ACCEPTE"],
      (fun a ->
          match a.Public_data.dens_candidate_ok with
          | None -> ""
          | Some true -> "O"
          | Some false -> "N");
       ["INE"],
          (fun a ->
              match a.Public_data.dens_candidate_ine with
              | None -> ""
              | Some i -> i );
              ["SAD"],
                 (fun a ->
                     match a.Public_data.dens_candidate_sad with
                     | None -> ""
                     | Some i -> Format.sprintf "%i" i) ;

    ]


  let dump_suggestion
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_dens_candidate_suggestions
    in
    let filter = Gen.filter_dens_candidate in
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
          (fun a -> a.Public_data.dens_candidate_lastname);
        Gen.lift_cmp
            (fun a -> a.Public_data.dens_candidate_firstname);
        Gen.lift_cmp
            (fun a -> a.Public_data.dens_candidate_main_dpt);
        Gen.lift_cmp
                (fun a -> a.Public_data.dens_candidate_promotion);
        Gen.lift_cmp
          (fun a -> a.Public_data.dens_candidate_diplomation_year);
      ]
    in
    dump_suggestion
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module SuggestionsDensCandidates =
  Build
    (struct
      let default_file_name = "suggestions_dens_candidates.csv"
      module Missing_entry =
        Remanent_state.Dens_candidate_suggestion
    end)
