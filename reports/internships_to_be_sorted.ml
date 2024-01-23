type dump =
  Gen.dump

module type ReportInternshipsToBeSorted =
sig
  val dump: Gen.dump
end


module Build
(I:Gen.Interface
  with type Missing_entry.entry = Public_data.stage_a_trier
  and type Missing_entry.collector = Public_data.stage_a_trier list)
   =
  struct


  let headers =
    []


  let columns =
    [
      ["NOM"],
      (fun a -> a.Public_data.stageat_nom);
      ["PRENOM"],
      (fun a -> a.Public_data.stageat_prenom);
      ["ANNEE DE VALIDATION"],
      (fun a -> a.Public_data.stageat_annee);
      ["SUJET(gps)"], (fun a -> a.Public_data.stageat_libelle);
      ["SUJET(français)"], (fun a -> a.Public_data.stageat_libelle_fr);
      ["SUJET(anglais)"], (fun a -> a.Public_data.stageat_libelle_en);
      ["ACTIVITE(francais)"],   (fun a ->
                                  match a.Public_data.stageat_activite_fr with None -> "" | Some a -> a);
      ["ACTIVITE(anglais)"],   (fun a ->
                                  match a.Public_data.stageat_activite_en with
                                  None -> "" | Some a -> a);

      ["EXPERIENCE"],  (fun a ->
                          match a.Public_data.stageat_type with None -> "" | Some a -> Public_data.string_of_experience a)

    ]


  let dump_internships_to_be_sorted
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_internships_to_be_sorted
    in
    let filter = Gen.filter_stageat in
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
          (fun a -> a.Public_data.stageat_nom);
        Gen.lift_cmp
            (fun a -> a.Public_data.stageat_prenom);
        Gen.lift_cmp
          (fun a -> a.Public_data.stageat_annee);
        Gen.lift_cmp
          (fun a -> a.Public_data.stageat_libelle);
  ]
    in
    dump_internships_to_be_sorted
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module InternshipsToBeSorted =
  Build
    (struct
      let default_file_name = "stages_a_trier.csv"
      module Missing_entry = Remanent_state.Internships_to_be_sorted
    end)
