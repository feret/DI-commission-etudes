type dump =
  Gen.dump

module type ReportCoursesToBeSorted =
sig
  val dump: Gen.dump
end


module Build
    (I:Gen.Interface with type elt = Public_data.cours_a_trier) =
struct


  let headers =
    []


  let columns =
    [
      ["NOM"],
      (fun a -> a.Public_data.coursat_nom);
      ["PRENOM"],
      (fun a -> a.Public_data.coursat_prenom);
      ["ANNEE DE VALIDATION"],
      (fun a -> a.Public_data.coursat_annee);
      ["CODE GPS"],
      (fun a -> a.Public_data.coursat_codegps);
      ["LIBELLE GPS"],
      (fun a -> a.Public_data.coursat_libelle);
      ["DPT"],
      (fun a ->
        match a.Public_data.coursat_dpt with
          | None -> ""
          | Some a -> Public_data.string_of_dpt a)
    ]


  let dump_courses_to_be_sorted
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_courses_to_be_sorted
    in
    let filter = Gen.filter_coursat in
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
          (fun a -> a.Public_data.coursat_nom);
        Gen.lift_cmp
            (fun a -> a.Public_data.coursat_prenom);
        Gen.lift_cmp
          (fun a -> a.Public_data.coursat_annee);
        Gen.lift_cmp
          (fun a -> a.Public_data.coursat_libelle);
  ]
    in
    dump_courses_to_be_sorted
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module CoursesToBeSorted =
  Build
    (struct
      type elt = Public_data.cours_a_trier

      let default_file_name = "cours_a_trier.csv"
      let get = Remanent_state.get_courses_to_be_sorted
      let get_repository =
        Remanent_state.get_courses_to_be_sorted_list_repository
    end)
