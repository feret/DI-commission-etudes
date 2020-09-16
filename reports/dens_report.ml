type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int ->
  ?promo:string ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
end

module Build
    (I:Gen.Interface
     with type elt = Public_data.dens) =
struct

  let dump_dens
    ?firstname
    ?lastname
    ?promo
    ?ninscription
    ?output_repository ?prefix ?file_name
    cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_dens_result
    in
    let filter = Gen.filter_dens in
    let get = I.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.get_repository in
    Gen.dump_elts
      ?firstname ?lastname ?ninscription ?promo
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state


  let nom_etudiant =
    "NOM",
    (fun a ->
       let () =
                Format.printf "%s," a.Public_data.dens_lastname
       in
       a.Public_data.dens_lastname)
  let prenom_etudiant =
    "PRENOM",
    (fun a ->
      let () =
        Format.printf "%s," a.Public_data.dens_firstname
      in a.Public_data.dens_firstname)
  let promotion =
    "PROMOTION",
    (fun a ->
       let () =
         Format.printf "%s," a.Public_data.dens_promotion
       in
       a.Public_data.dens_promotion
    )
  let total_year =
    "ECTS (année courante)",
    (fun a -> Notes.string_of_ects
        (Some (a.Public_data.dens_current_year_ects)))
  let total =
    "ECTC (cumul)",
    (fun a ->
      let () =
        Format.printf "%f," a.Public_data.dens_total_ects
      in
       Notes.string_of_ects
        (Some (a.Public_data.dens_total_ects)))

  let inscriptions =
    "Inscriptions au DENS",
    (fun a ->
       let () =
         Format.printf "%i" a.Public_data.dens_nb_inscriptions
       in
       string_of_int (a.Public_data.dens_nb_inscriptions))

  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_promo
      ?firstname ?lastname ?ninscription ?promo
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.dens_promotion );
        Gen.lift_cmp (fun a -> a.Public_data.dens_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.dens_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;inscriptions; total_year; total; promotion ] in
    let headers =
      [
        lift_id promotion ;
      ]
    in
    dump_dens
      ?firstname ?lastname ?ninscription ?promo
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_n_inscription
      ?firstname ?lastname ?ninscription ?promo
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.dens_nb_inscriptions);
        Gen.lift_cmp (fun a -> a.Public_data.dens_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.dens_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant; promotion; total_year; total;inscriptions ] in
    let headers =
      [
        lift_id inscriptions ;
      ]
    in
    let state =
      dump_dens
      ?firstname ?lastname ?ninscription ?promo
      ?output_repository ?prefix ?file_name cmp headers columns state
    in
    let _ = Format.print_flush () in
    let _ = Format.print_newline () in
    state 
  end


module DensReport =
  Build
    (struct
      type elt = Public_data.dens

      let default_file_name = "dens.html"
      let get = Remanent_state.get_dens
      let get_repository =
        Remanent_state.get_repository_to_dump_dens
    end)
