type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?ninscription:int ->
  ?promo:string ->
  ?headpage:
    (int -> ((Loggers.t ->
              (string -> unit, Format.formatter, unit) format ->
              string -> unit) *
             string)
       list ) ->
  ?footpage:((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list ->
  ?footcolor:Color.color ->
  ?title:((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list  ->
  ?preamble:(int -> ((Loggers.t ->
                      (string -> unit, Format.formatter, unit) format ->
                      string -> unit) *
                     string)
               list ) ->
  ?signature:(int -> ((Loggers.t ->
                       (string -> unit, Format.formatter, unit) format ->
                       string -> unit) *
                      string)
                list ) ->
  ?nb_inscription_list:int list
  ->
  Gen.dump

module type DensReport =
sig
  val dump_per_promo: dump
  val dump_per_n_inscription: dump
  val dump_per_alphabetic_order: dump
end

module Build
  (I:Gen.Interface
      with type Missing_entry.entry = Public_data.dens
      and type Missing_entry.collector = Public_data.dens list) =
  struct

  let dump_dens
    ?firstname
    ?lastname
    ?promo
    ?ninscription
    ?nb_inscription_list
    ?headpage ?footpage ?footcolor
    ?title ?preamble ?signature
    ?output_repository ?prefix ?file_name
    cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_dens_result
    in
    let filter = Gen.filter_dens ?nb_inscription_list in
    let get = I.Missing_entry.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    Gen.dump_elts
      ?firstname ?lastname ?ninscription ?promo
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      ~headerextralength:5
      state


  let nom_etudiant =
    ["NOM"],
    (fun a -> a.Public_data.dens_lastname)
  let prenom_etudiant =
    ["PRENOM"],
    (fun a -> a.Public_data.dens_firstname)
  let promotion =
    ["PROMOTION"],
    (fun a -> a.Public_data.dens_promotion)
  let full =
    ["ÉTUDIANT"],
    (fun a ->
       Printf.sprintf "%s %s (%s)"
         a.Public_data.dens_lastname
         a.Public_data.dens_firstname
         a.Public_data.dens_promotion
    )
  let total_year =
    ["ECTS";"(année courante)"],
    (fun a ->
       Notes.string_of_ects
        (Some (a.Public_data.dens_current_year_ects)))
  let _ = total_year
  let total =
    ["ECTS";"(cumul)"],
    (fun a ->
       Notes.string_of_ects
         (Some (a.Public_data.dens_total_ects)))
  let total_bis =
    ["ECTS"],
    (fun a ->
       Notes.string_of_ects
         (Some (a.Public_data.dens_total_ects)))

  let inscriptions =
    ["Inscriptions";"au DENS"],
    (fun a ->
       string_of_int (a.Public_data.dens_nb_inscriptions))

  let mandatory_course =
    ["Cours";"obligatoires"],
    (fun a ->
       string_of_int (a.Public_data.dens_nb_mandatory_course))

  let math_info_course =
    ["Cours de maths";"ou maths-info"],
    (fun a ->
       string_of_int (a.Public_data.dens_nb_math_and_math_info_course))

  let math_course =
    ["Cours";"de maths"],
    (fun a ->
       string_of_int (a.Public_data.dens_nb_math_course))

  let _ = math_course, math_info_course

  let math_math_info_course =
    ["Cours";"maths/maths-info"],
    (fun a ->
       Printf.sprintf
         "%i/%i"
         a.Public_data.dens_nb_math_course
         (a.Public_data.dens_nb_math_and_math_info_course-a.Public_data.dens_nb_math_course))


  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_alphabetic_order
      ?firstname ?lastname ?ninscription ?promo
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature ?nb_inscription_list
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.dens_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.dens_firstname) ;
        Gen.lift_cmp (fun a -> a.Public_data.dens_promotion );
      ]
    in
    let state, main_dpt =
      Remanent_state.get_main_dpt state
    in
    let columns =
      match
        main_dpt
      with
      | Public_data.CIENS 
      | Public_data.ENS | Public_data.PHYS | Public_data.CHIMIE
      | Public_data.IBENS | Public_data.DMA | Public_data.GEOSCIENCES
      | Public_data.ECO | Public_data.DRI | Public_data.ARTS
      | Public_data.LILA | Public_data.DEC
      | Public_data.DSA | Public_data.DSS
      | Public_data.GEOG | Public_data.HIST
      | Public_data.ECLA
        ->
        [prenom_etudiant;nom_etudiant;promotion;inscriptions; total ]
      | Public_data.DI ->
        [full;inscriptions;  mandatory_course ; math_math_info_course ; total_bis]
    in
    let headers =
      [
      ]
    in
    dump_dens
      ?firstname ?lastname ?ninscription ?promo
      ?nb_inscription_list
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_promo
      ?firstname ?lastname ?ninscription ?promo
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature ?nb_inscription_list
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.dens_promotion );
        Gen.lift_cmp (fun a -> a.Public_data.dens_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.dens_firstname) ;
      ]
    in
    let state, main_dpt =
      Remanent_state.get_main_dpt state
    in
    let columns =
      match
        main_dpt
      with
      | Public_data.CIENS 
      | Public_data.ENS | Public_data.PHYS | Public_data.CHIMIE
      | Public_data.IBENS | Public_data.DMA | Public_data.GEOSCIENCES
      | Public_data.LILA | Public_data.DEC
      | Public_data.ECO | Public_data.DRI | Public_data.ARTS
      | Public_data.DSA | Public_data.DSS
      | Public_data.GEOG | Public_data.HIST | Public_data.ECLA
        ->
        [prenom_etudiant;nom_etudiant;inscriptions;  total ]
      | Public_data.DI ->
        [prenom_etudiant;nom_etudiant;inscriptions; mandatory_course ; math_math_info_course ; total_bis ]
    in
    let headers =
      [
        lift_id promotion ;
      ]
    in
    dump_dens
      ?firstname ?lastname ?ninscription ?promo
      ?nb_inscription_list
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_n_inscription
      ?firstname ?lastname ?ninscription ?promo
      ?headpage ?footpage ?footcolor
      ?title ?preamble ?signature ?nb_inscription_list
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.dens_nb_inscriptions);
        Gen.lift_cmp (fun a -> a.Public_data.dens_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.dens_firstname) ;
      ]
    in
    let state, main_dpt =
      Remanent_state.get_main_dpt state
    in
    let columns =
      match
        main_dpt
      with
      | Public_data.CIENS 
      | Public_data.ENS | Public_data.PHYS | Public_data.CHIMIE
      | Public_data.IBENS | Public_data.DMA | Public_data.GEOSCIENCES
      | Public_data.LILA | Public_data.DEC
      | Public_data.ECO | Public_data.DRI | Public_data.ARTS
      | Public_data.DSA | Public_data.DSS
      | Public_data.GEOG | Public_data.HIST | Public_data.ECLA
        ->
        [prenom_etudiant;nom_etudiant;promotion; total ]
      | Public_data.DI ->
        [full; mandatory_course ; math_math_info_course ;  total_bis ]
    in
    let headers =
      [
        lift_id inscriptions ;
      ]
    in
    let state =
      dump_dens
        ?firstname ?lastname ?ninscription ?promo
        ?headpage ?footpage ?footcolor
        ?title ?preamble ?signature
        ?output_repository ?prefix ?file_name
        ?nb_inscription_list
        cmp headers columns state
    in
    let _ = Format.print_flush () in
    let _ = Format.print_newline () in
    state
  end


module DensReport =
  Build
    (struct
      let default_file_name = "dens.html"

      module Missing_entry = Remanent_state.Collector_dens_diplomas
    end)
