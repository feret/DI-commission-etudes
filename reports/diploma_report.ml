type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?recu:bool ->
  ?academicyear:string ->
  Gen.dump

module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
end

module Build
    (I:Gen.Interface
     with type elt = Public_data.diplome_national) =
struct

  let dump_national_diploma_list
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_national_diploma_list
    in
    let filter = Gen.filter_national_diploma in
    let get = I.get in
    let default_file_name = I.default_file_name in
    let get_repository = I.get_repository in
    Gen.dump_elts
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?output_repository ?prefix ?file_name ?event_opt
      ~cmp ~filter ~headers ~columns ~get ~default_file_name
      ~get_repository
      state


  let nom_etudiant =
    "NOM",
    (fun a -> a.Public_data.diplome_lastname)
  let prenom_etudiant =
    "PRENOM",
    (fun a -> a.Public_data.diplome_firstname)
  let promotion =
    "PROMOTION",
    (fun a -> a.Public_data.diplome_promotion
    )
  let moyenne =
    "Moyenne",
    (fun a ->
       match a.Public_data.diplome_moyenne with
       | None -> ""
       | Some f ->
         Notes.float_to_string_easy f)
  let mention =
    "Mention",
    (fun a ->
       match a.Public_data.diplome_mention with
       | None -> ""
       | Some a -> a)
  let resultat =
    "Résultat",
    (fun a ->
       if a.Public_data.diplome_recu then "Reçu" else "Ajourné(e)"
    )
  let departement =
    "Département",
    (fun a -> a.Public_data.diplome_dpt)
  let level =
    "Niveau",
    (fun a -> a.Public_data.diplome_niveau)
  let ects =
    "Nbects",(fun a -> string_of_float (a.Public_data.diplome_nb_ects))
  let year =
    "Annee",(fun a -> a.Public_data.diplome_year)

  let lift_id (a,b) = (a,(fun x -> x),b)

  let dump_per_result_per_student
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> if a.Public_data.diplome_recu then "0" else "1");
        Gen.lift_cmp (fun a -> a.Public_data.diplome_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;moyenne; mention] in
    let headers =
      [
        lift_id resultat;
      ]
    in
    dump_national_diploma_list
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?output_repository ?prefix ?file_name cmp headers columns state


  end


module DiplomaReport =
  Build
    (struct
      type elt = Public_data.diplome_national

      let default_file_name = "diploma.html"
      let get = Remanent_state.get_national_diplomas
      let get_repository =
        Remanent_state.get_repository_to_dump_national_diplomas
    end)
