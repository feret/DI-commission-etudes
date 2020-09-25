type dump =
  ?firstname:string ->
  ?lastname:string ->
  ?promo:string ->
  ?niveau:string ->
  ?dpt:string ->
  ?recu:bool ->
  ?academicyear:string ->
  ?headpage:(int -> string) ->
  ?footpage:string ->
  ?title:string ->
  ?preamble:(int -> string) ->
  ?signature:(int -> string) ->
  Gen.dump

module type DiplomaReport =
sig
  val dump_per_result_per_student: dump
  val dump_per_student: dump
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
      ?headpage ?footpage ?title ?preamble ?signature
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
      ?headpage ?footpage ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name ?event_opt
      ~headerextralength:8
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
       if a.Public_data.diplome_recu then "Reçu(e)" else "Ajourné(e)"
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

  let lresultat =
    "",(fun x -> x),
    (fun a ->
       if a.Public_data.diplome_recu then "Reçu(e)s" else "Ajourné(e)s"
    )
  let dump_per_result_per_student
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?headpage ?footpage ?title ?preamble ?signature
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
        lresultat;
      ]
    in
    dump_national_diploma_list
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?headpage ?footpage ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name cmp headers columns state

  let dump_per_student
      ?firstname
      ?lastname
      ?promo
      ?niveau
      ?dpt
      ?recu
      ?academicyear
      ?headpage ?footpage ?title ?preamble ?signature
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp (fun a -> a.Public_data.diplome_lastname);
        Gen.lift_cmp (fun a -> a.Public_data.diplome_firstname) ;
      ]
    in
    let columns = [prenom_etudiant;nom_etudiant;resultat;moyenne; mention] in
    let headers =
      []
    in
    dump_national_diploma_list
      ?firstname ?lastname ?promo ?niveau ?dpt ?recu ?academicyear
      ?headpage ?footpage ?title ?preamble ?signature
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

let next_year year =
  try
    string_of_int (1+(int_of_string year))
  with
  | _ -> year

let dump_pv
  ?output_repository
  ?prefix
  ?output_file_name
  diplome
  state =
  let firstname = diplome.Public_data.diplome_firstname in
  let lastname = diplome.Public_data.diplome_lastname in
  let promotion = Some (diplome.Public_data.diplome_promotion)
  in
  let get_local_repository = Remanent_state.get_local_repository in
  let get_repository =
    Remanent_state.get_repository_to_dump_attestations
  in
  let get_store_according_promotion =
    Remanent_state.get_store_attestations_according_to_their_promotions
  in
  let get_indicate_promotions_in_file_names =
    Remanent_state.get_indicate_promotions_in_attestation_file_names
  in
  let rec_mk_when_necessary =
    Safe_sys.rec_mk_when_necessary
  in
  let f_firstname = (fun x -> x) in
  let f_lastname = (fun x -> x) in
  let state, output_repository, output_file_name =
    Tools.build_output
      ~get_local_repository
      ~get_repository
      ~get_store_according_promotion
      ~get_indicate_promotions_in_file_names
      ~rec_mk_when_necessary
      ~f_firstname
      ~f_lastname
      ~firstname
      ~lastname
      ~promotion
      ~extension:(Format.sprintf ".%s.tex" diplome.Public_data.diplome_niveau)
      ?prefix
      ?output_repository
      ?output_file_name
      state
  in
  let file =
    if output_repository = ""
    then output_file_name
    else
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let extension_opt =
    Safe_sys.get_extension output_file_name
  in
  let state, output_channel_opt =
    try
      state, Some (open_out file)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@."
          file
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file)
        Exit
        state ,
      None
  in
  match output_channel_opt with
  | None -> state, None
  | Some out ->
    let state, mode =
      match extension_opt with
      | Some "html" -> state, Loggers.HTML
      | Some "tex" ->
        state, Loggers.Latex Loggers.Normal
      | Some _ ->
        Remanent_state.warn
          __POS__
          (Printf.sprintf
             "Extension of file %s is invalid"
             output_file_name)
          Exit
          state, Loggers.HTML
      | None ->
        Remanent_state.warn
          __POS__
          (Printf.sprintf
             "File %s has no extension"
             output_file_name)
          Exit
          state, Loggers.HTML
    in
    let logger =
      Loggers.open_logger_from_channel
        ~headerextralength:4 ~mode
        out in
    let state, enspsl =
      Remanent_state.get_ENSPSL_logo state
    in
    let () =
      Loggers.setheadpage logger
        (Format.sprintf
           "\\IfFileExists{%s}{\\includegraphics{%s}}"
           enspsl enspsl)
    in
    let () =
      Loggers.setfootpage logger
        "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 32  20 45 --  Fax : + 33 (0) 1 44 32 20 75 - diplome@di.ens.fr}"
    in
    let body =
      Format.sprintf
        "\\vfill\n\n\\begin{center}\\underline{ATTESTATION}\\end{center}\n\ \\vfill\n\n\  Je soussigné, \\textbf{%s}, directeur des études du département d’Informatique de l’École Normale Supérieure,\\bigskip\\\\CERTIFIE que,\\bigskip\\\\conformément aux dispositions générales de la scolarité au sein de la formation universitaire en informatique de l’ENS et aux décisions de la commission des études du département d’informatique de l’ENS, \\bigskip\\\\\\textbf{%s %s}, a obtenu en %s-%s\\\\\\textbf{%s}\\Parcours : \\textbf{Formation interuniversitaire en informatique – ENS} Paris.\bigskip\\%s \\\\\\vfill\n\\begin{center}Fait à Paris le %s\\smallskip\\\\Pour valoir et servir ce que de droit\\end{center}\\vfill"
        "Jérôme FERET"
        diplome.Public_data.diplome_lastname
        diplome.Public_data.diplome_firstname
        diplome.Public_data.diplome_year
        (next_year diplome.Public_data.diplome_year)
        diplome.Public_data.diplome_niveau
        "VALIDÉE"
        "\\today"
    in
    let _ = Loggers.fprintf logger  "%s" body in
    let () = Loggers.close_logger logger in
    state, Some (output_repository,output_file_name)

let dump_pvs
    ?output_repository
    ?prefix
    state =
  let state, diplome_list =
    Remanent_state.get_national_diplomas state
  in
  List.fold_left
    (fun state diplome ->
       let
         state, input =
         dump_pv
           ?output_repository
           ?prefix
           diplome
           state
       in
       Latex_engine.latex_opt_to_pdf state ~input)
    state
    diplome_list
