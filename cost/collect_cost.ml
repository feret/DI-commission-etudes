
type cost_member =
  {
    gender: Public_data.genre option;
    firstname: string option;
    lastname: string option;
    titre_fr: string option;
    titre_en: string option;
    initials: string option;
  }

let empty_cost_member =
  {
    gender = None;
    firstname = None;
    lastname = None;
    titre_fr = None;
    titre_en = None;
    initials = None
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Intitule ;
    Public_data.Genre ;
    Public_data.Titre_FR ;
    Public_data.Titre_EN ;
    ]

let keywords_of_interest =
  [
  Public_data.LastName ;
  Public_data.FirstName ;
  ]

let event_opt = Some Profiling.Collect_cost_members

let lift_string =
  (Lift.string empty_cost_member Public_data.empty_cost_member).Lift.safe
let lift_pred = Lift.pred_safe
let lift_gender =
    (Lift.gender empty_cost_member Public_data.empty_cost_member).Lift.opt_safe


let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname) "Cost member's family name";
    lift_pred (fun a -> a.firstname) "Cost member's first name";
  ]

let all_fields =
  let record_name = "the description of a cost member" in
  [lift_string
     ~keyword:Public_data.LastName
     ~set_tmp:(fun state lastname x ->
         state,
         let lastname =
           match lastname with
           | Some x when String.trim x = "" -> None
           | _ -> lastname
         in
         {x with lastname})
     ~get_tmp:(fun a -> a.lastname)
     ~get:(fun a -> a.Public_data.cost_lastname)
     ~set:(fun cost_lastname a ->
         {a with Public_data.cost_lastname})
     ~field_name:"the family name of the cost member"
     ~record_name
     ~pos:__POS__ ;
   lift_string
     ~keyword:Public_data.FirstName
     ~set_tmp:(fun state firstname x ->
         state,
         let firstname =
           match firstname with
           | Some x when String.trim x = "" -> None
           | _ -> firstname
         in
         {x with firstname})
     ~get_tmp:(fun a -> a.firstname)
     ~get:(fun a -> a.Public_data.cost_firstname)
     ~set:(fun cost_firstname a -> {a with Public_data.cost_firstname = cost_firstname})
     ~field_name:"the first name of the cost member"
     ~record_name
     ~pos:__POS__;

  lift_string
    ~keyword:Public_data.Initiales
    ~set_tmp:(fun state initials x ->
        state,
        let initials =
          match initials with
          | Some x when String.trim x = "" -> None
          | _ -> initials
        in
        {x with initials})
    ~get_tmp:(fun a -> a.initials)
    ~get:(fun a -> a.Public_data.cost_initials)
    ~set:(fun cost_initials a -> {a with Public_data.cost_initials = cost_initials})
    ~field_name:"the initials of the cost member"
    ~record_name
    ~pos:__POS__;

  lift_gender
      ~keyword:Public_data.Genre
      ~set_tmp:(fun state gender x ->
          let state, gender =
            match
              Tools.map_opt
                (fun x -> Special_char.lowercase
                    (Special_char.correct_string_txt
                       (String.trim x)))
                gender
            with
            | Some ("m" | "masc" | "masculin") ->
              state, Some Public_data.Masculin
            | Some ("f" | "fem" | "feminin") ->
              state, Some Public_data.Feminin
            | None | Some "" -> state, None
            | Some x ->
              let msg =
                Printf.sprintf
                  "Invalid cost member's gender (%s)"
                  x
              in
              Remanent_state.warn_dft
                __POS__
                msg
                Exit
                None
                state
          in
          state, {x with gender})
      ~get_tmp:(fun a -> a.gender)
      ~get:(fun a -> Some a.Public_data.cost_gender)
      ~set:(fun cost_gender a ->
         {a with Public_data.cost_gender = match cost_gender with Some x -> x | None -> Public_data.Unknown})
      ~field_name:"the gender of the cost member"
      ~record_name
      ~pos:__POS__;

      lift_string
        ~keyword:Public_data.Titre_EN
        ~set_tmp:(fun state titre_en x ->
            state,
            let titre_en =
              match titre_en with
              | Some x when String.trim x = "" -> None
              | _ -> titre_en
            in
            {x with titre_en})
        ~get_tmp:(fun a -> a.titre_en)
        ~get:(fun a -> a.Public_data.cost_titre_en)
        ~set:(fun cost_titre_en a -> {a with Public_data.cost_titre_en = cost_titre_en})
        ~field_name:"the title(EN) of the cost member"
        ~record_name
        ~pos:__POS__;


        lift_string
          ~keyword:Public_data.Titre_FR
          ~set_tmp:(fun state titre_fr x ->
              state,
              let titre_fr =
                match titre_fr with
                | Some x when String.trim x = "" -> None
                | _ -> titre_fr
              in
              {x with titre_fr})
          ~get_tmp:(fun a -> a.titre_fr)
          ~get:(fun a -> a.Public_data.cost_titre_fr)
          ~set:(fun cost_titre_fr a -> {a with Public_data.cost_titre_fr = cost_titre_fr})
          ~field_name:"the title(FR) of the cost member"
          ~record_name
          ~pos:__POS__;]

let compute_repository =
  Remanent_state.get_cost_members_repository

let get_cost_members
    ?repository
    ?prefix
    ?file_name
    state =
  Scan_csv_files.collect_gen
    ~strict:true 
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_cost_member
    ~empty_elt:Public_data.empty_cost_member
    ~add_elt:Remanent_state.add_cost_member
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
