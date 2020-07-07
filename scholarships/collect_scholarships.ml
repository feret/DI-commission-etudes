
type scholarship_id =
  {
    lastname: string option;
    firstname: string option;
    promotion: string option;
    organism: string option;
  }


let empty_scholarship =
  {
    lastname = None ;
    firstname = None ;
    promotion = None ;
    organism = None ;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Promo ;
    Public_data.Organisme_de_Financement ;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
  ]
let asso_list =
  [
    Public_data.LastName,
    (fun state lastname x ->
       state,
       let lastname =
         match lastname with
         | Some x when String.trim x = "" -> None
         | _ -> lastname
       in
       {x with lastname});
    Public_data.FirstName,
    (fun state firstname x ->
       state,
       let firstname =
         match firstname with
         | Some x when String.trim x = "" -> None
         | _ -> firstname
       in
       {x with firstname});
    Public_data.Promo,
    (fun state promotion x ->
       state, {x with promotion});
    Public_data.Organisme_de_Financement,
       (fun state organism x ->
          state, {x with organism});
  ]

let event_opt = Some Profiling.Collect_scholarships
let lift = Scan_csv_files.lift_safe
let lift_pred = Scan_csv_files.lift_pred_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname), "Student's family name";
    lift_pred (fun a -> a.firstname), "Student's first name";
    lift_pred (fun a -> a.promotion), "Student's promotion"]


let all_fields =
  [lift
     (fun a -> a.lastname)
     (fun a holder_lastname ->
        {a with Public_data.holder_lastname = holder_lastname})
     (Printf.sprintf "Student's family name: %s")
   __POS__ "Student's last name is missing in a scholarship description";
   lift
      (fun a -> a.firstname)
      (fun a holder_firstname ->
         {a with Public_data.holder_firstname = holder_firstname})
      (Printf.sprintf "Student's first name: %s")
      __POS__ "Student's first name is missing in a scholarship description";
   lift
     (fun a -> a.promotion)
     (fun a holder_promotion ->
        {a with Public_data.holder_promotion = Some holder_promotion})
     (Printf.sprintf "Student's promotion: %s")
         __POS__ "Student's promotion is missing in a scholarship description";
   lift
     (fun a -> a.organism)
     (fun a organism ->
        {a with Public_data.organism})
     (Printf.sprintf "Funding organism: %s")
     __POS__ "Funding organism is missing in a scholarship description";
  ]

let compute_repository =
  Remanent_state.get_scholarships_list_repository

let get_scholarships
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state =
  let p =
    (fun y ->
       match promotion, y.promotion with
       | Some a, Some b -> a=b
       | None,_ | _, None -> true
    )
  in
  Scan_csv_files.collect_gen
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~p
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~asso_list
    ~keywords_list
    ~init_state:empty_scholarship
    ~empty_elt:Public_data.empty_scholarship
    ~add_elt:Remanent_state.add_scholarship
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
