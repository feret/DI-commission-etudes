type ('data,'record_tmp,'record) gen =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:(Remanent_state.t -> 'record_tmp -> Remanent_state.t * 'data option) ->
  get:(Remanent_state.t -> 'record -> Remanent_state.t * 'data) ->
  set:(Remanent_state.t -> 'data -> 'record -> Remanent_state.t * 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp,'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_safe =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:('record_tmp -> 'data option) ->
  get:('record -> 'data) ->
  set:('data -> 'record -> 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp,'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_opt =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:(Remanent_state.t -> 'record_tmp -> Remanent_state.t * 'data option) ->
  get:(Remanent_state.t -> 'record -> Remanent_state.t * 'data option) ->
  set:(Remanent_state.t -> 'data option -> 'record -> Remanent_state.t * 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp, 'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_opt_safe =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:('record_tmp -> 'data option) ->
  get:('record -> 'data option) ->
  set:('data option -> 'record -> 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp, 'record) Keywords_handler.any_field

let lift_gen_pred_safe pred f =
  pred
    (fun state a -> state, f a)

let pred_gen_safe p =
  lift_gen_pred_safe
    (fun f s ->
       {
         Scan_gen_files.check =
           (fun state a ->
              let state, rep = f state a in
              state, p rep);
         Scan_gen_files.label = s
       })

let pred_safe  f s = pred_gen_safe  (fun rep -> rep<>None) f s
let pred_opt_safe f s = pred_gen_safe (fun _ -> true) f s


let gen ~keyword ~set_tmp ~eq ~get_tmp ~get ~set ~unify ~string_of1 ~string_of2 ~pos ~msg_update ~msg_unify
  =
  {
    Keywords_handler.keyword;
    Keywords_handler.set_tmp;
    Keywords_handler.update =
      (fun state elt new_elt ->
         let state, x_opt = get_tmp state elt in
         match x_opt with
         | None ->
           Remanent_state.warn_dft
             pos
             msg_update
             Exit
             new_elt
             state
         | Some x ->
           set state x new_elt );
    Keywords_handler.is_unifyable =
      (fun state elt1 elt2 ->
         let state, data1 = get state elt1 in
         let state, data2 = get state elt2 in
         state, eq data1 data2) ;
    Keywords_handler.unify =
      (fun state elt1 elt2 ->
         let state, data1 = get state elt1 in
         let state, data2 = get state elt2 in
         let state, data_opt = unify state data1 data2 in
         match data_opt with
         | Some data ->
           let state, elt = set state data elt1 in
           state, Some elt
         | None ->
           let state, msg_opt = string_of2 state data1 data2 in
           let msg =
             match msg_opt with
             | Some msg -> msg
             | None -> msg_unify
           in
           Remanent_state.warn
             pos
             msg
             Exit
             state,
           None
      ) ;
    Keywords_handler.label_tmp =
      (fun state elt ->
         let state, data_opt = get_tmp state elt in
         match data_opt with
         | None -> state, None
         | Some data ->
           string_of1 state data);
    Keywords_handler.label1 =
      (fun state elt1  ->
         let state, data1 = get state elt1 in
         string_of1 state data1 ) ;
    Keywords_handler.label2 =
      (fun state elt1 elt2 ->
         let state, data1 = get state elt1 in
         let state, data2 = get state elt2 in
         string_of2 state data1 data2)}


let gen_opt ~keyword ~set_tmp ~eq ~get_tmp ~get ~set ~unify
    ~msg_unify ~string_of1 ~string_of2 ~pos =
  {
    Keywords_handler.keyword;
    Keywords_handler.set_tmp;
    Keywords_handler.update =
      (fun state elt new_elt ->
         let state, x_opt = get_tmp state elt in
         match x_opt with
         | None -> state, new_elt
         | Some _ ->
           let state, output = set state x_opt new_elt in
           state, output );
    Keywords_handler.is_unifyable =
      (fun state elt1 elt2 ->
         let state, data_opt1 = get state elt1 in
         let state, data_opt2 = get state elt2 in
         match data_opt1, data_opt2
         with
         | None, _ | _,None -> state, true
         | Some data1, Some data2 -> state, eq data1 data2) ;
    Keywords_handler.unify =
      (fun state elt1 elt2 ->
         let state, data_opt1 = get state elt1 in
         let state, data_opt2 = get state elt2 in
         match data_opt1, data_opt2
         with
         | _, None -> state, Some elt1
         | None, Some _ ->
           let state, elt = set state data_opt2 elt1 in
           state, Some elt
         | Some data1, Some data2 when eq data1 data2 ->
           state, Some elt1
         | Some data1, Some data2 ->
           let state, data_opt = unify state data1 data2 in
           begin
             match data_opt with
             | Some _ ->
               let state, elt = set state data_opt elt1 in
               state, Some elt
             | None ->
               let state, msg_opt = string_of2 state data1 data2 in
               Remanent_state.warn
                 pos
                 (match msg_opt with None -> msg_unify
                                   | Some msg -> msg)
                 Exit
                 state,
               None
           end
      );

    Keywords_handler.label_tmp =
      (fun state a ->
         let state, x_opt = get_tmp state a in
         match x_opt with
         | None -> state, None
         | Some a -> string_of1 state a
      ) ;
    Keywords_handler.label1 =
      (fun state elt1 ->
         let state, data1_opt = get state elt1 in
         match data1_opt  with
         | None -> state, None
         | Some data1 ->
           string_of1 state data1 );
    Keywords_handler.label2 =
      (fun state elt1 elt2 ->
         let state, data1_opt = get state elt1 in
         let state, data2_opt = get state elt2 in
         match data1_opt,data2_opt with
         | None, _ | _, None -> state, None
         | Some data1, Some data2 ->
           string_of2 state data1 data2 )
  }

let lift1 f =
  (fun state a -> state, f a)
let lift2 f =
  (fun state a b -> state, f a b)

let lift_fstring1 f state s =
  let s = Format.sprintf f s in
  state, Some s

let lift_fstring2 f state s1 s2 =
  let s = Format.sprintf f s1 s2 in
  state, Some s

let build_lift conv =
  (fun f state x -> lift_fstring1 f state (conv x)),
  (fun f state x1 x2 -> lift_fstring2 f state (conv x1) (conv x2))

let string_of_bool b =
  if b then "true" else "false"

let string_of_gender s =
  match s with
  | Public_data.Masculin -> "M"
  | Public_data.Feminin -> "F"
  | Public_data.Unknown -> "?"

let lift_fbool1, lift_fbool2 = build_lift string_of_bool
let lift_fint1, lift_fint2 = build_lift string_of_int
let lift_ffloat1, lift_ffloat2 = build_lift string_of_float
let lift_fgender1, lift_fgender2 = build_lift string_of_gender
let lift_fcolor1, lift_fcolor2 = build_lift Color.to_string
let lift_fdpt1, lift_fdpt2 = build_lift Public_data.string_of_dpt
let lift_funiv1, lift_funiv2 = build_lift Public_data.string_of_universite
let lift_fexp1, lift_fexp2 = build_lift Public_data.string_of_experience

let gen_short
    lift_arg lift_arg_pair
    ~keyword ~set_tmp
    ~get_tmp ~get ~set
    ~field_name ~record_name ~pos =
  gen
    ~keyword ~set_tmp
    ~eq:(fun a b -> a=b)
    ~unify:(fun state a b ->
        if a=b then state, Some a else state, None)
    ~get_tmp ~get ~set
    ~string_of1:(lift_arg
                   (Scanf.format_from_string
                      ((String.capitalize_ascii field_name)^": %s")
                      "%s"))
    ~string_of2:(lift_arg_pair
                   (Scanf.format_from_string
                      ((String.capitalize_ascii field_name)^": %s/%s") "%s %s"))
    ~pos
    ~msg_update:(Printf.sprintf
                   "%s is missing in %s"
                   (String.capitalize_ascii field_name)
                   record_name)
    ~msg_unify:(Printf.sprintf
                  "%ss are incompatible in %s"
                  (String.capitalize_ascii field_name)
                  record_name)

let gen_opt_short
    ~keyword ~set_tmp
    ~get_tmp ~get ~set
    ~field_name ~record_name ~pos
    lift_arg
    lift_arg_pair
  =
  gen_opt
    ~keyword ~set_tmp
    ~eq:(fun a b -> a=b)
    ~unify:(fun state a b ->
        if a=b then state, Some a else state, None)
    ~get_tmp ~get ~set
    ~string_of1:(lift_arg
                   (Scanf.format_from_string
                      ((String.capitalize_ascii field_name)^": %s")
                      "%s"))
    ~string_of2:(lift_arg_pair
                   (Scanf.format_from_string
                      ((String.capitalize_ascii field_name)^": %s/%s") "%s %s"))
    ~pos
    ~msg_unify:(Printf.sprintf
                  "%ss are incompatible in %s"
                  (String.capitalize_ascii field_name)
                  record_name)

type ('data,'record_tmp,'record) l_record =
  {
    elt: ('data,'record_tmp,'record) gen;
    safe: ('data,'record_tmp,'record) gen_safe;
    opt: ('data,'record_tmp,'record) gen_opt;
    opt_safe: ('data,'record_tmp,'record) gen_opt_safe
  }

type ('data,'record_tmp,'record) lift_record =
  'record_tmp -> 'record -> ('data,'record_tmp,'record) l_record


let build lift_elt1 lift_elt2 (_:'record_tmp) (_:'record) =
  let elt ~keyword ~set_tmp  ~get_tmp ~get =
    gen_short lift_elt1 lift_elt2 ~keyword ~set_tmp  ~get_tmp ~get
  in
  let safe ~keyword ~set_tmp  ~get_tmp ~get ~set =
    elt
      ~keyword ~set_tmp
      ~get_tmp:(lift1 get_tmp)
      ~get:(lift1 get)
      ~set:(lift2 set)
  in
  let opt =
    gen_opt_short lift_elt1 lift_elt2
  in
  let opt_safe
    ~keyword ~set_tmp  ~get_tmp ~get ~set
    =
  opt
    ~keyword ~set_tmp ~get_tmp:(lift1 get_tmp)
    ~get:(lift1 get)
    ~set:(lift2 set)
  in
  ({elt; safe; opt; opt_safe}:('data,'record_tmp,'record) l_record)


let string a b = build lift_fstring1 lift_fstring2 a b
let bool a b = build lift_fbool1 lift_fbool2 a b
let int a b = build lift_fint1 lift_fint2 a b
let float a b = build lift_ffloat1 lift_ffloat2 a b
let gender a b = build lift_fgender1 lift_fgender2 a b
let color a b = build lift_fcolor1 lift_fcolor2 a b
let main_dpt a b = build lift_fdpt1 lift_fdpt2 a b
let universite a b = build lift_funiv1 lift_funiv2 a b
let experience a b = build lift_fexp1 lift_fexp2 a b
