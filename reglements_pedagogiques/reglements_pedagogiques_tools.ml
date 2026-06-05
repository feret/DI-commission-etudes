module type Double_keys = 
    sig 
        type obj 
        type key
        type dip 

        val empty_dip: dip 
        module KeyMap:Map.S with type key = key 
        val index1: obj -> key option 
        val index2: obj -> key option 
        val get_dip: obj -> dip 
        val string_of_dip: dip -> string 
        val get_ects: obj -> float 
        val get_note: obj -> Public_data.note 
        val get_validation: obj -> Public_data.valide 
        val get_year: obj -> string 
        val is_unallocated: dip -> bool 
        val dens: dip 
    end 

module type DMap = 
  sig 
    type key
    type obj  
    type dip 
    type t 

    val empty: t 
    val add: ?new_dip:dip -> obj -> t -> Remanent_state.t -> Remanent_state.t * t 
    val find_opt: key -> t -> Remanent_state.t -> Remanent_state.t * (obj*dip*dip) option 
    val fold: (dip -> obj -> 'a -> 'a) -> t -> 'a -> 'a 
    val filter_out: dip list -> t -> Remanent_state.t -> Remanent_state.t * t 
    val select_course_for_a_cursus_list: (dip * Public_data.reglement_diplome)  list -> t -> Remanent_state.t -> Remanent_state.t * t  
* (dip * (int * key list) list * float) list  
    val print: Remanent_state.t -> (Remanent_state.t -> (obj * dip * dip)  -> unit) -> t -> (dip * (int * key list) list * float)  list -> Remanent_state.t 

 end

module Course = 
    struct 
      type obj = Public_data.cours_supplement 
      type key = string 
      type dip = Public_data.diploma_level option * Public_data.main_dpt option 
      let empty_dip = None,None  
      module KeyMap=Map.Make (struct type t=string let compare = compare end)
      let string_of_dip (a,b) =  
        match a,b with 
        | None, _ -> Public_data.string_of_dpt_opt b 
        | Some a, None -> 
        (match a with 
         Public_data.L3 -> "L3" 
            | Public_data.M1 -> "M1" 
            | Public_data.M2 -> "M2" 
            | Public_data.DENS -> "DENS" 
            | Public_data.Other -> "Other") 
      | Some a, Some b -> 
         (match a with 
              Public_data.L3 -> Format.sprintf "L3 (%s)" (Public_data.string_of_dpt b)
            | Public_data.M1 -> Format.sprintf "M1 (%s)" (Public_data.string_of_dpt b)
            | Public_data.M2 -> Format.sprintf "M2 (%s)" (Public_data.string_of_dpt b)
            | Public_data.DENS -> "DENS" 
            | Public_data.Other -> Format.sprintf  "Other (%s)" (Public_data.string_of_dpt b)) 

            let index2 course = course.Public_data.supplement_code_gps  
      let index1 course = course.Public_data.supplement_code_helisa 
      let get_dip course = Some course.Public_data.supplement_diploma_level, course.Public_data.supplement_diploma_dpt
      let get_ects course = course.Public_data.supplement_ects 
      let get_note course = course.Public_data.supplement_note 
      let get_validation course = course.Public_data.supplement_validation 
        let get_year course = course.Public_data.supplement_validation_year 
      let is_unallocated dip = dip = (None,None)
      let (dens:dip) = ((Some Public_data.DENS),None) 


  end 

module DMap(A:Double_keys with type key = string) = 
  (struct 
    include A.KeyMap 
    type obj = A.obj 
    type dip = A.dip 
    type t = (obj*dip*dip) A.KeyMap.t 

    let extend ?new_dip x = 
      let new_dip = 
        match new_dip with None -> A.empty_dip 
        | Some a -> a in 
      (x,A.get_dip x, new_dip)

    let add ?new_dip x  map state = 
      let key1 = A.index1 x in 
      let key2 = A.index2 x in 
      let obj = extend ?new_dip x in
      let state, map = 
        match key2 with 
        | None -> state, map 
        | Some key2 -> state, A.KeyMap.add key2 obj map 
      in
      match key1 with 
      | None -> state, map 
      | Some key1 -> 
            state, A.KeyMap.add key1 obj map
    let find_opt x map state = 
      state, A.KeyMap.find_opt x map 

    let empty = A.KeyMap.empty 
    let rest_in_dens dip_list (t:t) state = 
      let dip_list = List.rev_map fst (List.rev dip_list) in 
      let state, keep_validated = Remanent_state.do_not_move_unvalidated state in 
      state, A.KeyMap.map 
        (fun (obj,x,y) -> 
          if A.is_unallocated y then 
            begin 
            if not ( match A.get_validation obj with 
              | Public_data.Bool false | Public_data.Abs -> false 
              | Public_data.Bool true | Public_data.Not_known_yet -> true)
            then 
              begin 
                if keep_validated 
                then 
                  (obj, x, x)
                else 
                  (obj, x, A.dens)
              end 
            else 
              if x = A.dens || List.mem x dip_list 
              then 
               (obj, x, A.dens) 
              else (obj, x, x)
          end
          else (obj, x, y)
          )
            t 
               
    let fold f a state = 
      A.KeyMap.fold (fun x (a,_,_) state -> 
        match A.index1 a, A.index2 a with 
          | None, _ | _, None ->  f (A.get_dip a) a state
          | (Some b),_ when b = x ->  f (A.get_dip a) a state
          | Some _, Some _ -> state) a state 

    let filter_out l t state = 
      state, A.KeyMap.filter 
        (fun _ (_, x, _) -> not (List.mem x l)) t  
    
      
    let select_obligatory reglement new_dip acc = 
      let l = reglement.Public_data.obligation in 
      List.fold_left 
            (fun (state, t, missing, ects) key -> 
              let state, cours = find_opt key t state in 
              match cours with 
                | Some (a,_,dip) when A.is_unallocated dip || dip = new_dip-> 
                  let state, t = add ~new_dip a t state in 
                  state, t, missing, ects +. A.get_ects a 
               | Some _   
               | None -> state, t, (1,[key])::missing, ects)
            acc
            l 
  

    let select_default reglement new_dip acc = 
      let l = reglement.Public_data.par_defaut in 
      List.fold_left 
            (fun acc (k,list)  -> 
              let rec aux k list (state, t, missing, ects) = 
                if k = 0 then (state, t, missing, ects) else 
                  match list with 
                | [] -> state, t, (k,list)::missing, ects
                | key::tail -> 
                  let state, cours = find_opt key t state in 
                  match cours with 
                  | Some (a,_,dip) when A.is_unallocated dip || dip=new_dip  -> 
                    let state, t = add ~new_dip a t state in 
                    aux (k-1) tail (state, t, missing, ects +. A.get_ects a)
                  | Some _ 
                  | None -> aux k tail (state, t, missing, ects) 
                  in aux k list acc)
            acc l 

    let check_for_courses new_dip list state t = 
        let rec aux l (state,missing,acc) = 
              match l with 
              | [] -> state, missing, acc 
              | key::tail -> 
                  let state, cours = find_opt key t state in 
                     match cours with 
                     | Some (a,_,dip) when A.is_unallocated dip || dip=new_dip -> aux tail (state, missing, (key,a)::acc)
                    | None | Some _ -> aux tail (state,key::missing,acc) 
        in 
        aux list (state,[],[])

    let sort_enriched_list = 
       List.sort 
                (fun (_,a) (_,b) -> 
                  let cmp_val = 
                    match A.get_validation a, A.get_validation b with 
                  | Public_data.Bool a, Public_data.Bool b -> 
                    if a=b then 0 
                    else if a then -1
                    else 1  
                  | Public_data.Bool true, Public_data.Not_known_yet -> -1 
                  | Public_data.Not_known_yet, Public_data.Bool false -> -1 
                  | Public_data.Bool false, Public_data.Not_known_yet -> 1 
                  | Public_data.Not_known_yet, Public_data.Bool true -> 1 
                  | Public_data.Abs,Public_data.Abs  -> 0 
                  | Public_data.Not_known_yet, Public_data.Not_known_yet -> 0 
                  | Public_data.Abs,_ -> 1 
                  | _, Public_data.Abs -> -1
                in 
                  match cmp_val with 
                  | 0 -> 
                    begin 
                      let a = A.get_note a in 
                      let b = A.get_note b in
                      if Notes.better a b then 1 
                      else if Notes.better b a then -1 
                      else 0 
                    end 
                  | _ -> cmp_val)  


    let select_groups reglement new_dip acc = 
      let l = reglement.Public_data.groups in   
      List.fold_left 
        (fun (state, t, missing, ects) (k,list) -> 
          let state, not_in, enriched_list = check_for_courses new_dip list state t in 
          let sorted_list = sort_enriched_list enriched_list in 
          let rec aux k list (state, t, missing, ects) = 
                if k = 0 then (state, t, missing, ects) else 
                  match list with 
                | [] ->  state, t, (k,not_in)::missing, ects
                | (key,b)::tail -> 
                  begin 
                    match A.get_validation b with 
                      | Public_data.Bool false | Public_data.Abs -> 
                         state, t, (k,(List.rev_map fst (List.rev list))@not_in)::missing, ects
                      | Public_data.Bool true | Public_data.Not_known_yet  -> 
                     let state, cours = find_opt key t state in 
                     match cours with 
                  | Some (a,_,dip) when A.is_unallocated dip || dip = new_dip -> 
                    let state, t = add ~new_dip a t state in 
                    aux (k-1) tail (state, t, missing, ects +. A.get_ects a)
                  | None | Some _ -> aux k tail (state, t, missing, ects) 
                  end 
                  in aux k sorted_list (state, t, missing, ects)) 
                  acc l 

  let select_options reglement new_dip acc = 
      let list = reglement.Public_data.options in   
      let (state, t, missing, ects) = acc in 
      let state, _not_in, enriched_list = check_for_courses new_dip list state t in 
      let sorted_list = sort_enriched_list enriched_list in 
      let rec aux list (state, t, ects) = 
          if ects >= 60. then (state, t, ects) else 
          match list with 
            | [] -> state, t,  ects
            | (key,b)::tail -> 
                  begin 
                    match A.get_validation b with 
                      | Public_data.Bool false | Public_data.Abs -> state, t, ects
                      | Public_data.Bool true | Public_data.Not_known_yet  -> 
                     let state, cours = find_opt key t state in 
                     match cours with 
                  | Some (a,_,dip) when A.is_unallocated dip || dip = new_dip -> 
                    let state, t = add ~new_dip a t state in 
                    aux tail (state, t, ects +. A.get_ects a)
                  | None | Some _ -> aux tail (state, t,  ects) 
                  end 
      in 
      let state, t, ects = aux  sorted_list (state, t,  ects) in 
      state, t, missing, ects 


    let keep_others dip_list t = 
      let dip_list = List.rev_map fst (List.rev dip_list) in 
      let t = A.KeyMap.map 
        (fun  (a,b,c) -> 
          if List.mem b dip_list then 
            (a,b,c)
          else (a,b,b)
          ) t in t 
    

    let select_course_for_a_cursus_list dip_list t state = 
      let t = keep_others dip_list t in 
      let state, t, list = 
        List.fold_left 
          (fun (state, t, list) (dip, reglement) -> 
          let missing = [] in 
          let ects = 0. in 
          let acc = state, t, missing, ects in 
          let acc = select_obligatory reglement dip acc in
          let acc = select_default reglement dip acc in 
          let acc = select_groups reglement dip acc in 
          let acc = select_options reglement dip acc in 
          let (state, t, missing, ects) = acc in 
          state, t, (dip,missing,ects)::list) 
           (state, t, []) dip_list  
      in 
      let state, t = rest_in_dens dip_list t state in 
      state, t, list 
      

   let print state print t list  =   
    if list = [] then state else  
    let size =    [None;None;None;None;None;None;None] in
    let bgcolor = [None;None;None;None;None;None;None] in
    let state, show_missing_entries = Remanent_state.show_missing_entries state in 
    let state, something = 
      if show_missing_entries then 
      List.fold_left 
      (fun (state,something) (dip, missing, _) -> 
      List.fold_left 
          (fun (state,_something) (k,l) -> 
            if k = List.length l then 
              if k = 1 then 
               let () = Remanent_state.fprintf state "The following %i course " (List.length l)  in 
               let () = List.iter (fun elt -> Remanent_state.fprintf state "%s " elt) l in 
               let () = Remanent_state.fprintf state "is missing for diploma %s" (A.string_of_dip dip) in       
               let () = Remanent_state.print_newline state in 
               state, true 
else  
  let () = Remanent_state.fprintf state "The following %i courses " (List.length l)  in 
               let () = List.iter (fun elt -> Remanent_state.fprintf state "%s, " elt) l in 
               let () = Remanent_state.fprintf state "are missing for diploma %s" (A.string_of_dip dip) in       
               let () = Remanent_state.print_newline state in 
               state, true 
            else          
            let () = if k = 1 then Remanent_state.fprintf state "It misses %i over %i course among " k (List.length l) 
            else 
              Remanent_state.fprintf state "It misses %i over %i courses among " k (List.length l) 
           in 
            let () = List.iter (fun elt -> Remanent_state.fprintf state "%s, " elt) l in 
            let () = Remanent_state.fprintf state " for diploma %s" (A.string_of_dip dip) in         
            let () = Remanent_state.print_newline state in 
            state, true) 
          (state, something) missing) (state,false) list else (state, false) 
    in 
    let () = Remanent_state.fprintf state "\\renewcommand{\\row}[7]{#1&#2&#3&#4&#5&#6&#7\\cr}" in
    let () = Remanent_state.fprintf state "\\renewcommand{\\innerline}{}" in
    let () = Remanent_state.fprintf state "\\vfill" in
    let by_year = Public_data.YearMap.empty in 
    let add k c map = 
      let cours,a,b = c in 
        if a = b then map 
        else 
        if A.index1 cours = Some k 
          || A.index1 cours  = None 
        then 
          let year = A.get_year cours in 
          let old = 
            match 
              Public_data.YearMap.find_opt year map 
            with
            | Some map -> map 
            | None -> A.KeyMap.empty 
          in 
          let k = 
            match A.index2 cours with 
            | None -> k 
            | Some a -> a 
          in 
          let updated = A.KeyMap.add k c old in 
          Public_data.YearMap.add year updated map  
        else 
         map 
          in 

    
    let by_year = A.KeyMap.fold add t by_year in   
      let state, something = 
      Public_data.YearMap.fold 
        (fun year t (state, _something) -> 
          let year_ext = 
            try 
              let year_int = int_of_string year in 
              Format.sprintf "%i - %i" year_int (year_int + 1) 
            with _ -> year 
          in
          let s_fr = Format.sprintf "Année académique %s" year_ext in 
          let s_en = Format.sprintf "Academic year %s" year_ext in 
          let state, s_bi = Remanent_state.bilingual_string ~english:s_en ~french:s_fr state in 
          let () = Remanent_state.fprintf state "%s" s_bi in 
          let () = Remanent_state.fprintf state "\\begin{center}" in
    let state =
      Remanent_state.open_array
        __POS__
        ~bgcolor
        ~size
        ~with_lines:true
        ~title:[["Code GPS"];["Code HELISA"];["Cours"];["Note"];["ECTS"]; ["DIPLOME (avant)"];["DIPLOME (après)"]]
        ~title_english:[["GPS Code"];["HELISA Code"];["Course"];["Grade"];["ECTS"];["DIPLOMA (before)"] ;["DIPLOMA (after)"]]
        state
    in
    let (state:Remanent_state.t) = 
      A.KeyMap.fold  
      (fun _k c state -> 
        let () = Remanent_state.open_row state in
        let () = print state c in 
        let () = Remanent_state.close_row state in 
        state 
        ) t state 
    in
    let () = Remanent_state.close_array state in 
    state,true) by_year (state, something) 
  in 
  let () = if something then 
  let () = Remanent_state.fprintf state "\\vfill" in
  let () = Remanent_state.breakpage state in () 
  in state 

  end: DMap with type key = A.key and type obj = A.obj and type dip = A.dip) 

module CourseDMap = DMap(Course) 