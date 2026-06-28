type t =
  {
    per_year_name_libelle:
      Public_data.pedagogical_charge 
      Public_data.StringMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t
         ;

    per_year_name_gps: 
    Public_data.pedagogical_charge 
      Public_data.StringMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t
         ;

    
    per_year_name_helisa: 
    Public_data.pedagogical_charge 
      Public_data.StringMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t
         ;

    
    per_year_libelle: 
       Public_data.pedagogical_charge 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t  Public_data.StringMap.t  Public_data.YearMap.t
         ;


    per_year_helisa: 
       Public_data.pedagogical_charge 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t  Public_data.StringMap.t  Public_data.YearMap.t
         ;

    per_year_gps: 
       Public_data.pedagogical_charge 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t  Public_data.StringMap.t  Public_data.YearMap.t
         ;
    
     per_name_year_libelle:
      Public_data.pedagogical_charge 
      Public_data.StringMap.t
      Public_data.YearMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t 
         ;

    per_name_year_gps: 
     Public_data.pedagogical_charge 
      Public_data.StringMap.t
      Public_data.YearMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t ;

    
    per_name_year_helisa: 
     Public_data.pedagogical_charge 
      Public_data.StringMap.t
      Public_data.YearMap.t 
      Public_data.FirstNameMap.t Public_data.LastNameMap.t 


  }

let empty =
  {
     per_year_name_libelle = Public_data.YearMap.empty ; 
     per_year_name_helisa = Public_data.YearMap.empty ; 
     per_year_name_gps = Public_data.YearMap.empty ; 
     per_year_libelle = Public_data.YearMap.empty ; 
     per_year_helisa = Public_data.YearMap.empty ; 
     per_year_gps = Public_data.YearMap.empty ; 
     per_name_year_libelle = Public_data.LastNameMap.empty ; 
     per_name_year_gps = Public_data.LastNameMap.empty ;  
     per_name_year_helisa = Public_data.LastNameMap.empty ; 
  }

let get_pedagogical_charge ~year ~firstname ~lastname  ?gps_code ?helisa_code ?course charges =
  let firstname =
    Special_char.lowercase firstname
  in
  let lastname =
    Special_char.lowercase lastname
  in
  match gps_code, helisa_code, course with 
  | None, None, None -> None 
  | (Some _), _, _ | _, (Some _), _ | _, _, (Some _) -> 
  let key,map = 
      match gps_code, helisa_code, course with 
      | Some gps, _, _  -> gps, charges.per_name_year_gps
      | _, Some helisa, _ -> helisa, charges.per_name_year_helisa 
      | _,_, Some libelle -> libelle, charges.per_name_year_libelle 
      | None, None, None -> assert false 
  in 
    match
       Public_data.LastNameMap.find_opt
         lastname
         map 
     with
     | None -> None
     | Some a ->
       let yearmap =
         (match
            Public_data.FirstNameMap.find_opt
              firstname
              a
          with
          | None -> Public_data.YearMap.empty
          | Some a -> a)
       in
       let course_map = 
       match 
         Public_data.YearMap.find_opt
           year yearmap
      with 
      | None -> Public_data.StringMap.empty 
      | Some a -> a 
    in 
    Public_data.StringMap.find_opt key course_map 
    
let add_pedagogical_charge
    unify pos state
    charge charges =
  let firstname = charge.Public_data.charge_firstname  in
  let lastname = charge.Public_data.charge_lastname  in
  let year = charge.Public_data.charge_attribution_year in
  let charge_opt' = 
    match charge.Public_data.charge_gps_code with
    | Some gps_code -> 
      get_pedagogical_charge 
        ~firstname ~lastname ~gps_code ~year 
        charges 
    | None -> 
      begin 
         match charge.Public_data.charge_helisa_code with
    | Some helisa_code -> 
      get_pedagogical_charge 
        ~firstname ~lastname ~helisa_code ~year 
        charges 
    | None -> begin 
       match charge.Public_data.charge_course_title with
    | Some course -> 
      get_pedagogical_charge 
        ~firstname ~lastname ~course ~year 
        charges 
    | None -> None end 
      end
    in 
  let state, charge =
    match charge_opt' with
    | None -> state, charge 
    | Some charge' ->
        unify pos state charge charge'
  in
  let charges = 
      match charge.Public_data.charge_course_title with 
        | None -> charges
        | Some libelle -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_name_libelle with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_ymap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
           let old_fnmap = 
              match Public_data.FirstNameMap.find_opt firstname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_year_name_libelle = 
          Public_data.YearMap.add year 
            (Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                  (Public_data.StringMap.add libelle charge old_fnmap) 
                    old_lnmap) 
                      old_ymap) charges.per_year_name_libelle
          in {charges with per_year_name_libelle} 
  in 
let charges = 
      match charge.Public_data.charge_gps_code with 
        | None -> charges
        | Some gps_code -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_name_gps with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_ymap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
           let old_fnmap = 
              match Public_data.FirstNameMap.find_opt firstname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_year_name_gps = 
          Public_data.YearMap.add year 
            (Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                  (Public_data.StringMap.add gps_code charge old_fnmap) 
                    old_lnmap) 
                      old_ymap) charges.per_year_name_gps
          in {charges with per_year_name_gps} 
  in 

let charges = 
      match charge.Public_data.charge_helisa_code with 
        | None -> charges
        | Some helisa_code -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_name_helisa with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_ymap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
           let old_fnmap = 
              match Public_data.FirstNameMap.find_opt firstname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_year_name_helisa = 
          Public_data.YearMap.add year 
            (Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                  (Public_data.StringMap.add helisa_code charge old_fnmap) 
                    old_lnmap) 
                      old_ymap) charges.per_year_name_helisa
          in {charges with per_year_name_helisa} 
  in 

   let charges = 
      match charge.Public_data.charge_course_title with 
        | None -> charges
        | Some libelle -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_libelle with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let old_coursemap = 
            match Public_data.StringMap.find_opt libelle old_ymap with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_coursemap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let per_year_libelle = 
              Public_data.YearMap.add year 
                  (Public_data.StringMap.add libelle 
                      (Public_data.LastNameMap.add lastname
                        (Public_data.FirstNameMap.add firstname charge 
                    old_lnmap) 
                      old_coursemap) old_ymap)  
                      charges.per_year_libelle
          in {charges with per_year_libelle} 
   in 
let charges = 
      match charge.Public_data.charge_helisa_code with 
        | None -> charges
        | Some helisa_code -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_helisa with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let old_coursemap = 
            match Public_data.StringMap.find_opt  helisa_code old_ymap with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_coursemap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let per_year_helisa = 
              Public_data.YearMap.add year 
                  (Public_data.StringMap.add helisa_code
                      (Public_data.LastNameMap.add lastname
                        (Public_data.FirstNameMap.add firstname charge 
                    old_lnmap) 
                      old_coursemap) old_ymap)  
                      charges.per_year_helisa
          in {charges with per_year_helisa} 
   in 
   let charges = 
      match charge.Public_data.charge_gps_code with 
        | None -> charges
        | Some gps_code -> 
          let old_ymap = 
            match Public_data.YearMap.find_opt year charges.per_year_gps with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let old_coursemap = 
            match Public_data.StringMap.find_opt  gps_code old_ymap with 
            | Some old -> old 
            | None -> Public_data.LastNameMap.empty 
          in 
          let old_lnmap = 
              match Public_data.LastNameMap.find_opt lastname old_coursemap with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let per_year_gps = 
              Public_data.YearMap.add year 
                  (Public_data.StringMap.add gps_code
                      (Public_data.LastNameMap.add lastname
                        (Public_data.FirstNameMap.add firstname charge 
                    old_lnmap) 
                      old_coursemap) old_ymap)  
                      charges.per_year_gps
          in {charges with per_year_gps} 
   in 

   let charges = 
      match charge.Public_data.charge_course_title with 
        | None -> charges
        | Some libelle -> 
          let old_lnmap = 
            match Public_data.LastNameMap.find_opt lastname charges.per_name_year_libelle with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let old_fnmap = 
              match Public_data.FirstNameMap.find_opt lastname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.YearMap.empty 
          in 
           let old_ymap = 
              match Public_data.YearMap.find_opt firstname old_fnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_name_year_libelle = 
            Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                 (Public_data.YearMap.add year 
                  (Public_data.StringMap.add libelle charge old_ymap) 
                    old_fnmap) 
                      old_lnmap) charges.per_name_year_libelle
          in {charges with per_name_year_libelle} 
  in  
  let charges = 
      match charge.Public_data.charge_gps_code with 
        | None -> charges
        | Some libelle -> 
          let old_lnmap = 
            match Public_data.LastNameMap.find_opt lastname charges.per_name_year_gps with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let old_fnmap = 
              match Public_data.FirstNameMap.find_opt lastname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.YearMap.empty 
          in 
           let old_ymap = 
              match Public_data.YearMap.find_opt firstname old_fnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_name_year_gps = 
            Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                 (Public_data.YearMap.add year 
                  (Public_data.StringMap.add libelle charge old_ymap) 
                    old_fnmap) 
                      old_lnmap) charges.per_name_year_gps
          in {charges with per_name_year_gps} 
  in 
  let charges = 
      match charge.Public_data.charge_helisa_code with 
        | None -> charges
        | Some libelle -> 
          let old_lnmap = 
            match Public_data.LastNameMap.find_opt lastname charges.per_name_year_helisa with 
            | Some old -> old 
            | None -> Public_data.FirstNameMap.empty 
          in 
          let old_fnmap = 
              match Public_data.FirstNameMap.find_opt lastname old_lnmap with 
            | Some old -> old 
            | None -> Public_data.YearMap.empty 
          in 
           let old_ymap = 
              match Public_data.YearMap.find_opt firstname old_fnmap with 
            | Some old -> old 
            | None -> Public_data.StringMap.empty 
          in 
          let per_name_year_helisa = 
            Public_data.LastNameMap.add lastname 
              (Public_data.FirstNameMap.add firstname 
                 (Public_data.YearMap.add year 
                  (Public_data.StringMap.add libelle charge old_ymap) 
                    old_fnmap) 
                      old_lnmap) charges.per_name_year_helisa
          in {charges with per_name_year_helisa} 
  in 
  state, charges 


let fold_left_rev f a b = List.fold_left (fun a b -> f b a) b a

let get_pedagogical_charge_list 
    ?year ?lastname ?firstname 
     ?gps_code ?helisa_code ?course 
    t =
  let get_per_year_name_cours, get_per_name_year_cours, code  = 
    match course,gps_code,helisa_code with 
    | None, None, None -> 
       t.per_year_name_libelle, 
       t.per_name_year_libelle, 
       None 
    | Some a, _,_  ->
        t.per_year_name_libelle, 
        t.per_name_year_libelle, 
        Some a 
    | _, Some a, _ -> 
        t.per_year_name_gps, 
        t.per_name_year_gps, 
        Some a 
    | _,_,Some a -> 
      t.per_year_name_helisa,
      t.per_name_year_helisa, 
       Some a 
    in 
  match year with
  | Some _ ->
    let acc0 =
      Public_data.YearExtendedMap.collect
        year 
        get_per_year_name_cours
        []
    in
    let acc1 =
      fold_left_rev
        (Public_data.LastNameExtendedMap.collect
           lastname)
        acc0
        []
    in
    let acc2 = 
      fold_left_rev
       (Public_data.FirstNameExtendedMap.collect
           firstname)
        acc1
        []
    in 
    fold_left_rev 
      (Public_data.StringExtendedMap.collect 
         code) acc2 []
  | None ->
    begin 
       match lastname with
        | Some _ ->
           let acc0 =
             Public_data.LastNameExtendedMap.collect
                 lastname  
                 get_per_name_year_cours 
                 []
           in
           let acc1 =
            fold_left_rev
              (Public_data.FirstNameExtendedMap.collect
           firstname)
        acc0
        []
      in
      let acc2 = fold_left_rev
        (Public_data.YearExtendedMap.collect
           year)
        acc1
        []
    in 
     fold_left_rev 
      (Public_data.StringExtendedMap.collect 
         code) acc2 []
      | None -> 
        begin 
             let acc0 =
             Public_data.LastNameExtendedMap.collect
                 lastname  
                 t.per_name_year_gps 
                 []
           in
           let acc1 =
            fold_left_rev
              (Public_data.FirstNameExtendedMap.collect
           firstname)
        acc0
        []
      in
      let acc2 = 
      fold_left_rev
        (Public_data.YearExtendedMap.collect
           year)
        acc1
        [] in 
fold_left_rev 
      (Public_data.StringExtendedMap.collect 
         code) acc2 []
         
        end 
    end 

   
