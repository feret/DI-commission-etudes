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
    val add: obj -> t -> Remanent_state.t -> Remanent_state.t * t 
    val find_opt: key -> t -> Remanent_state.t -> Remanent_state.t * (obj*dip*dip) option 
      val fold: (dip -> obj -> 'a -> 'a) -> t -> 'a -> 'a 
    val rest_in_dens: t -> Remanent_state.t -> Remanent_state.t * t 
    val filter_out: dip list -> t -> Remanent_state.t -> Remanent_state.t * t 
 end

module Course = 
    struct 
      type obj = Public_data.cours_supplement 
      type key = string 
      type dip = Public_data.diploma_level option * Public_data.main_dpt option 
      let empty_dip = None,None  
      module KeyMap=Map.Make (struct type t=string let compare = compare end)
      let index2 course = course.Public_data.supplement_code_gps  
      let index1 course = course.Public_data.supplement_code_helisa 
      let get_dip course = Some course.Public_data.supplement_diploma_level, course.Public_data.supplement_diploma_dpt
      let is_unallocated dip = dip = (None,None)
      let (dens:dip) = ((Some Public_data.DENS),None) 


  end 

module DMap(A:Double_keys) = 
  (struct 
    include A.KeyMap 
    type obj = A.obj 
    type dip = A.dip 
    type t = (obj*dip*dip) A.KeyMap.t 

    let extend x = (x,A.get_dip x,A.empty_dip) 
    let add x map state = 
      let key1 = A.index1 x in 
      let key2 = A.index2 x in 
      let obj = extend x in 

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
    let rest_in_dens (t:t) state = 
      state, A.KeyMap.map 
        (fun (obj,x,y) -> 
          if A.is_unallocated y then (obj, x, A.dens) 
          else (obj, x, y))
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
    
      
  end: DMap with type key = A.key and type obj = A.obj) 

module CourseDMap = DMap(Course) 