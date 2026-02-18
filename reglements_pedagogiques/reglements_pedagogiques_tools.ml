module type Double_keys = 
    sig 
        type obj 
        type key
        type dip 
        val empty_dip: dip 
        module KeyMap:Map.S with type key = key 
        val index1: obj -> key 
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
    val rest_in_dens: t -> Remanent_state.t -> Remanent_state.t * t 
    val filter_out: dip list -> t -> Remanent_state.t -> Remanent_state.t * t 
 end

module Course = 
    struct 
      type obj = Public_data.pedagogical_entry_pegasus
      type key = string 
      type dip = string option * Public_data.main_dpt option 
      let empty_dip = None,None  
      module KeyMap=Map.Make (struct type t=string let compare = compare end)
      let index2 course = course.Public_data.pe_code_gps 
      let index1 course = course.Public_data.pe_code_helisa 
      let get_dip course = course.Public_data.pe_diploma, course.Public_data.pe_dpt 
      let is_unallocated dip = dip = (None,None)
      let dens = (Some "dens",None) 

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
               
      
    let filter_out l t state = 
      state, A.KeyMap.filter 
        (fun _ (_, x, _) -> not (List.mem x l)) t  
    
      
  end: DMap with type key = A.key) 

module CourseDMap = DMap(Course) 