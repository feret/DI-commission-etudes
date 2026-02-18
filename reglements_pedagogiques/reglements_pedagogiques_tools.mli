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

module DMap(A:Double_keys): (DMap with type key = A.key)

module CourseDMap: DMap 

