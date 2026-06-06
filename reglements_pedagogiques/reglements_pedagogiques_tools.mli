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
    val find_opt: key -> t -> Remanent_state.t -> Remanent_state.t * (obj*(dip*string option)*(dip*string option)) option 
    val fold: (dip -> obj -> 'a -> 'a) -> t -> 'a -> 'a 
    val filter_out: dip list -> t -> Remanent_state.t -> Remanent_state.t * t 
    val select_course_for_a_cursus_list:  (dip * Public_data.reglement_diplome) list -> t -> Remanent_state.t -> Remanent_state.t * t  
* (dip * (int * key list) list * float) list  
    val export: Remanent_state.t  -> t -> (dip * (int * key list) list * float) list -> Remanent_state.t * ((dip * string option) * int * key list) list * (obj * (dip * string option) * (dip * string option)) Public_data.StringMap.t Public_data.YearMap.t

    val print: Remanent_state.t -> (Remanent_state.t -> (obj * (dip * string option) * (dip * string option))  -> unit) -> ((dip * string option)  * int * key list) list ->
    (obj * (dip * string option) * (dip * string option))  Public_data.StringMap.t Public_data.YearMap.t -> Remanent_state.t
end

module DMap(A:Double_keys with type key = string): (DMap with type key = A.key)

module CourseDMap: (DMap with type  obj = Public_data.cours_supplement and type key = string and type dip =  Public_data.diploma_level option * Public_data.main_dpt option and type t = (Public_data.cours_supplement * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option) * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option)) Public_data.StringMap.t )

