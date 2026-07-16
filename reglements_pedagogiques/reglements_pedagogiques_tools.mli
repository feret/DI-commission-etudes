module type Double_keys = 
    sig 
        type obj 
        type key
        type dip 
        val empty_dip: dip 
        module KeyMap:Map.S with type key = key 
        module KeySet:Set.S with type elt = key 
        val index1: obj -> key option 
        val index2: obj -> key option 
        val get_dip: obj -> dip 
        val string_of_dip: dip -> string 
        val get_ects: obj -> float 
         val get_note: obj -> Public_data.note 
        val get_validation: obj -> Public_data.valide 
        val get_year: obj -> string 
        val is_unallocated: dip -> bool 
         val check_dip_compatibility: (Public_data.diploma_level option * Public_data.main_dpt option)  -> dip -> bool  
                       
        val dens: dip 
        val unassigned: dip 
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
    val select_course_for_a_cursus_list:  (dip * Public_data.reglement_diplome) list -> t -> Remanent_state.t -> Remanent_state.t * t  * (dip * (int * key list) list * float) list 
    val select_course_for_dens_instead_of_dip: (dip * Public_data.reglement_diplome) list -> t -> Remanent_state.t -> Remanent_state.t * t  * (dip * (int * key list) list * float) list 
   val select_experience_in_bonus: Public_data.exp_allocation_map  ->  t -> Remanent_state.t -> Remanent_state.t *   
      ((Public_data.cours_supplement *
           (key *
            (dip * string option))) *
          (key *
           (dip * string option)) *
            Public_data.valide)
         list Public_data.StringMap.t Public_data.YearMap.t



    val export: Remanent_state.t  -> t -> (dip * (int * key list) list * float) list -> Remanent_state.t * ((dip * string option) * int * key list) list * (obj * (dip * string option) * (dip * string option)) Public_data.StringMap.t Public_data.YearMap.t

    val print: Remanent_state.t -> (Remanent_state.t -> (obj * (dip * string option) * (dip * string option))  -> Remanent_state.t) -> ((dip * string option)  * int * key list) list ->
    (obj * (dip * string option) * (dip * string option))  Public_data.StringMap.t Public_data.YearMap.t -> Remanent_state.t * bool 


val print_short: Remanent_state.t -> (Remanent_state.t -> (obj * (dip * string option) * (dip * string option))  -> Remanent_state.t) -> ((dip * string option)  * int * key list) list ->
    (obj * (dip * string option) * (dip * string option))  Public_data.StringMap.t Public_data.YearMap.t -> Remanent_state.t * bool 

val print_short_list: Remanent_state.t -> 
    (Remanent_state.t -> ((obj  *
           (string *
            (dip * string option))) *
          (string *
           (dip * string option)) *
          Public_data.valide)  -> Remanent_state.t) -> ((dip * string option)  * int * key list) list ->
   ((obj  *
           (string *
            (dip * string option))) *
          (string *
           (dip * string option)) *
          Public_data.valide)
    list  Public_data.StringMap.t Public_data.YearMap.t -> Remanent_state.t * bool 

end


module DMap(A:Double_keys with type key = string and type obj = Public_data.cours_supplement and type dip = Public_data.diploma_level option * Public_data.main_dpt option): (DMap with type key = A.key and type obj = A.obj and type dip = A.dip)

module CourseDMap: (DMap with type  obj = Public_data.cours_supplement and type key = string and type dip =  Public_data.diploma_level option * Public_data.main_dpt option and type t = (Public_data.cours_supplement * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option) * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option)) Public_data.StringMap.t )

