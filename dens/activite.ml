let l = ["INFO-AA-ACM-A", (fun y -> 
{
 Public_data.supplement_code="UNEXPA-01";
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience d'ouverture hors-les-murs sans ECTS ";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
});
"UNINF1-042", (fun y -> 
{
 Public_data.supplement_code="UNEXPA-38";
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience de recherche dans le cadre d'un enseignement déjà sélectionné dans un autre département";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
})
]

let map = 
  List.fold_left 
    (fun map (a,b) -> 
         let old = 
          match Public_data.StringMap.find_opt a map with 
            | None -> []
            | Some a -> a
         in 
         Public_data.StringMap.add a (b::old) map)
   Public_data.StringMap.empty l 


let free_exp_list a y = 
  match Public_data.StringMap.find_opt a map with 
  | None -> []
  | Some a -> List.rev_map (fun a -> a y) (List.rev a) 