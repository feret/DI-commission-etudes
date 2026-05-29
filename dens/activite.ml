let l = ["INFO-AA-ACM-A", (fun y -> 
{
 Public_data.supplement_code_helisa=Some "UNEXPA-01";
 Public_data.supplement_code_gps=None; 
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience d'ouverture hors-les-murs sans ECTS ";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
 Public_data.supplement_diploma_level = Public_data.DENS ; 
Public_data.supplement_diploma_dpt= None; 
Public_data.supplement_note = Public_data.Valide_sans_note; 
Public_data.supplement_validation = Public_data.Bool true; 
});
"UNINF1-042", (fun y -> 
{
 Public_data.supplement_code_helisa=Some "UNEXPA-38";
 Public_data.supplement_code_gps=None; 
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience de recherche dans le cadre d'un enseignement déjà sélectionné dans un autre département";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
Public_data.supplement_diploma_level = Public_data.DENS ; 
Public_data.supplement_diploma_dpt= None; 
Public_data.supplement_note = Public_data.Valide_sans_note; 
Public_data.supplement_validation = Public_data.Bool true; 

}) ;
"INFO-M1-PROJRECH-S1", (fun y -> 
{
 Public_data.supplement_code_helisa=Some "UNEXPA-38";
 Public_data.supplement_code_gps=None; 
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience de recherche dans le cadre d'un enseignement déjà sélectionné dans un autre département";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
  Public_data.supplement_diploma_level = Public_data.DENS ; 
Public_data.supplement_diploma_dpt= None; 
Public_data.supplement_note = Public_data.Valide_sans_note; 
Public_data.supplement_validation = Public_data.Bool true; 

});
"ACTD-OrgSE-S1", (fun y -> 
{
 Public_data.supplement_code_helisa=Some "UNEXPA-38";
 Public_data.supplement_code_gps=None; 
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Expérience de recherche dans le cadre d'un enseignement déjà sélectionné dans un autre département";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y; 
 Public_data.supplement_diploma_level = Public_data.DENS ; 
 Public_data.supplement_diploma_dpt= None; 
 Public_data.supplement_note = Public_data.Valide_sans_note; 
Public_data.supplement_validation = Public_data.Bool true; 

});
"ACTD-TAL-A", (fun y -> 
{
 Public_data.supplement_code_helisa= Some "UNEXP1-14";
 Public_data.supplement_code_gps= None;
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="Tutorat-Talens lycée S1";
 Public_data.supplement_ects=0.;
 Public_data.supplement_dens=true;
 Public_data.supplement_extra = true;
 Public_data.supplement_validation_year = y;
 Public_data.supplement_diploma_level = Public_data.DENS ; 
 Public_data.supplement_diploma_dpt= None; 
 Public_data.supplement_note = Public_data.Valide_sans_note; 
 Public_data.supplement_validation = Public_data.Bool true; 

});
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