let math_l3_tronc_commun = 
  ["DMA-L3-A01-S1"; (* Algèbre 1 *)
   "DMA-L3-A02-S1"; (* Intégration et probabilités *)
   "DMA-L3-A03-S1"; (* Tolologie et calcul différentiel *)
   ]

let math_l3_tronc_commun_par_dft = 
  1, [ 
    "DMA-L3-A04-S2"; (* Analyse complexe *) (* Analyse Fontionnelle *) 
    "DMA-M1-B05-S2"; (* analyse fonctionnelle *)
  ]   
  
let build_l3_maths s = 
  { 
    Public_data.obligation = s::math_l3_tronc_commun ; 
    Public_data.par_defaut = [math_l3_tronc_commun_par_dft] ; 
    Public_data.options = []; 
    Public_data.groups = []; 
  }
let licence_maths = 
  build_l3_maths "DMA-L3-M01-S2" (* Mémoire et exposé mathematiques 1ère année *)
   
let licence_maths_mathsinfo = 
  build_l3_maths "INFO-M1-MPRI113-S2" (* Intitiation à la cryptologie *) 

let licence_maths_mathsphys = 
  build_l3_maths "DMA-L3-A05-S2" (* Grande dimension *) (* Relativité *)

let licence_maths_mathsbio = 
  build_l3_maths "UNDMA2-012" (* exposé math-bio *) 

let licence_info_mathsinfo = 
   { 
    Public_data.obligation = 
    [
      "INFO-L3-MIIME-S2";  (* Mémoire *)
      "INFO-L3-STAGE-S2";  (* Stage *)
      "INFO-L3-SYSRES-S2"; (* OS *)
      ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = [
27.,["INFO-L3-ALGOPRO-S1"; (* Algorithmique *) 
"INFO-L3-LAPROCO-S1"; (*Langages de programmation et compilation*) 
"INFO-L3-LAFORMCC-S1"; (* Langages formels *)
"INFO-L3-SYSDIG-S1" ; (* Systèmes numériques *) 

"INFO-L3-APPREN-S2"; (*Apprentissage statistique*) 
"INFO-L3-DB-S2" ; (* BDD *)
"INFO-L3-LOGIN-S2"; (*Lambda calcul et logique informatique*)
"INFO-L3-SAA-S1"; (* SSA *)
"INFO-L3-SEMVP-S2" ; (* Sémantique *) 
"INFO-L3-THEOIC-S2" ; (* Théorie de l'information *)
"INFO-M1-MPRI123-S2"; (* Apprentissage scientifique par la pratique *)

    ]]; 
    Public_data.groups = []; 
  }

let licence_info = 
   { 
    Public_data.obligation = 
    [
      "INFO-L3-STAGE-S2";  (* Stage *)
      "INFO-L3-PRATIQUE-S1"; (* Info pratique *)
      ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = [48.,[

"INFO-L3-ALGOPRO-S1"; (* Algorithmique *) 
"INFO-L3-LAPROCO-S1"; (*Langages de programmation et compilation*) 
"INFO-L3-LAFORMCC-S1"; (* Langages formels *)
"INFO-L3-SYSDIG-S1" ; (* Systèmes numériques *) 

"INFO-L3-APPREN-S2"; (*Apprentissage statistique*) 
"INFO-L3-DB-S2" ; (* BDD *)
"INFO-L3-LOGIN-S2"; (*Lambda calcul et logique informatique*)
"INFO-L3-SAA-S1"; (* SSA *)
"INFO-L3-SEMVP-S2" ; (* Sémantique *) 
"INFO-L3-THEOIC-S2" ; (* Théorie de l'information *)
"INFO-L3-SYSRES-S2"; (* OS *)
"INFO-M1-MPRI123-S2"; (* Apprentissage scientifique par la pratique *)
"INFO-M1-MPRI113-S2" (* Intitiation à la cryptologie *) 
]]; 
    Public_data.groups = []; 
  }


let m1_info = 
   { 
    Public_data.obligation = 
    [ ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = [30.,[
"INFO-M1-LEARNING-S1" ; (* Apprentissage profond *)
"INFO-M1-VISA-S1" ;  (*Introduction à la vision artificielle *) 
"INFO-M1-OPTCOMB-S1" ; (*Optimisation Combinatoire *) 
"INFO-M1-MPRI119-S1" ; (* Robotique *)
"INFO-M1-MPRI120-S1" ; (* Lambda-calcul *)
"INFO-M1-REAC-S1" ; (* Reactive *)
"INFO-M1-OPTCONV-S1" ; (* Optimisation Convexe*)
"INFO-M1-MPRI117-S1" ; (* Complexité*)
"INFO-M1-MODRES-S1" ; (* MOD RED *)
]]; 
    Public_data.groups = [1, ["INFO-M1-STAGEE-S2";"INFO-M1-STAGEFF-S2"]]; 
  }

let m1_maths = 
   { 
    Public_data.obligation = []; 
    Public_data.options = []; 
    Public_data.par_defaut = []; 
    Public_data.groups = [
      3 (* cours fondamentaux *)
      , [
        "DMA-M1-B01-S1"; (* algèbre 2*)
        "DMA-L3-A04-S2"; (* analyse complexe *)
        "DMA-M1-B05-S2"; (* analyse fonctionnelle *)
        "DMA-M1-B06-S2"; (* géométrie différentielle *)
        "DMA-M1-B03-S1"; (* logique *)
        "DMA-M1-B02-S1"; (* processus stochastique *)
        "DMA-L3-A06-S2"; (* optimisation et transport optimal *)
        ]; 
      1, [
        "DMA-M1-C01-S1"; (* analyse des EDP *)
        "UNDMA2-088"; (* compléments de probabilités *)
        "DMA-M1-C03-S1"; (* mathématiques des données *)
        "DMA-M1-B04-S1"; (* statistique *)
        "DMA-M1-C02-S1"; (* systèmes dynamiques *)
        "DMA-M1-C04-S2"; (* topôlogie algébrique *)
        "UNDMA1-086"; (* convergences de spectres et notes fondamentales *)]; 
      1, [
      "DMA-M1-GT1-S1"; (* gt - theorie cinetique et theoreme de lanford *)
      "DMA-M1-GT2-S1"; (* gt - fatou, julia, et les fondements de la dynamique holomo *)
      "DMA-M1-GT3-S1"; 
      "DMA-M1-GT4-S1"; (* gt - congruence ciseaux *)
      "DMA-M1-GT5-S1"; (* gt - theorie spectrale pour la mecanique quantique *)
      "DMA-M1-GT6-S1"; (* gt - autour du modele d’ising *)
      "DMA-M1-GT7-S1"; (* gt - entropie : entre physique statistique, probabilites et *)
      "DMA-M1-GT8-S1"; (* gt polymeres *)
      "DMA-M1-GT9-S1"; (* gt - Limites locales de cartes planaires et empilement de cercles *) 
      "UNDMA1-081"; (* GT - Domaines d’holomorphie et variétés de Stein *)  
      "UNDMA1-082"; (* GT - Introduction à la géométrie sous-riemannienne *)  
      "UNDMA1-083"; (* GT - Représentations l-adiques et la fonction tau de Ramanujan*) 
      "UNDMA1-084"; (* GT - Théorie algébrique des équations différentielles en caractéristiques nulle et positive *)  
      "UNDMA1-085"; (* GT - Transport optimal et applications*)];
      ]
  }

  let licence_phys_mathsphys = 
 { 
    Public_data.obligation = 
    [
    "PHYS-L3-B11-S2" (* Stage*) ; 
    "DMA-L3-M03-S2" (* Mémoire Maths/Phys *)
      ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = []; 
    Public_data.groups = [4, 
    ["PHYS-L3-B09-S2" (*Hydrodynamique *); 
    "PHYS-L3-B10-S2" (*Physique du solide*); 
    "PHYS-L3-B03-S2" (*Relativité et électromagnétisme  *); 
    "PHYS-L3-A02-S1" (*Introduction à la mécanique quantique I *); 
    "PHYS-L3-A01-S1" (*Physique statistique des systèmes en équilibre*); ]]
  }


   let licence_bio_mathsbio = 
 { 
    Public_data.obligation = 
    [
     "DMA-L3-A06-S2"; (* optimisation et transport optimal *) 
      "DMA-M1-B05-S2"; (* analyse fonctionnelle *)
      "BIO-IN-G02-S1"; (* Sciences du vivant *)
      "BIO-IN-G03-S2"; (* bio de la cellule *)
      "BIO-IN-G10-S2"; (* gdt bio *)
      ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = []; 
    Public_data.groups = [2, [(* TO DO *)]; 2, [ (* TO DO *)]]
  }

  (*
"PHYS-L3-A05-S1" (*Eléments de m´ ecanique analytique *)
"PHYS-L3-A03-S1" (*Mathématiques pour physiciens *)
"PHYS-L3-A05-S1" (*Eléments de m´ ecanique analytique *)
"PHYS-L3-B12-S1" (*Python pour scientifiques I : traitement des donnÉes et
interfac¸age d’expÉriences *)
"PHYS-L3-B14-S1" (*Physique expérimentale II*) 
"PHYS-L3-B15-S1" (*Physique exp´ erimentale I *) 
"PHYS-L3-B16-S2" (*Introduction ` a l’Astrophysique *)
"PHYS-L3-B19-S2" (*Introduction à la mécanique quantique II*) 

"PHYS-L3-C20-S2" (*Optique*)
"UNPHY1-121" (*Mécanique des milieux continus *)
*) 

