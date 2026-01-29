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
      "INFO-L3-MIIME-S2"; 
      "INFO-L3-STAGE-S2" 
      ]  ; 
    Public_data.par_defaut = []; 
    Public_data.options = []; 
    Public_data.groups = []; 
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
        ]; 
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
      ]; 
    ]
  }