let e_of_direction dir =
  match dir.Public_data.direction_genre with
  | Public_data.Masculin | Public_data.Unknown -> ""
  | Public_data.Feminin -> "e"

let mp =
  {
    Public_data.direction_initiales = "MP";
    Public_data.direction_nom_complet = "Marc Pouzet";
    Public_data.direction_genre = Public_data.Masculin ;
    Public_data.direction_signature = None ;
    Public_data.direction_titre = "Directeur des études";
    Public_data.direction_departement = "d'informatique";
  }

let jf =
  {
    Public_data.direction_initiales = "JF";
    Public_data.direction_nom_complet = "Jérôme Feret";
    Public_data.direction_genre = Public_data.Masculin ;
    Public_data.direction_signature =
      Some Remanent_state.get_signature;
    Public_data.direction_titre = "Directeur des études";
    Public_data.direction_departement = "d'informatique";

  }

let am =
  {
    Public_data.direction_initiales = "AM";
    Public_data.direction_nom_complet = "Ariane Mézard";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature = None ;
    Public_data.direction_titre = "Directeur de la formation";
    Public_data.direction_departement = "de mathématiques";

  }

let lb =
  {
    Public_data.direction_initiales = "LB";
    Public_data.direction_nom_complet = "Linda Boulevart";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature =
      None;
    Public_data.direction_titre = "Secrétaire pédagogique";
    Public_data.direction_departement = "d'informatique";
  }

  let mpe =
  {
    Public_data.direction_initiales = "MPE";
    Public_data.direction_nom_complet = "Marion Peres";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature =
      None;
    Public_data.direction_titre = "Secrétaire pédagogique";
    Public_data.direction_departement = "de mathématiques";
  }

let di_list = [jf;mp;lb]
let dma_list = [am;mpe]
let phys_list = []
let ibens_list = []
let eco_list = []

let dpt_di = "informatique"
let dpt_dma = "mathématiques"
let dpt_phys = "physique"
let dpt_ibens = "biologie"
let dpt_eco = "économie"

let footpage_string = "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 32 20 45 --  Fax : + 33 (0) 1 44 32 20 75 -- direction.etudes@di.ens.fr}"
let footpage_string_dma =
  "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 31 72 45 --  Fax : + 33 (0) 1 44 32 20 69 -- education@math.ens.fr}"
let footpage_string_phys = ""
let footpage_string_ibens = ""
let footpage_string_eco = ""
