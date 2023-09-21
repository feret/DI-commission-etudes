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

let jd =
    {
      Public_data.direction_initiales = "JD";
      Public_data.direction_nom_complet = "Jérôme Delacotte";
      Public_data.direction_genre = Public_data.Masculin ;
      Public_data.direction_signature =
        Some Remanent_state.get_signature;
      Public_data.direction_titre = "Directeur des études";
      Public_data.direction_departement = "de chimie";

    }

    let pm =
        {
          Public_data.direction_initiales = "PM";
          Public_data.direction_nom_complet = "Patrick MEUNIER";
          Public_data.direction_genre = Public_data.Masculin ;
          Public_data.direction_signature =
            Some Remanent_state.get_signature;
          Public_data.direction_titre = "Directeur des études";
          Public_data.direction_departement = "de géosciences";

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

let dc =
  {
    Public_data.direction_initiales = "DC";
    Public_data.direction_nom_complet = "Djalil Chafaï";
    Public_data.direction_genre = Public_data.Masculin ;
    Public_data.direction_signature = None ;
    Public_data.direction_titre = "Directeur de la formation";
    Public_data.direction_departement = "de mathématiques";

  }

let _lb =
  {
    Public_data.direction_initiales = "LB";
    Public_data.direction_nom_complet = "Linda Boulevart";
    Public_data.direction_genre = Public_data.Feminin ;
    Public_data.direction_signature =
      None;
    Public_data.direction_titre = "Secrétaire pédagogique";
    Public_data.direction_departement = "d'informatique";
  }

  let msa =
    {
      Public_data.direction_initiales = "MSA";
      Public_data.direction_nom_complet = "Mohamed Salim Aboubacar";
      Public_data.direction_genre = Public_data.Masculin ;
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

let _ = am
let di_list = [jf;mp;msa]
let dma_list = [dc;mpe]
let phys_list = []
let ibens_list = []
let eco_list = []
let dri_list = []
let arts_list = []
let lila_list = []
let chimie_list = [jd]
let gsc_list = [pm]

let dpt_di = "informatique"
let dpt_dma = "mathématiques"
let dpt_phys = "physique"
let dpt_chimie = "chimie"
let dpt_ibens = "biologie"
let dpt_eco = "économie"
let dpt_dri = "relations internationales"
let dpt_arts = "arts"
let dpt_lila = "littératures et langage"
let dpt_gsc = "géosciences"

let footpage_string = "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 32 20 45 --  Fax : + 33 (0) 1 44 32 20 75 -- direction.etudes@di.ens.fr}"
let footpage_string_dma =
  "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : + 33 (0)1 44 31 72 45 --  Fax : + 33 (0) 1 44 32 20 69 -- education@math.ens.fr}"
let footpage_string_phys = ""
let footpage_string_ibens = ""
let footpage_string_chimie = "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : +33 (0)1 44 32 33 29 -- enseignement.chimie@ens.psl.eu}"
let footpage_string_gsc = "\\small{45, rue d'Ulm  75230 Paris Cedex 05  --  Tél. : +33 (0)1 44 32 22 91 -- enseignement@geosciences.ens.psl.eu}"
let footpage_string_eco = ""
let footpage_string_dri = ""
let footpage_string_arts = ""
let footpage_string_lila = ""
