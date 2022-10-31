let get_univ ~firstname ~lastname ~diplome_dpt ~diplome_niveau ~diplome_year code state =
  match
    Remanent_state.get_cursus
      ~firstname ~lastname
      ~year:diplome_year
      ~level:diplome_niveau
      ~dpt:diplome_dpt
      ~gpscodelist:code
      __POS__
      state
  with
  | state, None -> state, None, None
  | state, Some x -> state, x.Public_data.cursus_univ, Some x

let get_univ ~diplome_dpt ~diplome_niveau ~diplome_year ~firstname ~lastname code state =
  let state, a, b = get_univ ~firstname ~lastname ~diplome_dpt ~diplome_niveau ~diplome_year code state in
  let state, univ_opt =
    Remanent_state.get_inscription
    ~year:diplome_year
    ~level:diplome_niveau
    ~dpt:diplome_dpt
    ~firstname
    ~lastname
    state
  in
  match univ_opt with
  | None -> state,a,b
  | Some a -> state,a.Public_data.inscription_univ,b
