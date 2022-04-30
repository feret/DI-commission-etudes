let get_univ ~diplome_dpt ~diplome_niveau ~diplome_year code state =
  match
    Remanent_state.get_cursus
      ~year:diplome_year
      ~level:diplome_niveau
      ~dpt:diplome_dpt
      ~gpscodelist:code
      __POS__
      state
  with
  | state, None -> state, None, None
  | state, Some x -> state, x.Public_data.cursus_univ, Some x 
