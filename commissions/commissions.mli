val direction_etude:
  Remanent_state.t
    Public_data.direction_des_etudes
    Public_data.StringMap.t

val diplomes:
  Public_data.sous_commission
    Public_data.StringUnivMap.t

type direction_key = string
type sous_commission_key = string

val prepare_commission:
  commission_rep:string ->
  ?annee:string ->
  ?date_complete:string ->
  ?universites:(Public_data.universite list) ->
  ?signataires:(direction_key list) ->
  ?diplomes:((sous_commission_key * sous_commission_key option) list)  ->
  Remanent_state.t ->
  Remanent_state.t
