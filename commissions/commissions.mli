val direction_etude:
  Remanent_state.t
    Public_data.direction_des_etudes
    Public_data.StringMap.t

val diplomes:
  Public_data.sous_commission
    Public_data.StringMap.t

type direction_key = string
type sous_commission_key = string

val prepare_commission:
  commission_rep:string ->
  ?annee:string ->
  ?date_complete:string ->
  ?signataires:(direction_key list) ->
  ?diplomes:(sous_commission_key list)  ->
  Remanent_state.t ->
  Remanent_state.t
