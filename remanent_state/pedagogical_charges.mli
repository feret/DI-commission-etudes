type t
val empty: t
val get_pedagogical_charge:
  year:string ->
  firstname:string ->
  lastname:string -> 
  ?gps_code:string -> 
  ?helisa_code:string -> 
  ?course:string -> t -> Public_data.pedagogical_charge option
val add_pedagogical_charge:
  ((string * int * int * int ) ->
   'state ->
   Public_data.pedagogical_charge ->
   Public_data.pedagogical_charge -> 'state * Public_data.pedagogical_charge) ->
  (string * int * int * int) -> 'state -> Public_data.pedagogical_charge -> t -> 'state * t
val get_pedagogical_charge_list:
  ?year:string ->
  ?lastname:string -> ?firstname:string ->
   ?gps_code:string ->  ?helisa_code:string ->  ?course:string -> 
    t -> Public_data.pedagogical_charge list
