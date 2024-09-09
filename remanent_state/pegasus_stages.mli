type t
val empty: t

val get_pegasus_stages:
  firstname: string ->
  lastname: string ->
  t -> Public_data.stage_pegasus list

val add_pegasus_stage:
((string * int * int * int) ->
 'state ->
 Public_data.stage_pegasus ->
 Public_data.stage_pegasus -> 'state * Public_data.stage_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.stage_pegasus->
t ->
'state * t

val dump: t -> unit
