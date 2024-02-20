type t
val empty: t
val get_pegasus_pedagocial_registrations:
  firstname:string ->
  lastname:string ->
   t -> Public_data.pedagogical_entry_pegasus list

val add_pegasus_pedagocial_registrations:
((string * int * int * int) ->
 'state ->
 Public_data.pedagogical_entry_pegasus ->
 Public_data.pedagogical_entry_pegasus -> 'state * Public_data.pedagogical_entry_pegasus) ->
(string * int * int * int) -> 'state ->
Public_data.pedagogical_entry_pegasus->
t ->
'state * t

val dump: t -> unit
