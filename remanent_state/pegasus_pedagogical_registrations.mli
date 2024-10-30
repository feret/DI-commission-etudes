type t
val empty: t
val get_pegasus_pedagocial_registrations:
  firstname:string ->
  lastname:string ->
   t -> Public_data.pedagogical_entry_pegasus list list

val add_pegasus_pedagocial_registrations:
((string * int * int * int) ->
 'state ->
 Public_data.pedagogical_entry_pegasus list ->
 Public_data.pedagogical_entry_pegasus list -> 'state * Public_data.pedagogical_entry_pegasus list) ->
(string * int * int * int) -> 'state ->
Public_data.pedagogical_entry_pegasus list->
t ->
'state * t

val dump: t -> unit
