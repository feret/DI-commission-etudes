
val pred_safe:
  ('record -> 'b option) ->
  string ->
  'record Scan_csv_files.mandatory_field

val pred_opt_safe:
    ('record -> 'b option) ->
    string ->
    'record Scan_csv_files.mandatory_field

type ('data,'record_tmp,'record) gen =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:(Remanent_state.t -> 'record_tmp -> Remanent_state.t * 'data option) ->
  get:(Remanent_state.t -> 'record -> Remanent_state.t * 'data) ->
  set:(Remanent_state.t -> 'data -> 'record -> Remanent_state.t * 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp,'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_safe =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:('record_tmp -> 'data option) ->
  get:('record -> 'data) ->
  set:('data -> 'record -> 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp,'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_opt =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:(Remanent_state.t -> 'record_tmp -> Remanent_state.t * 'data option) ->
  get:(Remanent_state.t -> 'record -> Remanent_state.t * 'data option) ->
  set:(Remanent_state.t -> 'data option -> 'record -> Remanent_state.t * 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp, 'record) Keywords_handler.any_field

type ('data, 'record_tmp, 'record) gen_opt_safe =
  keyword:Public_data.keywords ->
  set_tmp:(Remanent_state.t ->
           string option -> 'record_tmp -> Remanent_state.t * 'record_tmp) ->
  get_tmp:('record_tmp -> 'data option) ->
  get:('record -> 'data option) ->
  set:('data option -> 'record -> 'record) ->
  field_name:string ->
  record_name:string ->
  pos:(string * int * int * int) ->
  ('record_tmp, 'record) Keywords_handler.any_field

type ('data,'record_tmp,'record) l_record =
  {
    elt: ('data,'record_tmp,'record) gen;
    safe: ('data,'record_tmp,'record) gen_safe;
    opt: ('data,'record_tmp,'record) gen_opt;
    opt_safe: ('data,'record_tmp,'record) gen_opt_safe
  }

type ('data,'record_tmp,'record) lift_record =
  'record_tmp -> 'record -> ('data,'record_tmp,'record) l_record

val string: (string, 'record_tmp, 'record) lift_record
val bool: (bool, 'record_tmp, 'record) lift_record
val int: (int, 'record_tmp, 'record) lift_record
val float: (float, 'record_tmp, 'record) lift_record
val gender: (Public_data.genre, 'record_tmp, 'record) lift_record
val color: (Color.color, 'record_tmp, 'record) lift_record
val main_dpt: (Public_data.main_dpt, 'record_tmp, 'record) lift_record
val experience: (Public_data.experience, 'record_tmp, 'record) lift_record 
val universite: (Public_data.universite, 'record_tmp, 'record) lift_record
