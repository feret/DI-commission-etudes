type t

(** initialization *)
val init: unit -> t

(** error handling *)

(** log an exception *)
val warn:
(string * int * int * int)
-> string -> exn  -> t
-> t

(** log an exception and output a default value *)
val warn_dft:
  (string * int * int * int)
  -> string -> exn  -> 'a -> t
  -> t * 'a

(** interrupt execution *)
val stop:
(string * int * int * int)
-> string -> exn  -> t
-> t

val print_errors:
  ?logger:Loggers.t -> string -> t -> t

  (** log *)
val log:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float -> t -> ('a, Format.formatter, unit) format -> 'a


val flush:
  ?logger:Loggers.t -> t -> unit

val print_newline:
  ?logger:Loggers.t -> t -> unit

(** profiling *)
val open_event_opt:
  Profiling.step_kind option -> t -> t

val close_event_opt:
  Profiling.step_kind option -> t -> t

(** Interaction with Cloud client *)
val get_cloud_synchronization_mode: t -> t * Public_data.cloud_synchronization_mode
val get_cloud_client: t -> t * Public_data.cloud_client
val get_cloud_repository: t -> t * string
val get_local_repository: t -> t * string
val get_distant_repository: t -> t * string
val get_cloud_client_options: t -> t * string

(** http access *)
val get_file_retriever:
  t -> t * Public_data.file_retriever
val get_file_retriever_options: t -> t * string
val get_file_retriever_log_repository: t -> t * string
val get_file_retriever_log_file: t -> t * string
val get_file_retriever_time_out_in_second: t -> t * int option
val get_file_retriever_checking_period: t -> t * int

(** gps crawler *)
val get_machine_to_access_gps: t -> t * string
val get_port_to_access_gps: t -> t * string
val get_repository_to_access_gps: t -> t * string
val get_repository_to_dump_gps_files: t -> t * string
val get_store_gps_files_according_to_their_promotions: t -> t * bool
val get_indicate_promotions_in_gps_file_names: t -> t * bool

(** CSV *)
val get_csv_separator: t -> t * char option

(** list of students *)
val get_students_list_prefix: t -> t * string
val get_students_list_repository: t -> t * string

(** list of scholarships *)
val get_scholarships_list_prefix: t -> t * string
val get_scholarships_list_repository: t -> t * string

(** list of mentoring *)
val get_monitoring_list_prefix: t -> t * string
val get_monitoring_list_repository: t -> t * string


val get_launching_date: t -> t * string

val get_comma_symbol: t -> t * char

type save_logger
val save_std_logger: t -> save_logger
val restore_std_logger: t -> save_logger -> t
val set_std_logger: t -> Loggers.t -> t

val std_logger: Loggers.t


(** scholarships *)
val get_scholarship:
  firstname:string ->
  lastname:string -> t ->
  t * Public_data.scholarship option

val add_scholarship:
  (string * int * int * int) ->
  Public_data.scholarship ->
  t ->
  t

val add_mentoring:
  (string * int * int * int) ->
  Public_data.tutorat ->
  t ->
  t

val get_mentoring:
  firstname:string ->
  lastname:string ->
  year:Public_data.annee ->
  ?tuteur_gps:Public_data.tutorat ->
  (string * int * int * int) ->
  t ->
  t * Public_data.tutorat option
