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

(** log *)
val log:
  t -> ('a, Format.formatter, unit) format -> 'a
val flush:
  t -> unit

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

(** list of students *)
val get_students_list_prefix: t -> t * string
val get_students_list_repository: t -> t * string
