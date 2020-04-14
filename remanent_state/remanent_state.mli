type t

val init: unit -> t

val warn_dft:
  (string * int * int * int)
  -> string -> exn  -> 'a -> t
  -> t * 'a
val warn:
(string * int * int * int)
-> string -> exn  -> t
-> t
val stop:
(string * int * int * int)
-> string -> exn  -> t
-> t

val get_cloud_synchronization_mode: t -> t * Public_data.cloud_synchronization_mode
val get_cloudclient: t -> t * Public_data.cloud_client
val get_cloud_repository: t -> t * string 
val get_local_repository: t -> t * string
val get_distant_repository: t -> t * string
val get_cloudclient_option: t -> t * string

val get_file_retriever:
  t -> t * Public_data.file_retriever
val get_file_retriever_options: t -> t * string
val get_machine_to_access_gps: t -> t * string
val get_port_to_access_gps: t -> t * string
val get_repository_to_access_gps: t -> t * string
val get_repository_to_dump_gps_files: t -> t * string

val get_students_list_prefix: t -> t * string
val get_students_list_repository: t -> t * string
