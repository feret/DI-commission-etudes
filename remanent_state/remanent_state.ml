type t =
  {
    parameters : unit ;
    error_log : unit option ;
    profiling : unit option ;
  }

let init () =
  {
    parameters = ();
    error_log = None ;
    profiling = None ;
  }

let warn_dft _pos _message _exn default state =
  state,default

let warn pos message exn state =
  fst (warn_dft pos message exn () state)

let stop pos message exn state =
  let state = warn pos message exn state in
  let () = exit 1 in
  state

let get_cloud_synchronization_mode t =
  t, Public_data.Manual

let get_cloudclient t =
  t, Public_data.NextCloudCmd

let get_cloud_repository t =
  t,"/users/absint3/feret/Nextcloud"

let get_local_repository t =
  let t, cloud =
    get_cloud_repository t
  in
  t,Printf.sprintf "%s/di/direction_des_etudes" cloud

let get_distant_repository t =
  t,"https://cloud.di.ens.fr/"

let get_cloudclient_option t =
  t,"-n"

let  get_file_retriever state =
  state, Public_data.WGET

let get_file_retriever_options state =
  state, ""

let get_machine_to_access_gps state =
  state,"violette.ens.fr"

let get_port_to_access_gps state =
  state,"8080"

let get_repository_to_access_gps state =
  state, "gps"

let get_repository_to_dump_gps_files state =
  state, "gps_files"

let get_students_list_prefix state =
  state, "etudiants"

let get_students_list_repository state =
  let state, main = get_local_repository state in
  let state, repository = get_students_list_prefix state in
  state, Printf.sprintf "%s/%s" main repository
