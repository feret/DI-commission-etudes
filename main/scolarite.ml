let state = Remanent_state.init ()
let state =
  Cloud_interaction.safe_synchronize_shared_repository
    state
let state, students_list =
  Get_gps_files.get_students_list state
let state, output_repository =
  Get_gps_files.get_dated_repository state
let state =
  List.fold_left
    (fun state id ->
       Get_gps_files.get_student_file
         ~output_repository
         id state)
    state
students_list
let state =
Cloud_interaction.synchronize_shared_repository
  state
