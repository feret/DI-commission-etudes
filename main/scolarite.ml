let state = Remanent_state.init ()
let state =
  Cloud_interaction.safe_synchronize_shared_repository
    state
let state, students_list =
  Get_gps_files.get_students_list state
let state =
  Collect_scholarships.get_scholarships state
let state =
  Collect_mentoring.get_mentoring state
let state, (output_repository, _) =
  Cloud_interaction.get_dated_repository state
let state =
  List.fold_left
    (fun state id ->
       let state, output =
         Get_gps_files.get_student_file
           ~output_repository
           id state
       in
       let state,patched_file_opt =
         Get_gps_files.patch_student_file
           state
           ~input:output
           ~output:output
       in
       let state, gps =
         match patched_file_opt with
         | None -> state, None
         | Some input ->
           Transcripts.get_gps_file
             ~input state
       in
       let output =
         (fst output, (snd output)^".tex")
       in
       let state, _ =
         match gps with
       | None -> state, None
       | Some gps ->
         Transcripts.export_transcript
           ~output state gps
       in
       state
    )
    state
    students_list
let state =
  Cloud_interaction.make_current_repository state 
let state =
  Cloud_interaction.synchronize_shared_repository
    state
let state =
    Remanent_state.print_errors "" state
let state =
    Remanent_state.print_errors ~logger:(Remanent_state.std_logger) "" state
