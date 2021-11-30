type t

(** initialization *)
val init: unit -> t

(** error handling *)

(** log an exception *)
val warn:
(string * int * int * int)
-> string -> exn  -> t
-> t

val warn_and_log:
  ?logger:Loggers.t
  -> ?backgroundcolor:Sco_tools.Color.color
  -> ?textcolor:Sco_tools.Color.color
  -> ?lineproportion:float
  -> (string * int * int * int)
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
val fprintf:
  ?logger:Loggers.t -> t -> ('a, Format.formatter, unit) format -> 'a

val fprintf_verbatim:
  ?logger:Loggers.t -> t -> ('a, Format.formatter, unit) format -> 'a

val log:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float -> t -> ('a, Format.formatter, unit) format -> 'a

val log_to_string:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float ->
  ?english:string -> t -> string -> string

val log_string:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float ->
  ?english:string -> t -> string -> unit

val open_array:
  (string * int * int * int) ->
  ?logger:Loggers.t ->
  with_lines:bool -> ?size:float option list -> ?color: Color.color option list -> ?bgcolor:Color.color option list -> ?align:char option list -> title:string list list -> ?title_english: string list list -> t -> t
val close_array: ?logger:Loggers.t -> t -> unit
val open_row: ?logger:Loggers.t -> ?macro:string -> t -> unit
val close_row: ?logger:Loggers.t -> t -> unit
val print_cell: ?logger:Loggers.t -> ?english:string -> string -> t  -> unit
val print_optional_cell: ?logger:Loggers.t -> ?english:string -> string -> t  -> unit

val breakpage: ?logger:Loggers.t -> t -> unit
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
val get_cloud_support_dynamic_link: t -> t * bool
val get_pdfgenerator_engine: t -> t * Public_data.pdf_generator
val get_pdfgenerator_options: t -> t * string
val get_output_repository: t -> t * string
val get_output_alias_repository: t -> t * string
val get_output_alias: t -> t * (string * string) option
val set_output_alias: t -> (string * string) -> t

(** http access *)
val get_file_retriever:
  t -> t * Public_data.file_retriever
val get_file_retriever_options: ?more_options:(t -> t * string) -> t -> t * string

val get_annuaire_access_options: t -> t * string
val get_gps_access_options: t -> t * string
val get_file_retriever_log_repository: t -> t * string
val get_file_retriever_log_file: t -> t * string
val get_file_retriever_annuaire_tmp_repository: t -> t * string
val get_file_retriever_annuaire_html_file: t -> t * string
val get_file_retriever_time_out_in_second: t -> t * int option
val get_file_retriever_checking_period: t -> t * int

(** gps crawler *)
val get_machine_to_access_gps: t -> t * string
val get_port_to_access_gps: t -> t * string
val get_url_to_access_annuaire: t -> t * string
val get_repository_to_access_gps: t -> t * string
val get_repository_to_dump_gps_files:
  ?output_repository:string -> t -> t * string
val get_repository_for_handmade_gps_files: t -> t * string
val get_repository_for_backup_gps_files: t -> t * string
val get_signature: t -> t * string list
val get_store_output_according_to_their_promotions: t -> t * bool
val get_indicate_promotions_in_gps_file_names: t -> t * bool
val get_repository_to_dump_attestations: t -> t * string
val get_indicate_promotions_in_attestation_file_names: t -> t * bool

val get_url_prefix_for_photos: t -> t * string
val get_rel_url_prefix_for_photos: t -> t * string
val get_correct_rel_url_prefix_for_photos: t -> t * string
val get_include_pictures: t -> t * bool

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

val get_course_exceptions_list_prefix: t -> t * string
val get_course_exceptions_list_repository: t -> t * string

val get_course_name_translation_list_prefix: t -> t * string
val get_course_name_translation_list_repository: t -> t * string

val get_departments_list_prefix: t -> t * string
val get_departments_list_repository: t -> t * string

val get_programs_list_prefix: t -> t * string
val get_programs_list_repository: t -> t * string

val get_cursus_list_prefix: t -> t * string
val get_cursus_list_repository: t -> t * string

val get_cursus_exceptions_list_prefix: t -> t * string
val get_cursus_exceptions_list_repository: t -> t * string

val get_compensations_list_prefix: t -> t * string
val get_compensations_list_repository: t -> t * string

val get_decisions_list_prefix: t -> t * string
val get_decisions_list_repository: t -> t * string

val get_admissions_list_prefix: t -> t * string
val get_admissions_list_repository: t -> t * string

val get_dispenses_list_prefix: t -> t * string
val get_dispenses_list_repository: t -> t * string

val get_additional_courses_list_prefix: t -> t * string
val get_additional_courses_list_repository: t -> t * string

val get_modified_grades_list_prefix: t -> t * string
val get_modified_grades_list_repository: t -> t * string

val get_launching_date: t -> t * string
val get_comma_symbol: t -> t * char


val get_language: t -> t * Public_data.language
val get_repartition: t -> t * Public_data.repartition

type save_logger
val save_std_logger: t -> save_logger
val restore_std_logger: t -> save_logger -> t
val set_std_logger: t -> Loggers.t -> t

val std_logger: Loggers.t
val close_logger: ?logger:Loggers.t -> t -> t

val get_students:
  t -> Public_data.student_id list

val add_student:
  (string * int * int * int ->
   t ->
   Public_data.student_id ->
   Public_data.student_id -> t * Public_data.student_id) ->
  (string * int * int * int) -> Public_data.student_id -> t -> t

(** scholarships *)
val get_scholarship:
  firstname:string ->
  lastname:string -> t ->
  t * Public_data.scholarship option

val add_scholarship:
  (string * int * int * int ->
   t ->
   Public_data.scholarship ->
   Public_data.scholarship -> t * Public_data.scholarship) ->
  (string * int * int * int) ->
  Public_data.scholarship ->
  t ->
  t

val add_mentoring:
  (string * int * int * int ->
   t ->
   Public_data.tutorat ->
   Public_data.tutorat -> t * Public_data.tutorat) ->
  (string * int * int * int) ->
  Public_data.tutorat ->
  t ->
  t

val get_mentoring:
  firstname:string ->
  lastname:string ->
  year:Public_data.annee ->
  ?tuteur_gps:Public_data.tutorat ->
  t ->
  t * Public_data.tutorat option

val get_mentoring_list:
  ?firstname:string ->
  ?lastname:string ->
  ?tuteur_firstname:string ->
  ?tuteur_lastname:string ->
  ?year:Public_data.annee ->
  t ->
  t * Public_data.tutorat list

val get_course_exception:
  codegps:string ->
  year:Public_data.annee ->
  t ->
  t * Public_data.course_exception option

val add_course_exception:
  (string * int * int * int ->
   t ->
   Public_data.course_exception ->
   Public_data.course_exception-> t * Public_data.course_exception) ->
  (string * int * int * int) ->
  Public_data.course_exception ->
  t -> t


  val get_course_name_translation:
    codegps:string ->
    year:Public_data.annee ->
    t ->
    t * Public_data.course_name_translation option

  val add_course_name_translation:
    (string * int * int * int ->
     t ->
     Public_data.course_name_translation ->
     Public_data.course_name_translation-> t * Public_data.course_name_translation) ->
    (string * int * int * int) ->
    Public_data.course_name_translation ->
    t -> t

val add_dpt:
  (string * int * int * int ->
   t ->
   Public_data.dpt ->
   Public_data.dpt -> t * Public_data.dpt) ->

  (string * int * int * int) ->
  Public_data.dpt ->
  t ->
  t

val get_dpt:
  acronym:string ->
  t ->
  t * Public_data.dpt option

val add_program:
  (string * int * int * int ->
   t ->
   Public_data.program ->
   Public_data.program -> t * Public_data.program) ->
  (string * int * int * int) ->
  Public_data.program ->
  t -> t

val get_program:
  code_gps:string ->
  t -> t * Public_data.program option

val add_cursus:
  (string * int * int * int ->
   t ->
   Public_data.cursus ->
   Public_data.cursus -> t * Public_data.cursus) ->
    (string * int * int * int) ->
    Public_data.cursus ->
    t -> t

val get_cursus:
  year:string ->
  level:string ->
  ?dpt:Public_data.main_dpt  ->
  (string * int * int * int) ->
  t -> t * Public_data.cursus option

val list_all_cursus:
  t -> unit

val add_cursus_exception:
  (string * int * int * int ->
   t ->
   Public_data.cursus_exception ->
   Public_data.cursus_exception -> t * Public_data.cursus_exception) ->
  (string * int * int * int) ->
  Public_data.cursus_exception ->
  t -> t

val get_cursus_exception:
  firstname:string -> lastname:string ->
  year: string ->
  code_gps:string ->
  t -> t * Public_data.cursus_exception option

val add_decision:
  (string * int * int * int ->
   t ->
   Public_data.decision ->
   Public_data.decision -> t * Public_data.decision) ->
  (string * int * int * int) ->
  Public_data.decision ->
  t -> t

val get_decision:
  firstname:string -> lastname:string -> year:string ->
  program:string -> dpt:Public_data.main_dpt ->
  t -> t * Public_data.decision option

val get_decision_list:
  firstname:string -> lastname:string ->
  ?year:string -> ?program:string -> ?dpt:Public_data.main_dpt ->
    t -> t * Public_data.decision list

val add_admission:
  (string * int * int * int ->
   t ->
   Public_data.admission ->
   Public_data.admission -> t * Public_data.admission) ->
    (string * int * int * int) ->
    Public_data.admission ->
    t -> t

val get_admission:
    firstname:string -> lastname:string -> year:string ->
    t -> t * Public_data.admission option

val add_compensation:
  (string * int * int * int ->
   t ->
   Public_data.compensation ->
   Public_data.compensation -> t * Public_data.compensation) ->
  (string * int * int * int) ->
  Public_data.compensation ->
  t -> t

val get_compensation:
  firstname:string -> lastname:string -> year:string -> codecours:string
  -> t -> t * Public_data.compensation option

val add_dispense:
  (string * int * int * int ->
   t ->
   Public_data.dispense ->
   Public_data.dispense -> t * Public_data.dispense) ->
  (string * int * int * int) ->
  Public_data.dispense ->
  t -> t

val get_dispenses:
  ?firstname:string -> ?lastname:string -> ?year:string -> ?program:string -> ?dpt:string->
  t -> t * Public_data.dispense list

val add_additional_course:
  (string * int * int * int ->
   t ->
   Public_data.cours_a_ajouter ->
   Public_data.cours_a_ajouter -> t * Public_data.cours_a_ajouter) ->
  (string * int * int * int) ->
  Public_data.cours_a_ajouter ->
  t -> t

val get_additional_course:
  firstname:string -> lastname:string ->
  t -> t * Public_data.cours_a_ajouter list

val add_note_a_modifier:
  (string * int * int * int ->
   t ->
   Public_data.note_a_modifier ->
     Public_data.note_a_modifier -> t * Public_data.note_a_modifier) ->
    (string * int * int * int) ->
    Public_data.note_a_modifier ->
  t -> t

val get_note_a_modifier:
  firstname:string -> lastname:string -> code:string -> year:string ->
  t -> t * float option

val get_ects_a_modifier:
  firstname:string -> lastname:string -> code:string -> year:string ->
  t -> t * float option

val get_current_academic_year:
  t -> t * Public_data.annee

val get_picture_potential_locations:
  firstname:string ->
  lastname:string ->
  year:Public_data.annee -> t -> t * string list

val get_target:
  t -> t * string option

val add_mentor:
  t -> Public_data.mentor -> t

val get_mentors:
  t -> t * Public_data.mentor list

val get_repository_to_dump_mentors:
  t -> t * string

val add_dens:
  t -> Public_data.dens -> t

val get_dens:
  t -> t * Public_data.dens list

val get_repository_to_dump_dens:
  t -> t * string

val add_national_diploma:
  t -> Public_data.diplome_national -> t

val get_national_diplomas:
  t -> t * Public_data.diplome_national list

val get_repository_to_dump_national_diplomas:
  t -> t * string

val get_repository_to_dump_missing_pictures:
  t -> t * string

val get_missing_pictures:
  t -> t * Public_data.student list

val add_missing_picture:
  t -> Public_data.student -> t

val get_repository_to_dump_gps_server_faillures:
  t -> t * string

val add_gps_server_faillure:
  t -> Public_data.student -> t

val get_gps_server_faillures:
  t -> t * Public_data.student list

val add_missing_grade:
  t -> Public_data.missing_grade -> t

val get_missing_grades:
  t -> t * Public_data.missing_grade list

val get_repository_to_dump_missing_grades:
    t -> t * string

val add_non_accepted_grade:
    t -> Public_data.missing_grade -> t

val get_non_accepted_grades:
    t -> t * Public_data.missing_grade list

val get_repository_to_dump_non_accepted_grades:
  t -> t * string

val add_missing_mentor:
  t -> Public_data.missing_mentor -> t

val get_missing_mentors:
  t -> t * Public_data.missing_mentor list

val get_repository_to_dump_missing_mentors:
  t -> t * string

val add_missing_ects_attribution:
  t -> Public_data.missing_grade -> t

val get_missing_ects_attributions:
  t -> t * Public_data.missing_grade list

val add_missing_course_name_translation:
  t -> Public_data.course_name_translation -> t

val get_missing_course_name_translations:
  t -> t * Public_data.course_name_translation list

val get_repository_to_dump_missing_ects_attributions:
  t -> t * string

val get_repository_to_dump_missing_course_name_translations:
  t -> t * string

val add_missing_internship_description:
  t -> Public_data.missing_internship_description -> t

val get_missing_internship_descriptions:
  t -> t * Public_data.missing_internship_description list

val get_repository_to_dump_non_validated_internships:
  t -> t * string

val add_non_validated_internship:
  t -> Public_data.missing_internship_description -> t

val get_non_validated_internships:
  t -> t * Public_data.missing_internship_description list


val get_repository_to_dump_missing_internship_descriptions:
  t -> t * string

val add_ambiguous_internship_description:
  t -> Public_data.missing_internship_description -> t

val get_ambiguous_internship_descriptions:
  t -> t * Public_data.missing_internship_description list

val get_repository_to_dump_ambiguous_internship_descriptions:
    t -> t * string

val get_ENSPSL_logo: t -> t * string

val get_commission: t -> t * (string * Public_data.annee) option
val get_main_commission_rep: t -> t * string

val log_mkdir: t -> t * bool

val store_errors_and_profiling_info :
  (string * int * int * int -> t -> string -> t * string) ->
  (string * int * int * int -> t -> string -> string -> t) -> t -> t

val get_promo:
  firstname:string -> lastname:string -> t -> t*string option

val get_student_personnal_repository:
  ?promo:string -> firstname:string -> lastname:string -> t -> t * string

val get_promo_personnal_repository:
  promo:string -> t -> t * string

val get_promos_personnal_repository:
   t -> t * string

val get_students_personnal_files:
  ?language:Public_data.language -> promo:string  -> t -> t * string

val get_main_dpt: t -> t * Public_data.main_dpt
val is_main_dpt_di: t -> t * bool
val is_main_dpt_dma: t -> t * bool
val is_main_dpt_phys: t -> t * bool

val get_file_retriever_skip: t -> t * bool
val file_retriever_fail: t -> t

val get_commission_rep_from_key: ?commission_rep:string -> string -> t -> t * (string * string * string)

val get_commission_rep: ?commission_rep:string -> sous_commission:Public_data.sous_commission -> t -> t * (string * string * string)

val push_copy: input_rep:string -> output_rep:string -> file_name:string -> t -> t
val pop_copy:
  copy:(input_rep:string ->
        file_name:string -> output_rep:string -> t -> t)
  -> t -> t
val empty_copy:
  copy:(input_rep:string ->
        file_name:string -> output_rep:string -> t -> t)
  -> t -> t

val get_is_bilingual: t -> t * bool
val bilingual_string: ?english:string -> french:string -> t -> t * string
