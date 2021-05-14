type dump =
  ?studentfirstname:string ->
  ?studentlastname:string ->
  ?mentorfirstname:string ->
  ?mentorlastname:string ->
  ?academicyear:string ->
  ?attributionyear:string ->
  ?promo:string ->
  ?title:((Loggers.t ->
           (string -> unit, Format.formatter, unit) format ->
           string -> unit) *
          string)
      list ->
  ?dpt:Public_data.main_dpt ->
  Gen.dump

module type ReportMentors =
sig
  val dump_per_year_mentor_student: correct_email:(string -> string) ->  dump
  val dump_per_year_student_mentor: correct_email:(string -> string) ->dump
  val dump_per_promo_mentor_student: correct_email:(string -> string) ->dump
  val dump_per_promo_student_mentor: correct_email:(string -> string) ->dump
  val dump_per_mentor_year_promo_student: correct_email:(string -> string) ->dump
  val dump_per_student: correct_email:(string -> string) ->dump
  val dump:
    ?studentfirstname:string ->
    ?studentlastname:string ->
    ?mentorfirstname:string ->
    ?mentorlastname:string ->
    ?academicyear:string ->
    ?attributionyear:string ->
    ?promo:string ->
    ?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
    ?dpt:Public_data.main_dpt ->
    ?output_repository:string ->
    ?prefix:string ->
    ?file_name:(string -> string -> string) ->
    Remanent_state.t -> Remanent_state.t

end

module ReportListMentors: ReportMentors
