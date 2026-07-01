type dump =
 ?firstname:string ->
    ?lastname:string ->
    ?academicyear:string ->
    ?attributionyear:string -> 
?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
Gen.dump

module type ReportRepartition =
sig
  val dump_per_year_course: correct_email:(string -> string) ->  dump
  val dump_per_year_teacher: correct_email:(string -> string) ->dump
 (* val dump_per_course_teacher: correct_email:(string -> string) ->dump
  val dump_per_course_year: correct_email:(string -> string) ->dump
  val dump_per_teacher_course: correct_email:(string -> string) ->dump
  val dump_per_teacher_year: correct_email:(string -> string) ->dump*) 
  
  val dump:
    ?firstname:string ->
    ?lastname:string ->
    ?academicyear:string ->
    ?attributionyear:string -> 
    ?title:((Sco_remanent_state.Loggers.t ->
                    (string -> unit, Format.formatter, unit) format ->
                    string -> unit) *
                   string)
                  list ->
    ?output_repository:string ->
    ?prefix:string ->
    ?file_name:(string -> string -> string) ->
    Remanent_state.t -> Remanent_state.t

end

module ReportListRepartition: ReportRepartition 
