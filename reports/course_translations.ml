type dump =
  ?codegps:string ->
  ?academicyear:string ->
  Gen.dump

module type ReportCourseEntries =
sig
  val dump: Gen.dump
end

let string_of_stringopt x =
  match x with
  | None -> ""
  | Some x -> x

module Buildtwo
    (I:Gen.Interface
      with type Missing_entry.entry = Public_data.course_entry
      and type Missing_entry.collector = Public_data.course_entry list) =
struct


  let headers =
    []

  let columns =
    [
      ["ENTREE GPS"],
      (fun a ->
         a.Public_data.gps_entry);
      ["LIBELLE"],
      (fun a -> string_of_stringopt a.Public_data.french_entry);
      ["LABEL"],
      (fun a ->
         string_of_stringopt a.Public_data.english_entry);
    ]

  let dump_missing_course_name_entry
      ?output_repository ?prefix ?file_name
      cmp headers columns state  =
    let event_opt =
      Some Profiling.Dump_course_entries
    in
    let filter = Gen.filter_course_entry in
    let default_file_name = I.default_file_name in
    let get_repository = I.Missing_entry.get_repository in
    let get = I.Missing_entry.get in
    Gen.dump_elts
      ?output_repository ?prefix ?file_name ?event_opt
      ~filter ~cmp ~headers ~columns ~get
      ~default_file_name ~get_repository
      state

  let dump
      ?output_repository ?prefix ?file_name
      state =
    let cmp =
      [
        Gen.lift_cmp
          (fun a -> a.Public_data.gps_entry)
      ]
    in
    dump_missing_course_name_entry
      ?output_repository ?prefix ?file_name cmp headers columns state

end


module MissingCourseEntries =
  Buildtwo
    (struct
      module Missing_entry = Remanent_state.Missing_course_entries
      let default_file_name = "course_entries.csv"
    end)

module CourseEntriesReport =
  Buildtwo
    (struct
      let default_file_name = "course_entries.csv"
      module Missing_entry =
      struct
      type entry = Public_data.course_entry
      type collector = entry list
      let get = Remanent_state.get_course_entries_report
      let get_repository =
        Remanent_state.get_repository_to_dump_course_entries_report
      let add a _ = a
      end
    end)
