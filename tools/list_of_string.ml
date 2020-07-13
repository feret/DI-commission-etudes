type stringlist =
  string list * string option

let add a (l,last) =
  match last with
  | None -> [], Some a
  | Some x -> x::l, Some a

let add_opt a_opt list =
  match a_opt with
  | None -> list
  | Some a -> add a list

let empty = ([],None)

let is_empty (_,a) = a=None

let to_string list sg pl =
  let _ = Format.flush_str_formatter () in
  let _ =
    Format.pp_print_list
      ~pp_sep:(fun log () -> Format.fprintf log ", ")
      (fun log a -> Format.fprintf log "%s" a)
      Format.str_formatter
      (fst list)
  in
  let _ =
    match list with
    | _, None -> ()
    | [], Some x -> Format.fprintf Format.str_formatter "%s %s" x sg
    | _, Some x -> Format.fprintf Format.str_formatter ", and %s %s " x
                     pl  in
  Format.flush_str_formatter ()
