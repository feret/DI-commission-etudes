let ifnum ~cond ?bfalse ~btrue  () =
  match bfalse with
  | Some bfalse ->
    Format.sprintf
      "\\ifnum%s%%\n\ %s%%\n\ \\else%%\n\ %s%%\n\ \\fi%%\n\ "
      cond btrue bfalse
  | None ->
  Format.sprintf
    "\\ifnum%s%%\n\ %s%%\n\ \\fi%%\n\ "
    cond btrue

let rec case fun_if list ~otherwise =
  match list with
  | [] -> otherwise
  | (cond,btrue)::tail ->
    let bfalse = Some (case fun_if tail ~otherwise) in
    fun_if ~cond ?bfalse ~btrue ()

let comment string =
  Format.sprintf
    "\\comment{%s}" string
