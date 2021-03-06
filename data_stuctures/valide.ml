type t = Public_data.valide
let to_string _pos  state t =
  match t with
  | Public_data.Abs -> state, "abs"
  | Public_data.Bool true-> state, "oui"
  | Public_data.Bool false -> state, "non"

let of_string ?context pos state s =
  if Tools.space_only s then
    state, None
  else
    let s = Special_char.correct_string s in
    if
      List.mem
        s ["o";"oui";"y";"yes";"ok"]
    then
      state, Some (Public_data.Bool true)
    else if
      List.mem
        s
        ["n";"non";"no"]
    then
      state, Some (Public_data.Bool false)
    else if
      List.mem s
        ["abs";"absent"]
    then
      state, Some Public_data.Abs
    else
      let context =
        match context with
        | None -> ""
        | Some s -> Format.sprintf " %s" s
      in
      let msg =
        Printf.sprintf
          "wrong format for the field validity (%s)%s" s
          context
      in
      Remanent_state.warn_dft
        pos
        msg
        Exit
        None
        state

let valide f =
  match f with
  | Public_data.Bool true -> Some true
  | Public_data.Bool false | Public_data.Abs -> Some false
