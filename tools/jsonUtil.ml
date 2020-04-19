(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let build_msg s = "Not a correct "^s
let of_string (s:string) = `String s

let to_string ?error_msg:(error_msg=build_msg "string") =
  function
  | `String (s:string) -> s
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_int (s:int) = `Int s

let to_int ?error_msg:(error_msg=build_msg "int") =
  function
  | `Int (s:int) -> s
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_bool (s:bool) = `String (if s then "true" else "false")

let to_bool ?error_msg:(error_msg=build_msg "boolean") =
  function
  | `String "true" -> true
  | `String "false" -> false
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_unit () = `Null

let to_unit ?error_msg:(error_msg=build_msg "unit") =
  function
  | `Null -> ()
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_list to_json l =
  `List (List.rev_map to_json (List.rev l))

let to_list ?error_msg:(error_msg=build_msg "list") of_json = function
  | `List l as x ->
    begin
      try List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | `Null -> []
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_array to_json a =
  `List (Array.fold_right (fun x acc -> to_json x::acc) a [])

let to_array ?error_msg:(error_msg=build_msg "array") of_json = function
  | `List l -> Tools.array_map_of_list of_json l
  | `Null -> [||]
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_assoc to_json l =
  `Assoc (List.rev_map to_json (List.rev l))

let to_assoc
    ?error_msg:(error_msg=build_msg "association")
    of_json json =
  match json
  with
  | `Assoc l as x ->
    begin
      try
        List.rev_map of_json (List.rev l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | `Null -> []
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_pair ?(lab1="first") ?(lab2="second") to_json1 to_json2 (a,b) =
  `Assoc [ lab1, to_json1 a; lab2, to_json2 b ]

let to_triple
    ?lab1:(lab1="first") ?lab2:(lab2="second") ?lab3:(lab3="third")
    ?error_msg:(error_msg=build_msg "triple")
    of_json1 of_json2 of_json3 =
  function
  | `Assoc l as x when List.length l = 3 ->
    begin
      try
        of_json1 (List.assoc lab1 l),
        of_json2 (List.assoc lab2 l),
        of_json3 (List.assoc lab3 l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_triple ?(lab1="first") ?(lab2="second") ?(lab3="third")
      to_json1 to_json2 to_json3 (a,b,c) =
    `Assoc [ lab1, to_json1 a; lab2, to_json2 b ; lab3, to_json3 c]

let to_pair ?lab1:(lab1="first") ?lab2:(lab2="second")
    ?error_msg:(error_msg=build_msg "pair") of_json1 of_json2 =
  function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        of_json1 (List.assoc lab1 l),
        of_json2 (List.assoc lab2 l)
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error (error_msg,x))
    end
  | x ->
    raise (Yojson.Basic.Util.Type_error (error_msg,x))

let of_map
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    ~fold key_to_json value_to_json map =
  `List
    (List.rev
       (fold
          (fun (key:'key) (value:'value) (list:Yojson.Basic.t list) ->
             (`Assoc [
                 lab_key,key_to_json key;
                 lab_value,value_to_json value
               ])::list
          )
          map
          [])
    )

let to_map
    ?lab_key:(lab_key="key")
    ?lab_value:(lab_value="value")
    ?error_msg:(error_msg=build_msg "map")
    ~add ~empty json_to_key json_to_value =
  function
  | `List l ->
    List.fold_left
      (fun map x ->
         match x
         with `Assoc  l as x when List.length l = 2 ->
           begin
             try
               add
                 (json_to_key (List.assoc lab_key l))
                 (json_to_value (List.assoc lab_value l))
                 map
             with Not_found ->
               raise
                 (Yojson.Basic.Util.Type_error (error_msg,x))
           end
            | x ->
              raise
                (Yojson.Basic.Util.Type_error (error_msg,x)))
      empty
      l
  | x -> raise (Yojson.Basic.Util.Type_error (error_msg,x))
