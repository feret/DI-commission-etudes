val ifnum: cond:string -> ?bfalse:string -> btrue:string ->  unit -> string
val case:
  (cond:string -> ?bfalse:string -> btrue:string  -> unit -> string) ->
  (string * string) list ->
  otherwise:string -> string
val comment: string -> string
