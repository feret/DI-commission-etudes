val ifnum: cond:string -> ?bfalse:string -> btrue:string ->  string
val case:
  (cond:string -> ?bfalse:string -> btrue:string  -> string) ->
  (string * string) list ->
  otherwise:string -> string
val comment: string -> string 
