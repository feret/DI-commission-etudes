type stringlist

val add: string -> stringlist -> stringlist
val add_opt: string option -> stringlist -> stringlist
val empty: stringlist
val is_empty: stringlist -> bool
val to_string: stringlist -> string -> string -> string 
