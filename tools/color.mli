type rgb =
  {
    name:string;
    red: int;
    green: int;
    blue: int
  }

type color

val blue: color
val green: color
val orange: color
val red: color
val yellow: color
val white: color
val black: color

val rgb_list: rgb list
val color_list: color list
val get_background_color: color -> rgb
val get_font_color: color -> rgb
val build_color: ?background:rgb -> rgb -> color

val rgb_code: rgb -> int*int*int
val label: rgb -> string

val color_of_string: string -> color option
