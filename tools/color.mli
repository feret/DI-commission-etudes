type rgb =
  {
    red: int;
    green: int;
    blue: int
  }

type color

val blue: color
val green: color
val red: color
val yellow: color
val white: color
val black: color

val get_background_color: color -> rgb
val get_font_color: color -> rgb
val build_color: ?background:rgb -> rgb -> color

val string_latex: rgb -> string
