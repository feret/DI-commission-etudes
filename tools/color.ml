type rgb =
  {
    red: int;
    green: int;
    blue: int
  }

type color =
  {
    text:rgb;
    background:rgb;
  }

let get_background_color c = c.background
let get_font_color c = c.text

let build_color ?background text =
  let background =
    match background with
    | None -> text
    | Some b -> b
  in
  {background;text}

let light_green =
  {red=204; green=255;blue=204}
let dark_green =
  {red=0;green=128;blue=0}
let light_blue =
  {red=204; green=255;blue=255}
let dark_blue =
  {red=0;green=0;blue=128}
let light_yellow =
  {red=255; green=255;blue=153}
let dark_yellow =
  {red=255;green=255;blue=0}
let light_red =
  {red=255; green=128;blue=128}
let light_orange =
  {red=255;green=207;blue=150}
let dark_red =
  {red=255;green=0;blue=0}
let dark_orange =
  {red=255;green=127;blue=80}
let black=
  {red=255;green=255;blue=255}
let white=
  {red=0;green=0;blue=0}

let blue = build_color ~background:light_blue dark_blue
let green = build_color ~background:light_green dark_green
let yellow = build_color ~background:light_yellow dark_yellow
let red = build_color ~background:light_red dark_red
let black = build_color black
let white= build_color white
let orange = build_color ~background:light_orange dark_orange

let string_latex rgb =
  Format.sprintf
    "{RGB}{%i,%i,%i}" rgb.red rgb.green rgb.blue
