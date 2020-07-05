type rgb =
  {
    name: string;
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
  {name="lightgreen";red=204; green=255;blue=204}
let dark_green =
  {name="darkgreen";red=0;green=128;blue=0}
let light_blue =
  {name="lightblue";red=204; green=255;blue=255}
let dark_blue =
  {name="darkblue";red=0;green=0;blue=128}
let light_yellow =
  {name="lightyellow";red=255; green=255;blue=153}
let dark_yellow =
  {name="darkyellow";red=255;green=255;blue=0}
let light_red =
  {name="lightred";red=255; green=128;blue=128}
let light_orange =
  {name="lightorange";red=255;green=207;blue=150}
let dark_red =
  {name="darkred";red=255;green=0;blue=0}
let dark_orange =
  {name="darkorange";red=255;green=127;blue=80}
let white =
  {name="white";red=255;green=255;blue=255}
let black=
  {name="black";red=0;green=0;blue=0}
let light_duck_blue =
  {name="lightduckblue";red=27;green=152;blue=177}
let dark_duck_blue =
  {name="darkduckblue";red=31;green=77;blue=86}

let label c = c.name
let rgb_code c = c.red, c.green, c.blue

let rgb_list =
  [
    light_green;dark_green;
    light_blue;dark_blue;
    light_yellow;dark_yellow;
    light_orange;dark_orange;
    light_red;dark_red;
    light_duck_blue; dark_duck_blue;
    black;white
  ]

let duckblue = build_color ~background:light_duck_blue dark_duck_blue
let blue = build_color ~background:light_blue dark_blue
let green = build_color ~background:light_green dark_green
let yellow = build_color ~background:light_yellow dark_yellow
let red = build_color ~background:light_red dark_red
let black = build_color black
let white= build_color white
let orange = build_color ~background:light_orange dark_orange

let color_list =
  [blue;green;yellow;red;black;white;orange;duckblue]

let color_of_string s =
  match s with
  | "duckblue" | "bleucanard" | "duck blue" | "bleu canard"
    -> Some duckblue 
  | "blue" | "bleu" -> Some blue
  | "green" | "vert" -> Some green
  | "yellow" | "jaune" -> Some yellow
  | "red" | "rouge"-> Some red
  | "black" | "noir" -> Some black
  | "white" | "blanc" -> Some white
  | "orange" -> Some orange
  | _ -> None
