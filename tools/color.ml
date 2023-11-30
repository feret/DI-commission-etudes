type rgb =
  {
    name: string;
    red: int;
    green: int;
    blue: int
  }

type color =
  {
    label:string;
    text:rgb;
    background:rgb;
  }

let get_background_color c = c.background
let get_font_color c = c.text

let build_color ?background text label =
  let background =
    match background with
    | None -> text
    | Some b -> b
  in
  {label;background;text}

let light_pink =
  {name="lightpink";red=250;green=218;blue=221}
let dark_pink =
  {name="darkpink";red=255;green=182;blue=193}
let light_green =
  {name="lightgreen";red=204; green=255;blue=204}
let dark_green =
  {name="darkgreen";red=0;green=128;blue=0}
let grey  =
  {name="grey";red=191;green=191;blue=191}
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
let di_green =
  {name="digreen";red=58;green=113;blue=104}
let brown =
  {name="brown";red=181;green=101;blue=29}
let purplesco =
  {name="purplesco";red=102;green=73;blue=102}
let bluesco =
{name="bluesco";red=84;green=141;blue=212}


let label c = c.name


let rgb_code c = c.red, c.green, c.blue

let rgb_list =
  [ grey;light_pink;dark_pink;
    light_green;dark_green;
    light_blue;dark_blue;
    light_yellow;dark_yellow;
    light_orange;dark_orange;
    light_red;dark_red;
    light_duck_blue; dark_duck_blue;
    black;white;di_green;
    brown
  ]

let duckblue = build_color ~background:light_duck_blue dark_duck_blue "duck blue"
let blue = build_color ~background:light_blue dark_blue "blue"
let green = build_color ~background:light_green dark_green "green"
let yellow = build_color ~background:light_yellow dark_yellow "yellow"
let red = build_color ~background:light_red dark_red "red"
let black = build_color black "black"
let white= build_color white "white"
let orange = build_color ~background:light_orange dark_orange "orange"
let pink = build_color ~background:light_pink dark_pink "pink"
let digreen = build_color ~background:di_green di_green
    "digreen"
let brown = build_color ~background:brown brown "brown"
let grey = build_color ~background:grey grey "grey"
let bluesco = build_color ~background:bluesco bluesco "bluesco"
let purplesco = build_color ~background:purplesco purplesco "purplesco"

let color_list =
  [
    grey;
    bluesco;
    purplesco; 
    pink;
    blue;
    green;
    yellow;
    red;
    black;
    white;
    orange;
    duckblue;
    digreen;
    brown
  ]

let color_of_string s =
  match s with
  | "duckblue" | "bleucanard" | "duck blue" | "bleu canard" -> Some duckblue
  | "pink" | "rose" -> Some pink
  | "blue" | "bleu" -> Some blue
  | "green" | "vert" -> Some green
  | "digreen" -> Some digreen
  | "yellow" | "jaune" -> Some yellow
  | "red" | "rouge"-> Some red
  | "black" | "noir" -> Some black
  | "white" | "blanc" -> Some white
  | "orange" -> Some orange
  | "brown" -> Some brown
  | "grey" -> Some grey
  | _ -> None

let to_string color = color.label
