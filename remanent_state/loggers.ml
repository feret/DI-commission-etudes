type orientation = Lanscape | Normal
type encoding =
  | HTML | HTML_Tabular | HTML_encapsulated
  | TXT | CSV | XLS | Json | Latex of orientation
  | Latex_encapsulated

module type FormatMap =
sig
  type 'a t
  val add : encoding -> 'a  -> 'a t -> 'a t
  val find : encoding -> 'a t -> 'a
  val empty : 'a t
end

module FormatMap =
  Map.Make(struct type t = encoding let compare = compare end)

type token =
  | String of string
  | Breakable_space
  | Breakable_hint

type logger =
  | DEVNUL
  | Formatter of Format.formatter
  | Circular_buffer of string Circular_buffers.t ref
  | Infinite_buffer of string Infinite_buffers.t ref


let breakable x =
  match
    x
  with
  | HTML_Tabular | HTML | HTML_encapsulated
  | TXT | Latex _ | Latex_encapsulated -> true
  | Json | CSV | XLS -> false

type t =
  {
    encoding:encoding;
    logger: logger;
    channel_opt: out_channel option;
    mutable current_line: token list;
    with_lines: bool;
  }

let with_lines t = {t with with_lines = true}
let without_lines t = {t with with_lines = false}
let devnul =
  {
    encoding= TXT;
    logger = DEVNUL;
    channel_opt=None;
    current_line=[];
    with_lines=false;
  }

let get_encoding_format t = t.encoding

let dummy_html_logger =
  {
    encoding = HTML;
    logger = DEVNUL;
    channel_opt = None;
    current_line = [];
    with_lines=false;
  }

let dummy_txt_logger =
  {
    encoding = TXT;
    channel_opt = None;
    logger = DEVNUL;
    current_line = [];
    with_lines=false;
  }

(* Warning, we have to keep the character @ when it is followed by a character followed by a letter or a digit should be preserved *)

let dump_clean_string fmt =
  String.iter
    (fun a ->
       if a = '\n' then ()
       else
         Format.fprintf fmt "%c" a)

let clean_string s =
  let buffer = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer buffer in
  let () = dump_clean_string fmt_buffer s in
  let () = Format.pp_print_flush fmt_buffer () in
  Buffer.contents buffer

let clean fmt =
  let s = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer s in
  Format.kfprintf
    (fun _ ->
       let () = Format.pp_print_flush fmt_buffer () in
       dump_clean_string fmt (Buffer.contents s))
    fmt_buffer

let fprintf ?fprintnewline:(fprintnewline=false) logger =
  match
    logger.logger, fprintnewline || breakable logger.encoding
  with
  | DEVNUL,_ -> Format.ifprintf Format.std_formatter
  | Formatter fmt, true -> Format.fprintf fmt
  | Formatter fmt, false -> clean fmt
  | Circular_buffer _,bool
  | Infinite_buffer _,bool ->
    let b = Buffer.create 0 in
    let fmt_buffer = Format.formatter_of_buffer b in
    Format.kfprintf
      (fun _ ->
         let () = Format.pp_print_flush fmt_buffer () in
         let str = Buffer.contents b in
         logger.current_line <- (String (if bool then str else clean_string str) )::logger.current_line)
      fmt_buffer

let fprintf ?fprintnewline:(fprintnewline=false) logger =
  match logger.encoding with
  | Latex _ | Latex_encapsulated ->
    let b = Buffer.create 0 in
    let fmt_buffer = Format.formatter_of_buffer b in
    Format.kfprintf
      (fun _ ->
         let () = Format.pp_print_flush fmt_buffer () in
         let str = Buffer.contents b in
         let str = Special_char.clean_spurious_uppercase_letters str in
         let str =
           Special_char.correct_string_latex str
         in
         fprintf ~fprintnewline logger
           "%s" str)
      fmt_buffer
  | HTML | HTML_encapsulated | HTML_Tabular ->
  let b = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer b in
  Format.kfprintf
    (fun _ ->
       let () = Format.pp_print_flush fmt_buffer () in
       let str = Buffer.contents b in
       let str =
         Special_char.correct_string_html str
       in
       let str = Special_char.clean_spurious_uppercase_letters str in
       fprintf ~fprintnewline logger
         "%s" str)
    fmt_buffer
  | TXT | CSV | XLS | Json ->
    fprintf ~fprintnewline logger


let log ?backgroundcolor ?textcolor ?lineproportion logger x  =
  let fprintnewline = false in
  match logger.encoding with
  | Latex _ | Latex_encapsulated ->
    begin
      let bgcolor =
        match backgroundcolor with
        | None -> "",""
        | Some a ->
          Format.sprintf
            "{\\colorbox{%s}{%%%%%%%%\n"
            (Color.label (Color.get_background_color a)),
          "}}"
      in
      let txtcolor =
        match textcolor with
        | None -> "",""
        | Some a ->
          Format.sprintf
            "{\\textcolor{%s}{%%%%%%%%\n"
            (Color.label (Color.get_font_color a)),
          "}}"
      in
      let size =
        match lineproportion with
        | None ->
          "\\begin{minipage}{\\textwidth}%%%%\n","\\end{minipage}%%%%%%%%\n"
        | Some f ->
          Format.sprintf
            "\\begin{minipage}{%f\\textwidth}%%%%%%%%\n" f,
          "\\end{minipage}%%%%\n"
      in
      let prefix =
        Format.sprintf "%s%s%s%%%%\n"
          (fst bgcolor) (fst size) (fst txtcolor)
      in
      let suffix =
        Format.sprintf "%%%%\n%s%s%s"
          (snd txtcolor) (snd size) (snd bgcolor)
      in
      let prefix = Scanf.format_from_string prefix "" in
      let suffix = Scanf.format_from_string suffix "" in
      fprintf ~fprintnewline logger (prefix^^x^^suffix)
    end
  | HTML | HTML_Tabular | HTML_encapsulated
  | TXT | CSV | XLS | Json ->
    fprintf ~fprintnewline logger x

let print_breakable_space logger =
  if breakable logger.encoding
  then
    match
      logger.logger
    with
    | DEVNUL
    | Formatter _ ->
      fprintf logger "@ "
    | Circular_buffer _
    | Infinite_buffer _ ->
      logger.current_line <- Breakable_space::logger.current_line
  else
    fprintf logger " "

let print_breakable_hint logger =
      if breakable logger.encoding
      then
        match
          logger.logger
        with
        | DEVNUL
        | Formatter _ ->
          fprintf logger "@,"
        | Circular_buffer _
        | Infinite_buffer _ ->
          logger.current_line <- Breakable_hint::logger.current_line
      else
        fprintf logger ""

let end_of_line_symbol logger =
  match
    logger.encoding
  with
  | HTML  -> "<Br>"
  | Latex _ | Latex_encapsulated -> "\n"
  | Json | HTML_Tabular | HTML_encapsulated | TXT | CSV | XLS  -> ""

let dump_token f x =
  match
    x
  with
  | String s ->
    Format.pp_print_string
      f s
  | Breakable_space ->
    Format.fprintf f "@ "
  | Breakable_hint ->
    Format.fprintf f "@,"

let listi pr_el f l =
  let rec aux acc f = function
    | [] -> ()
    | [el] ->
      pr_el acc f el
    | h :: t ->
      let () = pr_el acc f h in
      aux (succ acc) f t
  in aux 0 f l

let trailing_list pr_el f l =
  listi (fun _ f x -> pr_el f x) f l


let print_newline logger =
  let () =
    fprintf
      ~fprintnewline:true
      logger
      "%s%t"
      (end_of_line_symbol logger)
      (fun f -> Format.pp_print_newline f ())
  in
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer bf ->
    begin
      let bf' =
        Circular_buffers.add
          (Format.asprintf "%a"
             (trailing_list dump_token)
              (List.rev logger.current_line))
          !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
    end
  | Infinite_buffer bf ->
    begin
      let bf' =
        Infinite_buffers.add
          (Format.asprintf "%a"
              (trailing_list dump_token)
              (List.rev logger.current_line))
          !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
    end

let draw_line logger =
  match logger.encoding with
  | Latex _ | Latex_encapsulated ->
    if logger.with_lines then
      let () = fprintf logger "\\hline" in
      let () = print_newline logger
      in ()
  | Json | HTML | HTML_encapsulated | HTML_Tabular | CSV | TXT | XLS -> ()

let open_row ?macro logger =
  match
    logger.encoding
  with
  | HTML_Tabular | HTML | HTML_encapsulated
    -> fprintf logger "<tr>"
  | Latex _ | Latex_encapsulated ->
    let macro =
      match macro with
      | None -> "row"
      | Some x -> x
    in
    fprintf logger "\\%s" macro
  | Json | XLS | TXT | CSV -> ()

let close_row logger =
  match
    logger.encoding
  with
  | HTML_Tabular | HTML | HTML_encapsulated  -> fprintf logger "<tr>@."
  | Latex _ | Latex_encapsulated ->
    let () = fprintf logger "\\innerline@. " in
    print_newline logger
  | Json | XLS | TXT | CSV -> fprintf logger "@."

let print_cell logger s =
  let open_cell_symbol,s,close_cell_symbol =
    match
      logger.encoding
    with
    | HTML | HTML_encapsulated | HTML_Tabular ->
      "<TD>",s,"</TD>"
    | Latex _ | Latex_encapsulated -> "{",s,"}"
    | CSV  -> "",s,"\t"
    | Json  | TXT | XLS -> "",s,""
  in
  fprintf logger "%s%s%s" open_cell_symbol s close_cell_symbol

let open_array ?size ?color ?bgcolor ?align ~title logger =
  match logger.encoding with
    | Latex _ | Latex_encapsulated ->
    let size =
      match size with
      | None -> List.rev_map (fun _ -> None) title
      | Some a -> a
    in
    let color =
      match color with
      | None -> List.rev_map (fun _ -> None) title
      | Some a -> a
    in
    let bgcolor =
      match bgcolor with
      | None -> List.rev_map (fun _ -> None) title
      | Some a -> a
    in
    let align =
      match align with
      | None -> List.rev_map (fun _ -> None) title
      | Some a -> a
    in
    let () = fprintf logger "\\setcounter{total}{0}%%\n\ " in
    let () = fprintf logger "\\setcounter{ects}{0}%%\n\ " in
    let () = fprintf logger "\\setcounter{vsnects}{0}%%\n\ " in
    let () = fprintf logger "\\setcounter{potentialects}{0}%%\n\ " in
    let () = fprintf logger "\\setcounter{nrow}{0}%%\n\ " in
    let () = fprintf logger "{%%\n\ " in
    let () = fprintf logger "\\renewcommand{\\row}[%i]{" (List.length title) in
    let _ =
      List.fold_left
        (fun (b,i) _ ->
           let () = fprintf logger "%s#%i"
               (if b then "" else "&")
               i
           in
           false,i+1)
        (true,1)
        title
    in
    let () = fprintf logger "\\cr}\n\\begin{tabular}{" in
    let () = fprintf logger "|" in
    let rec aux title color bgcolor size align k error =
      match title with
      | _::ttitle->
        begin
          let hcolor,tcolor,error =
            match color with
            | h::t -> h,t,error
            | [] -> None, [], true
          in
          let hbgcolor,tbgcolor,error =
            match bgcolor with
            | h::t -> h,t,error
            | [] -> None, [], true
          in
          let hsize,tsize,error =
            match size with
            | h::t -> h,t,error
            | [] -> None, [], true
          in
          let halign,talign,error =
            match align with
            | h::t -> h,t,error
            | [] -> None, [], true
          in
          let align,error =
            match hsize, halign with
            | Some f, _ ->
              Printf.sprintf "m{%f\\textwidth}" f,error
            | _, (Some 'c' | None)  -> "c",error
            | None, Some 'r' -> "r",error
            | None, Some 'l' -> "l",error
            | _ -> "l",true
          in
          let bgcolor, error =
            match hbgcolor with
            | None -> "", error
            | Some elt ->
              Format.sprintf ">{\\columncolor{%s}}\n"
                (Color.label (Color.get_background_color elt)), error
          in
          let () = fprintf logger "|%s%s" bgcolor align in
          let _ = hcolor, hsize in
          aux ttitle tcolor tbgcolor tsize talign (k+1) error
        end
      | [] ->
        begin
          match color, bgcolor, size, align with
          | [], [], [], [] -> error
          | _ -> true
        end
    in
    let error = aux title color bgcolor size align 1 false in
    let () = fprintf logger "||}" in
    let () = print_newline logger in
    let () = draw_line logger  in
    let () = draw_line logger  in
    let _ =
      List.fold_left (fun is_start title ->
          let () =
            fprintf logger "%s\\cellcolor{white}{%s}"
              (if is_start then "" else "&") title
          in
          false)
        true
        title
    in
    let () = fprintf logger "\\cr%%\n\ " in
    let () = draw_line logger  in
    let () = draw_line logger  in
    error
    | HTML | HTML_encapsulated | HTML_Tabular ->
      let () = fprintf logger "<TABLE>\n" in
      let () = open_row logger in
      let () =
        List.iter
          (fun a -> print_cell logger (a:string))
          title
      in
      let () = close_row logger in
      false
  | Json | CSV | TXT | XLS -> false

  let close_array logger =
    match logger.encoding with
    | Latex _ | Latex_encapsulated ->
      let () = draw_line logger in
      let () = draw_line logger  in
      let () = fprintf logger "\\end{tabular}}" in
      let () = print_newline logger in
      ()
    | HTML | HTML_encapsulated | HTML_Tabular  ->
      let () = fprintf logger "</TABLE>\n" in ()
    | Json | CSV | TXT | XLS -> ()


let print_optional_cell logger s =
  let open_cell_symbol,s,close_cell_symbol =
    match
      logger.encoding
    with
    | Latex _ | Latex_encapsulated -> "[",s,"]"
    | HTML_Tabular
    | CSV
    | Json | HTML | HTML_encapsulated | TXT | XLS -> "","",""
  in
  fprintf logger "%s%s%s" open_cell_symbol s close_cell_symbol


let flush_logger logger =
  match
    logger.logger
  with
  | DEVNUL -> ()
  | Formatter fmt -> Format.pp_print_flush fmt ()
  | Circular_buffer _
  | Infinite_buffer _ -> ()

let close_logger logger =
  let () =
    match
      logger.encoding
    with
    | HTML ->
      fprintf logger "</div>\n</body>\n"
    | HTML_Tabular ->
      fprintf logger "</TABLE>\n</div>\n</body>"
    | Latex _ ->
      fprintf logger "\\label{LastPage}\\end{document}"
    | Json | TXT | CSV | XLS | HTML_encapsulated | Latex_encapsulated -> ()
  in
  let () = flush_logger logger in
  ()

let print_preamble ?decimalsepsymbol logger =
  match
    logger.encoding
  with
  | HTML ->
    fprintf logger "<body>\n<div>\n"
  | HTML_Tabular ->
    fprintf logger "<body>\n<div>\n<TABLE>\n"
  | Latex orientation ->
    let package, size =
      match orientation with
      | Lanscape -> "\\usepackage{lscape}",
                    "\\landscape\n\n\\setlength{\\textwidth}{28.3cm}\n\\setlength{\\hoffset}{-1.84cm}\n\\setlength{\\headsep}{0pt}\n\\setlength{\\topmargin}{0mm}\n\\setlength{\\footskip}{0mm}\n\\setlength{\\oddsidemargin}{0pt}\n\\setlength{\\evensidemargin}{0pt}\n\\setlength{\\voffset}{-2.15cm}\n\\setlength{\\textheight}{19.6cm}\n\\setlength{\\paperwidth}{21cm}\n\\setlength{\\paperheight}{29.7cm}\n\\setlength\\parindent{0pt}\n"
      | Normal -> "\\usepackage{fancyhdr}\\setlength{\\headheight}{5cm}\n",""
    in
    let decimal =
      match
        decimalsepsymbol
      with
      | None -> ""
      | Some a ->
        Format.sprintf "\\npdecimalsign{%s}\n" a
    in
    let () =
      fprintf logger
      "\\documentclass[10pt]{extarticle}%%\n%%\n\
\\usepackage[latin1]{inputenc}%%\n\
%s\n\
\\usepackage[french]{babel}%%\n\
\\usepackage{xfp}%%\n\
\\usepackage{xstring}%%\n\
\\usepackage{ifthen}%%\n\
\\usepackage{numprint}%%\n\
\\usepackage{multirow}%%\n\
       \\usepackage[table]{xcolor}%%\n\
       \\usepackage{graphicx}%%\n\
\\def\\rmdefault{phv}%%\n\
%%\n\
%s\
\\pagestyle{%s}\n\
\n\
\\newcounter{nrow}\n\
\\setcounter{nrow}{0}\n\
\\newcounter{totalrows}\n\
\\setcounter{totalrows}{0}\n\
\\newcounter{total}\n\
\\setcounter{total}{0}\n\
\\newcounter{ects}\n\
\\setcounter{ects}{0}\n\
\\newcounter{vsnects}\n\
\\setcounter{vsnects}{0}\n\
\\newcounter{pects}\n\
\\setcounter{pects}{0}\n\
\\newcounter{potentialects}\n\
\\setcounter{potentialects}{0}\n\
\\newcounter{cects}\n\
\\setcounter{cects}{0}\n\
\\newcounter{vects}\n\
\\setcounter{vects}{0}\n\
\\newcounter{cnote}\n\
\\setcounter{cnote}{0}\n\
\n\
\\begin{document}\n\ %s\n\
\\newcommand{\\factorsquare}{10000}\n\
\\newcommand{\\factor}{100}\n\
\\newcommand{\\myifdecimal}[3]{%%\n\
\\StrGobbleRight{#1}{1}[\\prefix]%%\n\
\\IfDecimal{#1}{#2}{%%\n\
\\IfEndWith{#1}{.}%%\n\
{\\IfInteger{\\prefix}{#2}{#3}}%%\n\
{\\IfEndWith{#1}{,}{\\IfInteger{\\prefix}{#2}{#3}}{#3}}}}%%\n\
\n\
\\newcommand{\\mynumprint}[1]{%%\n\
\\StrSubstitute{#1}{,}{.}[\\res]\\myifdecimal{#1}{\\numprint{\\res}}{#1}}%%\n\
\\newcommand{\\correctnum}[1]%%\n\
{\\StrSubstitute{#1}{,}{.}[\\res]\\myifdecimal{#1}{\res}{0}}%%\n\
\\newcommand{\\computecects}[2]{\\myifdecimal{1}%%\n\
{\\ifnum{\\fpeval{\\correctnum{#1}<10} = 1}%%\n\
\\setcounter{cects}{0}%%\n\
\\else%%\n\
\\setcounter{cects}{2}%%\n\
\\fi%%\n\
}%%\n\
{\\setcounter{cects}{0}}}%%\n\
%%\n\
       %%\n\ " package size
      (match orientation with
         Lanscape -> "empty" | Normal -> "fancy")   decimal
    in
    let () =
      List.iter
        (fun color ->
           let (r,g,b) = Color.rgb_code color in
           let label = Color.label color in
           let () =
             fprintf
               logger
               "\\definecolor{%s}{RGB}{%i,%i,%i}%%\n\ "
               label
               r g b
           in ())
        Color.rgb_list
    in
    let () =
      fprintf logger
        "\\newcommand{\\mean}{}%%\n\ \\newcommand{\\comment}[1]{}%%\n\ \\newcommand{\\mandatory}[1]{\\textcolor{darkred}{#1}}%%\n\ \\newcommand{\\countformaths}[1]{\\textcolor{darkorange}{#1}}%%\n\ \\newcommand{\\innerline}{%%\n\
\\ifnum \\thenrow=\\thetotalrows%%\n\
\\hline%%\n\
\\else\\cline{1-1}\\cline{3-7}\\fi%%\n\
         }%%\n\
         \\newcommand{\\row}{}\n\
\\newcommand{\\cours}[8][]{%%\n\
\\addtocounter{nrow}{1}%%\n\ 
\\StrSubstitute{#7}{,}{.}[\\res]%%\n\
\\StrSubstitute{#8}{,}{.}[\\resects]%%\n\
\\myifdecimal{#7}%%\n\
{%%\n\
\\setcounter{cnote}{\\fpeval{\\res*\\factorsquare}}%%\n\
\\ifnum\\fpeval{\\res<10} = 1%%\n\
\\IfStrEq{#1}{compensation}%%\n\
{\\setcounter{cects}{\\fpeval{\\resects*\\factor}}%%\n\
{\\setcounter{cects}{0}}%%\n\
\\else%%\n\
\\setcounter{cects}{\\fpeval{\\resects*\\factor}}%%\n\
\\fi%%\n\
}%%\n\
{%%\n\
\\setcounter{cnote}{0}%%\n\
\\setcounter{cects}{0}%%\n\
}%%\n\
%%\n\
%%\n\
\\IfStrEq{#7}{en cours}%%\n\
{\\setcounter{pects}{\\fpeval{\\resects*\\factor}}}%%\n\
{\\setcounter{pects}{0}}%%\n\
 %%\n\
 \\IfStrEq{#7}{%s}%%\n\
{\\setcounter{vects}{\\fpeval{\\resects*\\factor}}}%%\n\
 {\\setcounter{vects}{0}}%%\n\
  %%\n\
\\addtocounter{total}{\\fpeval{\\thecects*\\thecnote}}%%\n\
\\addtocounter{ects}{\\fpeval{\\thecects*\\factor}}%%\n\
\\addtocounter{potentialects}{\\fpeval{\\thepects*\\factor}}%%\n\
%%\n\
\\addtocounter{vsnects}{\\fpeval{\\thevects*\\factor}}%%\n\
 %%\n\       #2 & \\ifnum \\thenrow=\\thetotalrows %%\n\ \\multirow{-\\thetotalrows}{\\hsize}{{\\centering #3}}\\fi & \\ifnum \\thetotalrows=1 %%\n\  \\mbox{}\\newline\\newline#4\\newline\\newline\\else#4\\fi  & #5 & #6 & \\IfStrEq{#1}{compensation}{\\cellcolor{lightpink}{\\mynumprint{#7}}}{\\mynumprint{#7}} & \\mynumprint{#8}\\cr%%\n\
}%%\n\
%%\n\ " Tools.valide_sans_note
    in
    ()
  | Json | TXT | CSV | XLS | Latex_encapsulated | HTML_encapsulated -> ()

let breakpage t =
  match t.encoding with
  | Latex _ | Latex_encapsulated -> fprintf t "\\clearpage%%\n\ "
  | HTML | HTML_Tabular | HTML_encapsulated| Json | TXT | CSV | XLS -> ()

let open_logger_from_channel ?mode:(mode=TXT) channel =
  let formatter = Format.formatter_of_out_channel channel in
  let logger =
    {
      logger = Formatter formatter;
      channel_opt = Some channel;
      encoding = mode;
      current_line = [];
      with_lines = false;
    }
  in
  let () = print_preamble logger in
  logger

let open_logger_from_formatter ?mode:(mode=TXT) formatter =
  let logger =
    {
      logger = Formatter formatter;
      channel_opt = None;
      encoding = mode;
      current_line = [];
      with_lines = false;
    }
  in
  let () = print_preamble logger in
  logger

let open_circular_buffer ?mode:(mode=TXT) ?size:(size=10) () =
  {
    logger = Circular_buffer (ref (Circular_buffers.create size "" ));
    channel_opt = None;
    encoding = mode;
    current_line = [];
    with_lines = false;
  }

let open_infinite_buffer ?mode:(mode=TXT) () =
  let logger =
    {
      logger = Infinite_buffer (ref (Infinite_buffers.create 0 ""));
      channel_opt = None;
      encoding = mode;
      current_line = [];
      with_lines = false;
    }
  in
  let () = print_preamble logger in
  logger


let formatter_of_logger logger =
  match
    logger.logger
  with
  | Formatter fmt -> Some fmt
  | DEVNUL
  | Circular_buffer _
  | Infinite_buffer _ -> None

let redirect logger fmt =
  {logger with logger = Formatter fmt}

let print_as_logger logger f =
  fprintf logger "%t" f

let flush_buffer logger fmt =
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer a -> Circular_buffers.iter (Format.fprintf fmt "%s") !a
  | Infinite_buffer b ->
      Infinite_buffers.iter (Format.fprintf fmt "%s") !b

let flush_and_clean logger fmt =
  let () = flush_buffer logger fmt in
  match logger.logger with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer a ->
    a:=Circular_buffers.clean !a
  | Infinite_buffer b ->
    b:=Infinite_buffers.clean !b


let fprintf logger = fprintf ~fprintnewline:false logger

let channel_of_logger logger = logger.channel_opt


let dump_json logger json =
  let channel_opt = channel_of_logger logger in
  let () =
    match channel_opt
    with
    | None -> ()
    | Some channel ->
      let () =
        Yojson.Basic.to_channel channel json
      in
      ()
  in
  ()

let line_to_json line =
  `Assoc ["line", JsonUtil.of_string line]

let line_of_json json =
  match
    json
  with
  | `Assoc ["line", `String s] -> s
  | _ -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "line" ,json))

let gen_iter iter list =
  let output = ref [] in
  let () = iter (fun line -> output:=line::!output) list in
  JsonUtil.of_list line_to_json (List.rev !output)

let of_json = JsonUtil.to_list ~error_msg:"line list" line_of_json

let to_json logger =
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> `List []
  | Circular_buffer a ->
    gen_iter Circular_buffers.iter !a
  | Infinite_buffer b ->
    gen_iter Infinite_buffers.iter !b

let encapsulate mode =
  match mode with
  | Latex _ | Latex_encapsulated -> Latex_encapsulated
  | HTML | HTML_encapsulated -> HTML_encapsulated
  | HTML_Tabular -> HTML_Tabular
  | TXT -> TXT
  | CSV -> CSV
  | XLS -> XLS
  | Json -> Json

let print_headers logger level title =
  match logger.encoding with
  | Latex _ | Latex_encapsulated ->
    let command =
      match level with
      | 1 -> "\\section"
      | 2 -> "\\subsection"
      | 3 -> "\\subsubsection"
      | 4 -> "\\paragraph"
      | 5 -> "\\subparagraph"
      | _ -> ""
    in
    let () = fprintf logger "%s{%s}" command title in
    let () = print_newline logger in
    ()
  | HTML | HTML_encapsulated
  | HTML_Tabular ->
    let () = fprintf logger "<h%i>%s</h%i>" level title level in
    let () = print_newline logger in
    ()
  | TXT
  | CSV
  | XLS
  | Json -> ()

let maketitle logger title =
  match logger.encoding with
  | HTML | HTML_encapsulated | HTML_Tabular ->
    let () =
      fprintf logger "<title>%s</title>" title
    in
    let () = print_newline logger in
    ()
  | Latex _ | Latex_encapsulated ->
    let () =
      fprintf logger "\\title{%s}" title
    in
    let () = print_newline logger in
    let () =
      fprintf logger "\\maketitle"
    in
    let () = print_newline logger in
    ()
  | TXT
  | CSV
  | XLS
  | Json -> ()


let setheadpage logger s =
  match logger.encoding with
  | Latex _ | Latex_encapsulated ->
    let () = fprintf logger "\\chead[%s]{%s}\n\\rhead[]{}\n\\lhead[]{}" s s in
    let () = print_newline logger in
    ()
  | HTML | HTML_encapsulated | HTML_Tabular
  | TXT
  | CSV
  | XLS
  | Json -> ()

let setsignature logger s =
  match logger.encoding with
  | HTML | HTML_encapsulated | HTML_Tabular ->
    let () =
      fprintf logger "<P>%s</P>" s
    in
    let () = print_newline logger in
    ()
  | Latex _ | Latex_encapsulated ->
    let () =
      fprintf logger "\\begin{center}%s\\end{center}" s
    in
    let () = print_newline logger in
    ()
  | TXT
  | CSV
  | XLS
  | Json -> ()

let setpreamble logger s =
  match logger.encoding with
  | HTML | HTML_encapsulated | HTML_Tabular ->
    let () =
      fprintf logger "<P>%s</P>" s
    in
    let () = print_newline logger in
    ()
  | Latex _ | Latex_encapsulated ->
    let () =
      fprintf logger "%s" s
    in
    let () = print_newline logger in
    ()
  | TXT
  | CSV
  | XLS
  | Json -> ()
