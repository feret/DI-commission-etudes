val correct_string: string -> string
val remove_acute: string -> string
val remove_simple_quote: string -> string 
val correct_string_txt: string -> string
val correct_string_latex: string -> string
val correct_string_email_latex: string -> string
val correct_string_html: string -> string
val correct_string_url: string -> string
val correct_string_filename: string -> string
val correct_string_utf8: string -> string
val correct_string_csv: string -> string
val correct_string_percent_from_csv_to_latex: string -> string

val expand_string: string -> string list

val lowercase: string -> string
val uppercase: string -> string
val capitalize: string -> string

val clean_spurious_uppercase_letters: string -> string
val clean_mlle: string -> string

val split_name: string -> string * string * string
