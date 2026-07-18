type t = Public_data.pedagogical_registration_suggestion
        Public_data.FirstNameMap.t Public_data.LastNameMap.t

type t' = 
 ((Public_data.cours_supplement * (string * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option * Public_data.valide))) * (string * ((Public_data.diploma_level option * Public_data.main_dpt option) * string option * Public_data.valide ))) list 
        Public_data.FirstNameMap.t Public_data.LastNameMap.t
        Public_data.StringMap.t Public_data.YearMap.t 
        
(*val convert: t -> t' *)        
val empty: t
val empty': t'
val is_empty: t -> bool 
val is_empty': t' -> bool 
val get_pedagogical_registration_suggestions:
  firstname:string ->
  lastname:string ->
   t ->  Public_data.pedagogical_registration_suggestion option 

val add_pedagogical_registration_suggestions:
firstname:string -> lastname:string -> 
((string * int * int * int) ->
 'state ->  Public_data.pedagogical_registration_suggestion
         ->  Public_data.pedagogical_registration_suggestion -> 'state *  Public_data.pedagogical_registration_suggestion) -> 
(string * int * int * int) -> 'state ->
 Public_data.pedagogical_registration_suggestion ->
t ->
'state * t

val add_pedagogical_registration_suggestions':
firstname:string -> lastname:string -> 
((string * int * int * int) ->
 'state ->  Public_data.pedagogical_registration_suggestion
         ->  Public_data.pedagogical_registration_suggestion -> 'state *  Public_data.pedagogical_registration_suggestion) -> 
(string * int * int * int) -> 'state ->
 Public_data.pedagogical_registration_suggestion ->
t' ->
'state * t'


val fold: 
 skip_name:(Public_data.pedagogical_registration_suggestion ->
                     'a -> 'a * bool) -> 
 fold_name:(firstname:string -> lastname:string -> 'a -> 'a) -> 
 fold_year:(string -> 'a -> 'a) -> 
 fold_missing: (((Public_data.diploma_level option *
                         Public_data.main_dpt option) * string option) *
                        int * string list -> 'a -> 'a) -> 
 fold_entry:(firstname:string -> lastname:string -> (Public_data.cours_supplement *
          ((Public_data.diploma_level option * Public_data.main_dpt option) * string option) *
          ((Public_data.diploma_level option * Public_data.main_dpt option) * string option))
         Public_data.StringMap.t  -> 'a -> 'a) -> 
fold_bonusses:(firstname:string ->
                         lastname:string ->
                         ((Public_data.cours_supplement *
                           (string *
                            ((Public_data.diploma_level option *
                              Public_data.main_dpt option) *
                             string option * Public_data.valide))) *
                          (string *
                           ((Public_data.diploma_level option *
                             Public_data.main_dpt option) *
                            string option *Public_data.valide)) 
                          )
                         list Public_data.StringMap.t -> 'a -> 'a) 
          ->  t -> 'a -> 'a 

val fold': 
 fold_string:(string -> 'a -> 'a) -> 
 fold_year:(string -> 'a -> 'a) -> 
fold_bonusses:(year:string ->
                         string :string ->
                         ((Public_data.cours_supplement *
                           (string *
                            ((Public_data.diploma_level option *
                              Public_data.main_dpt option) *
                             string option * Public_data.valide))) *
                          (string *
                           ((Public_data.diploma_level option *
                             Public_data.main_dpt option) *
                            string option *Public_data.valide))) list  Public_data.FirstNameMap.t
                         Public_data.LastNameMap.t 
                          
                          -> 'a -> 'a) 
          ->  t' -> 'a -> 'a 

val dump: 
          show_missing_entries:('f -> 'f * bool) ->
          fprintf:('f -> string -> unit) ->
          print_newline:('f -> unit) ->
          print_cell:(string -> 'f -> unit) ->
          breakpage:('f -> unit) ->
          title: string -> 
            bilingual_string:(?english:string ->
                            french:string -> 'f -> 'f * string) ->
          open_array:(string * int * int * int ->
                      bgcolor:'j option list ->
                      size:float option list ->
                      with_lines:bool ->
                      title:string list list ->
                      title_english:string list list -> 'f -> 'f) ->
          open_row:('f -> unit) ->
          close_row:('f -> unit) ->
          close_array:('f -> unit) ->
           'f -> 
                        Public_data.pedagogical_registration_suggestion -> 'f * bool 
