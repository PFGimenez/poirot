type t

type heuristic = No_heuristic | Complicated

val init : Oracle.t -> Grammar.grammar option -> Grammar.grammar -> Grammar.element -> Grammar.element list -> string option -> (Grammar.element,string) Hashtbl.t option -> int -> int -> string option -> string option -> string option -> char list -> bool -> heuristic -> t

val search : t -> (Grammar.ext_grammar * string list) option
