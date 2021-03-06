type t

val init : string option -> Grammar.grammar -> char list -> (Grammar.element,string) Hashtbl.t option -> string option -> t

val get_injection : t -> Grammar.ext_element -> Grammar.element option -> Grammar.part option * bool
val get_grammar : t -> Grammar.ext_element -> Grammar.ext_grammar
val is_in_language : t -> Grammar.ext_element -> Grammar.part -> bool

val print_statistics : t -> unit
val get_call_time : t -> float

(** Used for closing the dot file containing the graph. *)
val finalizer : t -> unit
