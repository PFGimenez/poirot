type t

val init : string option -> Grammar.grammar -> char list -> (Grammar.element,string) Hashtbl.t option -> string option -> Grammar.element list -> t

val get_injection : t -> Grammar.ext_element -> bool * Grammar.part list
val get_grammar : t -> Grammar.ext_element -> Grammar.ext_grammar
val is_in_language : t -> Grammar.ext_element -> Grammar.part -> bool
val get_rhs : t -> Grammar.ext_element -> Grammar.ext_part list

val can_reach_goal : t -> Grammar.ext_element -> bool

(* val refuse_injections : t -> Grammar.element -> unit *)

val get_possible_query_from_ext_element : t -> Grammar.ext_element -> Grammar.element -> string

val print_statistics : t -> unit
val get_call_time : t -> float
val get_fuzzer_time : t -> float

(** Used for closing the dot file containing the graph. *)
val finalizer : t -> unit
