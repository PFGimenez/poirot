(** Poirot is a grammar-based injection fuzzer for black box systems
 @author Pierre-François Gimenez *)

(** {1 Type} *)

(** The grammar manipulated by Poirot *)
type grammar

(** A symbol of the grammar, either a terminal or a nonterminal *)
type element


(** {1 Main functions} *)

(** [search oracle_filename g goal start] returns the grammar of injection from the base grammar [g] according to the oracle [oracle_filename], starting from the trivial injection [start].
 @param oracle_filename the filename of an oracle script that returns error code 0 if the injection is lexically correct, 1 otherwise.
 @param g the grammar of the query language (e.g. SQL)
 @param goal the goal of the search, i.e. the element (terminal or nonterminal) you seek to get in the grammar of injection. Poirot stops the search once it is reached.
 @param start a element (terminal or nonterminal) that is a injection.
 @param verbose (optional) makes Poirot verbose when true.
 @param subst (optional) an Hashtable containing the semantics substitution.
 @param max_depth (optional) modify the maximal depth of the search
 @param forbidden_chars (optional) a list of forbidden characters. Such characters won't be used in injection. Useful to avoid escaped characters.
 @param sgraph_fname (optional, for debug) export the search graph in graphviz dot format.
 @param qgraph_fname (optional, for debug) export the quotient graph in graphviz dot format.
 *)
val search : ?verbose:bool -> ?subst:(element,string) Hashtbl.t option -> ?max_depth:int -> ?forbidden_chars:char list -> ?sgraph_fname:string option -> ?qgraph_fname:string option -> string -> grammar -> element -> element list -> grammar option

(** [quotient g left_quotient right_quotient] returns the grammar [g] after a left quotient by [left_quotient] and a right quotient by [right_quotient]. [left_quotient] and [right_quotient] can contain nonterminals or be empty. *)
val quotient : grammar -> element list -> element list -> grammar

(** [fuzzer ~complexity:10 ~goal:e g] returns a word from [g]. If [complexity] is high, longer words should be generated. If [complexity = 0], the returned word is deterministic (always the same) and should be short. If a goal is specified, then it will be included in the generated word. Beware: this is a primitive fuzzer. *)
val fuzzer : ?subst:(element,string) Hashtbl.t option -> ?complexity:int -> ?goal:element option -> grammar -> string option


(** {1 Grammar manipulation functions} *)

(** [to_uppercase g] returns the grammar [g] with uppercased chars. The grammar is simplified, and nonterminals may disappear. *)
val to_uppercase :  grammar -> grammar

(** [to_lowercase g] returns the grammar [g] with lowercased chars. The grammar is simplified, and nonterminals may disappear. *)
val to_lowercase : grammar -> grammar

(** [set_axiom g new_axiom] modifies the axiom of [g] with [new_axiom]. *)
val set_axiom : grammar -> element -> grammar

(** [string_of_grammar g] returns the string representation of [g]. *)
val string_of_grammar : grammar -> string

(** {1 I/O functions} *)

(** [read_subst filename] read the semantics substitution from the file [filename]. *)
val read_subst : string -> (element,string) Hashtbl.t

(** [read_bnf_grammar filename] reads a grammar from a bnf file [filename]. *)
val read_bnf_grammar : ?unravel:bool -> string -> grammar

(** [read_tokens str] reads a list of element from a string [str]. *)
val read_tokens : ?unravel:bool -> string -> element list

(** [read_token str] reads an element from a string [str]. *)
val read_token : ?unravel:bool -> string -> element

(** [export_antlr4 filename g] export the grammar [g] to antlr4 format into the file [filename].g4 *)
val export_antlr4 : string -> grammar -> unit
