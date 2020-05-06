(** Poirot is a grammar-based injection fuzzer for black box systems
 @author Pierre-François Gimenez *)

(** {1 Type} *)

(** The grammar manipulated by Poirot *)
type grammar

(** A symbol of the grammar, either a terminal or a nonterminal *)
type element

(** A variant type describing the oracle status *)
type oracle_status

(** The current version of Poirot *)
val version : string

(** {1 Main functions} *)

(** [search oracle g goal start] returns the grammar of injection from the base grammar [g] according to the oracle [oracle], starting from the trivial injection [start].
 @param oracle an oracle built with [make_oracle_from_script] or [make_oracle_from_fun]
 @param g the grammar of the query language (e.g. SQL)
 @param goal the goal of the search, i.e. the element (terminal or nonterminal) you seek to get in the grammar of injection. Poirot stops the search once it is reached.
 @param start a element (terminal or nonterminal) that is a injection.
 @param subst (optional) an Hashtable containing the semantics substitution.
 @param max_depth (optional) modify the maximal depth of the search
 @param max_steps (optional) modify the maximal number of steps of the search
 @param forbidden_chars (optional) a list of forbidden characters. Such characters won't be used in injection. Useful to avoid escaped characters.
 @param sgraph_fname (optional, for debug) export the search graph in graphviz dot format.
 @param qgraph_fname (optional, for debug) export the quotient graph in graphviz dot format.
 *)
val search : ?oneline_comment: string option -> ?subst:(element,string) Hashtbl.t option -> ?max_depth:int -> ?max_steps:int -> ?forbidden_chars:char list -> ?sgraph_fname:string option -> ?qgraph_fname:string option -> (string option -> oracle_status) -> grammar -> element -> element list -> (grammar * string) option

(** [quotient g_fname left_quotient right_quotient goal] returns the grammar in file [g_fname] after a left quotient by [left_quotient] and a right quotient by [right_quotient], as well as an word of this language. The word will contain the goal (if possible) if it is not None. The boolean returned tells whether the goal has been reached. The grammar must be in BNF format *)
val quotient : ?oneline_comment: string option -> ?qgraph_fname: string option -> string -> string -> string -> element option -> grammar * (string option) * bool

(** {1 Oracle functions} *)

(** [make_oracle_from_script filename] returns an oracle based on the script [filename]. The script must return error code 0 if the injection is lexically correct, 1 otherwise. *)
val make_oracle_from_script : ?timeout: float option -> string -> (string option -> oracle_status)

(** [make_oracle_from_fun f] returns an oracle based on the function [f]. The function must return 0 if the injection is lexically correct, 1 otherwise. *)
val make_oracle_from_fun : (string -> int) -> (string option -> oracle_status)

(** {1 Grammar manipulation functions} *)

(** [to_uppercase g] returns the grammar [g] with uppercased chars. If simplified, nonterminals may disappear. *)
val to_uppercase : ?simplify: bool -> grammar -> grammar

(** [to_lowercase g] returns the grammar [g] with lowercased chars. If simplified, nonterminals may disappear. *)
val to_lowercase : ?simplify: bool -> grammar -> grammar

(** [set_axiom g new_axiom] modifies the axiom of [g] with [new_axiom]. *)
val set_axiom : grammar -> element -> grammar

(** [string_of_grammar g] returns the string representation of [g]. *)
val string_of_grammar : grammar -> string

(** {1 I/O functions} *)

(** [read_bnf_grammar filename] reads a grammar from a bnf file [filename]. *)
val read_bnf_grammar : ?unravel:bool -> string -> grammar

(** [read_tokens str] reads a list of element from a string [str]. *)
val read_tokens : ?unravel:bool -> string -> element list

(** [read_token str] reads an element from a string [str]. *)
val read_token : ?unravel:bool -> string -> element

(** [export_antlr4 filename g] export the grammar [g] to antlr4 format into the file [filename].g4 *)
val export_antlr4 : string -> grammar -> unit

(** [read_subst filename] read the semantics substitution from the file [filename]. *)
val read_subst : string -> (element,string) Hashtbl.t

(** {1 Log parameters} *)

(** [set_log_level lvl] sets the verbosity level of Poirot to [lvl]. *)
val set_log_level : Logs.level option -> unit

(** [set_log_reporter r] sets the reporter of Poirot. *)
val set_reporter : Logs.reporter -> unit
