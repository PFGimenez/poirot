(** Poirot is a grammar-based injection fuzzer for black box systems
 @author Pierre-François Gimenez *)

(** The current version of Poirot *)
val version : string

(** {1 Type} *)

(** The grammar manipulated by Poirot *)
type grammar

(** A symbol of the grammar, either a terminal or a nonterminal *)
type element

(** {1 Search functions} *)

(** [search oracle g goal start] returns the grammar of injection from the base grammar [g] according to the oracle [oracle], starting from the trivial injection [start].
 @param oracle an oracle built with the Oracle module
 @param g the grammar of the query language (e.g. SQL)
 @param goal the goal of the search, i.e. the element (terminal or nonterminal) you seek to get in the grammar of injection. Poirot stops the search once it is reached.
 @param start a element (terminal or nonterminal) that is a injection.
 @param dict (optional) an Hashtable containing the semantics dictionary.
 @param max_depth (optional) modify the maximal depth of the search
 @param max_steps (optional) modify the maximal number of steps of the search
 @param forbidden_chars (optional) a list of forbidden characters. Such characters won't be used in injection. Useful to avoid escaped characters.
 @param sgraph_fname (optional, for debug) export the search graph in graphviz dot format.
 @param qgraph_fname (optional, for debug) export the quotient graph in graphviz dot format.
 *)
val search : ?inference_g: grammar option -> ?heuristic: Inference.heuristic -> ?manual_stop: bool -> ?oneline_comment: string option -> ?dict:(element,string) Hashtbl.t option -> ?max_depth:int -> ?max_steps:int -> ?forbidden_chars:char list -> ?sgraph_fname:string option -> ?qgraph_fname:string option -> ?save_h:bool -> ?save_oracle: bool -> Oracle.t -> grammar -> element -> element list -> (grammar * string list) option

(** [whitebox_search g_fname left_quotient right_quotient goal] returns the grammar in file [g_fname] after a left quotient by [left_quotient] and a right quotient by [right_quotient], as well as an word of this language. The word will contain the goal (if possible) if it is not None. The boolean returned tells whether the goal has been reached. The grammar must be in BNF format *)
val whitebox_search : ?oneline_comment: string option -> ?qgraph_fname: string option -> string -> string -> string -> element option -> grammar * (string list) * bool

(** {1 Grammar manipulation functions} *)

(** [to_uppercase g] returns the grammar [g] with uppercased chars. *)
val to_uppercase : grammar -> grammar

(** [to_lowercase g] returns the grammar [g] with lowercased chars. *)
val to_lowercase : grammar -> grammar

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

(** [read_dict filename] read the semantics dictionary from the file [filename]. *)
val read_dict : string -> (element,string) Hashtbl.t

(** {1 Log parameters} *)

(** [set_log_level lvl] sets the verbosity level of Poirot to [lvl]. *)
val set_log_level : Logs.level option -> unit

(** [set_log_reporter r] sets the reporter of Poirot. *)
val set_reporter : Logs.reporter -> unit
