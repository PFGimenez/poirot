(** Poirot is a grammar-based injection fuzzer for black box systems
 @author Pierre-François Gimenez *)

(** The grammar manipulated by Poirot *)
type grammar

(** A symbol of the grammar, either a terminal or a nonterminal *)
type element

(** [search None oracle_filename g goal start max_depth [] None None] returns the grammar of injection from the base grammar [g] according to the oracle [oracle_filename].
 @param goal the goal of the search, i.e. the element (terminal or nonterminal) you seek to get in the grammar of injection. Poirot stops the search once it is reached.
 @param start a element (terminal or nonterminal) that is a injection
 @param max_depth Poirot won't search beyond the maximal depth. By default, we recommend 10. *)
val search : (element,string) Hashtbl.t option -> string -> grammar -> element -> element list option -> int -> char list -> string option -> out_channel option -> grammar option

(** [read_conf filename] read the semantics substitution from the file [filename] *)
val read_subst : string -> (element,string) Hashtbl.t

(** [quotient g left_quotient right_quotient] returns the grammar [g] after a left quotient by [left_quotient] and a right quotient by [right_quotient]. [left_quotient] and [right_quotient] can contain nonterminals or be empty. *)
val quotient : grammar -> element list -> element list -> grammar

(** [fuzzer depth g] returns a word from [g]. If [depth] is high, longer words should be generated. If [depth = 0], the returned word is deterministic (always the same). This is a primitive fuzzer. *)
val fuzzer : int -> grammar -> string option

(** [to_uppercase g] returns the grammar [g] with uppercased chars *)
val to_uppercase :  grammar -> grammar

(** [to_lowercase g] returns the grammar [g] with lowercased chars *)
val to_lowercase : grammar -> grammar

(** [simplify g] modifies the grammar [g] to simplify it. Nonterminals may disappear. *)
val simplify : grammar -> grammar

(** [set_axiom g new_axiom] modifies the axiom of [g] with [new_axiom]. *)
val set_axiom : grammar -> element -> grammar

(** [string_of_grammar g] returns the string representation of [g] *)
val string_of_grammar : grammar -> string

(** [read_bnf_grammar unravel filename] reads a grammar from a bnf file [filename]. It decomposes the terminals if [unravel] is true. *)
val read_bnf_grammar : bool -> string -> grammar

(** [read_tokens unravel str] reads a list of element from a string [str]. It decomposes the terminals if [unravel] is true. *)
val read_tokens : bool -> string -> element list

(** [read_token unravel str] reads an element from a string [str]. It decomposes the terminals if [unravel] is true. *)
val read_token : bool -> string -> element

(** [export_antlr4 filename g] export the grammar [g] to antlr4 format into the file [filename].g4 *)
val export_antlr4 : string -> grammar -> unit
