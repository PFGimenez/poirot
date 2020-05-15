(** {1 Type} *)

(** The type of an oracle. *)
type t

(**
 * syntax error: the injection is not syntactically correct
 * no error: the injection is syntactically correct
 *)
type status = Syntax_error | No_error

(** {1 Oracle creation} *)

(** [make_oracle_from_fun f] returns an oracle based on the function [f]. The function must return No_error if the injection is lexically correct, Syntax_error otherwise. *)
val oracle_from_fun : float option -> (string -> status) -> t

(** [make_oracle_from_script filename] returns an oracle based on the script [filename]. The script must return error code 0 if the injection is lexically correct, 180 otherwise. *)
val oracle_from_script : float option -> float option -> string -> t

(** [make_oracle_from_pf_sf g_fname prefix suffix] returns an oracle based on a given grammar quotiented by a prefix and a suffix. Useful to simulate a system. *)
val oracle_from_pf_sf : ?oneline_comment: string option -> float option -> string -> string -> string -> t



(** {1 Oracle call} *)

(** Call the oracle and handle the memoization, the interval, etc. *)
val call : t -> string -> status

(** {1 I/O} *)

(** Save the memoization into a file. *)
val save_mem : t -> string -> unit

(** Load the memoization into a file. *)
val load_mem : t -> string -> unit

(** {1 Other} *)

val get_call_time : t -> float
val get_idle_time : t -> float

val print_mem : t -> unit

val string_of_status : status -> string
