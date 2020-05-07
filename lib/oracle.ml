open Grammar

(*
 * syntax error: the injection is not syntactically correct
 * semantic error: the injection is syntactically correct but not semantically correct
 * no error: the injection is syntactically and semantically correct
 * grammar error: no injection
 *)
type oracle_status = Syntax_error | (*Semantic_error |*) No_error | Grammar_error

let call_time = ref 0. and idle_time = ref 0. and call_nb = ref 0 and last_oracle_call = ref 0.

let string_of_oracle_status (s: oracle_status) : string = match s with
    | Syntax_error -> "Syntax error"
(*    | Semantic_error -> "Semantic error"*)
    | No_error -> "No error"
    | Grammar_error -> "Grammar error"

let oracle_status_of_int : int -> oracle_status = function
    | 0 -> No_error
    | 124 -> Log.L.warn (fun m -> m "Oracle timeout!"); Syntax_error (* timeout *)
    | 180 -> Syntax_error
(*    | 181 -> Semantic_error*)
    | n -> Log.L.err (fun m -> m "Oracle failure: %d" n); raise Sys.Break

(* add memoization to an oracle *)
let oracle_mem_no_option (o: string -> oracle_status) : string -> oracle_status =
    let mem : (string, oracle_status) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: string): oracle_status ->
        if Hashtbl.mem mem inj then begin
            Log.L.debug (fun m -> m "Oracle memoization: %s" inj);
            Hashtbl.find mem inj
        end else begin
            let answer = o inj in
            Hashtbl.add mem inj answer;
            (*if verbose then print_endline ((string_of_int (Hashtbl.length mem))^"th call to oracle: "^inj^" ("^(string_of_oracle_status answer)^")");*)
            answer
        end

(* handle the empty grammar *)
let handle_option (oracle : string -> oracle_status): string option -> oracle_status = function
    | None -> Log.L.debug (fun m -> m "No word in language"); Grammar_error (* no word in the language *)
    | Some inj -> oracle inj

(* construct an oracle from a script *)
let oracle_from_script (interval: float option) (timeout: float option) (fname: string) (inj: string) : oracle_status =
    let escape_char (c: char) : string = match c with
        | '\'' -> "'\"'\"'" (* in single quote string, only ' must be escaped *)
        | _ -> String.make 1 c in
    let esc_inj = (string_of_list "" "" escape_char (List.init (String.length inj) (String.get inj))) in

    let prefix = match timeout with
        | Some n -> "timeout "^(string_of_float n)^" "
        | None -> "" in

    if interval <> None then begin
        let sleep = Option.get interval -. (Unix.gettimeofday () -. !last_oracle_call) in
        last_oracle_call := Unix.gettimeofday ();
        if sleep > 0. then begin
            Log.L.debug (fun m -> m "Wait %.2fs before oracle call" sleep);
            idle_time := !idle_time +. sleep;
            Unix.sleepf sleep
        end
    end;

    let cmd = match Logs.Src.level Log.poirotsrc with
        | Some Logs.Debug -> fname^" \'"^esc_inj^"\'"
        | _ -> fname^" \'"^esc_inj^"\' >/dev/null 2>&1" in

    Log.L.debug (fun m -> m "Call to oracle: %s." cmd);
    call_nb := !call_nb + 1;
    let start_time = Unix.gettimeofday () in
    let error_code = Sys.command (prefix^cmd) in
    call_time := !call_time +. (Unix.gettimeofday () -. start_time);
    let answer = oracle_status_of_int error_code in
    Log.L.info (fun m -> m "Oracle answer to %s: %s" inj (string_of_oracle_status answer));
    answer

(* construct an oracle from a function *)
let oracle_mem (oracle: string -> oracle_status) : (string option -> oracle_status) = handle_option (oracle_mem_no_option oracle)

(* construct an oracle from a script *)
let oracle_mem_from_script (interval: float option) (timeout: float option) (fname: string) : (string option -> oracle_status) = oracle_mem (oracle_from_script interval timeout fname)
