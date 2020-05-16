open Grammar

type status = Syntax_error | No_error

let string_of_status (s: status) : string = match s with
    | Syntax_error -> "Syntax error"
    | No_error -> "No error"

let status_of_int : int -> status = function
    | 0 -> No_error
    | 124 -> Log.L.warn (fun m -> m "Oracle timeout!"); Syntax_error (* timeout *)
    | 180 -> Syntax_error
    | n -> Log.L.err (fun m -> m "Oracle failure: %d" n); raise Sys.Break

type t = {  mutable call : string -> status;
            mutable call_time : float;
            mutable idle_time : float;
            mutable call_nb : int;
            mutable last_oracle_call : float;
            mutable initial_oracle_length : int;
            mutable mem : (string, status) Hashtbl.t;
            interval : float option}

let get_call_time (o: t) : float =
    o.call_time

let get_idle_time (o: t) : float =
    o.idle_time

let print_mem (o: t) : unit =
    Log.L.info (fun m -> m "%d oracle calls:" o.call_nb);
    Hashtbl.iter (fun k v ->  Log.L.info (fun m -> m "  %s: %s" k (string_of_status v))) o.mem

let save_mem (o: t) (fname: string) : unit =
    if Hashtbl.length o.mem > o.initial_oracle_length then begin
        Log.L.info (fun m -> m "Save oracle calls into %s" fname);
        Marshal.to_channel (open_out_bin fname) o.mem []
    end

let load_mem (o: t) (fname: string) : unit =
    try o.mem <- Marshal.from_channel (open_in_bin fname);
        Log.L.info (fun m -> m "Imported oracle calls from %s" fname);
        o.initial_oracle_length <- Hashtbl.length o.mem
    with _ -> Log.L.info (fun m -> m "New oracle file: %s" fname)

(* wrap the function to add the memoization, interval management, etc. *)
let call (o: t) (inj: string) : status =
    if Hashtbl.mem o.mem inj then begin
        (* Log.L.debug (fun m -> m "Oracle memoization: %s" inj); *)
        Hashtbl.find o.mem inj
    end else begin
        if o.interval <> None then begin
            let sleep = Option.get o.interval -. (Unix.gettimeofday () -. o.last_oracle_call) in
            o.last_oracle_call <- Unix.gettimeofday ();
            if sleep > 0. then begin
                Log.L.info (fun m -> m "Wait %.2fs before oracle call" sleep);
                o.idle_time <- o.idle_time +. sleep;
                Unix.sleepf sleep
            end
        end;

        o.call_nb <- o.call_nb + 1;
        let start_time = Unix.gettimeofday () in
        let answer = o.call inj in
        o.call_time <- o.call_time +. (Unix.gettimeofday () -. start_time);
        Log.L.info (fun m -> m "Oracle answer to %s: %s" inj (string_of_status answer));
        Hashtbl.add o.mem inj answer;
        (*if verbose then print_endline ((string_of_int (Hashtbl.length mem))^"th call to oracle: "^inj^" ("^(string_of_status answer)^")");*)
        answer
    end

let escape_char (c: char) : string = match c with
    | '\'' -> "'\"'\"'" (* in single quote string, only ' must be escaped *)
    | _ -> String.make 1 c

let fun_from_script (fname: string) (timeout: float option) : string -> status =
    let prefix = match timeout with
        | Some n -> "timeout "^(string_of_float n)^" "
        | None -> "" in

    fun (inj: string) : status ->
        let esc_inj = (string_of_list "" "" escape_char (List.init (String.length inj) (String.get inj))) in
        let cmd = match Logs.Src.level Log.poirotsrc with
            | Some Logs.Debug -> fname^" \'"^esc_inj^"\'"
            | _ -> fname^" \'"^esc_inj^"\' >/dev/null 2>&1" in
        Log.L.debug (fun m -> m "Call to script: %s." cmd);
        status_of_int (Sys.command (prefix^cmd))


(* construct an oracle from a function *)
let oracle_from_fun (interval: float option) (f: string -> status) : t =
    {call=f; call_time=0.; idle_time=0.; call_nb=0; last_oracle_call=0.; initial_oracle_length=0; mem=Hashtbl.create 1000; interval=interval}

(* construct an oracle from a script *)
let oracle_from_script (interval: float option) (timeout: float option) (fname: string) : t =
    oracle_from_fun interval (fun_from_script fname timeout)

let oracle_from_pf_sf ?(oneline_comment: string option = None) (interval: float option) (grammar_fname: string) (prefix: string) (suffix: string) : t =
    let g = Grammar_io.read_bnf_grammar true grammar_fname in
    let explode s = List.init (String.length s) (fun i -> Grammar.Terminal (String.make 1 (String.get s i))) in
    let prefix = explode prefix
    and suffix = explode suffix in
    let quotient = Quotient.init oneline_comment g [] None None None in
    let e : Grammar.ext_element = {pf=List.rev prefix;e=g.axiom;sf=suffix} in
    let f (s: string) : status =
        (* print_endline (Grammar.string_of_word (explode s)); *)
        if Quotient.is_in_language quotient e (explode s) then No_error else Syntax_error in
    oracle_from_fun interval f
