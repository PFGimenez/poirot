open Grammar

type oracle_status = Syntax_error | Semantic_error | No_error | Grammar_error

let string_of_oracle_status (s: oracle_status) : string = match s with
    | Syntax_error -> "Syntax error"
    | Semantic_error -> "Semantic error"
    | No_error -> "No error"
    | Grammar_error -> "Grammar error"

let oracle_status_of_int : int -> oracle_status = function
    | 0 -> No_error
    | 1 -> Syntax_error
    | 2 -> Semantic_error
    | _ -> failwith "Unknown error code!"

let oracle_mem (o: string -> oracle_status) (verbose: bool) : string -> oracle_status =
    let mem : (string, oracle_status) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: string): oracle_status ->
        if Hashtbl.mem mem inj then begin
            if verbose then print_endline ("Memoization: "^inj);
            Hashtbl.find mem inj
        end else begin
            let answer = o inj in
            Hashtbl.add mem inj answer;
            (*if verbose then print_endline ((string_of_int (Hashtbl.length mem))^"th call to oracle: "^inj^" ("^(string_of_oracle_status answer)^")");*)
            answer
        end

let handle_option (oracle : string -> oracle_status): string option -> oracle_status = function
    | None -> Grammar_error (* no word in the language *)
    | Some inj -> oracle inj

let oracle_from_script (verbose: bool) (fname: string) (inj: string) : oracle_status =
    let escape_char (c: char) : string = match c with
        | '"' -> "\\\""
        | '\\' -> "\\"
        | _ -> String.make 1 c in
    let inj = (string_of_list "" "" escape_char (List.init (String.length inj) (String.get inj))) in
    let cmd = match verbose with
        | true -> fname^" \""^inj^"\""
        | _ -> fname^" \""^inj^"\" >/dev/null 2>&1" in
    print_endline ("Call to oracle: "^inj);
    let answer = oracle_status_of_int (Sys.command cmd) in
    print_endline ((string_of_oracle_status answer));
    answer

let oracle_mem (verbose: bool) (oracle: string -> oracle_status) : (string option -> oracle_status) = handle_option (oracle_mem oracle verbose)

let oracle_mem_from_script (fname: string) (verbose: bool) : (string option -> oracle_status) = oracle_mem verbose (oracle_from_script verbose fname)