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

(* temporary ! *)
let parenth_oracle (prefix: part) (suffix: part) (inj: part) : oracle_status =
    let rec parenth_oracle_aux (word: part) (stack: part): oracle_status = match (word,stack) with
        | [],_ -> if stack = [] then No_error else Syntax_error
        | ((Terminal "(") as t)::q,_ -> parenth_oracle_aux q (t::stack)
        | ((Terminal "[") as t)::q,_ -> parenth_oracle_aux q (t::stack)
        | (Terminal ")")::q1,(Terminal "(")::q2 -> parenth_oracle_aux q1 q2
        | (Terminal ")")::q1,_ -> Syntax_error
        | (Terminal "]")::q1,(Terminal "[")::q2 -> parenth_oracle_aux q1 q2
        | (Terminal "]")::q1,_ -> Syntax_error
        | (Terminal "b")::q1,[] -> parenth_oracle_aux q1 []
        | (Terminal "b")::q1,t::q2 -> Syntax_error
        | t::q,_ -> parenth_oracle_aux q stack in
    parenth_oracle_aux (prefix@inj@suffix) []

(* temporary ! *)
let oracle_mem2 (o: part -> oracle_status) : part option -> oracle_status =
    let mem : (part, oracle_status) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: part option): oracle_status -> match inj with
        | None -> Grammar_error
        | Some inj ->
            if Hashtbl.mem mem inj then
                Hashtbl.find mem inj
            else begin
                let answer = o inj in
                print_endline ("Call to oracle: "^(string_of_word inj)^": "^(string_of_oracle_status answer));
                Hashtbl.add mem inj answer;
                answer
            end

let oracle_mem (o: string -> oracle_status) : string option -> oracle_status =
    let mem : (string, oracle_status) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: string option): oracle_status -> match inj with
        | None -> Grammar_error (* no word in the language *)
        | Some inj ->
            if Hashtbl.mem mem inj then
                Hashtbl.find mem inj
            else begin
                let answer = o inj in
                print_endline ("Call to oracle: "^inj^": "^(string_of_oracle_status answer));
                Hashtbl.add mem inj answer;
                answer
            end

let oracle_from_script (fname: string) (inj: string) : oracle_status =
    let cmd = fname^" '"^inj^"'" in
    let answer = oracle_status_of_int (Sys.command cmd) in
    print_endline ("Call to oracle: "^cmd^": "^(string_of_oracle_status answer));
    answer

let oracle_mem_from_script (fname: string) = oracle_mem (oracle_from_script fname)
