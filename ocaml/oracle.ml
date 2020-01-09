open Grammar

(* temporary ! *)
let parenth_oracle (prefix: part) (suffix: part) (inj: part) : bool =
    let rec parenth_oracle_aux (word: part) (sofar: part): bool = match (word,sofar) with
        | [],_ -> sofar = []
        | ((Terminal "(") as t)::q,_ -> parenth_oracle_aux q (t::sofar)
        | ((Terminal "[") as t)::q,_ -> parenth_oracle_aux q (t::sofar)
        | (Terminal ")")::q1,(Terminal "(")::q2 -> parenth_oracle_aux q1 q2
        | (Terminal ")")::q1,_ -> false
        | (Terminal "]")::q1,(Terminal "[")::q2 -> parenth_oracle_aux q1 q2
        | (Terminal "]")::q1,_ -> false
        | (Terminal "b")::q1,[] -> parenth_oracle_aux q1 []
        | (Terminal "b")::q1,t::q2 -> false
        | t::q,_ -> parenth_oracle_aux q sofar in
    parenth_oracle_aux (prefix@inj@suffix) []

(* temporary ! *)
let oracle_mem2 (o: part -> bool) : part option -> bool =
    let mem : (part, bool) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: part option): bool -> match inj with
        | None -> true
        | Some inj ->
            if Hashtbl.mem mem inj then
                Hashtbl.find mem inj
            else begin
                let answer = o inj in
                print_endline ("Call to oracle: "^(string_of_word inj)^": "^(string_of_bool answer));
                Hashtbl.add mem inj answer;
                answer
            end

let oracle_mem (o: string -> bool) : string option -> bool =
    let mem : (string, bool) Hashtbl.t = Hashtbl.create 1000 in
    fun (inj: string option): bool -> match inj with
        | None -> true (* no word in the language : it is untestable *)
        | Some inj ->
            if Hashtbl.mem mem inj then
                Hashtbl.find mem inj
            else begin
                let answer = o inj in
                print_endline ("Call to oracle: "^inj^": "^(string_of_bool answer));
                Hashtbl.add mem inj answer;
                answer
            end

let oracle_from_script (fname: string) (inj: string) : bool =
    let cmd = fname^" '"^inj^"'" in
    let answer =  (Sys.command cmd) == 0 in
    print_endline ("Call to oracle: "^cmd^": "^(string_of_bool answer));
    answer

let oracle_mem_from_script (fname: string) = oracle_mem (oracle_from_script fname)
