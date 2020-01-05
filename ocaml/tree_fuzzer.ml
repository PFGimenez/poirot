open Grammar

type parse_tree = Leaf of element | Node of rule * (parse_tree list) | Error

exception No_word_in_language

(* all nonterminal must be the left-hand side of a rule *)
let fuzzer (g : grammar) : part list =
    Random.self_init ();
    let nonrec_rules : (element, rule) Hashtbl.t = Hashtbl.create (List.length g.rules) in
    let rec fuzzer_aux (used_rules: rule list) (e: element) : parse_tree =
        if is_terminal e then
            Leaf e
        else begin
            let possible_rules = List.filter (fun r -> r.left_symbol = e && (List.for_all ((<>) r) used_rules)) g.rules in
            let r : rule option = match possible_rules with
            | [] -> Hashtbl.find_opt nonrec_rules e
            | l -> if Hashtbl.mem nonrec_rules e then Hashtbl.find_opt nonrec_rules e else (* remove this line for more complex examples *)
                    Some(List.nth l (Random.int (List.length l))) in
            if r = None then
                Error
            else
                let r2 = Option.get r in
                let trees = List.map (fuzzer_aux (r2::used_rules)) r2.right_part in
                if List.exists ((=) Error) trees then
                    (fuzzer_aux [@tailcall]) (r2::used_rules) e (* restart with a different rule *)
                else begin
                    if not (Hashtbl.mem nonrec_rules e) then Hashtbl.add nonrec_rules e r2;
                    Node(r2, trees)
                end
        end in
    let rec part_of_tree (t: parse_tree) : part = match t with
        | Error -> raise No_word_in_language
        | Leaf(e) -> [e]
        | Node(_,l) -> l |> List.map part_of_tree |> List.flatten in
    let s = part_of_tree (fuzzer_aux [] g.axiom) in
    print_endline ("Fuzzer: "^(string_of_word s));
    [s]
