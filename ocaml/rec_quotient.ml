open Base

(* get the first symbol of the rhs of a rule *)
let extract_left_symbol (r : rule) : element option = match r.right_part with
    | [] -> None
    | t::_ -> Some(t)

(* get the last symbol of the rhs of a rule *)
let extract_right_symbol (r : rule) : element option = match List.length r.right_part with
    | 0 -> None
    | n -> assert (n > 0); Some(List.nth r.right_part (n-1))

let rec symbol_list_aux (f : rule -> element option) (g: grammar) (tasks: element list) (seen: element list) : element list = match tasks with
    | [] -> seen
    | t::q when List.mem t seen -> (symbol_list_aux [@tailcall]) f g q seen
    | t::q -> (symbol_list_aux [@tailcall]) f g (q@(List.filter_map f (List.filter (fun r -> r.left_symbol = t) g.rules))) (t::seen)

(* get the list of symbols that can be a prefix of the derivation of element *)
let left_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_left_symbol g [task] []

(* get the list of symbols that can be a suffix of the derivation of element *)
let right_symbol_list (g: grammar) (task: element) : element list =
    symbol_list_aux extract_right_symbol g [task] []

(* get the list of rules that can directly derive this element *)
let get_generative_rules (g: grammar) (e: element) : rule list =
    List.filter (fun r -> List.mem e r.right_part) g.rules

let quotient_mem mem_size =
    let mem : (ext_element, ext_rule list) Hashtbl.t = Hashtbl.create mem_size
    and rsym : (ext_element, element list) Hashtbl.t = Hashtbl.create mem_size
    and lsym : (ext_element, element list) Hashtbl.t = Hashtbl.create mem_size in
        fun (g: grammar) (e: ext_element) : grammar -> g (* TODO *)

