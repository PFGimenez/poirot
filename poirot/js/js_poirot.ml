open Js_of_ocaml
module Html = Dom_html

let js = Js.string
let document = Html.window##.document

let string_input name value =
  let res = document##createDocumentFragment in
  Dom.appendChild res (document##createTextNode (js name));
  let input = Html.createInput ~_type:(js "text") document in
  input##.value := js !value;
  input##.onchange :=
    Html.handler (fun _ ->
        (try value := Js.to_string input##.value
         with Invalid_argument _ -> ());
        input##.value := js !value;
        Js._false);
  Dom.appendChild res input;
  res

let exec_poirot grammar_str goal_str start_str =
    try
        let grammar = Poirot.read_bnf_grammar grammar_str
        and goal = Poirot.read_token goal_str
        and start = Poirot.read_tokens start_str
        and oracle_fname = "oracles/prefix-suffix.py msg_exec axiom 'msg key = ' ' & key = value'" in (* TODO: js externe ? *)
        print_endline "Start searching…";
        let g = Poirot.search oracle_fname grammar goal start in
        match g with
        | Some gram -> Html.window##alert ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:(Some goal) gram)))
        | None -> Html.window##alert (js "No injection found")
    with Failure str | Sys_error str -> Html.window##alert (js ("Error: "^str))
        | _ -> Html.window##alert (js "Unknown error!")

let button name callback =
    let res = document##createDocumentFragment in
    let input = Html.createInput ~_type:(js "submit") document in
    input##.value := js name;
    input##.onclick := Html.handler callback;
    Dom.appendChild res input;
    res

let onload _ =
    let main = Js.Opt.get (document##getElementById (js "js_poirot")) (fun () -> assert false) in
    let grammar_str = ref "msg_exec.bnf" and
    goal_str = ref "Exe" and
    start_str = ref "'value'" in

    Dom.appendChild main (string_input "Grammar: " grammar_str);
    Dom.appendChild main (Html.createBr document);
    Dom.appendChild main (string_input "Goal: " goal_str);
    Dom.appendChild main (Html.createBr document);
    Dom.appendChild main (string_input "Start: " start_str);
    Dom.appendChild main (Html.createBr document);

    Dom.appendChild
    main
    (button "Launch Poirot" (fun _ ->
         let div = Html.createDiv document in
         Dom.appendChild main div;
         exec_poirot !grammar_str !goal_str !start_str;
         Js._false));
  Js._false


let () = Html.window##.onload := Html.handler onload
