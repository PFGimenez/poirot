open Js_of_ocaml
open Js_of_ocaml_lwt
module Html = Dom_html

let ( >>= ) = Lwt.bind
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

let http_get url =
    XmlHttpRequest.get url
    >>= fun r ->
    let cod = r.XmlHttpRequest.code in
    let msg = r.XmlHttpRequest.content in
    if cod = 0 || cod = 200 then Lwt.return msg else fst (Lwt.wait ())

let oracle_fun (url: string) (_: string) : int =
    ignore (http_get url
    >>= (fun res -> print_endline res; Lwt.return res)); 0

let exec_poirot grammar_str goal_str start_str =
    try
        let grammar = Poirot.read_bnf_grammar grammar_str
        and goal = Poirot.read_token goal_str
        and start = Poirot.read_tokens start_str
        and oracle = Poirot.make_oracle_from_fun (oracle_fun "http://127.0.0.1") in
        print_endline "Start searching…";
        let g = Poirot.search oracle grammar goal start in
        match g with
        | Some gram -> Html.window##alert (js ("Injection: "^(Option.get (Poirot.fuzzer ~complexity:0 ~goal:(Some goal) gram))))
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
