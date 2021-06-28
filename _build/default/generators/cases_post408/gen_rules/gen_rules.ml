let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

let () =
  let paths =
    read_file_from_dir (Fpath.filename Gen_rules_lib.cases)
  in
  let paths =
    List.filter (fun p -> not (Gen_rules_lib.is_dot_ocamlformat p)) paths
  in
  let stanzas = Gen_rules_lib.gen_rule
  [ (Html_t_rule.html_target_rule, Fpath.v "html");
    (Latex_t_rule.latex_target_rule, Fpath.v "latex");
    (Man_t_rule.man_target_rule, Fpath.v "man")
  ]
  paths "4.08" in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas
