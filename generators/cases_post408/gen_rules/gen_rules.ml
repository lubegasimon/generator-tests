let () =
  let paths =
    Gen_rules_lib.read_file_from_dir (Fpath.filename Gen_rules_lib.cases)
  in
  let paths =
    List.filter (fun p -> not (Gen_rules_lib.is_dot_ocamlformat p)) paths
  in
  let stanzas = Gen_rules_lib.gen_rule Html_t_rule.html_target_rule paths "4.08" in
  List.iter (Sexplib0.Sexp.pp Format.std_formatter) stanzas


