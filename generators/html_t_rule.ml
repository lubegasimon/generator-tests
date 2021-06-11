let html_target_rule odocl targets : Gen_rules_lib.sexp =
  List
    [
      Atom "rule";
      List
        [
          Atom "action";
          List
            (Atom "progn"
             ::
             List
               [
                 Atom "run";
                 Atom "odoc";
                 Atom "html-generate";
                 Atom "--indent";
                 Atom "-o";
                 Atom "html.gen";
                 Atom ("%{dep:" ^ Fpath.to_string odocl ^ "}");
               ]
             :: Gen_rules_lib.gen_targets targets);
        ];
    ]