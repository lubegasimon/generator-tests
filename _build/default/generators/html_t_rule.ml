let html_target_rule path : Gen_rules_lib.sexp =
      List
        [
          Atom "action";
          List[
            Atom "progn";
              List[
                Atom "run";
                Atom "odoc";
                Atom "html-generate";
                Atom "--indent";
                Atom "--flat";
                Atom "--extra-suffix";
                Atom "gen";
                Atom "-o";
                Atom ".";
                Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
               ]
        ]
        ]
