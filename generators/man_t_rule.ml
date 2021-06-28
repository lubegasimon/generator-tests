let man_target_rule path : Gen_rules_lib.sexp =
      List
        [
          Atom "action";
          List[
            Atom "progn";
              List[
                Atom "run";
                Atom "odoc";
                Atom "man-generate";
                Atom "-o";
                Atom ".";
                Atom "--flat";
                Atom "--extra-suffix";
                Atom "gen";
                Atom ("%{dep:" ^ Fpath.to_string path ^ "}");
               ]
        ]
        ]
