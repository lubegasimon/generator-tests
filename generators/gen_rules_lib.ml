type sexp = Sexplib0.Sexp.t = Atom of string | List of sexp list

let cu_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom target_path ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "ocamlc";
              Atom "-c";
              Atom "-bin-annot";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let odocl_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.basename dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "link";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let mld_odoc_target_rule dep_path target_path =
  List
    [
      Atom "rule";
      List [ Atom "target"; Atom (Fpath.basename target_path) ];
      List [ Atom "deps"; Atom (Fpath.to_string dep_path) ];
      List
        [
          Atom "action";
          List
            [
              Atom "run";
              Atom "odoc";
              Atom "compile";
              Atom "--pkg";
              Atom "test";
              Atom "-o";
              Atom "%{target}";
              Atom "%{deps}";
            ];
        ];
    ]

let set_odocl_ext = Fpath.set_ext ".odocl"

let set_odoc_ext = Fpath.set_ext ".odoc"

let file_rule path ext =
  let cm_file = Fpath.set_ext ext path in
  let odoc_file = set_odoc_ext path in
  let odocl_file = set_odocl_ext path in
  [
    cu_target_rule path (Fpath.basename cm_file);
    odoc_target_rule cm_file odoc_file;
    odocl_target_rule odoc_file odocl_file;
  ]

let mld_file_rule path =
  let path' = Fpath.(v ("page-" ^ basename path)) in
  let odoc_file = set_odoc_ext path' in
  let odocl_file = set_odocl_ext path' in
  [
    mld_odoc_target_rule path odoc_file; odocl_target_rule odoc_file odocl_file;
  ]

let die s =
  prerr_endline s;
  exit 1

let path' () f = Filename.quote (Fpath.to_string f)

let ext' () f = Filename.quote (Fpath.get_ext f)

let cases = Fpath.v "cases"

let is_dot_ocamlformat p = Fpath.filename p = ".ocamlformat"

let gen_rule_for_source_file path =
  let ext = Fpath.get_ext path in
  match ext with
  | ".ml" -> file_rule path ".cmt"
  | ".mli" -> file_rule path ".cmti"
  | ".mld" -> mld_file_rule path
  | _ ->
      die
        (Printf.sprintf
           "Don't know what to do with %a because of unrecognized %a extension."
           path' path ext' path)

let html, latex = Fpath. ( v "html", v "latex")

let odocls backend paths =
  paths
  |> List.map (fun p ->
         let path = Fpath.relativize ~root:backend p in
         match path with Some p -> p | None -> assert false)

let read_lines ic =
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    assert false
  with End_of_file -> List.rev !lines

  let lines_of_file path =
    let ic = open_in (Fpath.to_string path) in
    let lines = read_lines ic in
    close_in ic;
    lines

let create_targets_file f =
  Fpath.(base f |> set_ext ".targets")

let get_path_to_targets_file backend f =
  create_targets_file f
    |> Fpath.to_string |> Fpath.(/) backend
    
let expected_targets backend test_case =
  let targets_file = get_path_to_targets_file backend test_case in
  try
    lines_of_file targets_file
  with _ -> []

let expected_targets' backend test_case = expected_targets backend test_case |> List.map ( fun t -> Atom (t ^ ".gen"))

let gen_targets_file backend target_path path =
List [
    Atom "subdir";
    Atom (Fpath.to_string backend);
    List[
      Atom "rule";
      List[
        Atom "action";
        List[
          Atom "with-outputs-to";
          Atom (Fpath.(to_string (set_ext ".gen" target_path)));
          List[
            Atom "run";
            Atom "odoc"; Atom (Fpath.to_string backend ^ "-targets"); Atom "-o"; Atom ".";
            Atom ("%{dep:" ^ (Fpath.to_string path) ^ "}"); Atom "--flat";
          ]
        ]
      ]
    ]
]
  
let target_diff_rule backend path =
    let p = Fpath.(//) backend path in
  List[
  Atom "rule";
  List[
    Atom "alias"; Atom "runtest"
  ];
  List[
    Atom "action";
    List[
      Atom "diff";
      Atom (Fpath.to_string p);
      Atom (Fpath.(to_string (p |> set_ext ".gen")))
    ]
  ]
]

let gen_and_diff_target_files_rules backend paths =
  List.map (fun p ->
    let path = Fpath.relativize ~root: backend p in
    match path with
    | Some p -> (
    let p' = create_targets_file p in
    gen_targets_file backend p' p :: [target_diff_rule backend p'])
    | None -> []
  ) paths |> List.concat

let gen_backend_diff_rule (b_t_r, b) paths =
          List.map (fun p ->
            match expected_targets' b p with
            [] -> []
            | _ ->
            [  List 
              [
                Atom "subdir";
                Atom (Fpath.to_string b);
                List [
                  Atom "rule";
                  List(
                    Atom "targets" :: expected_targets' b p
                  );
                  b_t_r p 
                ]
              ]]
            ) 
            (odocls b paths) |> List.concat
         

let diff_rule backend t ocaml_ver =
  let t' = Fpath.(//) backend t in
     List
    [
      Atom "rule";
      List [ Atom "alias"; Atom "runtest" ];
      List
        [
          Atom "action";
          List
            [
              Atom "diff";
              Atom (Fpath.to_string t');
              Atom (Fpath.to_string t' ^ ".gen");
            ];
        ];
      List
        [
          Atom "enabled_if";
          List [ Atom ">="; Atom "%{ocaml_version}"; Atom ocaml_ver ];
        ];
    ]

let diff_rules backend paths ocaml_ver =
  List.map (
    fun t -> diff_rule backend t ocaml_ver
    )(List.(map (expected_targets backend )paths |> concat |> map Fpath.v))

let gen_backend_rule backend_target_rules paths ocaml_ver =
  List.map ( fun b_t_r ->
    let _, b = b_t_r in
  [ (gen_backend_diff_rule b_t_r paths);
    diff_rules b paths ocaml_ver; gen_and_diff_target_files_rules b paths 
  ] |> List.concat
    ) backend_target_rules |> List.flatten

let gen_rule backend_target_rules paths ocaml_ver =
  let paths' =
    List.map
      (fun p ->
        let path = Fpath.relativize ~root:cases p in
        match path with
        | Some p ->
            if Fpath.get_ext p = ".mld" then
              set_odocl_ext Fpath.(parent p / ("page-" ^ filename p))
            else set_odocl_ext Fpath.(parent p / filename p)
        | None -> assert false)
      paths
  in
  List.concat
    [
      List.(concat (map gen_rule_for_source_file paths));
      gen_backend_rule backend_target_rules paths' ocaml_ver;
    ]
