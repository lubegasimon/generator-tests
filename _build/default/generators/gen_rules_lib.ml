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

let html, latex, man = ("html", "latex", "man")

let dune_inc, dune_inc_gen, gen_, exe =
  (".dune.inc", ".dune.inc.gen", "gen_", ".exe")

type backend = { subdir : Fpath.t }

let html = { subdir = Fpath.v html }

let backends = [ html ]

let read_file_from_dir dir =
  let filenames =
    let arr = Sys.readdir dir in
    Array.sort String.compare arr;
    Array.to_list arr
  in
  let dir = Fpath.v dir in
  List.map (Fpath.( / ) dir) filenames

(* let dep_atom p = Atom (Printf.sprintf "%%{dep:%s}" (Fpath.to_string p)) *)

let odocls backend paths =
  paths
  |> List.map (fun p ->
         let path = Fpath.relativize ~root:backend p in
         match path with Some p -> p | None -> assert false)

let tweak_target target =
match Fpath.segs target with
| _ :: rest -> String.concat "." rest
| _ -> assert false
    
let targets = read_file_from_dir "html"

let tweak_target' t = match Fpath.(segs (rem_ext t)) with
| hd :: tl ->
    let split_tail = String.split_on_char '.' (List.hd tl) in
    let foo = (List.hd split_tail ^ "/") :: split_tail in
    String.concat "/" (( hd ^ ".gen") :: foo) |> Fpath.v |> Fpath.set_ext ".html" (*it's not proper to move from string to fpath*)
| [] ->  assert false

let filter_targets p =
  let filename = Fpath.(rem_ext (base p)) |> Fpath.to_string |> String.capitalize_ascii in
  List.filter (fun t -> 

    (* we might want to pattern match to handle empty list on split*)
    let t'  = tweak_target  t |> String.split_on_char '.' |> List.hd in
      t' = filename
    ) (List.map tweak_target' targets)

let gen_backend_diff_rule html_target_rule paths =
  List.map
    (fun b -> 
        List.map (fun p -> 
            List 
            [
              Atom "subdir";
              Atom (Fpath.to_string b.subdir);
              html_target_rule p (filter_targets p)
            ]
          ) (odocls b.subdir paths)
      )
    backends |> List.flatten

let gen_targets targets =
  List.map
    (fun t ->
      List
        [
          Atom "with-stdout-to";
          Atom (tweak_target t ^ ".gen");
          List
            [
              Atom "progn";
              List
                [
                  Atom "system";
                  Atom ("cat " ^ Filename.quote (Fpath.to_string t));
                ];
            ];
        ])
     targets

let strip_target t = match Fpath.segs t with
| _ :: rest -> Fpath.v (String.concat " " rest)
| [] -> assert false

let diff_rule t ocaml_ver =
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
              Atom (Fpath.to_string t);
              Atom ("html/html.gen/" ^ Fpath.to_string (strip_target t) ^ ".gen");
            ];
        ];
      List
        [
          Atom "enabled_if";
          List [ Atom ">="; Atom "%{ocaml_version}"; Atom ocaml_ver ];
        ];
    ]

let diff_rules targets ocaml_ver =
  List.map (fun t -> diff_rule t ocaml_ver) targets

let gen_backend_rule html_target_rule paths ocaml_ver =
  [ gen_backend_diff_rule html_target_rule paths; diff_rules targets ocaml_ver ] |> List.flatten

let gen_rule html_target_rule paths ocaml_ver =
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
  List.flatten
    [
      List.(flatten (map gen_rule_for_source_file paths));
      gen_backend_rule html_target_rule paths' ocaml_ver;
    ]
