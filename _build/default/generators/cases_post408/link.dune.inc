(rule
 (target bugs_post_406.cmti)
 (deps cases/bugs_post_406.mli)
 (action
  (run ocamlc -c -bin-annot -o %{target} %{deps})))

(rule
 (target bugs_post_406.odoc)
 (deps bugs_post_406.cmti)
 (action
  (run odoc compile --pkg test -o %{target} %{deps})))

(rule
 (target bugs_post_406.odocl)
 (deps bugs_post_406.odoc)
 (action
  (run odoc link -o %{target} %{deps})))

(subdir
 html
 (rule
  (targets
   test-Bugs_post_406.html.gen
   test-Bugs_post_406-class-type-let_open.html.gen
   test-Bugs_post_406-class-let_open'.html.gen)
  (action
   (progn
    (run
     odoc
     html-generate
     --indent
     --flat
     --extra-suffix
     gen
     -o
     .
     %{dep:../bugs_post_406.odocl})))))

(rule
 (alias runtest)
 (action
  (diff html/test-Bugs_post_406.html html/test-Bugs_post_406.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   html/test-Bugs_post_406-class-type-let_open.html
   html/test-Bugs_post_406-class-type-let_open.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   html/test-Bugs_post_406-class-let_open'.html
   html/test-Bugs_post_406-class-let_open'.html.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(subdir
 html
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc html-targets -o . %{dep:../bugs_post_406.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff html/bugs_post_406.targets html/bugs_post_406.gen)))

(subdir
 latex
 (rule
  (targets test.Bugs_post_406.tex.gen test.Bugs_post_406.let_open'.tex.gen)
  (action
   (progn
    (run
     odoc
     latex-generate
     -o
     .
     --flat
     --extra-suffix
     gen
     %{dep:../bugs_post_406.odocl})))))

(rule
 (alias runtest)
 (action
  (diff latex/test.Bugs_post_406.tex latex/test.Bugs_post_406.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   latex/test.Bugs_post_406.let_open'.tex
   latex/test.Bugs_post_406.let_open'.tex.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(subdir
 latex
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc latex-targets -o . %{dep:../bugs_post_406.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff latex/bugs_post_406.targets latex/bugs_post_406.gen)))

(subdir
 man
 (rule
  (targets
   test.Bugs_post_406.Bugs_post_406.3o.gen
   test.Bugs_post_406.Bugs_post_406.let_open'.let_open'.3o.gen)
  (action
   (progn
    (run
     odoc
     man-generate
     -o
     .
     --flat
     --extra-suffix
     gen
     %{dep:../bugs_post_406.odocl})))))

(rule
 (alias runtest)
 (action
  (diff
   man/test.Bugs_post_406.Bugs_post_406.3o
   man/test.Bugs_post_406.Bugs_post_406.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(rule
 (alias runtest)
 (action
  (diff
   man/test.Bugs_post_406.Bugs_post_406.let_open'.let_open'.3o
   man/test.Bugs_post_406.Bugs_post_406.let_open'.let_open'.3o.gen))
 (enabled_if
  (>= %{ocaml_version} 4.08)))

(subdir
 man
 (rule
  (action
   (with-outputs-to
    bugs_post_406.gen
    (run odoc man-targets -o . %{dep:../bugs_post_406.odocl} --flat)))))

(rule
 (alias runtest)
 (action
  (diff man/bugs_post_406.targets man/bugs_post_406.gen)))
