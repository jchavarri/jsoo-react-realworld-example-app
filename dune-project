(lang dune 2.7)

(name jsoo-react-realworld-example)

(source
 (github ml-in-barcelona/jsoo-react-template))

(license MIT)

(authors "davesnx" "jchavarri")

(maintainers "davesnx" "jchavarri")

(generate_opam_files true)

(package
 (name jsoo-react-realworld-example)
 (synopsis "Project template for jsoo-react")
 (description
  "Template for a minimal jsoo-react project")
 (depends
  ;; General system dependencies
  (dune (>= 2))
  (ocaml (>= 4.10.0))

  ;; Web toolkit
  (js_of_ocaml (and (>= 3.8.0) (< 3.11.0)))
  (js_of_ocaml-ppx (and (>= 3.8.0) (< 3.11.0)))
  (jsoo-react :dev)
  (atdgen (and (>= 2.2.1) (< 2.3.0)))

  ;; Dev dependencies
  (ocaml-lsp-server :dev)
))
