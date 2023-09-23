(lang dune 3.0)

(name rinha)

(generate_opam_files true)

;; TODO: Move to flambda
(package
 (name rinha)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving ppx_deriving_yojson yojson))
