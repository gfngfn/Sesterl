(lang dune 2.5)
(using menhir 2.1)

(generate_opam_files true)

(source (github gfngfn/Sesterl))
(authors "gfngfn")
(maintainers "gfngfn")

(package
 (name sesterl)
 (synopsis "A Session-Typed Erlang")
 (description "A Session-Typed Erlang")
 (depends
  (alcotest :with-test)
  (dune (> 2.5.1))
  (ocamlformat :dev)
  (ppx_deriving (> 1.1.0))
  ))
