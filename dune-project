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
  (alcotest (and :with-test (>= 1.1.0)))
  (dune (>= 2.5.1))
  (ocamlformat (and :dev (>= 0.14.1)))
  (ppx_deriving (>= 1.1.0))
  ))
