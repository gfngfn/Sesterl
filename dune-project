(lang dune 2.5)
(name sesterl)
(version 0.2.1)

(using menhir 2.0)
(generate_opam_files true)

(source (github gfngfn/Sesterl))
(license MIT)
(authors "T. Suwa")
(maintainers "bd[dot]gfngfn[at]gmail[dot]com")

(package
  (name sesterl)
  (synopsis "Sesterl: A Session-Typed Erlang")
  (description "Sesterl: A Session-Typed Erlang")
  (depends
    (alcotest :with-test)
    (dune (>= 2.5))
    (menhir (>= 20200211))
    (menhirLib (>= 20200211))
    (cmdliner (>= 1.0.4))
    (ocamlgraph (>= 1.8.8))
    (semver2 (>= 1.2.0))
    (core (>= 0.13.0))
    (uutf (>= 1.0.2))
    (yaml (>= 2.1.0))
    (omd (>= 1.3.1))
    (ppx_deriving (>= 4.4.1))))
