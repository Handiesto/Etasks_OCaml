(lang dune 2.9)
(name euler)

(generate_opam_files true)

(source
 (github Handiesto/Etasks_OCaml))

(authors "Plyasov K. E.")

(maintainers "Plyasov K. E.")

(license LICENSE)

(documentation https://github.com/Handiesto/Etasks_OCaml)

(package
 (name euler)
 (synopsis "Lab1")
 (description "Functional programming first lab in OCaml")
 (depends ocaml dune ounit2))