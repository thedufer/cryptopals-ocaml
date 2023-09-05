(lang dune 3.10)

(name cryptopals_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Aaron Dufour")

(maintainers "Aaron Dufour")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cryptopals_ocaml)
 (synopsis "Solutions to Matasano's Cryptopals challenges, in OCaml")
 (description "A longer description")
 (depends core ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
