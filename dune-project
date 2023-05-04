(lang dune 3.7)

(name algo)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name algo)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   core_bench
   ppx_inline_test
   fmt
   logs)
 (tags
  (topics "to describe" your project)))
