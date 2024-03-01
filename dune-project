(lang dune 3.14)

(name cli_math)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cli_math)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   (ocaml
     (>= 5.1.1)) 
   dune
   ocaml-lsp-server
   ocamlformat
   utop
   (odoc :with-doc))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
