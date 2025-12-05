(lang dune 2.9)

(name FinalProject)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Team80NetheriteTheta <author@example.com>")

(maintainers "Team80NetheriteTheta <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name FinalProject)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml lwt yojson cohttp-lwt-unix lwt_ppx lwt_ssl bisect_ppx ounit2)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
