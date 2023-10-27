(lang dune 3.10)

(name ochess)

(generate_opam_files true)

(source
 (github flocks/ochess))

(authors "Florent Teissier")

(maintainers "Florent Teissier")

(license LICENSE)

(documentation https://github.com/flocks/ochess)

(package
 (name ochess)
 (synopsis "Little chess learning game")
 (description "Train your brain to recognize chess board positions quickly")
 (depends ocaml dune)
 (tags
  (topics "ocaml" "chess" "raylib")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
