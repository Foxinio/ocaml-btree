(lang dune 3.11)

(name btree)

(generate_opam_files true)

(source
 (github Foxinio/ocaml-btree))

(authors "Szymon Jedras")

(maintainers "Szymon Jedras")

(license MIT LICENSE)

(package
 (name btree)
 (synopsis "Immutable BTree")
 (description "My humble implementation of Immutable b-trees in ocaml")
 (depends ocaml dune))

