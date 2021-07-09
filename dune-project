(lang dune 2.6)

(name odup)

(source
 (github jfeser/odup))

(license MIT)

(authors "Jack Feser <jack.feser@gmail.com>")

(maintainers "Jack Feser <jack.feser@gmail.com>")

(documentation https://jfeser.github.io/odup/)

(package
 (name odup)
 (synopsis "Searches for duplicate files")
 (description "")
 (depends
  dune
  (logs
   (>= 0.7))
  (core
   (>= v0.14))
  (async
   (>= v0.14))
  (ppx_jane
   (>= v0.14))
  (fmt
   (>= 0.8))
  (progress
   (>= 0.1))
  (rpc_parallel
   (>= v0.14))
  (ctypes
   (>= 0.17))
  (ctypes-foreign
   (>= 0.17))
  (async_find
   (>= v0.14))
  (camlzip
   (>= 1.10))))

(generate_opam_files true)

; Local Variables:
; mode: dune
; End:
