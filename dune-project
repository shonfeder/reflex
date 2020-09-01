(lang dune 2.7)

(name reflex)
(version 0.1.0)

(generate_opam_files true)

; TODO
; (source (github mirage/ocaml-cohttp))
; (license ISC)
(authors "Shon Feder")
(maintainers "shon@informal.systems")

(package
 (name reflex)
 (synopsis "TODO")
 (description "TODO")
 (depends
  (dune (> 2))
  base
  calendar
  cmdliner
  irmin
  irmin-git
  irmin-unix
  bos
  ppx_inline_test
  ppx_irmin
  ppx_deriving
  ppx_sexp_conv))

;; Local Variables:
;; mode: dune
;; End:
