(lang dune 2.0)
(name p0_lib)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/p0))
(homepage             https://github.com/tomjridge/p0)
(documentation        https://tomjridge.github.io/ocamldocs/p0)
(generate_opam_files  true)


(package 
 (name p0_lib)
 (synopsis "p0: a simple combinator parsing library")
 (depends ppx_jane core_kernel re
))

; (package 
;  (name p0_examples)
;  (synopsis "Minimal combinator parser (examples)")
;  (depends
; p0
;    ))
