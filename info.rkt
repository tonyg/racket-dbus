#lang setup/infotab

(define collection 'multi)
(define deps '("xexpr-path"
               "misc1" ;; for misc1/throw
               "base"
               "parser-tools-lib"
               "unstable-lib"
               ))
(define build-deps '("racket-doc"
                     "scribble-lib"))

; vim:set ts=2 sw=2 et:
