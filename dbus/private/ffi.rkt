#lang racket/base
;
; Private FFI Bindings
;

(require (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/define)

(provide (all-defined-out))


;; We are going to pull the functions from libc.
(define-ffi-definer define-libc #f)


(define-libc geteuid
             (_fun #:save-errno 'posix
                   --> _int))


; vim:set ts=2 sw=2 et:
