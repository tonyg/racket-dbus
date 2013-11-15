#lang racket/base
;
; Common D-Bus Utilities
;

(require racket/contract
         racket/function
         tandem)

(provide (all-defined-out))


;; Custom exceptions for D-Bus related issues.
(define-struct (exn:fail:dbus exn:fail) ())
(define-struct (exn:fail:dbus:signature exn:fail:dbus) ())
(define-struct (exn:fail:dbus:connection exn:fail:dbus) ())
(define-struct (exn:fail:dbus:call exn:fail:dbus) ())


;; D-Bus connection private data.
(define-struct/contract dbus-connection
  ((tandem tandem?)))


;; Define parameter protected by a contract.
(define-syntax-rule (define-parameter/contract name value/c initial)
  (define/contract name (case-> (-> value/c void?)
                                (-> value/c))
    (make-parameter initial)))


;; Serialize procedure invocations using a private semaphore.
(define/contract (synchronized proc)
                 (-> procedure? procedure?)
  (let ((semaphore (make-semaphore 1)))
    (lambda args
      (call-with-semaphore semaphore (thunk (apply proc args))))))


; vim:set ts=2 sw=2 et:
