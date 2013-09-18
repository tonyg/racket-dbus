#lang racket/base
;
; D-Bus Proxy Objects
;

(require racket/contract
         racket/class
         racket/match)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/strip-context))

(require "private/common.rkt"
         "private/signature.rkt"
         "private/message.rkt")

(require (for-syntax "private/signature.rkt"))

(provide current-dbus-connection
         current-dbus-endpoint
         dbus-object%
         dbus-object%/c
         define-dbus-interface)


;; Connection to a bus, used when instantiating proxies.
(define-parameter/contract current-dbus-connection
                           (or/c dbus-connection? #f)
  #f)


;; Endpoint we are working with, used when instantiating proxies.
(define-parameter/contract current-dbus-endpoint
                           (or/c dbus-endpoint-name? #f)
  #f)


;; Contract for generic object proxy class below.
(define dbus-object%/c
  (class/c
    (init-field (path dbus-object-path?)
                (endpoint dbus-endpoint-name?)
                (connection dbus-connection?))))


;; Generic object proxy with no methods.
(define/contract dbus-object%
                 dbus-object%/c
  (class object%
    ;; Object path within the endpoint.
    (init-field path)

    ;; Remote endpoint that owns the object.
    (init-field (endpoint (current-dbus-endpoint)))

    ;; D-Bus connection for communication with the endpoint.
    (init-field (connection (current-dbus-connection)))

    ;; Initialize the parent object.
    (super-new)))


;; Convert result either to set of values or to an exception.
(define/contract (handle-result interface-name method-name result)
                 (-> string? string? pair? any)
  (match result
    ((cons type value)
     (match type
       ('error (throw exn:fail:dbus:call
                      (string->symbol
                        (string-append interface-name "." method-name))
                      "dbus remote procedure call failed"
                      "reason" (if (and (list? value)
                                        (string? (car value)))
                                 (car value)
                                 "unknown")))

       ('return (if (null? value)
                  (void)
                  (apply values value)))))))


;; Defines mixin class for D-Bus interface.
(define-syntax (define-dbus-interface stx)
  (syntax-case stx ()
    ((_ name interface-name (method-name args-type) ...)
     (with-syntax ((((arg-contracts ...) ...)
                    (map signature-contract-list
                         (syntax->datum #'(args-type ...))))
                   (contract-name
                     (format-id #'name "~a/c" (syntax->datum #'name))))
       #'(begin
           (define contract-name
             (class/c (method-name (->m arg-contracts ... any)) ...))

           (define/contract (name %)
                            (-> dbus-object%/c contract-name)
             (class %
               (inherit-field endpoint connection path)

               (define/public (method-name . args)
                 (let* ((string-method-name (symbol->string 'method-name))
                        (caller             (make-caller args-type)))
                   (handle-result interface-name string-method-name
                     (caller connection endpoint path interface-name
                             string-method-name args))))
               ...

               (super-new))))))))


; vim:set ts=2 sw=2 et:
