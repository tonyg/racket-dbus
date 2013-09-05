#lang racket/base
;
; D-Bus Proxy Objects
;

(require racket/contract
         racket/class
         racket/match)

(require "private/common.rkt"
         "private/signature.rkt"
         "private/message.rkt")

(provide current-dbus-connection
         current-dbus-endpoint
         dbus-object%
         define-dbus-interface)


;; Connection to a bus, used when instantiating proxies.
(define-parameter/contract current-dbus-connection
                           (or/c dbus-connection? #f)
  #f)


;; Endpoint we are working with, used when instantiating proxies.
(define-parameter/contract current-dbus-endpoint
                           (or/c dbus-endpoint-name? #f)
  #f)


;; Generic object proxy with no methods.
(define dbus-object%
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
(define-syntax-rule (define-dbus-interface name interface-name
                      (method-name args-type) ...)
  (define (name %)
    (class %
      (inherit-field endpoint connection path)

      (define/public (method-name . args)
        (let* ((string-method-name (symbol->string 'method-name))
               (caller              (make-caller args-type)))
          (handle-result interface-name string-method-name
            (caller connection endpoint path interface-name
                    string-method-name args))))
      ...

      (super-new))))


; vim:set ts=2 sw=2 et:
