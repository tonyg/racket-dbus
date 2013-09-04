#lang racket/base
;
; D-Bus Proxy Objects
;

(require racket/contract
         racket/class)

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


;; Defines mixin class for D-Bus interface.
(define-syntax-rule (define-dbus-interface name interface-name
                      (method-name args-type) ...)
  (define (name %)
    (class %
      (inherit-field endpoint connection path)

      (define/public (method-name . args)
        (let* ((caller (make-caller args-type))
               (result (caller
                         connection endpoint path interface-name
                         (symbol->string 'method-name) args)))
          (if (null? result) (void) (apply values result))))
      ...

      (super-new))))


; vim:set ts=2 sw=2 et:
