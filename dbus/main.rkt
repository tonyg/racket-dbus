#lang racket/base
;
; Native D-Bus Client
;

(require racket/contract
         racket/function
         racket/class
         racket/match
         racket/string
         racket/tcp
         file/sha1
         unstable/socket
         misc1/throw)

(require (for-syntax racket/base
                     racket/syntax
                     syntax/strip-context))

(require "private/common.rkt"
         "private/signature.rkt"
         "private/message.rkt"
         "private/util.rkt"
         "private/ffi.rkt")

(require (for-syntax "private/signature.rkt"))

(provide dbus-connection?
         dbus-connect-session-bus
         dbus-connect-system-bus
         dbus-connect/address
         dbus-connect/socket
         dbus-connect/tcp
         dbus-auth-external
         dbus-auth-anonymous
         dbus-listen-evt
         current-dbus-connection
         current-dbus-endpoint
         dbus-object%
         dbus-object%/c
         define-dbus-interface
         exn:fail:dbus?
         exn:fail:dbus:signature?
         exn:fail:dbus:connection?
         dbus-signature?
         dbus-single-signature?
         dbus-object-path?
         dbus-interface-name?
         dbus-error-name?
         dbus-member-name?
         dbus-endpoint-name?)


;; Connection to a bus, used when instantiating proxies.
(define-parameter/contract current-dbus-connection
                           (or/c dbus-connection? #f)
  #f)


;; Endpoint we are working with, used when instantiating proxies.
(define-parameter/contract current-dbus-endpoint
                           (or/c dbus-endpoint-name? #f)
  #f)


;; Returns integer as hex-encoded character string.
(define/contract (hex-number num)
                 (-> integer? string?)
  (bytes->hex-string
    (string->bytes/utf-8
      (number->string num))))


;; Make dbus connection from in/out streams and an authentication method.
(define (make-connection in out auth-method)
  ;; Perform user-supplied authentication handshake.
  (auth-method in out)

  ;; Switch to messaging phase.
  (fprintf/safe out "BEGIN\r\n")
  (flush-output/safe out)

  ;; Return prepared connection.
  (dbus-connection (make-dbus-thread in out)))


;; Connect to the session D-Bus via an environment variable.
(define/contract (dbus-connect-session-bus (auth-method dbus-auth-external))
  (->* () ((-> input-port? output-port? void?)) dbus-connection?)
  (define envvarname "DBUS_SESSION_BUS_ADDRESS")
  (define address (getenv envvarname))
  (when (not address)
    (throw exn:fail:dbus 'dbus-connect-session-bus
           "Environment variable ~a not defined"
           envvarname))
  (dbus-connect/address address auth-method))


;; The system D-Bus unix socket path.
(define system-bus-socket-path "/var/run/dbus/system_bus_socket")


;; Connect to the system D-Bus.
(define/contract (dbus-connect-system-bus (auth-method dbus-auth-external))
  (->* () ((-> input-port? output-port? void?)) dbus-connection?)
  (dbus-connect/socket system-bus-socket-path auth-method))

;; Connect to D-Bus using the given address.
(define/contract (dbus-connect/address address (auth-method dbus-auth-external))
  (->* (string?) ((-> input-port? output-port? void?)) dbus-connection?)
  (or (match address
        [(regexp #px"^unix:(.*)$" (list _ kvs))
         (define pieces (for/list [(kv (string-split kvs ","))] (string-split kv "=")))
         (for/or [(piece pieces)]
           (match piece
             [(list "abstract" p)
              (dbus-connect/socket (bytes-append #"\0" (string->bytes/utf-8 p)) auth-method)]
             [(list "path" p)
              (dbus-connect/socket p auth-method)]
             [_ #f]))]
        [_ #f])
      (throw exn:fail:dbus 'dbus-connect/address
             "Unsupported address: ~v" address)))


;; Connect to D-Bus using an UNIX domain socket.
(define/contract (dbus-connect/socket (path system-bus-socket-path)
                                      (auth-method dbus-auth-external))
                 (->* ()
                      (unix-socket-path?
                       (-> input-port? output-port? void?))
                      dbus-connection?)
  (let-values (((in out) (unix-socket-connect path)))
    (make-connection in out auth-method)))


;; Connect to D-Bus using a TCP socket.
(define/contract (dbus-connect/tcp host port (auth-method dbus-auth-anonymous))
                 (->* (string? (integer-in 1 65535))
                      ((-> input-port? output-port? void?))
                      dbus-connection?)
  (let-values (((in out) (tcp-connect host port)))
    (make-connection in out auth-method)))


;; Perform external authentication with effective user id.
(define/contract (dbus-auth-external in out)
                 (-> input-port? output-port? void?)
  ;; Try to negotiate...
  (fprintf/safe out "\0AUTH EXTERNAL ~a\r\n" (hex-number (geteuid)))
  (flush-output/safe out)

  ;; Check that server accepted our authentication request.
  (let ((line (read-line in 'any)))
    (unless (regexp-match? #rx"^OK " line)
      (throw exn:fail:dbus 'dbus-auth-external "authentication failed"))))


;; Perform external authentication with effective user id.
(define/contract (dbus-auth-anonymous in out)
                 (-> input-port? output-port? void?)
  ;; Try to negotiate...
  (fprintf/safe out "\0AUTH ANONYMOUS ~a\r\n"
                    (bytes->hex-string #"anonymous"))
  (flush-output/safe out)

  ;; Check that server accepted our authentication request.
  (let ((line (read-line in 'any)))
    (unless (regexp-match? #rx"^OK " line)
      (throw exn:fail:dbus 'dbus-auth-anonymous "authentication failed"))))


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


;; Event yielding incoming notifications.
(define/contract (dbus-listen-evt (connection (current-dbus-connection)))
                 (->* ()
                      (dbus-connection?)
                      (evt/c (list/c dbus-object-path?
                                     dbus-interface-name?
                                     dbus-member-name?
                                     any/c)))
  (listen-evt connection))


; vim:set ts=2 sw=2 et:
