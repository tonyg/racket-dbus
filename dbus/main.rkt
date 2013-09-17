#lang racket/base
;
; Native D-Bus Client
;

(require racket/contract
         racket/function
         racket/class
         racket/tcp
         file/sha1
         unstable/socket)

(require "private/common.rkt"
         "private/ffi.rkt"
         "private/signature.rkt"
         "private/message.rkt"
         "private/util.rkt")

(provide dbus-connection?
         dbus-connect/socket
         dbus-connect/tcp
         dbus-auth-external
         dbus-auth-anonymous
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
  (dbus-connection
    (make-dbus-tandem in out)))


;; Connect to D-Bus using an UNIX domain socket.
(define/contract (dbus-connect/socket path (auth-method dbus-auth-external))
                 (->* (path-string?)
                      ((-> input-port? output-port? void?))
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


; vim:set ts=2 sw=2 et:
