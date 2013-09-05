#lang racket/base
;
; Native D-Bus Client
;

(require racket/contract
         racket/function
         racket/class
         file/sha1
         unstable/socket)

(require "private/common.rkt"
         "private/ffi.rkt"
         "private/signature.rkt"
         "private/message.rkt"
         "private/util.rkt")

(provide dbus-connection?
         dbus-connect/socket
         dbus-auth-external
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


;; Connect to D-Bus using an UNIX domain socket.
(define/contract (dbus-connect/socket path (auth-method dbus-auth-external))
                 (->* (path-string?)
                      ((-> input-port? output-port? void?))
                      dbus-connection?)
  (let-values (((in out) (unix-socket-connect path)))
    ;; Perform user-supplied authentication handshake.
    (auth-method in out)

    ;; Switch to messaging phase.
    (fprintf/safe out "BEGIN\r\n")
    (flush-output/safe out)

    ;; Return prepared connection.
    (dbus-connection
      (make-dbus-tandem in out))))


;; Perform external authentication with effective user id.
(define/contract (dbus-auth-external in out)
                 (-> input-port? output-port? void?)
  ;; Try to negotiate...
  (fprintf/safe out "\0AUTH EXTERNAL ~a\r\n" (hex-number (geteuid)))
  (flush-output/safe out)

  ;; Check that server accepted our authentication request.
  (let ((line (read-line in 'any)))
    (unless (regexp-match? #rx"^OK " line)
      (raise (exn:fail:dbus "external dbus authentication failed"
                            (current-continuation-marks))))))


; vim:set ts=2 sw=2 et:
