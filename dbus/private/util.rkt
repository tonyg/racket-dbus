#lang racket/base
;
; Misc Utilities
;

(require racket/contract)

(require "common.rkt")

(provide (all-defined-out))


(define (integer->integer-bytes/ext n size-n signed? be?)
  (if (= size-n 1)
    (bytes n)
    (integer->integer-bytes n size-n signed? be?)))


(define (integer-bytes->integer/ext bstr signed? be?)
  (if (= (bytes-length bstr) 1)
    (bytes-ref bstr 0)
    (integer-bytes->integer bstr signed? be?)))


;; Offset in the input or output stream.
(define current-offset
  (make-parameter #f))


(define (read-failed (exn #f))
  (throw exn:fail:dbus:connection
         'libvirt "server closed our connnection during read"))


(define (write-failed (exn #f))
  (throw exn:fail:dbus:connection
         'libvirt "server closed our connnection during write"))


;; Read bytes from given input port or throw an exception if
;; given amount could not be read or end of file have been reached.
(define/contract (read-bytes/safe amt (in (current-input-port)))
                 (->* (exact-nonnegative-integer?) (input-port?) bytes?)
  (with-handlers ((exn:fail? read-failed))
    (let ((bstr (read-bytes amt in)))
      (when (or (eof-object? bstr) (< (bytes-length bstr) amt))
        (read-failed))
      bstr)))


;; Write bytes to given port or throw an exception if
;; given amount could not be written.
(define/contract (write-bytes/safe bstr (out (current-output-port)))
                 (->* (bytes?) (output-port?) exact-nonnegative-integer?)
  (with-handlers ((exn:fail? write-failed))
    (let ((amt (write-bytes bstr out)))
      (when (> (bytes-length bstr) amt)
        (write-failed))
      amt)))


;; Flush output port or throw an exception if port is closed.
(define/contract (flush-output/safe (out (current-output-port)))
                 (->* () (output-port?) void?)
  (with-handlers ((exn:fail? write-failed))
    (flush-output out)))


;; Perform fprintf with exception conversion.
(define/contract (fprintf/safe out form . args)
                 (->* (output-port? string?) () #:rest list? void?)
  (with-handlers ((exn:fail? write-failed))
    (apply fprintf out form args)))


;; Write bytes with proper alignment using (current-offset).
;; If the write fails, (current-offset) is botched.
(define/contract (write-bytes/align alignment bstr)
                 (-> exact-positive-integer? bytes? void?)
  (let* ((aligned  (* alignment (ceiling (/ (current-offset) alignment))))
         (required (make-bytes (- aligned (current-offset)))))
    (write-bytes/safe required)
    (write-bytes/safe bstr)
    (current-offset (+ aligned (bytes-length bstr)))))


;; Read bytes with proper alignment using (current-offset).
(define/contract (read-bytes/align alignment amt)
                 (-> exact-positive-integer?
                     exact-nonnegative-integer?
                     bytes?)
  (let* ((aligned  (* alignment (ceiling (/ (current-offset) alignment))))
         (read-len (- aligned (current-offset)))
         (padding  (read-bytes/safe read-len)))
    (current-offset (+ aligned amt))
    (read-bytes/safe amt)))


; vim:set ts=2 sw=2 et:
