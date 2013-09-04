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


;; Write bytes with proper alignment using (current-offset).
(define/contract (write-bytes/align alignment bstr)
                 (-> exact-positive-integer? bytes? void?)
  (let* ((aligned  (* alignment (ceiling (/ (current-offset) alignment))))
         (required (make-bytes (- aligned (current-offset)))))
    (write-bytes required)
    (current-offset (+ aligned (bytes-length bstr)))
    (write-bytes bstr))
  (void))


;; Read bytes with proper alignment using (current-offset).
(define/contract (read-bytes/align alignment len)
                 (-> exact-positive-integer?
                     exact-nonnegative-integer?
                     bytes?)
  (let ((aligned (* alignment (ceiling (/ (current-offset) alignment)))))
    (let* ((read-len (- aligned (current-offset)))
           (padding  (read-bytes read-len)))
      (when (eof-object? padding)
        (raise (exn:fail:dbus:format
                 (format "failed to read ~a bytes of alignment" read-len)
                 (current-continuation-marks)))))

    (current-offset (+ aligned len))

    (let ((data (read-bytes len)))
      (when (eof-object? data)
        (raise (exn:fail:dbus:format
                 (format "failed to read ~a bytes of data" len)
                 (current-continuation-marks))))
      data)))


; vim:set ts=2 sw=2 et:
