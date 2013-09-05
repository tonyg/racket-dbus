#lang racket/base
;
; D-Bus Message Format
;

(require racket/class
         racket/contract
         racket/function
         racket/generator
         racket/match
         racket/list
         racket/port
         racket/dict
         tandem)

(require "common.rkt"
         "signature.rkt"
         "util.rkt")

(provide make-caller
         make-dbus-tandem)


;; Infinite sequence of serial numbers for messages.
(define next-serial
  (synchronized
    (generator ()
      (for ((i (in-naturals 1)))
        (yield i)))))


;; Message types.
(define *call*   1)
(define *return* 2)
(define *error*  3)
(define *signal* 4)


;; Current protocol version.
(define protocol-version 1)


;; Codec for message headers.
(define-values (head-encoder head-decoder)
  (make-encoder+decoder "yyyyuu"))


;; Codec for additional message options.
(define-values (options-encoder options-decoder)
  (make-encoder+decoder "a(yv)"))


;; Codec for 8-byte aligned nothing.
(define-values (padding-encoder padding-decoder)
  (make-encoder+decoder "()"))


;; Message sent over the wire.
(define message%
  (class object%
    ;; We only require message type to be specified.
    (init-field message-type)

    ;; We do not require values of these fields, but D-Bus might.
    (init-field (reply-expected? (= message-type *call*))
                (auto-start? #t)
                (serial-number 0)
                (object-path #f)
                (interface-name #f)
                (member-name #f)
                (error-name #f)
                (reply-serial #f)
                (destination #f)
                (signature #f)
                (payload null))


    ;; Return byte string representation of the message.
    (define/public (serialize)
      (parameterize ((current-offset 0))

        ;; Serialize message payload.
        (define body-bytes
          ((make-encoder signature) (system-big-endian?) payload))

        ;; Serialize the fixed-length header leading the message.
        (define head-bytes
          (head-encoder
            (system-big-endian?)
            (list
              (if (system-big-endian?) 66 108)
              message-type
              (bitwise-ior (if reply-expected? 0 1)
                           (if auto-start?     0 2))
              protocol-version
              (bytes-length body-bytes)
              serial-number)))

        ;; Flag indicating a signature is to be sent.
        (define signature?
          (and signature
               (not (equal? signature ""))))

        ;; Serialize variable-length header options.
        (define option-bytes
          (options-encoder
            (system-big-endian?)
            (list
              (filter values
                (list
                  (and object-path    `(1 ("o" . ,object-path)))
                  (and interface-name `(2 ("s" . ,interface-name)))
                  (and member-name    `(3 ("s" . ,member-name)))
                  (and error-name     `(4 ("s" . ,error-name)))
                  (and reply-serial   `(5 ("u" . ,reply-serial)))
                  (and destination    `(6 ("s" . ,destination)))
                  (and signature?     `(8 ("g" . ,signature))))))))

        ;; Create padding to be inserted between header and body.
        (define padding-bytes
          (padding-encoder
            (system-big-endian?)
            (list null)))

        ;; Glue all the parts together.
        (bytes-append head-bytes option-bytes padding-bytes body-bytes)))


    ;; Return message type and payload.
    (define/public (get-result)
      (match message-type
        (1 (cons 'call   payload))
        (2 (cons 'return payload))
        (3 (cons 'error  payload))
        (4 (cons 'signal payload))))


    ;; Initialize the parent object.
    (super-new)))


;; Create function that serves to call remote procedure with
;; predefined argument signature.
(define/contract (make-caller args-type)
                 (-> dbus-signature?
                     (-> dbus-connection?
                         dbus-endpoint-name?
                         dbus-object-path?
                         dbus-interface-name?
                         dbus-member-name?
                         list?
                         list?))
  (lambda (connection endpoint object-path interface-name method-name items)
    ;; Send the message and wait for the reply.
    (let ((serial-number (next-serial)))
      (tandem-call
        (dbus-connection-tandem connection)
        serial-number
        (new message% (message-type *call*)
                      (serial-number serial-number)
                      (destination endpoint)
                      (object-path object-path)
                      (interface-name interface-name)
                      (member-name method-name)
                      (signature args-type)
                      (payload items))))))


;; Read a single message from given input port.
(define/contract (read-message in)
                 (-> input-port? (is-a?/c message%))
  ;; The read-bytes/align does work on the current-input-port only and
  ;; current-offset is needed for the decoders to keep track of alignment.
  (parameterize ((current-input-port in)
                 (current-offset 0))

    ;; Read header bytes but do not yet interpret them.
    (define head-bytes
      (parameterize ((current-offset 0))
        (read-bytes/align 1 12)))

    ;; Detect message byte order.
    (define big-endian? (bytes=? (subbytes head-bytes 0 1) #"B"))

    ;; Decode header using the detected byte order.
    (define head (head-decoder big-endian? head-bytes))

    ;; Read and decode header options the usual way.
    (define options (car (options-decoder big-endian? in)))

    ;; Read and ditch the alignment bytes.
    (padding-decoder big-endian? in)

    ;; Read the payload.
    (define body-bytes
      (parameterize ((current-offset (current-offset)))
        (read-bytes/align 1 (fifth head))))

    ;; Find out the body signature.
    (define body-signature
      (cdar (dict-ref options 8 '((#f . "")))))

    ;; Decode the payload.
    (define body
      ((make-decoder body-signature) big-endian? body-bytes))

    ;; Create the message instance with all the information we now have.
    (new message% (message-type    (second head))
                  (reply-expected? (bitwise-bit-set? (third head) 0))
                  (auto-start?     (bitwise-bit-set? (third head) 1))
                  (serial-number   (sixth head))
                  (object-path     (cdar (dict-ref options 1 '((#f . #f)))))
                  (interface-name  (cdar (dict-ref options 2 '((#f . #f)))))
                  (member-name     (cdar (dict-ref options 3 '((#f . #f)))))
                  (error-name      (cdar (dict-ref options 4 '((#f . #f)))))
                  (reply-serial    (cdar (dict-ref options 5 '((#f . #f)))))
                  (destination     (cdar (dict-ref options 6 '((#f . #f)))))
                  (signature       (cdar (dict-ref options 8 '((#f . #f)))))
                  (payload         body))))


;; Creates tandem structure.
(define/contract (make-dbus-tandem in out)
                 (-> input-port? output-port? tandem?)
  (tandem
    (lambda (tag value)
      (write-bytes/safe (send value serialize) out)
      (flush-output/safe out))

    (lambda ()
      (let ((message (read-message in)))
        (values (get-field reply-serial message)
                (send message get-result))))))


; vim:set ts=2 sw=2 et:
