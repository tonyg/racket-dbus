#lang racket/base
;
; D-Bus Signature String Parser
;

(require racket/class
         racket/contract
         racket/function
         racket/port
         racket/list
         parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         misc1/throw)

(require "common.rkt"
         "util.rkt")

(provide dbus-signature?
         dbus-single-signature?
         dbus-object-path?
         dbus-interface-name?
         dbus-error-name?
         dbus-member-name?
         dbus-endpoint-name?
         dbus-encoder/c
         dbus-decoder/c
         make-encoder
         make-decoder
         make-encoder+decoder
         signature-contract-list)


;; Predicate for object path like "/org/freedesktop/DBus".
(define (dbus-object-path? v)
  (and (string? v)
       (or (string=? "/" v)
           (regexp-match? #rx"^(/[A-Za-z0-9_]+)+$" v))))


;; Predicate for interface names like "org.freedesktop.DBus".
(define (dbus-interface-name? v)
  (and (string? v)
       (<= (string-length v) 255)
       (regexp-match?
         #rx"^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)+$" v)))


;; Error names have the same format as interface names.
(define dbus-error-name? dbus-interface-name?)


;; Member (attribute or method) name like "GetAttribute".
(define (dbus-member-name? v)
  (and (string? v)
       (<= (string-length v) 255)
       (regexp-match? #rx"^[A-Za-z_][A-Za-z0-9_]*$" v)))


(define (dbus-endpoint-name? v)
  (and (string? v)
       (<= (string-length v) 255)
       (or (regexp-match?
             #rx"^[A-Za-z_-][A-Za-z0-9_-]*(\\.[A-Za-z_-][A-Za-z0-9_-]*)+$" v)
           (regexp-match?
             #rx"^:[A-Za-z0-9_-]+(\\.[A-Za-z0-9_-]+)+$" v))))


;; Parsed signature.
(define-struct/contract signature
  ((alignment exact-positive-integer?)
   (contract  any/c)
   (children  list?)
   (dump      procedure?)
   (load      procedure?)))


;; Cache of parsed signatures we have seen in the past.
(define/contract interned-signatures
                 (hash/c string? signature?)
  (make-hash))


;; Return cached signature or parse it if seen for the first time.
(define/contract (intern-signature signature-string)
                 (-> string? signature?)
  (hash-ref! interned-signatures
             signature-string
             (thunk (parse-signature signature-string))))


;; Encoder procedure contract.
(define dbus-encoder/c
  (->* (boolean? list?) (output-port?) bytes?))


;; Decoder procedure contract.
(define dbus-decoder/c
  (-> boolean? (or/c bytes? input-port?) list?))


;; Predicate for signatures. Interns them as a side effect.
(define (dbus-signature? signature-string)
  (with-handlers ((exn:fail:dbus:signature? not))
    (and (string? signature-string)
         (intern-signature signature-string)
         #t)))


;; Predicate for single-value signatures. Otherwise normal.
(define (dbus-single-signature? signature-string)
  (with-handlers ((exn:fail? not))
    (let ((signature (intern-signature signature-string)))
      (and signature
           (<= (length (signature-children signature)) 1)))))


(define-empty-tokens type-tokens
  (uint8-t int16-t uint16-t int32-t uint32-t int64-t uint64-t
   boolean-t double-t string-t object-t signature-t))

(define-empty-tokens container-tokens
  (start-structure-t end-structure-t start-dict-t end-dict-t
   variant-t array-t))

(define-empty-tokens control-tokens
  (eof-t))


(define signature-lexer
  (lexer
    ;; Match type tokens.
    ("y"  (token-uint8-t))
    ("b"  (token-boolean-t))
    ("n"  (token-int16-t))
    ("q"  (token-uint16-t))
    ("i"  (token-int32-t))
    ("u"  (token-uint32-t))
    ("h"  (token-uint32-t))
    ("x"  (token-int64-t))
    ("t"  (token-uint64-t))
    ("d"  (token-double-t))
    ("s"  (token-string-t))
    ("o"  (token-object-t))
    ("g"  (token-signature-t))
    ("a"  (token-array-t))
    ("("  (token-start-structure-t))
    (")"  (token-end-structure-t))
    ("v"  (token-variant-t))
    ("{"  (token-start-dict-t))
    ("}"  (token-end-dict-t))

    ;; Signal end of input.
    ((eof) (token-eof-t))))


(define signature-parser
  (parser
    (start signature)
    (end   eof-t)

    (error (lambda (tok-ok? tok-name tok-value)
             (throw exn:fail:dbus:signature
                    'dbus-signature-parser "unexpected token in signature"
                    "token" tok-name)))

    (tokens type-tokens container-tokens control-tokens)

    (grammar
      (signature
        ((types)       (make-wrapper-type $1)))

      (types
        (()            null)
        ((types type)  (append $1 (list $2))))

      (type
        ((dict)       $1)
        ((array)      $1)
        ((structure)  $1)
        ((base-type)  $1))

      (dict
        ((start-dict-t type type end-dict-t)
         (make-dict-type $2 $3)))

      (structure
        ((start-structure-t types end-structure-t)
         (make-structure-type $2)))

      (array
        ((array-t type)
         (make-array-type $2)))

      (base-type
        ((uint8-t)      uint8-type)
        ((boolean-t)    boolean-type)
        ((int16-t)      int16-type)
        ((uint16-t)     uint16-type)
        ((int32-t)      int32-type)
        ((uint32-t)     uint32-type)
        ((int64-t)      int64-type)
        ((uint64-t)     uint64-type)
        ((double-t)     double-type)
        ((string-t)     string-type)
        ((object-t)     object-type)
        ((signature-t)  signature-type)
        ((variant-t)    variant-type)))))


;; Convert signature string to signature structure.
(define/contract (parse-signature signature)
                 (-> string? signature?)
  (define (lex in)
    (with-handlers ((exn? (lambda (exn)
                            (throw exn:fail:dbus:signature
                                   'dbus-signature-parser "invalid signature"
                                   "signature" signature))))
      (signature-lexer in)))

  (call-with-input-string signature
    (lambda (in)
      (signature-parser (thunk (lex in))))))


;; Create integer type with given specifics.
(define (make-integer-type minimum maximum byte-length signed?)
  (signature byte-length
    `(integer-in ,minimum ,maximum)
    null
    (lambda (be? value)
      (write-bytes/align byte-length
        (integer->integer-bytes/ext value byte-length signed? be?)))
    (lambda (be?)
      (integer-bytes->integer/ext
        (read-bytes/align byte-length byte-length) signed? be?))))


(define uint8-type
  (make-integer-type 0 255 1 #f))

(define int16-type
  (make-integer-type (- (expt 2 15)) (sub1 (expt 2 15)) 2 #t))

(define uint16-type
  (make-integer-type 0 (sub1 (expt 2 16)) 2 #f))

(define int32-type
  (make-integer-type (- (expt 2 31)) (sub1 (expt 2 31)) 4 #t))

(define uint32-type
  (make-integer-type 0 (sub1 (expt 2 32)) 4 #f))

(define int64-type
  (make-integer-type (- (expt 2 63)) (sub1 (expt 2 63)) 8 #t))

(define uint64-type
  (make-integer-type 0 (sub1 (expt 2 64)) 8 #f))


;; Create dictionary entry type from key and value sub-types.
(define/contract (make-dict-type key-type value-type)
                 (-> signature? signature? signature?)
  (signature 8
    `(cons/c ,(signature-contract key-type)
             ,(signature-contract value-type))
    (list key-type value-type)
    (lambda (be? value)
      (write-bytes/align 8 #"")
      ((signature-dump key-type) be? (car value))
      ((signature-dump value-type) be? (cdr value)))
    (lambda (be?)
      (read-bytes/align 8 0)
      (cons ((signature-load key-type) be?)
            ((signature-load value-type) be?)))))


;; Create unaligned multiple value wrapper type.
;; This is the signature top-level type.
(define/contract (make-wrapper-type item-types)
                 (-> (listof signature?) signature?)
  (signature 1
    `(list/c ,@(map signature-contract item-types))
    item-types
    (lambda (be? value)
      (for ((item-type  (in-list item-types))
            (item-value (in-list value)))
        ((signature-dump item-type) be? item-value)))
    (lambda (be?)
      (for/list ((item-type (in-list item-types)))
        ((signature-load item-type) be?)))))


;; Create structure type from item sub-types.
(define/contract (make-structure-type item-types)
                 (-> (listof signature?) signature?)
  (signature 8
    `(list/c ,@(map signature-contract item-types))
    item-types
    (lambda (be? value)
      (write-bytes/align 8 #"")
      (for ((item-type  (in-list item-types))
            (item-value (in-list value)))
        ((signature-dump item-type) be? item-value)))
    (lambda (be?)
      (read-bytes/align 8 0)
      (for/list ((item-type (in-list item-types)))
        ((signature-load item-type) be?)))))


;; Create array type from the item sub-type.
(define/contract (make-array-type item-type)
                 (-> signature? signature?)
  (signature 4
    `(listof ,(signature-contract item-type))
    (list item-type)

    (lambda (be? value)
      ;; Dump individual array items without alignment.
      (define array-bytes
        (parameterize ((current-offset 0))
          (with-output-to-bytes
            (thunk (for ((sub-value (in-list value)))
                     ((signature-dump item-type) be? sub-value))))))

      ;; Output length of the array captured above.
      ((signature-dump uint32-type) be? (bytes-length array-bytes))

      ;; Write array contents with manual alignment.
      (write-bytes/align (signature-alignment item-type) array-bytes))

    (lambda (be?)
      ;; Read length of the array in bytes.
      (define array-length ((signature-load uint32-type) be?))

      ;; Align ourselves with the start of the payload.
      (read-bytes/align (signature-alignment item-type) 0)

      ;; Calculate end of the payload.
      (define array-end (+ (current-offset) array-length))

      ;; Read in all array elements and return them as a list.
      (let loop ()
        (if (< (current-offset) array-end)
          (cons ((signature-load item-type) be?) (loop))
          null)))))


;; Boolean true/false type.
(define boolean-type
  (signature 4
    'boolean?
    null
    (lambda (be? value)
      ((signature-dump uint32-type) be? (if value 1 0)))
    (lambda (be?)
      (not (= ((signature-load uint32-type) be?) 0)))))


(define double-type
  (signature 8
    'real?
    null
    (lambda (be? value)
      (write-bytes/align 8 (real->floating-point-bytes value 8 be?)))
    (lambda (be?)
      (floating-point-bytes->real (read-bytes/align 8 8) be?))))


(define/contract (make-string-type contract len-len)
                 (-> any/c exact-positive-integer? signature?)
  (signature len-len
    contract
    null
    (lambda (be? value)
      (write-bytes/align len-len
        (integer->integer-bytes/ext (string-length value) len-len #f be?))
      (write-bytes/align 1
        (string->bytes/utf-8 value))
      (write-bytes/align 1 #"\0"))
    (lambda (be?)
      (let* ((str-len (integer-bytes->integer/ext
                        (read-bytes/align len-len len-len) #f be?))
             (str-body (read-bytes/align 1 str-len)))
        (read-bytes/align 1 1)
        (bytes->string/utf-8 str-body)))))


;; Derive string types.
(define signature-type (make-string-type 'dbus-signature? 1))
(define string-type    (make-string-type 'string? 4))
(define object-type    (make-string-type 'dbus-object-path? 4))


;; Return first single value signature from a potentially complex one.
(define (single-signature signature)
  (cond
    ((null? (signature-children signature))
     signature)

    ((> (length (signature-children signature)) 0)
     (car (signature-children signature)))))


;; Variant: a single value accompanied by it's own signature.
(define variant-type
  (signature 1
    '(cons/c dbus-single-signature? any/c)
    null
    (lambda (be? value)
      (let* ((signature  (intern-signature (car value)))
             (value-type (single-signature signature)))
        ((signature-dump signature-type) be? (car value))
        ((signature-dump value-type)     be? (cdr value))))
    (lambda (be?)
      (let* ((type      ((signature-load signature-type) be?))
             (signature (single-signature (intern-signature type))))
        (cons type ((signature-load signature) be?))))))


;; Create procedure that encodes items according to specified
;; signature, including proper contract checks etc..
(define/contract (make-encoder type)
                 (-> dbus-signature? dbus-encoder/c)
  (let ((dump (signature-dump (intern-signature type))))
    (lambda (big-endian? items (out #f))
      (if out
        (parameterize ((current-output-port out))
          (dump big-endian? items))
        (with-output-to-bytes
          (thunk (dump big-endian? items)))))))


;; Create procedure that decodes byte string according to specified
;; signature and returns list of the values.
(define/contract (make-decoder type)
                 (-> dbus-signature? dbus-decoder/c)
  (let ((load (signature-load (intern-signature type))))
    (lambda (big-endian? bstr-or-in)
      (if (bytes? bstr-or-in)
        (with-input-from-bytes bstr-or-in
          (thunk (load big-endian?)))
        (parameterize ((current-input-port bstr-or-in))
          (load big-endian?))))))


;; Create both encoder and the decoder at once.
(define/contract (make-encoder+decoder type)
                 (-> dbus-signature? (values dbus-encoder/c dbus-decoder/c))
  (values (make-encoder type)
          (make-decoder type)))


;; Return list of contracts for given signature.
(define/contract (signature-contract-list type)
                 (-> dbus-signature? list?)
  (let ((signature (intern-signature type)))
    (map signature-contract (signature-children signature))))


; vim:set ts=2 sw=2 et:
