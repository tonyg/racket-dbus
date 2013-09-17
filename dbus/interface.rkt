#lang racket/base
;
; Standard D-Bus Interfaces
;

(require racket/class
         racket/contract
         racket/function
         racket/string
         xml/xexpr-path
         xml)

(require "proxy.rkt"
         "main.rkt")

(provide (all-defined-out))


(define-dbus-interface dbus-introspectable<%>
                       "org.freedesktop.DBus.Introspectable"
  (Introspect ""))


;; Return list of child paths for given introspectable object.
(define/contract (dbus-introspect-children object)
                 (-> (instanceof/c dbus-introspectable<%>/c)
                     (listof string?))
  (xexpr-path-list '(node (name))
                   (string->xexpr (send object Introspect))))


(define/contract (dbus-introspect-methods object)
                 (-> (instanceof/c dbus-introspectable<%>/c)
                     (listof
                       (cons/c dbus-interface-name?
                               (listof
                                 (cons/c dbus-member-name?
                                         (cons/c dbus-signature?
                                                 dbus-signature?))))))
  (map (lambda (iface)
         (cons (xexpr-path-first '((name)) iface)
               (map (lambda (m)
                      (let ((in  '(arg (direction "in") (type)))
                            (out '(arg (direction "out") (type))))
                        (list* (xexpr-path-first '((name)) m)
                               (string-append* (xexpr-path-list in m))
                               (string-append* (xexpr-path-list out m)))))
                    (xexpr-path-list '(method) iface))))
       (xexpr-path-list '(interface)
                        (string->xexpr (send object Introspect)))))


(define/contract (dbus-introspect-signals object)
                 (-> (instanceof/c dbus-introspectable<%>/c)
                     (listof
                       (cons/c dbus-interface-name?
                               (listof
                                 (cons/c dbus-member-name?
                                         dbus-signature?)))))
  (map (lambda (iface)
         (cons (xexpr-path-first '((name)) iface)
               (map (lambda (m)
                      (let ((arg '(arg (type))))
                        (cons (xexpr-path-first '((name)) m)
                              (string-append* (xexpr-path-list arg m)))))
                    (xexpr-path-list '(signal) iface))))
       (xexpr-path-list '(interface)
                        (string->xexpr (send object Introspect)))))


(define-dbus-interface dbus-properties<%>
                       "org.freedesktop.DBus.Properties"
  (Get "ss")
  (Set "ssv")
  (GetAll "s"))


(define-dbus-interface dbus<%>
                       "org.freedesktop.DBus"
  (Hello "")
  (RequestName "su")
  (ReleaseName "s")
  (StartServiceByName "su")
  (UpdateActivationEnvironment "a{ss}")
  (NameHasOwner "s")
  (ListNames "")
  (ListActivatableNames "")
  (AddMatch "s")
  (RemoveMatch "s")
  (GetNameOwner "s")
  (ListQueuedOwners "s")
  (GetConnectionUnixUser "s")
  (GetConnectionUnixProcessID "s")
  (GetAdtAuditSessionData "s")
  (GetConnectionSELinuxSecurityContext "s")
  (ReloadConfig "")
  (GetId ""))


;; Class for the bus object.
(define dbus%
  ((compose dbus<%> dbus-introspectable<%>) dbus-object%))


; vim:set ts=2 sw=2 et: