#lang racket/base
;
; Standard D-Bus Interfaces
;

(require racket/function)

(require "proxy.rkt")

(provide (all-defined-out))


(define-dbus-interface dbus-introspectable<%>
                       "org.freedesktop.DBus.Introspectable"
  (Introspect ""))


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
