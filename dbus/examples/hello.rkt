#lang racket/base

(module+ main
  (require racket/class
           racket/port
           racket/pretty
           dbus
           dbus/interface
           xml)

  (parameterize ((current-dbus-connection (dbus-connect-session-bus)))
    (define m (dbus-manager))
    (define unique-name (send m Hello)) ;; the connection's "unique name"
    ;; (pretty-print
    ;;  (with-input-from-string (send m Introspect) (compose xml->xexpr
    ;;                                                       (eliminate-whitespace '() not)
    ;;                                                       document-element
    ;;                                                       read-xml)))
    ;; (pretty-print `((children ,(dbus-introspect-children m))
    ;;                 (methods ,(dbus-introspect-methods m))
    ;;                 (signals ,(dbus-introspect-signals m))))
    (pretty-print `((names ,(send m ListNames))
                    (activatable-names ,(send m ListActivatableNames))))
    (let loop ()
      (printf "Asynchronous event: ~v\n" (sync (dbus-listen-evt)))
      (loop))))
