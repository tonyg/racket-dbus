#lang racket

(module+ main
  (require dbus dbus/interface)

  (parameterize ((current-dbus-connection (dbus-connect-system-bus)))
    (define m (dbus-manager))
    (define unique-name (send m Hello)) ;; the connection's "unique name"

    (define network-manager
      (new (dbus-properties<%> (dbus-introspectable<%> dbus-object%))
           (path "/org/freedesktop/NetworkManager")
           (endpoint "org.freedesktop.NetworkManager")
           (connection (current-dbus-connection))))

    (send m AddMatch
          "type=signal,interface=org.freedesktop.NetworkManager,member=PropertiesChanged")

    ;; From information on "enum NMState" from
    ;; https://developer.gnome.org/NetworkManager/stable/nm-dbus-types.html
    (define *state-map*
      '((NM_STATE_UNKNOWN 0) ;; networking state is unknown
        (NM_STATE_ASLEEP 10) ;; networking is not enabled
        (NM_STATE_DISCONNECTED 20) ;; there is no active network connection
        (NM_STATE_DISCONNECTING 30) ;; network connections are being cleaned up
        (NM_STATE_CONNECTING 40) ;; a network connection is being started
        (NM_STATE_CONNECTED_LOCAL 50) ;; there is only local IPv4 and/or IPv6 connectivity
        (NM_STATE_CONNECTED_SITE 60) ;; there is only site-wide IPv4 and/or IPv6 connectivity
        (NM_STATE_CONNECTED_GLOBAL 70) ;; there is global IPv4 and/or IPv6 Internet connectivity
        ))
    (define (state:number->symbol n)
      (cond [(findf (lambda (e) (= (cadr e) n)) *state-map*) => car]
            [else #f]))

    (printf "Initial state: ~a\n"
            (match (send network-manager Get "org.freedesktop.NetworkManager" "State")
              [(cons "u" value) (state:number->symbol value)]))

    (let loop ()
      (match (sync (dbus-listen-evt))
        [(list "/org/freedesktop/NetworkManager"
               "org.freedesktop.NetworkManager"
               "PropertiesChanged"
               (list 'signal changes))
         (match (assoc "State" changes)
           [(list* "State" "u" value)
            (printf "New state: ~a\n" (state:number->symbol value))]
           [_ (void)])]
        [other
         (printf "Asynchronous event: ~v\n" other)])
      (loop))))
