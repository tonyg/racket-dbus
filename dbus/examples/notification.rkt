#lang racket

(module+ main
  (require dbus
           dbus/interface)

  (parameterize ((current-dbus-connection (dbus-connect-session-bus)))
    (define m (dbus-manager))
    (define unique-name (send m Hello)) ;; the connection's "unique name"

    (define-dbus-interface notifications<%> "org.freedesktop.Notifications"
      (CloseNotification "u")
      (GetCapabilities "")
      (GetServerInformation "")
      (Notify "susssasa{sv}i"))

    (define notifications%
      (class (notifications<%> (dbus-introspectable<%> dbus-object%))
        (inherit Notify CloseNotification)
        (super-new)
        (define/public (notify #:summary summary
                               #:body body
                               #:app-name [app-name ""]
                               #:replaces-id [replaces-id 0]
                               #:app-icon [app-icon ""]
                               #:actions [actions '()] ;; Listof String
                               #:hints [hints '()] ;; Alist String Any
                               #:expire-timeout [expire-timeout -1])
          (Notify app-name replaces-id app-icon summary body actions hints expire-timeout))
        (define/public (close notification-id)
          (CloseNotification notification-id))))

    (define notifications
      (new notifications%
           (path "/org/freedesktop/Notifications")
           (endpoint "org.freedesktop.Notifications")
           (connection (current-dbus-connection))))

    (send m AddMatch (format "type=signal,interface=org.freedesktop.Notifications"))

    (define notification-id
      ;; NB: If you give a list of odd length for #:actions,
      ;; gnome-flashback crashes! (as of 20160919, anyway)
      (send notifications notify
            #:summary "Test notification"
            #:body "This is the notification body text!"
            #:actions '("default" "Do the thing")))

    (let loop ()
      (match (sync (dbus-listen-evt))
        [(list _ "org.freedesktop.Notifications" "ActionInvoked"
               (list 'signal (== notification-id) action))
         (printf "User clicked on the notification! They chose the ~v action.\n" action)]
        [(list _ "org.freedesktop.Notifications" "NotificationClosed"
               (list 'signal (== notification-id) reason))
         (printf "The notification was closed (reason: ~a).\n"
                 (match reason
                   [1 'expired]
                   [2 'dismissed]
                   [3 'explicitly-closed]
                   [4 'undefined/reserved]
                   [other other]))]
        [other
         (printf "Asynchronous event: ~v\n" other)])
      (loop))))
