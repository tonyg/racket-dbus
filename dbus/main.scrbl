#lang scribble/manual

@require[(for-label dbus)
         (for-label racket)]

@title{D-Bus}
@author+email["Jan Dvorak" "mordae@anilinux.org"]

Native D-Bus Client For Racket.


@defmodule[dbus]


@section{Connecting}

@defproc[(dbus-connection? (v any/c)) boolean?]{
  Determines if value is a D-Bus connection.
}

@defproc[(dbus-connect/socket
           (path path-string?)
           (auth-method (-> input-port? output-port? void?)))
         dbus-connection?]{
  Connects to message bus using an UNIX domain socket using pluggable
  authentication mechanism.
}

@defproc[(dbus-auth-external (in input-port?) (out output-port?)) void?]{
  External authentication mechanism.
  Makes use of operating system capabilities to identify the user.
}


So, in order to connect to the system bus on a typical Linux system:

@racketblock[
  (current-dbus-connection
    (dbus-connect/socket "/var/run/dbus/system_bus_socket"
                         dbus-auth-external))
]


@include-section["proxy.scrbl"]


@section{Other}

@subsection{Exceptions}

@defproc[(exn:fail:dbus? (v any/c)) boolean?]{
  Generic bus failure exception.
}

@defproc[(exn:fail:dbus:signature? (v any/c)) boolean?]{
  Signature or encoding-related failure exception.
}

@defproc[(exn:fail:dbus:connection? (v any/c)) boolean?]{
  Communication-related failure exception.
}

@defproc[(exn:fail:dbus:call? (v any/c)) boolean?]{
  Remote call failed.
}


@subsection{Bus Names}

D-Bus uses a whole lot of specially formatted strings.
You can use these predicates to check your data early.

@defproc[(dbus-signature? (v any/c)) boolean?]{
  Check that the value is a valid type signature, such as
  @racket["ii"] or @racket["a(yv)"].
}

@defproc[(dbus-single-signature? (v any/c)) boolean?]{
  Check that the value is a valid type signature that encodes a single
  type, such as @racket["i"] or @racket["(ii)"].
}

@defproc[(dbus-object-path? (v any/c)) boolean?]{
  Check that the value is a valid object path, such as
  @racket["/org/freedesktop/DBus"].
}

@defproc[(dbus-interface-name? (v any/c)) boolean?]{
  Check that the value is a valid interface name, such as
  @racket["org.freedesktop.DBus"].
}

@defproc[(dbus-error-name? (v any/c)) boolean?]{
  Check that the value is a valid error name, which has the same format
  as the interface name.
  An example could be @racket["org.freedesktop.DBus.Error.Failed"].
}

@defproc[(dbus-member-name? (v any/c)) boolean?]{
  Check that the value is a valid member name, such as @racket["Hello"].
}

@defproc[(dbus-endpoint-name? (v any/c)) boolean?]{
  Check that the value is a valid endpoint name, such as
  @racket[":123.42"] or @racket["org.freedesktop.DBus"].
}


@; vim:set ft=scribble sw=2 ts=2 et:
