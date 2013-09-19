#lang scribble/manual

@require[(for-label dbus racket)]

@title{D-Bus}
@author+email["Jan Dvorak" "mordae@anilinux.org"]

Native D-Bus Client For Racket.


@defmodule[dbus]


@section{Connecting}

@defproc[(dbus-connection? (v any/c)) boolean?]{
  Determines if value is a D-Bus connection.
}

@defproc[(dbus-connect/socket
           (path path-string? "/var/run/dbus/system_bus_socket")
           (auth-method
            (-> input-port? output-port? void?)
           dbus-auth-external))
         dbus-connection?]{
  Connect to message bus via an UNIX domain socket using pluggable
  authentication mechanism.
}

@defproc[(dbus-connect/tcp
           (host string?)
           (port (integer-in 1 65535))
           (auth-method
            (-> input-port? output-port? void?)
            dbus-auth-anonymous))
         dbus-connection?]{
  Connect to message bus via a TCP socket using pluggable authentication
  mechanism.  It does not make much of a sense to use anything else than
  @racket[dbus-auth-anonymous].
}

@defproc[(dbus-auth-external (in input-port?) (out output-port?)) void?]{
  External authentication mechanism.
  Makes use of operating system capabilities to identify the user.
}

@defproc[(dbus-auth-anonymous (in input-port?) (out output-port?)) void?]{
  Anonymous authentication mechanism.
  It's usually not enabled on the server.
}


So, in order to connect to the system bus on a typical Linux system:

@racketblock[
  (current-dbus-connection
    (dbus-connect/socket "/var/run/dbus/system_bus_socket"))
]

Or, to connect to a remote bus without authentication:

@racketblock[
  (current-dbus-connection
    (dbus-connect/tcp "localhost" 1234))
]

@defproc[(dbus-listen (callback (-> dbus-object-path?
                                    dbus-interface-name?
                                    dbus-member-name?
                                    any/c
                                    void?))
                      (connection dbus-connection? (current-dbus-connection)))
         void?]{
  Read notifications from the D-Bus connection and call specified
  @racket[callback] with appropriate parameters every time one arrives.
}


@section{Proxy Objects}

In order to operate on remote objects, you first need to describe their
interfaces and create proxy object instances that will map remote objects
to regular ones.


@defclass[dbus-object% object% ()]{
  Generic D-Bus object proxy that have no methods defined.

  @defconstructor[((path dbus-object-path?)
                   (endpoint dbus-endpoint-name? (current-dbus-endpoint))
                   (connection dbus-connection? (current-dbus-connection)))]{
    Every @racket[dbus-object%] instance need to know these three things in
    order to communicate with the actual remote object.  It can be
    advantageous to define @racket[current-dbus-connection] or even
    @racket[current-dbus-endpoint] beforehand.
  }
}

@defthing[dbus-object%/c contract?]{
  Contract for the @racket[dbus-object%] class interface.
}


Objects without any methods are not very useful.  In order to create
proxies that can actually call anything, you need to make use of
@racket[define-dbus-interface] macro that will create a mixin with
proxy calls to defined methods.

@defform[(define-dbus-interface interface-name
           (method-name args-type) ...)]{
  For example:

  @racketblock[
    (define-dbus-interface dbus-interface<%> "org.freedesktop.DBus"
      (Hello "")
      (ListNames ""))

    (define dbus% (dbus-interface<%> dbus-object%))
    (define dbus (new dbus% (object-path "/org/freedesktop/DBus")
                            (endpoint "org.freedesktop.DBus")))
    (send dbus ListNames)
  ]
}

@subsection{Parameters}

@defparam[current-dbus-connection connection (or/c #f dbus-connection?)]{
  Parameter identifying current dbus connection used when constructing
  proxies.  Comes handy when one uses just one connection, such as
  the system bus.
}

@defparam[current-dbus-endpoint endpoint (or/c #f dbus-endpoint-name?)]{
  Parameter identifying current endpoint used when constructing proxies.
  Comes handy when one interfaces with just one peer, such as NetworkManager.
}


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


@include-section["interface.scrbl"]


@; vim:set ft=scribble sw=2 ts=2 et:
