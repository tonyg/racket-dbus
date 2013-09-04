#lang scribble/manual

@require[(for-label dbus/proxy)
         (for-label racket)]

@title{Proxy Objects}

@defmodule[dbus/proxy]

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


Objects without any methods are not very useful.  In order to create
proxies that can actually call anything, you need to make use of
@racket[define-dbus-interface] macro that will create a mixin with
proxy calls to defined methods.

@defform[(define-dbus-interface interface-name
           (method-name args-type) ...)]{
  For example:

  @racketblock[
    (define-dbus-interface dbus-interface% "org.freedesktop.DBus"
      (Hello "")
      (ListNames ""))

    (define dbus% (dbus-interface% dbus-object%))
    (define dbus (new dbus% (object-path "/org/freedesktop/DBus")
                            (endpoint "org.freedesktop.DBus")))
    (send dbus ListNames)
  ]
}


@section{Parameters}

@defparam[current-dbus-connection connection (or/c #f dbus-connection?)]{
  Parameter identifying current dbus connection used when constructing
  proxies.  Comes handy when one uses just one connection, such as
  the system bus.
}

@defparam[current-dbus-endpoint endpoint (or/c #f dbus-endpoint-name?)]{
  Parameter identifying current endpoint used when constructing proxies.
  Comes handy when one interfaces with just one peer, such as NetworkManager.
}


@; vim:set ft=scribble sw=2 ts=2 et:
