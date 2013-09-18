#lang scribble/manual

@require[(for-label dbus dbus/interface racket)]

@title{Interfaces}

@defmodule[dbus/interface]

@defmixin[dbus-introspectable<%> (dbus-object%/c)
                                 (dbus-introspectable<%>/c)]{
  Introspectable remote object.

  @defmethod[(Introspect) string?]{
    Retrieve the introspection XML document.
  }
}

@defthing[dbus-introspectable<%>/c contract?]{
  Contract for the @racket[dbus-introspectable<%>] mixin interface.
}

@defmixin[dbus-properties<%> (dbus-object%/c) (dbus-properties<%>/c)]{
  Remote object with properties.

  @defmethod[(Get (interface-name dbus-interface-name?)
                  (property-name dbus-member-name?))
             (cons/c dbus-single-signature? any/c)]{
    Get property value.
  }

  @defmethod[(Set (interface-name dbus-interface-name?)
                  (property-name dbus-member-name?)
                  (value (cons/c dbus-single-signature? any/c)))
             void?]{
    Set property to specified value.
  }

  @defmethod[(GetAll (interface-name dbus-interface-name?)) dict?]{
    Get values of all properties for given interface.
  }
}

@defthing[dbus-properties<%>/c contract?]{
  Contract for the @racket[dbus-properties<%>] class interface.
}


@defmixin[dbus<%> (dbus-object%/c) (dbus<%>/c)]{
  Message bus interface.

  @defmethod[(Hello) string?]{
    First method to call when communicating using a message bus.
    Returns your connection name.
  }

  @defmethod[(ListNames) (listof string?)]{
    Get list of names claimed on the bus.
  }

  @defmethod[(RequestName (name string?) (flags exact-nonnegative-integer?))
             exact-nonnegative-integer?]{
    Ask bus to assign you given name.
    See @hyperlink["http://dbus.freedesktop.org/doc/dbus-specification.html#message-bus-names"]{Message Bus Names}
    section in the D-Bus specification for details.
  }

  @defmethod[(ReleaseName (name string?)) exact-nonnegative-integer?]{
    Release owned bus name.
  }

  @defmethod[(AddMatch (rule string?)) void?]{
    Ask bus to route some messages your way.
    Used mainly for signal subscription.

    See @hyperlink["http://dbus.freedesktop.org/doc/dbus-specification.html#message-bus-routing-match-rules"]{Match Rules}
    section in the D-Bus specification for details.
  }

  @defmethod[(RemoveMatch (rule string?)) void?]{
    Cancel specified message matching rule.
  }
}

@defthing[dbus<%>/c contract?]{
  Contract for the @racket[dbus<%>] class interface.
}


@defthing[dbus% (and/c dbus<%>/c dbus-introspectable<%>/c)]{
  Proxy class for communication with the bus itself.
}


@; vim:set ft=scribble sw=2 ts=2 et:

