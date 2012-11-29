*******
General
*******

GtkAda 3.x is a binding to the C library gtk+ 3.x. This is a major
release, with several incompatible changes. Most of those incompatibilities
are due to major changes in the C library. Mostly, the gtk+ developers
have performed a general cleanup, removing old types and subprograms that
were rarely used and belong to more specialized libraries.

They have also made significant changes in the internals of the library.
A lot of these changes should not impact typical user code, although they
will if you are writting your own container widgets.

The gtk+ developers have documented various things that will likely need
to be changed in user applications. The page at
http://developer.gnome.org/gtk3/3.3/gtk-migrating-2-to-3.html provides a
migration guide. Its code samples are in C, but should be applicable to
Ada quite easily.

GtkAda itself has also undergone its own changes. One of the most
significants is that most of the binding is now automatically generated
from XML files provided by the gtk+ developers. This ensures that the
binding is much more complete than it was before, and will be much
easier to evolve when new releases of gtk+ are made available.

It also means that users can, theoritically at least, automatically bind
a number of libraries from the gtk+/GNOME ecosystem. The automatic
generation relies on XML files, called GIR files from their ``.gir``
extension. If you wish to parse other files, you should likely modify
the toplevel Makefile (the ``generate`` target), as well as the file
:file:`contrib/data.py` to list which types should be bound. We do not
necessarily encourage you to generate your own bindings, and this
generation is likely to be more than just modifying one or two files...

Interfaces
==========

One other advantage of the automatic generation is that it allows us
to provide more advanced feature in the binding.

For instance, gtk+ has the notion of interfaces (which play a similar
role to Ada05 interfaces).

In GtkAda interfaces no longer require an explicit "with" of the interface
package, and a cast to the interface type (with "-" and "+"). Instead,
each package now contains the list of subprograms inherited from the
various interfaces.

So basically, all subprograms inherited from an interface become
available as primitive operations in the types that implement the interface.

We also expect to simplify the handling of signals and signal handlers.

Ada 2005
========

GtkAda 3 makes use of Ada 2005 and requires GtkAda applications
to be compiled in Ada 2005 or Ada 2012 mode (e.g. using the -gnat05 or
-gnat2012 switch).

This makes it possible to use the object-dotted notation when calling
primitive operations. For instance, the following code::

    Gtk.Window.Set_Default_Size (Window, 800, 600);

can be replaced with::

    Window.Set_Default_Size (800, 600);

