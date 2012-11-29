*******
General
*******

Interfaces
==========

Gtk+ interfaces no longer require an explicit "with" of the interface
package, and a cast to the interface type (with "-" and "+"). Instead,
each package now contains the list of subprograms inherited from the
various interfaces.

gtk+ transition guide
=====================

The page at http://developer.gnome.org/gtk3/3.3/gtk-migrating-2-to-3.html
provides a migration guide, from the gtk+ authors. The code examples are
in C.

Ada 2005
========

The GtkAda 3 toolkit makes use of Ada 2005 and requires GtkAda applications
to be compiled in Ada 2005 or Ada 2012 mode (e.g. using the -gnat05 or
-gnat2012 switch).

