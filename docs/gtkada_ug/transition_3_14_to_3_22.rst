.. _Transitioning_from_Gtk_3_14_to_Gtk_3_22:

***************************************
Transitioning from Gtk 3.14 to Gtk 3.22
***************************************

General
=======

Here are the package-by-package notes to transition from a GtkAda based
on Gtk+ 3.14 to one based on Gtk+ 3.22.

Gtk
===

.. highlight:: ada

Gtk.Text_View
----------

``Get_Iter_At_Position`` ``Get_Iter_At_Position`` now has a Boolean parameter
indicating whether the iterator was found inside the text.


