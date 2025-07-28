.. _Support_for_Glade,_the_Gtk_GUI_builder:

****************************************************************
Support for Glade, the Gtk GUI builder (planned for deprecation)
****************************************************************

Deprecation
===========

GtkAda 26 will be the last release that includes Glade; Glade will be removed
from GtkAda starting with the 27.0 release.

`Glade <https://glade.gnome.org/>`_ is no longer maintained in the wider GNOME
community, as per the release notes of the last stable release 3.40.0. Using
Glade to produce XML GtkBuilder UI definitions is discouraged.

However the XML GtkBuilder UI definitions format itself is still a supported
artifact of the `Gtk.Builder <https://docs.gtk.org/gtk3/class.Builder.html>`_
API and a valid means of interaction with the `Gtkada.Builder` package.

Introduction
============

GtkAda comes with support for the GUI builder Glade-3.  Glade-3 provides a
graphical interface for designing windows and dialogs.  The interface
description is saved in an XML file which can be loaded at run-time by your
GtkAda application. With this approach, there is no need to write or generate
Ada code to describe the interface, all is needed is to write the callbacks for
various actions.

Launching Glade
===============

Under UNIX and Linux, Glade is invoked by the command-line script `glade-3`
which is located in the `bin` directory of your GtkAda installation.  Under
Windows, Glade is invoked by clicking on the executable `glade-3.exe`, also
located in the `bin` directory of your GtkAda installation.

Building your interface
=======================

In Glade-3 the interface is done by point-and-clicking. The first step is to
create one or more toplevel window and then placing widgets in these windows.

Detailed tutorials can be found at: `https://wiki.gnome.org/Apps/Glade/Tutorials
<https://wiki.gnome.org/Apps/Glade/Tutorials>`_

In the Preferences for your project (menu Edit->Preferences), make sure that
the preference "Project file format" is set to "GtkBuilder".

Using the interface in your application.
========================================

Once the interface is built and saved in an XML file, you can use it in your
GtkAda application. You will need to use objects defined in the package
`Gtkada.Builder` to load the interface file and to connect subprograms defined
in your application to signals emitted by the interface. See the detailed
explanations and examples in `gtkada-builder.ads`

