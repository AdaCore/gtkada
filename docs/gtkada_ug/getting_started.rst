.. _Getting_started_with_GtkAda:

***************************
Getting started with GtkAda
***************************

This chapter describes how to start a new GtkAda application. It explains the
basic features of the toolkit, and shows how to compile and run your
application.

It also gives a brief overview of the extensive widget hierarchy available in
GtkAda.

How to build and install GtkAda
===============================

This section explains how to build and install GtkAda on your machine.

On Windows systems, we provide an automatic installer that installs GtkAda
along with dependent components like gtk+ libraries and `Glade`.  If you are a
Windows user, you can skip the rest of this section which will address
installation on Unix systems.

On Unix systems, you first need to install the glib and gtk+ libraries.
Download the compatible packages from the gtk+ web site (`http://www.gtk.org
<http://www.gtk.org>`_), compile and install it.  Alternatively, if your
operating system vendor provides glib and gtk+ development packages, you can
install the libraries they provide.

Change your PATH environment variable so that the script `pkg-config`, which
indicates where gtk+ was installed and what libraries it needs is automatically
found by GtkAda. You will no longer need this script once GtkAda is installed,
unless you develop part of your application in C.

OpenGL support will not be activated in GtkAda unless you already have the
OpenGL libraries on your systems. You can for instance look at Mesa, which is
free implementation.

Optionally, you can also install the `Glade` interface builder. Get the
compatible package from the Glade web site, compile and install it.

You can finally download the latest version of GtkAda from the web site.  Untar
and uncompress the package, then simply do the following steps::

  $ ./configure
  $ make
  $ make tests     (this step is optional)
  $ make install

As usual with the `configure` script, you can specify where you want
to install the GtkAda libraries by using the `--prefix` switch.

You can specify the switch `--disable-shared` to prevent building shared
libraries, even if your system supports them (by default, both shared and
static libraries are installed). By default, your application will be linked
statically with the GtkAda libraries. You can override this default by
specifying `--enable-shared` as a switch to `configure`, although you can
override it later through the LIBRARY_TYPE scenario variable.

If you have some OpenGL libraries installed on your system, you can make sure
that `configure` finds them by specifying the `--with-GL-prefix` switch on the
command line. `configure` should be able to automatically detect the libraries
however.

You must then make sure that the system will be able to find the dynamic
libraries at run time if your application uses them. Typically, you would do
one of the following:

* run `ldconfig` if you installed GtkAda in one of the standard
  location and you are super-user on your machine
* edit `/etc/ld.conf` if you are super-user but did not install
  GtkAda in one of the standard location. Add the path that contains
  libgtkada.so (by default :file:`/usr/local/lib` or :file:`$prefix/lib`.
* modify your `LD_LIBRARY_PATH` environment variable if you are
  not super-user. You should simply add the path to libgtkada.

In addition, if you are using precompiled Gtk+ binary packages, you will
also need to set the `FONTCONFIG_FILE` environment variable to point to
the :file:`prefix/etc/fonts/fonts.conf` file of your binary installation.

For example, assuming you have installed Gtk+ under :file:`/opt/gtk` and
using bash::

  $ export FONTCONFIG_FILE=/opt/gtk/etc/fonts/fonts.conf

If your application is using printing, on UNIX and Linux you will need to
point your environment variable GTK_PATH to the directory that contains
your Gtk+ libraries, appending it with the :file:`gtk-3.0` suffix::

  $ export GTK_PATH=<gtk_install_dir>/lib/gtk-3.0

This will allow Gtk+ to show the available printers and options when using
`Gtk.Print` and `Gtk.PrintOperations` packages (or `Gtkada.Printing`, which
is a high-level interface built on top of these packages).

How to distribute a GtkAda application
======================================

Since GtkAda depends on Gtk+, you usually need to distribute some Gtk+
libraries along with your application.

Under some OSes such as Linux, Gtk+ comes preinstalled, so in this case, a
simple solution is to rely on the preinstalled Gtk+ libraries. See below for
more information on the gtkada library itself.

Under other unix systems, GtkAda usually comes with a precompiled set of Gtk+
libraries that have been specifically designed to be easily redistributed.

In order to use the precompiled Gtk+ binaries that we distribute with GtkAda,
you need to distribute all the Gtk+ .so libraries along with your application,
and use the LD_LIBRARY_PATH environment variable to point to these libraries.

The list of libraries needed is :file:`<gtkada-prefix>/lib/lib*.so.?` or
:file:`<gtkada-prefix>/lib64/lib*.so.?` along with your executable, and set
LD_LIBRARY_PATH.

You may also need the :file:`libgtkada-xxx.so` file. This dependency is
optional since gtkada supports both static and dynamic linking, depending
on how your project sets up the library type in :file:`gtkada.gpr`. You
might chose to link with the static library :file:`libgtkada.a` for
convenience.

Under Windows, you need to distribute the following files and directories
along with your application, and respect the original directory set up:

* :file:`bin/*.dll`
* :file:`etc/`
* :file:`lib/gtk-2.0`

How to use GtkAda
=================

On Unix systems, to use GtkAda, you need to have you PATH and LD_LIBRARY_PATH
environment variables set, as explained above::

   PATH=$prefix/bin:$PATH
   LD_LIBRARY_PATH=$prefix/lib:$LD_LIBRARY_PATH
   export PATH LD_LIBRARY_PATH

Set the following variables as well when using a custom gtk+ build (but not
if you are using the system's libraries)::

   GDK_PIXBUF_MODULE_FILE=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
   GDK_PIXBUF_MODULEDIR=$prefix/lib/gdk-pixbuf-2.0/2.10.0/loaders/
   export GDK_PIXBUF_MODULEDIR GDK_PIXBUF_MODULE_FILE

   FONTCONFIG_FILE=$prefix/etc/fonts/fonts.conf
   export FONTCONFIG_FILE

   XDG_DATA_DIRS=$XDG_DATA_DIRS:$prefix/share
   export XDG_DATA_DIRS

GDK_PIXBUF_MODULE_FILE contains the paths to find the libpixbufloader-*
libraries. By default, the paths are relative to the executable loading the
libraries: thus, GDK_PIXBUF_MODULEDIR must be installed relatively to the
executable. When the paths are absolute, the location of
GDK_PIXBUF_MODULEDIR doesn't matter when loading the libraries however it will
be necessary to re-generate GDK_PIXBUF_MODULE_FILE on each host.
Two executables are packaged with GtkAda to re-generate the modules' paths:
`gdk-pixbuf-query-loaders`, which generates relative paths, and
`gdk-pixbuf-query-loaders-absolute`, which generates absolute paths. Both should
be launched with the `--update-cache` option to re-generate the proper cache file
(by default it will just output the contents on stdout).

Organization of the GtkAda package
==================================

In addition to the full sources, the GtkAda package contains a lot of heavily
commented examples. If you haven't been through those examples, we really
recommend that you look at them and try to understand them, since they contain
some examples of code that you might find interesting for your own application.


* :file:`testgtk/` directory:

  This directory contains the application `testgtk` that tests all the
  widgets in GtkAda. It gives you a quick overview of what can be found in the
  toolkit, as well as some detailed information on the widgets and their
  parameters.

  Each demo is associated with contextual help pointing to aspects worth
  studying.

  It also contains an OpenGL demo, if GtkAda was compiled with support for
  OpenGL.

  This program is far more extensive that its C counterpart, and the GtkAda
  team has added a lot of new examples.

  This directory also contains the application `testcairo` which
  demonstrates the use of various Cairo functions in GtkAda.

* :file:`docs/` directory:

  It contains the html, info, text and @TeX{} versions of the documentation you
  are currently reading. Note that the documentation is divided into two
  subdirectories, one containing the user guide, which you are currently
  reading, the other containing the reference manual, which gives detailed
  information on all the widgets found in GtkAda. The docs directory also
  contains a subdirectory with some slides that were used to present GtkAda
  at various shows.

How to compile an application with GtkAda
=========================================

This section explains how you can compile your own applications.

A set of project files is installed along with GtkAda. If you have installed
GtkAda in the same location as GNAT itself, nothing else needs to be done.

Otherwise, you need to make the directory that contains these project files
visible to the compiler. This is done by adding the directory to the
`GPR_PROJECT_PATH` environment variable. Assuming you have installed the
library in :file:`prefix`, the directory you need to add is
:file:`prefix/lib/gnat`.

On Unix, this is done with::

  csh:
     setenv GPR_PROJECT_PATH $prefix/lib/gnat:$GPR_PROJECT_PATH
  sh:
     GPR_PROJECT_PATH=$prefix/lib/gnat:$GPR_PROJECT_PATH
     export GPR_PROJECT_PATH

.. highlight:: ada

To build your own application, you should then setup a project file (see
the GNAT documentation for more details on project files), which simply
contains the statement::

  with "gtkada";

This will automatically set the right compiler and linker options, so that
your application is linked with GtkAda.

By default, the linker will use GtkAda's shared library, if it was built.
If you would prefer to link with the static library, you can set the
environment variable::

  LIBRARY_TYPE=static
  export LIBRARY_TYPE

before launching the compiler or linker, which will force it to use the
static library instead.

Architecture of the toolkit
===========================

The gtk+ toolkit has been designed from the beginning to be portable.  It is
made of two libraries: `gtk` and `gdk`.  In addition, GtkAda provides binding
to three supporting libraries: `pango`, `cairo` and `glib`.

`Glib` is a non-graphical library that includes support for lists, h-tables,
threads, and so on. It is a highly optimized, platform-independent library.
Since most of its contents are already available in Ada (or in the
:file:`GNAT.*` hierarchy in the GNAT distribution), GtkAda does not include a
complete binding to it.  For the parts of `Glib` that we do depend on, we
provide :file:`Glib.*` packages in the GtkAda distribution.

`Gdk` is the platform-dependent part of gtk+, and so there are different
implementations (for instance, for Win32 and X11 based systems) that implement
a common API. `Gdk` provides basic graphical functionality to, for instance,
draw lines, rectangles and pixmaps on the screen, as well as manipulate colors.
The :file:`Gdk.*` packages provide a full Ada interface to `Gdk`.

`Pango` is a modern font handling system. Bindings in GtkAda gives access to
the API to manipulate font descriptions and text attributes.

`Cairo` is the low-level 2D drawing library used by `Gdk` to render widgets.
`Cairo` provides a rich set of vector drawing features, supporting
anti-aliasing, transparency, and 2D matrix transformations.The :file:`Cairo.*`
packages provide a complete Ada binding to `Cairo`.

`Gtk` is the top level library. It is platform independent, and does all its
drawing through calls to Gdk and Cairo. This is where the high-level widgets
are defined. It also includes support for callbacks. Its equivalent in the
GtkAda libraries are the :file:`Gtk.*` packages. It is made of a fully
object-oriented hierarchy of widgets (see :ref:`Widgets_Hierarchy`).

Since your application only calls GtkAda, it is fully portable, and can be
recompiled as-is on other platforms::

  +------------------------------- ----------+
  |             Your Application             |
  +------------------------------------------+
  |                 GtkAda                   |
  |              +-----------------+         |
  |              |      GTK        |         |
  |         +----+-----------------+----+    |
  |         |           GDK             |    |
  |    +----+------+         +----------+----+
  |    |   Pango   |         |     Cairo     |
  +----+-----------+----+----+---------------+
  |        GLIB         |   X-Window / Win32  |
  +---------------------+--------------------+

Although the packages have been evolving a lot since the first versions of
GtkAda, the specs are stabilizing now. We will try as much as possible to
provide backward compatibility whenever possible.

Since GtkAda is based on gtk+ we have tried to stay as close to it as possible
while using high-level features of the Ada language. It is thus relatively easy
to convert external examples from C to Ada.

We have tried to adopt a consistent naming scheme for Ada identifiers:

* The widget names are the same as in C, except that an underscore
  sign (_) is used to separate words, e.g::

    Gtk_Button   Gtk_Color_Selection_Dialog

* Because of a clash between Ada keywords and widget names, there
  are two exceptions to the above general rule::

    Gtk.GEntry.Gtk_Entry   Gtk.GRange.Gtk_Range

* The function names are the same as in  C, ignoring the leading
  `gtk_` and the widget name, e.g::

    gtk_misc_set_padding        =>  Gtk.Misc.Set_Padding
    gtk_toggle_button_set_state =>  Gtk.Toggle_Button.Set_State

* Most enum types have been grouped in the :file:`gtk-enums.ads` file

* Some features have been implemented as generic packages. These
  are the timeout functions (see `Gtk.Main.Timeout`), the idle functions
  (see `Gtk.Main.Idle`), and the data that can be attached to any object
  (see `Gtk.Object.User_Data`). Type safety is ensured through these
  generic packages.

* Callbacks were the most difficult thing to interface with. These
  are extremely powerful and versatile, since the callbacks can have any
  number of arguments and may or may not return values. These are once
  again implemented as generic packages, that require more explanation
  (:ref:`Signal_handling`).

**WARNING:** all the generic packages allocate some memory for internal
structures, and call internal functions. This memory is freed by gtk
itself, by calling some Ada functions. Therefore the generic packages
have to be instantiated at library level, not inside a subprogram, so
that the functions are still defined when gtk needs to free the memory.

**WARNING** Before any other call to the GtkAda library is performed,
`Gtk.Main.Init` must be invoked first. Most of the time, this
procedure is invoked from the main procedure of the application, in
which case no use of GtkAda can be done during the application
elaboration.

Widgets Hierarchy
=================

All widgets in `GtkAda` are implemented as tagged types. They all have a common
ancestor, called `Gtk.Object.Gtk_Object`. All visual objects have a common
ancestor called `Gtk.Widget.Gtk_Widget`.

The following table describes the list of objects and their inheritance tree.
As usual with tagged types, all the primitive subprograms defined for a type
are also known for all of its children. This is a very powerful way to create
new widgets, as will be explained in :ref:`Creating_new_widgets_in_Ada`.

Although gtk+ was written in C its design is object-oriented, and thus GtkAda
has the same structure. The following rules have been applied to convert from C
names to Ada names: a widget `Gtk_XXX` is defined in the Ada package `Gtk.XXX`,
in the file :file:`gtk-xxx.ads`. This follows the GNAT convention for file
names.  For instance, the `Gtk_Text` widget is defined in the package
`Gtk.Text`, in the file :file:`gtk-text.ads`.

Note also that most of the documentation for GtkAda is found in the spec files
themselves.

It is important to be familiar with this hierarchy. It is then easier to know
how to build and organize your windows. Most widgets are demonstrated in the
:file:`testgtk/` directory in the GtkAda distribution.

.. _Widgets_Hierarchy:
.. figure:: hierarchy.jpg

   Widgets Hierarchy
