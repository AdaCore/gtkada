-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  This package provides some basic Gtk+ functionalities such as getting the
--  version number.  For general GtkAda initializations,
--  @pxref{Package_Gtk.Main}.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Glib;                use Glib;
with Gdk;                 use Gdk;

pragma Warnings (Off);
with Glib.GObjects;       use Glib.GObjects;
with System;
pragma Warnings (On);

package Gtk is

   type Gtk_Rc_Style is new Gdk.C_Proxy;
   --  Type used to handle resource styles.
   --  See package Gtk.Rc for more details.

   function Major_Version return Guint;
   --  Return the major version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 1.

   function Minor_Version return Guint;
   --  Return the minor version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 2.

   function Micro_Version return Guint;
   --  Return the micro version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 6.

   subtype Gtk_Type is Glib.GType;
   --  Renaming used for compatiblity.
   --  Note: Gtk_Type_* constants have been replaced by GType_* constants
   --  in Glib.

   type Gtk_Notebook_Page is new Gdk.C_Proxy;
   --  A page of the notebook.
   --  It can contain a single child, and is also associated with a tab
   --  label used to select that page in the notebook.

   ------------------------
   -- Interfacing with C --
   ------------------------
   --  The following functions are made public so that one can easily create
   --  new widgets outside the Gtk package hierarchy.
   --  Only experienced users should make use of these functions.

   function Count_Arguments
     (The_Type : Gtk_Type; Name : in String) return Guint;
   --  Return the number of arguments used in the handlers for the signal.
   --  Note that in the Connect functions, we always test whether the user
   --  has asked for *at most* the number of arguments defined by gtk+ for the
   --  callback. This is because having less argument is authorized (the
   --  extra parameters passed by gtk+ will simply be ignored), whereas having
   --  more arguments is impossible (they would never be set).
   --  Note that we provide this procedure here to avoid circularities.
   --  <doc_ignore>
   --  ??? This function should probably be moved to Glib.Gobjects
   --  </doc_ignore>

   function Argument_Type
     (The_Type : Gtk_Type;
      Name     : in String;
      Num      : in Gint) return Gtk_Type;
   --  Return the type of the num-th argument for the handlers of signal name.
   --  If Num is negative, return the type returned by the handlers for this
   --  signal.
   --  Note that we provide this procedure here to avoid circularities.
   --  <doc_ignore>
   --  ??? This function should probably be moved to Glib.Gobjects
   --  </doc_ignore>

private

   pragma Import (C, Major_Version, "ada_gtk_major_version");
   pragma Import (C, Minor_Version, "ada_gtk_minor_version");
   pragma Import (C, Micro_Version, "ada_gtk_micro_version");

end Gtk;
