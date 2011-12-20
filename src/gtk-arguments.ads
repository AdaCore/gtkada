------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  This package is obsolete and replaced by Glib.Values.
--  Future versions of GtkAda will no longer provide this package.
--
--  This package provides a convenient interface to C, providing easy
--  conversion from a C's (void*) pointer to any Ada type used in
--  GtkAda.  Although this package has been designed to be easily
--  reusable by being as general as possible, these functions are mainly
--  used when writing callbacks and/or marshallers (see Gtk.Marshallers
--  and Gtk.Handlers).
--
--  Therefore, the main type in this package is Gtk_Args, which is the
--  equivalent of the C's (GtkArg*) array, i.e an array of unions.  This
--  package provides functions to extract the values from this type.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Signal handling</group>

with Glib.Values;
with Glib.Object;
with Gdk.Event;
with Gtk.Widget;

package Gtk.Arguments is

   --  <doc_ignore>Do not create automatic documentation for this package

   subtype Gtk_Args is Glib.Values.GValues;
   --  This type represents a table of arguments. Each argument of the
   --  table can be of any type. You can access them through one of the
   --  To_* functions found below. The index of the first element is
   --  always 1.

   function Make_Args (Nb : Guint; Args : System.Address) return Gtk_Args
     renames Glib.Values.Make_Values;
   --  Build a Gtk_Args structure from the given C array. Nb should be the
   --  number of elements in the Args array.

   ---------------------------------------------------
   -- Conversion functions, interfacing to Gtk_Args --
   ---------------------------------------------------

   function To_Gint    (Args : Gtk_Args; Num : Positive) return Gint;
   function To_Guint   (Args : Gtk_Args; Num : Positive) return Guint;
   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean;
   function To_Event   (Args : Gtk_Args; Num : Positive)
     return Gdk.Event.Gdk_Event;
   function To_String  (Args : Gtk_Args; Num : Positive) return UTF8_String;
   function To_Notebook_Page
     (Args : Gtk_Args; Num : Positive) return Gtk_Notebook_Page;
   function To_Address (Args : Gtk_Args; Num : Positive) return System.Address;
   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Gdk.C_Proxy;
   function To_Object
     (Args : Gtk_Args; Num : Positive) return Glib.Object.GObject;
   --  This function can return null, if the C object was not created.
   function To_Requisition (Args : Gtk_Args; Num : Positive)
      return Gtk.Widget.Gtk_Requisition_Access;
   function To_Allocation
     (Args : Gtk_Args; Num : Positive) return Gtk.Widget.Gtk_Allocation_Access;

private
   pragma Inline (To_Gint);
   pragma Inline (To_Guint);
   pragma Inline (To_Boolean);
   pragma Inline (To_Object);
   pragma Inline (To_Event);
   pragma Inline (To_String);

   --  </doc_ignore>
end Gtk.Arguments;
