-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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


with System;
with Gdk.Event;
with Gtk.Object;
with Gtk.Widget;
with Gtk.Notebook;

package Gtk.Arguments is

   --  <doc_ignore>Do not create automatic documentation for this package

   type Gtk_Args is private;
   --  This type represents a table of arguments. Each argument of the
   --  table can be of any type. You can access them through one of the
   --  To_* functions found below. The index of the first element is
   --  always 1.


   function Make_Args (Nb : Guint; Args : System.Address) return Gtk_Args;
   --  Build a Gtk_Args structure from the given C array. Nb should be the
   --  number of elements in the Args array.

   function Get_Nth   (Args : Gtk_Args; Num : Positive) return System.Address;
   --  Returns the Num-th element from Args. You should only have to use it if
   --  you write your own conversion functions.

   ---------------------------------------------------
   -- Conversion functions, interfacing to Gtk_Args --
   ---------------------------------------------------

   function To_Gint    (Args : Gtk_Args; Num : Positive) return Gint;
   function To_Guint   (Args : Gtk_Args; Num : Positive) return Guint;
   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean;
   function To_Event   (Args : Gtk_Args; Num : Positive)
     return Gdk.Event.Gdk_Event;
   function To_String  (Args : Gtk_Args; Num : Positive) return String;
   function To_Notebook_Page
     (Args : Gtk_Args; Num : Positive) return Gtk.Notebook.Gtk_Notebook_Page;
   function To_Address (Args : Gtk_Args; Num : Positive) return System.Address;
   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Gdk.C_Proxy;
   function To_Object
     (Args : Gtk_Args; Num : Positive) return Gtk.Object.Gtk_Object;
   --  This function can return null, if the C object was not created.
   function To_Requisition (Args : Gtk_Args; Num : Positive)
      return Gtk.Widget.Gtk_Requisition_Access;
   function To_Allocation
     (Args : Gtk_Args; Num : Positive) return Gtk.Widget.Gtk_Allocation_Access;

   ----------------------------------
   -- General conversion functions --
   ----------------------------------

   function To_Gint          (C : System.Address) return Gint;
   function To_Guint         (C : System.Address) return Guint;
   function To_Boolean       (C : System.Address) return Boolean;
   function To_Event         (C : System.Address) return Gdk.Event.Gdk_Event
                             renames Gdk.Event.From_Address;
   function To_String        (C : System.Address) return String;
   function To_Notebook_Page (C : System.Address)
     return Gtk.Notebook.Gtk_Notebook_Page;
   function To_Object        (C : System.Address) return Gtk.Object.Gtk_Object;
   function To_C_Proxy       (C : System.Address) return Gdk.C_Proxy;

   ----------------------------------
   -- Reverse conversion functions --
   ----------------------------------

   function To_Address (E : Gdk.Event.Gdk_Event) return System.Address
     renames Gdk.Event.To_Address;

private
   type Gtk_Args is record
      Nb  : Guint;
      Arr : System.Address;
   end record;

   pragma Inline (Make_Args);
   pragma Inline (Get_Nth);
   pragma Inline (To_Gint);
   pragma Inline (To_Guint);
   pragma Inline (To_Boolean);
   pragma Inline (To_Object);
   pragma Inline (To_Event);
   pragma Inline (To_String);

   --  </doc_ignore>
end Gtk.Arguments;
