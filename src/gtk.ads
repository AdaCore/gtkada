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

with Glib; use Glib;
with Gdk;  use Gdk;
with System;

with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;
with Ada.Text_IO; use Ada.Text_IO;
pragma Warnings (Off, Glib.Glade);
pragma Warnings (Off, Glib_XML);
pragma Warnings (Off, Ada.Text_IO);
--  Text_IO is required by almost all the package for Gate and
--  DGate, so we put the 'with' here.

package Gtk is

   function Major_Version return Guint;
   function Minor_Version return Guint;
   function Micro_Version return Guint;

   function Type_Name (Type_Num : in Gint) return String;

private

   --  Note: the following functions and types should only be used
   --  for internal usage, not in the user's applications.
   --  If you use type inheritance for new widgets, you should not need
   --  these functions.

   GtkAda_String : constant String := "_GtkAda" & Ascii.NUL;
   --  The name for the user data that we set in the objects.

   procedure Initialize_User_Data (Obj : access Root_Type'Class);
   --  Sets a user data field for the C object associated with Obj.
   --  This field will be used so that it is possible, knowing a
   --  C object, to get the full ada object.

   function Get_User_Data (Obj : in System.Address; Stub : in Root_Type'Class)
                           return Root_Type_Access;
   --  Get the user data that was set by GtkAda.
   --  If the Data is not set, then returns a new access type, that points to
   --  a structure with the same tag as Stub.

   type Type_Conversion_Func is access
     function (Obj : System.Address; Stub : Root_Type'Class)
              return Root_Type_Access;
   Type_Conversion_Function : Type_Conversion_Func;
   --  This is a "soft link" for the type conversion function.  This
   --  function has to convert a C object to an Ada object. By
   --  default, it just creates a Gtk.Object, no matter what the real
   --  C type is.  It can be set to another function that will convert
   --  to the appropriate type (this is not the default since this will
   --  slow down initializations a little bit, and will 'with' all the
   --  packages from GtkAda).
   --  Stub is the expect type (it is used by the simple conversion
   --  function only).

end Gtk;
