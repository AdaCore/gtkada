-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Glib;                use Glib;
with Gdk;                 use Gdk;
with System;
with Glib.Glade;          use Glib.Glade, Glib.Glade.Glib_XML;
with Ada.Text_IO;         use Ada.Text_IO;
pragma Warnings (Off, Glib.Glade);
pragma Warnings (Off, Glib_XML);
pragma Warnings (Off, Ada.Text_IO);
--  Text_IO is required by almost all the package for Gate and
--  DGate, so we put the 'with' here.

package Gtk is

   function Major_Version return Guint;
   --  Returns the major version number for gtk+ (this is not the same
   --  as for GtkAda). If the version is 1.2.6, returns 1.

   function Minor_Version return Guint;
   --  Returns the minor version number for gtk+ (this is not the same
   --  as for GtkAda). If the version is 1.2.6, returns 2.

   function Micro_Version return Guint;
   --  Returns the micro version number for gtk+ (this is not the same
   --  as for GtkAda). If the version is 1.2.6, returns 6.

   type Gtk_Type is new Guint;
   Gtk_Type_None : constant Gtk_Type := 1;
   --  This type describes a type internally in gtk+. You shouldn't have to
   --  use it in your own applications, however it might be useful sometimes.
   --  Every widget type is associated with a specific value, created
   --  dynamically at run time the first time you instantiate a widget of that
   --  type (thus if you have never used a Gtk_File_Selection, it won't have
   --  any Gtk_Type associated with it).
   --  You can get the exact type value for each type by using the functions
   --  Get_Type provided in all the packages in GtkAda.
   --  You can get the specific value for an existing widget by using the
   --  function Gtk.Object.Get_Type.

   function Type_Name (Type_Num : in Gtk_Type) return String;
   --  Returns the type name corresponding to a Gtk_Type.
   --  This might be useful in debug messages.

   function Type_From_Name (Name : in String) return Gtk_Type;
   --  Converts a string to the matching type. NAME should be the C widget's
   --  name, such as GtkScrollbar or GtkButton, rather than the Ada name.

private

   --  Note: the following functions and types should only be used
   --  for internal usage, not in the user's applications.
   --  If you use type inheritance for new widgets, you should not need
   --  these functions.

   GtkAda_String : constant String := "_GtkAda" & Ascii.NUL;
   GtkAda_String_Quark : Glib.GQuark := Glib.Unknown_Quark;
   --  The name for the user data that we set in the objects.
   --  The Quark version is to speed up the string lookup (this is done
   --  only once).

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
   --  See Gtk.Type_Conversion for its use.
   --  Stub is the expect type (it is used by the simple conversion
   --  function only).

end Gtk;
