------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Allows objects to extend and customize deserialization from ui files.
--
--  The `GtkBuildable` interface includes methods for setting names and
--  properties of objects, parsing custom tags and constructing child objects.
--
--  It is implemented by all widgets and many of the non-widget objects that
--  are provided by GTK. The main user of this interface is [classGtk.Builder].
--  There should be very little need for applications to call any of these
--  functions directly.
--
--  An object only needs to implement this interface if it needs to extend the
--  `GtkBuilder` XML format or run any extra routines at deserialization time.

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Glib.Types; use Glib.Types;

package Gtk.Buildable is

   type Gtk_Buildable is new Glib.Types.GType_Interface;
   Null_Gtk_Buildable : constant Gtk_Buildable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_buildable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Buildable_Id (Self : Gtk_Buildable) return UTF8_String;
   --  Gets the ID of the Buildable object.
   --  `GtkBuilder` sets the name based on the ID attribute of the `<object>`
   --  tag used to construct the Buildable.
   --  @return the ID of the buildable object

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Buildable"

   function "+" (W : Gtk_Buildable) return Gtk_Buildable;
   pragma Inline ("+");

private

   Null_Gtk_Buildable : constant Gtk_Buildable :=
      Gtk_Buildable (Glib.Types.Null_Interface);
end Gtk.Buildable;
