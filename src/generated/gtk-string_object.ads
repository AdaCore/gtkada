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

--  The type of items in a `GtkStringList`.
--
--  A `GtkStringObject` is a wrapper around a `const char*`; it has a
--  [propertyGtk.StringObject:string] property that can be used for property
--  bindings and expressions.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.String_Object is

   type Gtk_String_Object_Record is new GObject_Record with null record;
   type Gtk_String_Object is access all Gtk_String_Object_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_String_Object; String : UTF8_String);
   procedure Initialize
      (Self   : not null access Gtk_String_Object_Record'Class;
       String : UTF8_String);
   --  Wraps a string in an object for use with `GListModel`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param String The string to wrap

   function Gtk_String_Object_New
      (String : UTF8_String) return Gtk_String_Object;
   --  Wraps a string in an object for use with `GListModel`.
   --  @param String The string to wrap

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_string_object_get_type");

   -------------
   -- Methods --
   -------------

   function Get_String
      (Self : not null access Gtk_String_Object_Record) return UTF8_String;
   --  Returns the string contained in a `GtkStringObject`.
   --  @return the string of Self

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   String_Property : constant Glib.Properties.Property_String;
   --  The string.

private
   String_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("string");
end Gtk.String_Object;
