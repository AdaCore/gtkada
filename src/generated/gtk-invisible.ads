------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  The Gtk.Invisible.Gtk_Invisible widget is used internally in GTK+, and is
--  probably not very useful for application developers.
--
--  It is used for reliable pointer grabs and selection handling in the code
--  for drag-and-drop.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Invisible is

   type Gtk_Invisible_Record is new Gtk_Widget_Record with null record;
   type Gtk_Invisible is access all Gtk_Invisible_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Invisible);
   procedure Initialize (Self : not null access Gtk_Invisible_Record'Class);
   --  Creates a new Gtk.Invisible.Gtk_Invisible.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Invisible_New return Gtk_Invisible;
   --  Creates a new Gtk.Invisible.Gtk_Invisible.

   procedure Gtk_New_For_Screen
      (Self   : out Gtk_Invisible;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   procedure Initialize_For_Screen
      (Self   : not null access Gtk_Invisible_Record'Class;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Creates a new Gtk.Invisible.Gtk_Invisible object for a specified screen
   --  Since: gtk+ 2.2
   --  Initialize_For_Screen does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "screen": a Gdk.Screen.Gdk_Screen which identifies on which the new
   --  Gtk.Invisible.Gtk_Invisible will be created.

   function Gtk_Invisible_New_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Invisible;
   --  Creates a new Gtk.Invisible.Gtk_Invisible object for a specified screen
   --  Since: gtk+ 2.2
   --  "screen": a Gdk.Screen.Gdk_Screen which identifies on which the new
   --  Gtk.Invisible.Gtk_Invisible will be created.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_invisible_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Screen
      (Self : not null access Gtk_Invisible_Record)
       return Gdk.Screen.Gdk_Screen;
   --  Returns the Gdk.Screen.Gdk_Screen object associated with Invisible
   --  Since: gtk+ 2.2

   procedure Set_Screen
      (Self   : not null access Gtk_Invisible_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class);
   --  Sets the Gdk.Screen.Gdk_Screen where the Gtk.Invisible.Gtk_Invisible
   --  object will be displayed.
   --  Since: gtk+ 2.2
   --  "screen": a Gdk.Screen.Gdk_Screen.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Screen_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Screen.Gdk_Screen

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Invisible_Record, Gtk_Invisible);
   function "+"
     (Widget : access Gtk_Invisible_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Invisible
   renames Implements_Gtk_Buildable.To_Object;

private
   Screen_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("screen");
end Gtk.Invisible;
