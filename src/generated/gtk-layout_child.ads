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

--  The base class for objects that are meant to hold layout properties.
--
--  If a `GtkLayoutManager` has per-child properties, like their packing type,
--  or the horizontal and vertical span, or the icon name, then the layout
--  manager should use a `GtkLayoutChild` implementation to store those
--  properties.
--
--  A `GtkLayoutChild` instance is only ever valid while a widget is part of a
--  layout.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gtk.Layout_Child is

   pragma Elaborate_Body;

   type Gtk_Layout_Child_Record is new GObject_Record with null record;
   type Gtk_Layout_Child is access all Gtk_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_layout_child_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget that is associated to the `GtkLayoutChild` instance.

   Layout_Manager_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Layout_Manager.Gtk_Layout_Manager
   --  The layout manager that created the `GtkLayoutChild` instance.

private
   Layout_Manager_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("layout-manager");
   Child_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child-widget");
end Gtk.Layout_Child;
