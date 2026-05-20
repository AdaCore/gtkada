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

--  `GtkLayoutChild` subclass for children in a `GtkFixedLayout`.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Gtk.Layout_Child; use Gtk.Layout_Child;

package Gtk.Fixed_Layout_Child is

   pragma Elaborate_Body;

   type Gtk_Fixed_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Fixed_Layout_Child is access all Gtk_Fixed_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_layout_child_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Transform_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gsk.Transform
   --  The transform of the child.

private
   Transform_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("transform");
end Gtk.Fixed_Layout_Child;
