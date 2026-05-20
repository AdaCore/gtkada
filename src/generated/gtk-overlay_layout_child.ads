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

--  `GtkLayoutChild` subclass for children in a `GtkOverlayLayout`.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Properties;  use Glib.Properties;
with Gtk.Layout_Child; use Gtk.Layout_Child;

package Gtk.Overlay_Layout_Child is

   pragma Elaborate_Body;

   type Gtk_Overlay_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Overlay_Layout_Child is access all Gtk_Overlay_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_overlay_layout_child_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Clip_Overlay
      (Child : not null access Gtk_Overlay_Layout_Child_Record)
       return Boolean;
   --  Retrieves whether the child is clipped.
   --  @return whether the child is clipped

   procedure Set_Clip_Overlay
      (Child        : not null access Gtk_Overlay_Layout_Child_Record;
       Clip_Overlay : Boolean);
   --  Sets whether to clip this child.
   --  @param Clip_Overlay whether to clip this child

   function Get_Measure
      (Child : not null access Gtk_Overlay_Layout_Child_Record)
       return Boolean;
   --  Retrieves whether the child is measured.
   --  @return whether the child is measured

   procedure Set_Measure
      (Child   : not null access Gtk_Overlay_Layout_Child_Record;
       Measure : Boolean);
   --  Sets whether to measure this child.
   --  @param Measure whether to measure this child

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Clip_Overlay_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the child should be clipped to fit the parent's size.

   Measure_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the child size should contribute to the `GtkOverlayLayout`'s
   --  measurement.

private
   Measure_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("measure");
   Clip_Overlay_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("clip-overlay");
end Gtk.Overlay_Layout_Child;
