------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  Gtk.Scrollable.Gtk_Scrollable is an interface that is implemented by
--  widgets with native scrolling ability.
--
--  To implement this interface you should override the
--  Gtk.Scrollable.Gtk_Scrollable:hadjustment and
--  Gtk.Scrollable.Gtk_Scrollable:vadjustment properties.
--
--  == Creating a scrollable widget ==
--
--  All scrollable widgets should do the following.
--
--     * When a parent widget sets the scrollable child widget's adjustments,
--  the widget should populate the adjustments'
--  Gtk.Adjustment.Gtk_Adjustment:lower, Gtk.Adjustment.Gtk_Adjustment:upper,
--  Gtk.Adjustment.Gtk_Adjustment:step-increment,
--  Gtk.Adjustment.Gtk_Adjustment:page-increment and
--  Gtk.Adjustment.Gtk_Adjustment:page-size properties and connect to the
--  Gtk.Adjustment.Gtk_Adjustment::value-changed signal.
--
--     * Because its preferred size is the size for a fully expanded widget,
--  the scrollable widget must be able to cope with underallocations. This
--  means that it must accept any value passed to its
--  Gtk.Widget_Class.Gtk_Widget_Class.size_allocate function.
--
--     * When the parent allocates space to the scrollable child widget, the
--  widget should update the adjustments' properties with new values.
--
--     * When any of the adjustments emits the
--  Gtk.Adjustment.Gtk_Adjustment::value-changed signal, the scrollable widget
--  should scroll its contents.
--
--
--  </description>
--  <group>Interfaces</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Enums;       use Gtk.Enums;

package Gtk.Scrollable is

   type Gtk_Scrollable is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrollable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Hadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the Gtk.Adjustment.Gtk_Adjustment used for horizontal
   --  scrolling.
   --  Since: gtk+ 3.0

   procedure Set_Hadjustment
      (Self        : Gtk_Scrollable;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the horizontal adjustment of the Gtk.Scrollable.Gtk_Scrollable.
   --  Since: gtk+ 3.0
   --  "hadjustment": a Gtk.Adjustment.Gtk_Adjustment

   function Get_Hscroll_Policy
      (Self : Gtk_Scrollable) return Gtk.Enums.Gtk_Scrollable_Policy;
   pragma Import (C, Get_Hscroll_Policy, "gtk_scrollable_get_hscroll_policy");
   --  Gets the horizontal Gtk.Enums.Gtk_Scrollable_Policy.
   --  Since: gtk+ 3.0

   procedure Set_Hscroll_Policy
      (Self   : Gtk_Scrollable;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);
   pragma Import (C, Set_Hscroll_Policy, "gtk_scrollable_set_hscroll_policy");
   --  Sets the Gtk.Enums.Gtk_Scrollable_Policy to determine whether
   --  horizontal scrolling should start below the minimum width or below the
   --  natural width.
   --  Since: gtk+ 3.0
   --  "policy": the horizontal Gtk.Enums.Gtk_Scrollable_Policy

   function Get_Vadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the Gtk.Adjustment.Gtk_Adjustment used for vertical
   --  scrolling.
   --  Since: gtk+ 3.0

   procedure Set_Vadjustment
      (Self        : Gtk_Scrollable;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the vertical adjustment of the Gtk.Scrollable.Gtk_Scrollable.
   --  Since: gtk+ 3.0
   --  "vadjustment": a Gtk.Adjustment.Gtk_Adjustment

   function Get_Vscroll_Policy
      (Self : Gtk_Scrollable) return Gtk.Enums.Gtk_Scrollable_Policy;
   pragma Import (C, Get_Vscroll_Policy, "gtk_scrollable_get_vscroll_policy");
   --  Gets the vertical Gtk.Enums.Gtk_Scrollable_Policy.
   --  Since: gtk+ 3.0

   procedure Set_Vscroll_Policy
      (Self   : Gtk_Scrollable;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);
   pragma Import (C, Set_Vscroll_Policy, "gtk_scrollable_set_vscroll_policy");
   --  Sets the Gtk.Enums.Gtk_Scrollable_Policy to determine whether vertical
   --  scrolling should start below the minimum height or below the natural
   --  height.
   --  Since: gtk+ 3.0
   --  "policy": the vertical Gtk.Enums.Gtk_Scrollable_Policy

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Hadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --  Horizontal Gtk.Adjustment.Gtk_Adjustment of the scrollable widget. This
   --  adjustment is shared between the scrollable widget and its parent.
   --
   --  Name: Hscroll_Policy_Property
   --  Type: Gtk.Enums.Gtk_Scrollable_Policy
   --  Flags: read-write
   --  Determines whether horizontal scrolling should start once the
   --  scrollable widget is allocated less than its minimum width or less than
   --  its natural width.
   --
   --  Name: Vadjustment_Property
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Flags: read-write
   --  Verical Gtk.Adjustment.Gtk_Adjustment of the scrollable widget. This
   --  adjustment is shared between the scrollable widget and its parent.
   --
   --  Name: Vscroll_Policy_Property
   --  Type: Gtk.Enums.Gtk_Scrollable_Policy
   --  Flags: read-write
   --  Determines whether vertical scrolling should start once the scrollable
   --  widget is allocated less than its minimum height or less than its
   --  natural height.

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   Hscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;
   Vadjustment_Property : constant Glib.Properties.Property_Object;
   Vscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;

private
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Hscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy :=
     Gtk.Enums.Build ("hscroll-policy");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Vscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy :=
     Gtk.Enums.Build ("vscroll-policy");
end Gtk.Scrollable;
