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
--  Gtk.Scrollable.Gtk_Scrollable is an interface that is implemented by
--  widgets with native scrolling ability.
--
--  To implement this interface you should override the
--  Gtk.Scrollable.Gtk_Scrollable:hadjustment and
--  Gtk.Scrollable.Gtk_Scrollable:vadjustment properties.
--
--  ## Creating a scrollable widget
--
--  All scrollable widgets should do the following.
--
--  - When a parent widget sets the scrollable child widget's adjustments, the
--  widget should populate the adjustments'
--  Gtk.Adjustment.Gtk_Adjustment:lower, Gtk.Adjustment.Gtk_Adjustment:upper,
--  Gtk.Adjustment.Gtk_Adjustment:step-increment,
--  Gtk.Adjustment.Gtk_Adjustment:page-increment and
--  Gtk.Adjustment.Gtk_Adjustment:page-size properties and connect to the
--  Gtk.Adjustment.Gtk_Adjustment::value-changed signal.
--
--  - Because its preferred size is the size for a fully expanded widget, the
--  scrollable widget must be able to cope with underallocations. This means
--  that it must accept any value passed to its
--  Gtk.Widget.GObject_Class.size_allocate function.
--
--  - When the parent allocates space to the scrollable child widget, the
--  widget should update the adjustments' properties with new values.
--
--  - When any of the adjustments emits the
--  Gtk.Adjustment.Gtk_Adjustment::value-changed signal, the scrollable widget
--  should scroll its contents.
--
--  </description>
--  <group>Interfaces</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Style;       use Gtk.Style;

package Gtk.Scrollable is

   type Gtk_Scrollable is new Glib.Types.GType_Interface;
   Null_Gtk_Scrollable : constant Gtk_Scrollable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrollable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Border
      (Self   : Gtk_Scrollable;
       Border : access Gtk.Style.Gtk_Border) return Boolean;
   --  Returns the size of a non-scrolling border around the outside of the
   --  scrollable. An example for this would be treeview headers. GTK+ can use
   --  this information to display overlayed graphics, like the overshoot
   --  indication, at the right position.
   --  Since: gtk+ 3.16
   --  "border": return location for the results

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

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Horizontal Gtk.Adjustment.Gtk_Adjustment of the scrollable widget. This
   --  adjustment is shared between the scrollable widget and its parent.

   Hscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;
   --  Determines whether horizontal scrolling should start once the
   --  scrollable widget is allocated less than its minimum width or less than
   --  its natural width.

   Vadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Verical Gtk.Adjustment.Gtk_Adjustment of the scrollable widget. This
   --  adjustment is shared between the scrollable widget and its parent.

   Vscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;
   --  Determines whether vertical scrolling should start once the scrollable
   --  widget is allocated less than its minimum height or less than its
   --  natural height.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Scrollable"

   function "+" (W : Gtk_Scrollable) return Gtk_Scrollable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Border is access function
     (Self   : Gtk_Scrollable;
      Border : access Gtk.Style.Gtk_Border) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Border);
   --  Returns the size of a non-scrolling border around the outside of the
   --  scrollable. An example for this would be treeview headers. GTK+ can use
   --  this information to display overlayed graphics, like the overshoot
   --  indication, at the right position.
   --  Since: gtk+ 3.16
   --  "border": return location for the results

   subtype Scrollable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Border
     (Self    : Scrollable_Interface_Descr;
      Handler : Virtual_Get_Border);
   pragma Import (C, Set_Get_Border, "gtkada_Scrollable_set_get_border");
   --  See Glib.Object.Add_Interface

private
   Vscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy :=
     Gtk.Enums.Build ("vscroll-policy");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Hscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy :=
     Gtk.Enums.Build ("hscroll-policy");
   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");

Null_Gtk_Scrollable : constant Gtk_Scrollable :=
   Gtk_Scrollable (Glib.Types.Null_Interface);
end Gtk.Scrollable;
