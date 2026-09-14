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

--  An interface for widgets with native scrolling ability.
--
--  To implement this interface you should override the
--  [propertyGtk.Scrollable:hadjustment] and
--  [propertyGtk.Scrollable:vadjustment] properties.
--
--  ## Creating a scrollable widget
--
--  All scrollable widgets should do the following.
--
--  - When a parent widget sets the scrollable child widget's adjustments, the
--  widget should connect to the [signalGtk.Adjustment::value-changed] signal.
--  The child widget should then populate the adjustments' properties as soon
--  as possible, which usually means queueing an allocation right away and
--  populating the properties in the [vfuncGtk.Widget.size_allocate]
--  implementation.
--
--  - Because its preferred size is the size for a fully expanded widget, the
--  scrollable widget must be able to cope with underallocations. This means
--  that it must accept any value passed to its [vfuncGtk.Widget.size_allocate]
--  implementation.
--
--  - When the parent allocates space to the scrollable child widget, the
--  widget must ensure the adjustments' property values are correct and up to
--  date, for example using [methodGtk.Adjustment.configure].
--
--  - When any of the adjustments emits the
--  [signalGtk.Adjustment::value-changed] signal, the scrollable widget should
--  scroll its contents.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Enums;       use Gtk.Enums;

package Gtk.Scrollable is

   type Gtk_Scrollable is new Glib.Types.GType_Interface;
   Null_Gtk_Scrollable : constant Gtk_Scrollable;

   type Gtk_Border is record
      Left : Gint16;
      Right : Gint16;
      Top : Gint16;
      Bottom : Gint16;
   end record;
   pragma Convention (C, Gtk_Border);
   --  Specifies a border around a rectangular area.
   --
   --  Each side can have a different width.

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
       Border : out Gtk_Border) return Boolean;
   --  Returns the size of a non-scrolling border around the outside of the
   --  scrollable.
   --  An example for this would be treeview headers. GTK can use this
   --  information to display overlaid graphics, like the overshoot indication,
   --  at the right position.
   --  @param Border return location for the results
   --  @return True if Border has been set

   function Get_Hadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the `GtkAdjustment` used for horizontal scrolling.
   --  @return horizontal `GtkAdjustment`. Has transfer-ownership='none'.

   procedure Set_Hadjustment
      (Self        : Gtk_Scrollable;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the horizontal adjustment of the `GtkScrollable`.
   --  @param Hadjustment a `GtkAdjustment`

   function Get_Hscroll_Policy
      (Self : Gtk_Scrollable) return Gtk.Enums.Gtk_Scrollable_Policy;
   pragma Import (C, Get_Hscroll_Policy, "gtk_scrollable_get_hscroll_policy");
   --  Gets the horizontal `GtkScrollablePolicy`.
   --  @return The horizontal `GtkScrollablePolicy`.

   procedure Set_Hscroll_Policy
      (Self   : Gtk_Scrollable;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);
   pragma Import (C, Set_Hscroll_Policy, "gtk_scrollable_set_hscroll_policy");
   --  Sets the `GtkScrollablePolicy`.
   --  The policy determines whether horizontal scrolling should start below
   --  the minimum width or below the natural width.
   --  @param Policy the horizontal `GtkScrollablePolicy`

   function Get_Vadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the `GtkAdjustment` used for vertical scrolling.
   --  @return vertical `GtkAdjustment`. Has transfer-ownership='none'.

   procedure Set_Vadjustment
      (Self        : Gtk_Scrollable;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Sets the vertical adjustment of the `GtkScrollable`.
   --  @param Vadjustment a `GtkAdjustment`

   function Get_Vscroll_Policy
      (Self : Gtk_Scrollable) return Gtk.Enums.Gtk_Scrollable_Policy;
   pragma Import (C, Get_Vscroll_Policy, "gtk_scrollable_get_vscroll_policy");
   --  Gets the vertical `GtkScrollablePolicy`.
   --  @return The vertical `GtkScrollablePolicy`.

   procedure Set_Vscroll_Policy
      (Self   : Gtk_Scrollable;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);
   pragma Import (C, Set_Vscroll_Policy, "gtk_scrollable_set_vscroll_policy");
   --  Sets the `GtkScrollablePolicy`.
   --  The policy determines whether vertical scrolling should start below the
   --  minimum height or below the natural height.
   --  @param Policy the vertical `GtkScrollablePolicy`

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Border) return Gtk_Border;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Horizontal `GtkAdjustment` of the scrollable widget.
   --
   --  This adjustment is shared between the scrollable widget and its parent.

   Hscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;
   --  Determines when horizontal scrolling should start.

   Vadjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  Vertical `GtkAdjustment` of the scrollable widget.
   --
   --  This adjustment is shared between the scrollable widget and its parent.

   Vscroll_Policy_Property : constant Gtk.Enums.Property_Gtk_Scrollable_Policy;
   --  Determines when vertical scrolling should start.

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
      Border : out Gtk_Border) return Glib.Gboolean;
   pragma Convention (C, Virtual_Get_Border);
   --  Returns the size of a non-scrolling border around the outside of the
   --  scrollable.
   --  An example for this would be treeview headers. GTK can use this
   --  information to display overlaid graphics, like the overshoot indication,
   --  at the right position.
   --  @param Border return location for the results
   --  @return True if Border has been set

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
