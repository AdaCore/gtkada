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

--  Implements scrollability for widgets that don't support scrolling on their
--  own.
--
--  Use `GtkViewport` to scroll child widgets such as `GtkGrid`, `GtkBox`, and
--  so on.
--
--  The `GtkViewport` will start scrolling content only if allocated less than
--  the child widget's minimum size in a given orientation.
--
--  # CSS nodes
--
--  `GtkViewport` has a single CSS node with name `viewport`.
--
--  # Accessibility
--
--  Until GTK 4.10, `GtkViewport` used the [enumGtk.AccessibleRole.group]
--  role.
--
--  Starting from GTK 4.12, `GtkViewport` uses the
--  [enumGtk.AccessibleRole.generic] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Scroll_Info;       use Gtk.Scroll_Info;
with Gtk.Scrollable;        use Gtk.Scrollable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Viewport is

   type Gtk_Viewport_Record is new Gtk_Widget_Record with null record;
   type Gtk_Viewport is access all Gtk_Viewport_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Viewport;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize
      (Self        : not null access Gtk_Viewport_Record'Class;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new `GtkViewport`.
   --  The new viewport uses the given adjustments, or default adjustments if
   --  none are given.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Hadjustment horizontal adjustment
   --  @param Vadjustment vertical adjustment

   function Gtk_Viewport_New
      (Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Viewport;
   --  Creates a new `GtkViewport`.
   --  The new viewport uses the given adjustments, or default adjustments if
   --  none are given.
   --  @param Hadjustment horizontal adjustment
   --  @param Vadjustment vertical adjustment

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_viewport_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Viewport.
   --  @return the child widget of Viewport. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Viewport_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Viewport.
   --  @param Child the child widget

   function Get_Scroll_To_Focus
      (Self : not null access Gtk_Viewport_Record) return Boolean;
   --  Gets whether the viewport is scrolling to keep the focused child in
   --  view.
   --  @return True if the viewport keeps the focus child scrolled to view

   procedure Set_Scroll_To_Focus
      (Self            : not null access Gtk_Viewport_Record;
       Scroll_To_Focus : Boolean);
   --  Sets whether the viewport should automatically scroll to keep the
   --  focused child in view.
   --  @param Scroll_To_Focus whether to keep the focus widget scrolled to
   --  view

   procedure Scroll_To
      (Self       : not null access Gtk_Viewport_Record;
       Descendant : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Scroll     : Gtk.Scroll_Info.Gtk_Scroll_Info);
   --  Scrolls a descendant of the viewport into view.
   --  The viewport and the descendant must be visible and mapped for this
   --  function to work, otherwise no scrolling will be performed.
   --  Since: gtk+ 4.12
   --  @param Descendant a descendant widget of the viewport
   --  @param Scroll details of how to perform the scroll operation or NULL to
   --  scroll into view. Has transfer-ownership='full'.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Viewport_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Viewport_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Viewport_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Viewport_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Viewport_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Viewport_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Viewport_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Viewport_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Viewport_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Viewport_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Border
      (Self   : not null access Gtk_Viewport_Record;
       Border : out Gtk.Scrollable.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Viewport_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Viewport_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Viewport_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Viewport_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Viewport_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Scroll_To_Focus_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to scroll when the focus changes.
   --
   --  Before 4.6.2, this property was mistakenly defaulting to FALSE, so if
   --  your code needs to work with older versions, consider setting it
   --  explicitly to TRUE.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Scrollable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Viewport_Record, Gtk_Viewport);
   function "+"
     (Widget : access Gtk_Viewport_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Viewport
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Viewport_Record, Gtk_Viewport);
   function "+"
     (Widget : access Gtk_Viewport_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Viewport
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Viewport_Record, Gtk_Viewport);
   function "+"
     (Widget : access Gtk_Viewport_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Viewport
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Viewport_Record, Gtk_Viewport);
   function "+"
     (Widget : access Gtk_Viewport_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Viewport
   renames Implements_Gtk_Scrollable.To_Object;

private
   Scroll_To_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scroll-to-focus");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Viewport;
