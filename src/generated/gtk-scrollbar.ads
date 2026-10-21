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

--  Shows a horizontal or vertical scrollbar.
--
--  <picture> <source srcset="scrollbar-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkScrollbar"
--  src="scrollbar.png"> </picture>
--  Its position and movement are controlled by the adjustment that is passed
--  to or created by [ctorGtk.Scrollbar.new]. See [classGtk.Adjustment] for
--  more details. The [propertyGtk.Adjustment:value] field sets the position of
--  the thumb and must be between [propertyGtk.Adjustment:lower] and
--  [propertyGtk.Adjustment:upper] - [propertyGtk.Adjustment:page-size]. The
--  [propertyGtk.Adjustment:page-size] represents the size of the visible
--  scrollable area.
--
--  The fields [propertyGtk.Adjustment:step-increment] and
--  [propertyGtk.Adjustment:page-increment] fields are added to or subtracted
--  from the [propertyGtk.Adjustment:value] when the user asks to move by a
--  step (using e.g. the cursor arrow keys) or by a page (using e.g. the Page
--  Down/Up keys).
--
--  # CSS nodes
--
--  ``` scrollbar ╰── range[.fine-tune] ╰── trough ╰── slider ```
--
--  `GtkScrollbar` has a main CSS node with name scrollbar and a subnode for
--  its contents. The main node gets the .horizontal or .vertical style classes
--  applied, depending on the scrollbar's orientation.
--
--  The range node gets the style class .fine-tune added when the scrollbar is
--  in 'fine-tuning' mode.
--
--  Other style classes that may be added to scrollbars inside
--  [classGtk.ScrolledWindow] include the positional classes (.left, .right,
--  .top, .bottom) and style classes related to overlay scrolling
--  (.overlay-indicator, .dragging, .hovering).
--
--  # Accessibility
--
--  `GtkScrollbar` uses the [enumGtk.AccessibleRole.scrollbar] role.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Accessible_Range;  use Gtk.Accessible_Range;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Scrollbar is

   type Gtk_Scrollbar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Scrollbar is access all Gtk_Scrollbar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Scrollbar;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Initialize
      (Self        : not null access Gtk_Scrollbar_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Creates a new scrollbar with the given orientation.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Orientation the scrollbar's orientation.
   --  @param Adjustment the [classGtk.Adjustment] to use, or null to create a
   --  new adjustment.

   function Gtk_Scrollbar_New
      (Orientation : Gtk.Enums.Gtk_Orientation;
       Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
       return Gtk_Scrollbar;
   --  Creates a new scrollbar with the given orientation.
   --  @param Orientation the scrollbar's orientation.
   --  @param Adjustment the [classGtk.Adjustment] to use, or null to create a
   --  new adjustment.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_scrollbar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Adjustment
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Returns the scrollbar's adjustment.
   --  @return the scrollbar's adjustment. Has transfer-ownership='none'.

   procedure Set_Adjustment
      (Self       : not null access Gtk_Scrollbar_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Makes the scrollbar use the given adjustment.
   --  @param Adjustment the adjustment to set

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Scrollbar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Scrollbar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Scrollbar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Scrollbar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Scrollbar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Scrollbar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Scrollbar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Scrollbar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Scrollbar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Scrollbar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Scrollbar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Scrollbar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Adjustment_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Adjustment.Gtk_Adjustment
   --  The `GtkAdjustment` controlled by this scrollbar.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.AccessibleRange"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Scrollbar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Accessible_Range is new Glib.Types.Implements
     (Gtk.Accessible_Range.Gtk_Accessible_Range, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Accessible_Range.Gtk_Accessible_Range
   renames Implements_Gtk_Accessible_Range.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Range.Gtk_Accessible_Range)
   return Gtk_Scrollbar
   renames Implements_Gtk_Accessible_Range.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Scrollbar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Scrollbar
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Scrollbar_Record, Gtk_Scrollbar);
   function "+"
     (Widget : access Gtk_Scrollbar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Scrollbar
   renames Implements_Gtk_Orientable.To_Object;

private
   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
end Gtk.Scrollbar;
