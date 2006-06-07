-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This widget provides a low level graphical representation of a range of
--  values. It is used by other widgets such as Gtk_Scale and Gtk_Scrollbar.
--  </description>
--  <c_version>2.8.17</c_version>
--  <testgtk>create_range.adb</testgtk>
--  <screenshot>gtk-range</screenshot>

with Glib.Properties;
with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.GRange is

   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Range is access all Gtk_Range_Record'Class;
   subtype Gtk_GRange is Gtk_Range;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Range.

   procedure Set_Update_Policy
     (The_Range : access Gtk_Range_Record;
      Policy    : Gtk_Update_Type);
   --  Set the update policy for the range.
   --  Update_Continuous means that anytime the range slider is moved, the
   --  range value will change and the value_changed signal will be emitted.
   --  Update_Delayed means that the value will be updated after a brief
   --  timeout where no slider motion occurs, so updates are spaced by a short
   --  time rather than continuous.
   --  Update_Discontinuous means that the value will only be updated when the
   --  user releases the button and ends the slider drag operation.

   function Get_Update_Policy
     (The_Range : access Gtk_Range_Record) return Gtk_Update_Type;
   --  Return the current update policy.

   procedure Set_Adjustment
     (The_Range  : access Gtk_Range_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the adjustment to be used as the "model" object for this range
   --  widget. The adjustment indicates the current range value, the
   --  minimum and maximum range values, the step/page increments used
   --  for keybindings and scrolling, and the page size. The page size
   --  is normally 0 for Gtk_Scale and nonzero for Gtk_Scrollbar, and
   --  indicates the size of the visible area of the widget being scrolled.
   --  The page size affects the size of the scrollbar slider.

   function Get_Adjustment
     (The_Range : access Gtk_Range_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment associated with the range widget.

   procedure Set_Inverted
     (The_Range : access Gtk_Range_Record;
      Setting   : Boolean := True);
   --  Ranges normally move from lower to higher values as the slider moves
   --  from top to bottom or left to right. Inverted ranges have higher values
   --  at the top or on the right rather than on the bottom or left.

   function Get_Inverted (The_Range : access Gtk_Range_Record) return Boolean;
   --  Return whether the range is inverted.

   procedure Set_Increments
     (The_Range : access Gtk_Range_Record;
      Step      : Gdouble;
      Page      : Gdouble);
   --  Set the Step and the Page size for the range. The Step size is used when
   --  the user clicks on the Gtk_Scrollbar arrows or moves the Gtk_Scale via
   --  the arrow keys. The Page size is used when moving by pages via the
   --  Page-Up and Page-Down keys for instance.

   procedure Set_Range
     (The_Range : access Gtk_Range_Record;
      Min       : Gdouble;
      Max       : Gdouble);
   --  Set the allowable values in the Gtk_Range, and clamps the range value to
   --  the between Min and Max.

   procedure Set_Value
     (The_Range : access Gtk_Range_Record;
      Value     : Gdouble);
   --  Set the current value of the given Range. If the value is outside the
   --  minimum or the maximum value range, it will be clamped to fit inside
   --  the range.
   --  Cause the "value_changed" signal to be emitted if the value is
   --  different.

   function Get_Value (The_Range : access Gtk_Range_Record) return Gdouble;
   --  Return the current value of the range.

   ------------
   -- Signal --
   ------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "value_changed"
   --    procedure Handler (R :  access Gtk_Range_Record'Class);
   --    Emitted when the current value of the range has changed
   --
   --  - "adjust_bounds"
   --    procedure Handler
   --       (R     : access Gtk_Range_Record'Class;
   --        Value : Gdouble);
   --
   --  - "change_value"
   --    function Handler
   --       (R     : access Gtk_Range_Record'Class;
   --        Typ   : Gtk_Scroll_Type;
   --        Value : Gdouble) return Gboolean;
   --    Emitted when a scroll action is performed on the range. The type of
   --    event that occurred and the new value are returned. The application
   --    should return True if it has handled the event itself.
   --
   --  - "move_slider"
   --    procedure Handler (R : access Gtk_Range_Record'Class);
   --    Emitted when the slider has changed
   --
   --  </signals>

   Signal_Adjust_Bounds : constant String := "adjust_bounds";
   Signal_Change_Value  : constant String := "change_value";
   Signal_Move_Slider   : constant String := "move_slider";
   Signal_Value_Changed : constant String := "value_changed";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Update_Policy_Property
   --  Type:  Gtk_Update_Type
   --  Flags: read-write
   --  Descr: How the range should be updated on the screen
   --  See also: Set_Update_Policy
   --
   --  Name:  Adjustment_Property
   --  Type:  Object
   --  Descr: The GtkAdjustment that contains the current value of this range
   --         object
   --
   --  Name:  Inverted_Property
   --  Type:  Boolean
   --  Descr: Invert direction slider moves to increase range value
   --
   --  </properties>

   Update_Policy_Property : constant Gtk.Enums.Property_Gtk_Update_Type;
   Adjustment_Property    : constant Glib.Properties.Property_Object;
   Inverted_Property      : constant Glib.Properties.Property_Boolean;

private
   type Gtk_Range_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

   Update_Policy_Property : constant Gtk.Enums.Property_Gtk_Update_Type :=
     Gtk.Enums.Build ("update_policy");

   Adjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("adjustment");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");

   pragma Import (C, Get_Type, "gtk_range_get_type");
end Gtk.GRange;
