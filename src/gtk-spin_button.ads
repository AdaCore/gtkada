-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--
--  A Gtk_Spin_Button is a single line text editing widget for text that
--  represents a number. At the right hand side of the text line there are
--  small up- and down arrow buttons for incrementing or decrementing
--  (spinning) the number within a given range.
--  It allows the value to have zero or a number of decimal places and
--  to be incremented/decremented in configurable steps.
--  The action of holding down one of the buttons optionally results in an
--  acceleration of change in the value according to how long it is
--  depressed.
--
--  @pxref{Package_Gtk.GEntry} for a text editing widget without spin buttons.
--
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Adjustment;
with Gtk.GEntry;

package Gtk.Spin_Button is

   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtk_Spin_Button is access all Gtk_Spin_Button_Record'Class;

   type Gtk_Spin_Button_Update_Policy is
     (Update_Always,
      --  Update always, errors are ignored while converting text into a
      --  numeric value.

      Update_If_Valid
      --  The spin button's value gets changed if the text input is a numeric
      --  value that is within the range specified by the adjustment.
     );
   --  Determine the update policy of the spin button which affects the
   --  behaviour when parsing inserted text and syncing its value with the
   --  values of the adjustment.
   for Gtk_Spin_Button_Update_Policy'Size use Gint'Size;

   type Gtk_Spin_Type is
     (Spin_Step_Forward,
      Spin_Step_Backward,
      Spin_Page_Forward,
      Spin_Page_Backward,
      Spin_Home,
      Spin_User_Defined);
   --  Determine how manual spinning should be done.
   --  See also the Spin procedure.
   for Gtk_Spin_Type'Size use Gint'Size;

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint);
   --  Create a spin button with the given parameters.
   --  Adjustment contains the range, current value, step value and
   --  "page" value. The step value is the increment/decrement when pressing
   --  mouse button 1 on a button; the page value when mouse button 2 is
   --  pressed. Additionally, mouse button 3 can be used to jump directly to
   --  the or lower values when used to select one of the buttons.
   --  Climb_Rate takes a value between 0.0 and 1.0 and indicates the
   --  amount of acceleration that the Spin Button has.
   --  The_Digits is the number of digits behind the decimal point to be
   --  displayed for the value.

   procedure Gtk_New
     (Spin_Button : out Gtk_Spin_Button;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble);
   --  Same as above, but with explicit range instead of an adjustment.
   --  The adjustment associated with Spin_Button is created internally.

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Climb_Rate  : Gdouble;
      The_Digits  : Gint);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize
     (Spin_Button : access Gtk_Spin_Button_Record'Class;
      Min         : Gdouble;
      Max         : Gdouble;
      Step        : Gdouble);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Spin_Button.

   procedure Set_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment);
   --  Set the adjustment settings of the spin button.

   function Get_Adjustment
     (Spin_Button : access Gtk_Spin_Button_Record)
      return Gtk.Adjustment.Gtk_Adjustment;
   --  Return the adjustment settings of the spin button.

   procedure Set_Digits
     (Spin_Button : access Gtk_Spin_Button_Record;
      The_Digits  : Guint);
   --  Set number of decimals of the spin button.

   procedure Set_Increments
     (Spin_Button : access Gtk_Spin_Button_Record;
      Step        : Gdouble;
      Page        : Gdouble);
   --  Set number of decimals of the spin button.

   procedure Set_Range
     (Spin_Button : access Gtk_Spin_Button_Record;
      Min         : Gdouble;
      Max         : Gdouble);
   --  Set number of decimals of the spin button.

   function Get_Value_As_Float
     (Spin_Button : access Gtk_Spin_Button_Record) return Gdouble;
   --  Return the current value of the spin button in a float.

   function Get_Value_As_Int
     (Spin_Button : access Gtk_Spin_Button_Record) return Gint;
   --  Return the current value of the spin button in an integer.

   procedure Set_Value
     (Spin_Button : access Gtk_Spin_Button_Record;
      Value       : Gdouble);
   --  Set the current value of the spin button.

   procedure Set_Update_Policy
     (Spin_Button : access Gtk_Spin_Button_Record;
      Policy      : Gtk_Spin_Button_Update_Policy);
   --  Set the update policy of the spin button.
   --  See Gtk_Pin_Button_Update_Policy for the meaning of Policy.

   procedure Set_Numeric
     (Spin_Button : access Gtk_Spin_Button_Record;
      Numeric     : Boolean);
   --  If Numeric is True, then only a numeric value can be typed in the
   --  text entry, otherwise also nonnumeric text.

   procedure Spin
     (Spin_Button : access Gtk_Spin_Button_Record;
      Direction   : Gtk_Spin_Type;
      Step        : Gdouble);
   --  Set the value of the spin button relative to its current value.
   --  Depending on Direction, it will be incremented or decremented with
   --  the step value.

   procedure Set_Wrap
     (Spin_Button : access Gtk_Spin_Button_Record; Wrap : Boolean);
   --  Set whether the spin button should "wrap around" when exceeding the
   --  upper and lower limits.

   procedure Set_Snap_To_Ticks
    (Spin_Button   : access Gtk_Spin_Button_Record;
     Snap_To_Ticks : Boolean);
   --  Set the spin button to round the value to the nearest step value
   --  which is set within its adjustment settings.

   procedure Update (Spin_Button : access Gtk_Spin_Button_Record);
   --  Manually force an update of the spin button.

private
   type Gtk_Spin_Button_Record is new Gtk.GEntry.Gtk_Entry_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_spin_button_get_type");
end Gtk.Spin_Button;
