-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  This object represents an adjustable bounded value.
--  It is used in many other widgets that have such internal values,
--  like Gtk_Scrollbar, Gtk_Spin_Button, Gtk_Range, ...
--  Modifying the value of these widgets is done through their
--  associated adjustments.
--
--  The modification of the value is left to the user, who should
--  call Value_Changed or Changed to emit the relevant signals.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Data;

package Gtk.Adjustment is

   type Gtk_Adjustment_Record is new Data.Gtk_Data_Record with private;
   type Gtk_Adjustment is access all Gtk_Adjustment_Record'Class;

   Null_Adjustment : constant Gtk_Adjustment;

   procedure Gtk_New (Adjustment     : in out Gtk_Adjustment;
                      Value          : in     Gfloat;
                      Lower          : in     Gfloat;
                      Upper          : in     Gfloat;
                      Step_Increment : in     Gfloat;
                      Page_Increment : in     Gfloat;
                      Page_Size      : in     Gfloat);
   --  Creates a new adjustment.
   --  VALUE is the initial value of the adjustment. It must be in the
   --  range (LOWER .. UPPER) and the adjustment's value will never be
   --  outside this range.
   --  STEP_INCREMENT is the value used to make minor adjustments, such
   --  as when the user clicks on the arrows of a scrollbar.
   --  PAGE_INCREMENT is used to make major adjustments, such as when
   --  the user clicks in the through on a scrollbar.
   --  PAGE_SIZE is the size of the area that is currently visible
   --  (for instance in a Gtk_Scrolled_Window).

   procedure Initialize (Adjustment     : access Gtk_Adjustment_Record'Class;
                         Value          : in     Gfloat;
                         Lower          : in     Gfloat;
                         Upper          : in     Gfloat;
                         Step_Increment : in     Gfloat;
                         Page_Increment : in     Gfloat;
                         Page_Size      : in     Gfloat);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   --------------------
   -- Read functions --
   --------------------

   function Get_Value (Adjustment : access Gtk_Adjustment_Record)
                      return Gfloat;
   --  Returns the current value of the adjustment.

   function Get_Lower (Adjustment : access Gtk_Adjustment_Record)
                      return Gfloat;
   --  Returns the lower bound of the adjustment.

   function Get_Upper (Adjustment : access Gtk_Adjustment_Record)
                      return Gfloat;
   --  Returns the upper bound of the adjustment.

   function Get_Step_Increment (Adjustment : access Gtk_Adjustment_Record)
                               return Gfloat;
   --  Returns the step increment of the adjustment.

   function Get_Page_Increment (Adjustment : access Gtk_Adjustment_Record)
                               return Gfloat;
   --  Returns the page increment of the adjustment.

   function Get_Page_Size (Adjustment : access Gtk_Adjustment_Record)
                          return Gfloat;
   --  Returns the page size of the adjustment.

   ---------------------
   -- Write functions --
   ---------------------

   procedure Set_Upper (Adjustment : access Gtk_Adjustment_Record;
                        Upper      : in Gfloat);
   --  Modifies the upper bound of the adjustment.
   --  You should call Changed() after modifying this value.

   procedure Set_Lower (Adjustment : access Gtk_Adjustment_Record;
                        Lower      : in Gfloat);
   --  Modifies the lower bound of the adjustment.
   --  You should call Changed() after modifying this value.

   procedure Set_Value (Adjustment : access Gtk_Adjustment_Record;
                        Value      : in Gfloat);
   --  Modifies the current value of the adjustment.
   --  You do not need to call Value_Changed after modifying this value,
   --  this is done automatically.

   procedure Set_Page_Size (Adjustment : access Gtk_Adjustment_Record;
                            Page_Size  : in Gfloat);
   --  Modifies the page size of the adjustment.
   --  You should call Changed() after modifying this value.

   procedure Set_Page_Increment (Adjustment     : access Gtk_Adjustment_Record;
                                 Page_Increment : in Gfloat);
   --  Modifies the page increment of the adjustment.
   --  You should call Changed() after modifying this value.

   procedure Set_Step_Increment (Adjustment : access Gtk_Adjustment_Record;
                                 Step_Increment : in Gfloat);
   --  Modifies the step increment of the adjustment.
   --  You should call Changed() after modifying this value.

   --------------------
   -- Misc functions --
   --------------------

   procedure Clamp_Page (Adjustment : access Gtk_Adjustment_Record;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat);
   --  Updates the Adjustment value to ensure that the range between LOWER and
   --  UPPER is in the current page (i.e. between value and value +
   --  page_size). If the range is larger than the page size, then only the
   --  start of it will be in the current page.
   --  A "value_changed" signal will be emitted if the value is changed.

   procedure Changed (Adjustment : access Gtk_Adjustment_Record);
   --  Emits the "changed" signal on ADJUSTMENT.
   --  This warns any listener that some field other than the value has been
   --  changed.

   procedure Value_Changed (Adjustment : access Gtk_Adjustment_Record);
   --  Emits the "value_changed" signal on ADJUSTMENT.
   --  This warns any listener that the value has been changed.

private

   type Gtk_Adjustment_Record is new Data.Gtk_Data_Record with null record;

   Null_Adjustment_Record : aliased Gtk_Adjustment_Record;
   Null_Adjustment : constant Gtk_Adjustment := Null_Adjustment_Record'Access;

end Gtk.Adjustment;
