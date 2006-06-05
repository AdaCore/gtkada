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
--  The meaning of the most important fields can be explained on the
--  following figure (imagine this is a scrollbar):
--
--  <example>
--     [-------|=================|-------------------]
--    lower    value        value + page_size       upper
--
--  </example>
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Scrolling</group>

with Glib.Properties;
with Gtk.Object;

package Gtk.Adjustment is

   type Gtk_Adjustment_Record is new Object.Gtk_Object_Record with private;
   type Gtk_Adjustment is access all Gtk_Adjustment_Record'Class;

   procedure Gtk_New
     (Adjustment     : out Gtk_Adjustment;
      Value          : Gdouble;
      Lower          : Gdouble;
      Upper          : Gdouble;
      Step_Increment : Gdouble;
      Page_Increment : Gdouble;
      Page_Size      : Gdouble);
   --  Create a new adjustment.
   --  Value is the initial value of the adjustment. It must be in the
   --  range (Lower .. Upper) and the adjustment's value will never be
   --  outside this range.
   --  Step_Increment is the value used to make minor adjustments, such
   --  as when the user clicks on the arrows of a scrollbar.
   --  Page_Increment is used to make major adjustments, such as when
   --  the user clicks in the through on a scrollbar.
   --  Page_Size is the size of the area that is currently visible
   --  (for instance in a Gtk_Scrolled_Window).

   procedure Initialize
     (Adjustment     : access Gtk_Adjustment_Record'Class;
      Value          : Gdouble;
      Lower          : Gdouble;
      Upper          : Gdouble;
      Step_Increment : Gdouble;
      Page_Increment : Gdouble;
      Page_Size      : Gdouble);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Adjustment.

   procedure Set_Value
     (Adjustment : access Gtk_Adjustment_Record; Value : Gdouble);
   function Get_Value
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the current value of the adjustment.
   --  You do not need to call Value_Changed after modifying this value,
   --  this is done automatically.

   procedure Set_Lower
     (Adjustment : access Gtk_Adjustment_Record;
      Lower      : Gdouble);
   function Get_Lower
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the lower bound of the adjustment.
   --  You should call Changed after modifying this value.

   procedure Set_Upper
     (Adjustment : access Gtk_Adjustment_Record;
      Upper      : Gdouble);
   function Get_Upper
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the upper bound of the adjustment.
   --  You should call Changed after modifying this value.

   procedure Set_Step_Increment
     (Adjustment     : access Gtk_Adjustment_Record;
      Step_Increment : Gdouble);
   function Get_Step_Increment
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the step increment of the adjustment.
   --  You should call Changed after modifying this value.

   procedure Set_Page_Increment
     (Adjustment     : access Gtk_Adjustment_Record;
      Page_Increment : Gdouble);
   function Get_Page_Increment
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the page increment of the adjustment.
   --  You should call Changed after modifying this value.

   procedure Set_Page_Size
     (Adjustment : access Gtk_Adjustment_Record;
      Page_Size  : Gdouble);
   function Get_Page_Size
     (Adjustment : access Gtk_Adjustment_Record) return Gdouble;
   --  Modify the page size of the adjustment.
   --  You should call Changed after modifying this value.

   --------------------
   -- Misc functions --
   --------------------

   procedure Clamp_Page
     (Adjustment : access Gtk_Adjustment_Record;
      Lower      : Gdouble;
      Upper      : Gdouble);
   --  Update the Adjustment value to ensure that the range between Lower and
   --  Upper is in the current page (i.e. between value and value +
   --  page_size). If the range is larger than the page size, then only the
   --  start of it will be in the current page.
   --  A "value_changed" signal will be emitted if the value is changed.

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Changed (Adjustment : access Gtk_Adjustment_Record);
   --  Emit the "changed" signal on Adjustment.
   --  This warns any listener that some field other than the value has been
   --  changed.

   procedure Value_Changed (Adjustment : access Gtk_Adjustment_Record);
   --  Emit the "value_changed" signal on Adjustment.
   --  This warns any listener that the value has been changed.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Lower_Property
   --  Type:  Double
   --  Descr: The minimum value of the adjustment
   --
   --  Name:  Page_Increment_Property
   --  Type:  Double
   --  Descr: The page increment of the adjustment
   --
   --  Name:  Page_Size_Property
   --  Type:  Double
   --  Descr: The page size of the adjustment
   --
   --  Name:  Step_Increment_Property
   --  Type:  Double
   --  Descr: The step increment of the adjustment
   --
   --  Name:  Upper_Property
   --  Type:  Double
   --  Descr: The maximum value of the adjustment
   --
   --  Name:  Value_Property
   --  Type:  Double
   --  Descr: The value of the adjustment
   --
   --  </properties>

   Lower_Property          : constant Glib.Properties.Property_Double;
   Page_Increment_Property : constant Glib.Properties.Property_Double;
   Page_Size_Property      : constant Glib.Properties.Property_Double;
   Step_Increment_Property : constant Glib.Properties.Property_Double;
   Upper_Property          : constant Glib.Properties.Property_Double;
   Value_Property          : constant Glib.Properties.Property_Double;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Adjustment : access Gtk_Adjustment_Record'Class);
   --
   --    This signal is emitted every time one of the parameters is modified,
   --    except the value.
   --
   --  - "value_changed"
   --    procedure Handler (Adjustment : access Gtk_Adjustment_Record'Class);
   --
   --    This signal is emitted every time the value of the adjustment is
   --    modified
   --  </signals>

   Signal_Changed       : constant String := "changed";
   Signal_Value_Changed : constant String := "value_changed";

private
   type Gtk_Adjustment_Record is new Object.Gtk_Object_Record with null record;

   Lower_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("lower");
   Page_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-increment");
   Page_Size_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("page-size");
   Step_Increment_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("step-increment");
   Upper_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("upper");
   Value_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("value");

   pragma Import (C, Get_Type, "gtk_adjustment_get_type");
end Gtk.Adjustment;
