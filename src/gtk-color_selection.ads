-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
--  A Gtk_Color_Selection widget is a complex dialog that allows the user
--  to select a color based either on its (Red, Green, Blue) or its
--  (Hue, Saturation, Value).
--  An additional field is provided to select the opacity of the color (this
--  is usually called the alpha channel).
--
--  @pxref{Package_Gtk.Color_Selection_Dialog} for a version of this widget
--  that comes with its own dialog.
--
--  @pxref{Package_Gtk.Extra.Color_Combo} for a different way to select colors.
--  </description>
--  <c_version>1.2.7</c_version>

with Gtk.Enums;
with Gtk.Box;

package Gtk.Color_Selection is

   type Gtk_Color_Selection_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Color_Selection is access all Gtk_Color_Selection_Record'Class;

   type Color_Index is (Red, Green, Blue, Opacity);
   --  Used as an index to the table used to set and get the currently
   --  selected color.

   type Color_Array is array (Color_Index'Range) of Gdouble;
   --  Array that indicates the currently selected color.
   --  All the values are between 0.0 and 1.0 (a percentage value).
   --  They should be converted to absolute values before using them to create
   --  a new color, with the following piece of code:
   --    Absolute := To_Absolute (Color_Array (Index))

   procedure Gtk_New (Widget : out Gtk_Color_Selection);
   --  Create a new color selection widget.

   procedure Initialize (Widget : access Gtk_Color_Selection_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Color_Selection.

   procedure Set_Update_Policy (Colorsel : access Gtk_Color_Selection_Record;
                                Policy   : in Enums.Gtk_Update_Type);
   --  Set the behavior of the scales used to select a value (red, green,...)
   --  Set Policy to Update_Continuous if you want to update the color
   --  continuously as the slider is mode, Update_Discontinuous to update the
   --  color only when the mouse is released and Update_Delayed to update when
   --  the mouse is released or has been motionless for a while.

   procedure Set_Opacity (Colorsel    : access Gtk_Color_Selection_Record;
                          Use_Opacity : in Boolean);
   --  Indicate whether the dialog should provide a way to change the
   --  opacity of the color.
   --  Since not every application can handle transparent colors, it is better
   --  to deactivate this feature if you don't intend to use it.
   --  Note also that if you deactivated that feature, Get_Color will not
   --  set a valid value for the Opacity index of its return type.

   procedure Set_Color (Colorsel : access Gtk_Color_Selection_Record;
                        Color    : in Color_Array);
   --  Modify the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.

   procedure Get_Color (Colorsel : access Gtk_Color_Selection_Record;
                        Color    : out Color_Array);
   --  Get the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.

   function To_Absolute (Color : Gdouble) return Gushort;
   --  Convert from a percentage value as returned by Get_Color to an
   --  absolute value as can be used with Gdk_Color.

   function To_Percent (Color : Gushort) return Gdouble;
   --  Convert from an absolute value as used in Gdk_Color to a percentage
   --  value as used in Set_Color.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "color_changed"
   --    procedure Handler
   --       (Selection : access Gtk_Color_Selection_Record'Class);
   --
   --    Called every time a new color is selected in the dialog
   --  </signals>

private
   type Gtk_Color_Selection_Record is new Gtk.Box.Gtk_Box_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_color_selection_get_type");
end Gtk.Color_Selection;
