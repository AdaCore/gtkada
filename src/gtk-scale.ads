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
--  A scale is a horizontal or vertical widget that a user can slide to choose
--  a value in a given range. This is a kind of cursor, similar to what one
--  finds on audio systems to select the volume for instance.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Numeric/Text Data Entry</group>
--  <screenshot>gtk-scale.png</screenshot>

with Glib.Properties;
with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GRange;
with Pango.Layout;

package Gtk.Scale is

   type Gtk_Scale_Record is new Gtk.GRange.Gtk_Range_Record with private;
   subtype Gtk_Hscale_Record is Gtk_Scale_Record;
   subtype Gtk_Vscale_Record is Gtk_Scale_Record;

   type Gtk_Scale is access all Gtk_Scale_Record'Class;
   subtype Gtk_Hscale is Gtk_Scale;
   subtype Gtk_Vscale is Gtk_Scale;

   procedure Gtk_New_Hscale
     (Scale      : out Gtk_Scale;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Gtk_New_Hscale
     (Scale : out Gtk_Scale;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble);
   --  Create a new horizontal scale widget that lets the user input a number
   --  between Min and Max with an increment of Step. Step must be non-zero; it
   --  is the distance the slider moves when using the arrow keys to adjust the
   --  scale value. An adjustment can be used to specify the range instead.

   procedure Gtk_New_Vscale
     (Scale      : out Gtk_Scale;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Gtk_New_Vscale
     (Scale : out Gtk_Scale;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble);
   --  Create a new vertical scale widget that lets the user input a number
   --  between Min and Max with an increment of Step. Step must be non-zero; it
   --  is the distance the slider moves when using the arrow keys to adjust the
   --  scale value. An adjustment can be used to specify the range instead.

   function Get_Type        return Gtk.Gtk_Type;
   function Hscale_Get_Type return GType;
   function Vscale_Get_Type return GType;
   --  Return the internal value associated with a Gtk_Scale, a
   --  Gtk_Hscale or a Gtk_Vscale.

   procedure Initialize_Hscale
     (Scale      : access Gtk_Scale_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Hscale
     (Scale : access Gtk_Scale_Record'Class;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble);
   --  Internal initialization procedure.

   procedure Initialize_Vscale
     (Scale      : access Gtk_Scale_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Initialize_Vscale
     (Scale : access Gtk_Scale_Record'Class;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble);
   --  Internal initialization procedure.

   procedure Set_Digits
     (Scale      : access Gtk_Scale_Record;
      The_Digits : Gint);
   function Get_Digits (Scale : access Gtk_Scale_Record) return Gint;
   --  Sets the number of decimal places that are displayed in the value. Also
   --  causes the value of the adjustment to be rounded off to this number of
   --  digits, so the retrieved value matches the value the user saw.

   procedure Set_Draw_Value
     (Scale      : access Gtk_Scale_Record;
      Draw_Value : Boolean);
   function Get_Draw_Value (Scale : access Gtk_Scale_Record) return Boolean;
   --  Specifies whether the current value is displayed as a string next to the
   --  slider.

   procedure Set_Value_Pos
     (Scale : access Gtk_Scale_Record;
      Pos   : Gtk_Position_Type);
   function Get_Value_Pos
     (Scale : access Gtk_Scale_Record) return Gtk_Position_Type;
   --  Sets the position in which the current value is displayed.

   function Get_Layout
     (Scale : access Gtk_Scale_Record) return Pango.Layout.Pango_Layout;
   --  Gets the Pango_Layout used to display the scale. The returned object
   --  is owned by the scale so does not need to be freed by the caller.

   procedure Get_Layout_Offsets
     (Scale : access Gtk_Scale_Record;
      X, Y  : out Gint);
   --  Obtains the coordinates where the scale will draw the Pango_Layout
   --  representing the text in the scale. Remember
   --  when using the Pango_Layout functions you need to convert to
   --  and from pixels using Pango.Enums.To_Pixels
   --  If the draw_value property is False, the return values are undefined.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Digits_Property
   --  Type:  Int
   --  Descr: The number of decimal places that are displayed in the value
   --
   --  Name:  Draw_Value_Property
   --  Type:  Boolean
   --  Descr: Whether the current value is displayed as a string next to the
   --         slider
   --
   --  Name:  Value_Pos_Property
   --  Type:  Gtk_Position_Type
   --  Descr: The position in which the current value is displayed
   --
   --  </properties>

   Digits_Property     : constant Glib.Properties.Property_Int;
   Draw_Value_Property : constant Glib.Properties.Property_Boolean;
   Value_Pos_Property  : constant Gtk.Enums.Property_Gtk_Position_Type;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  -  "format_value"
   --     function Handler
   --        (Scale : access Gtk_Scale_Record'Class;
   --         Value : Gdouble) return Interfaces.C.Strings.chars_ptr;
   --     Emitted by the scale to request a formating of its value. The handler
   --     should return the string representing the value. The returned value
   --     will be freed by gtk+
   --
   --  </signals>

   Signal_Format_Value : constant String := "format_value";

private
   type Gtk_Scale_Record is new Gtk.GRange.Gtk_Range_Record with null record;

   Digits_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("digits");
   Draw_Value_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("draw-value");
   Value_Pos_Property  : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("value-pos");

   pragma Import (C, Get_Type,        "gtk_scale_get_type");
   pragma Import (C, Hscale_Get_Type, "gtk_hscale_get_type");
   pragma Import (C, Vscale_Get_Type, "gtk_vscale_get_type");
end Gtk.Scale;
