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
--  This widget is generally put on the sides of a drawing area to help the
--  user measure distances. It indicates the current position of the mouse
--  cursor within the drawing area, and can be graduated in multiple units.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Drawing</group>

with Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;

package Gtk.Ruler is

   type Gtk_Ruler_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   subtype Gtk_Hruler_Record is Gtk_Ruler_Record;
   subtype Gtk_Vruler_Record is Gtk_Ruler_Record;

   type Gtk_Ruler is access all Gtk_Ruler_Record'Class;
   subtype Gtk_Hruler is Gtk_Ruler;
   subtype Gtk_Vruler is Gtk_Ruler;

   procedure Gtk_New_Hruler (Ruler : out Gtk_Ruler);
   procedure Initialize_Hruler (Ruler : access Gtk_Ruler_Record'Class);
   --  Creates or initializes a new horizontal ruler

   procedure Gtk_New_Vruler (Ruler : out Gtk_Ruler);
   procedure Initialize_Vruler (Ruler : access Gtk_Ruler_Record'Class);
   --  Creates or initializes a new vertical ruler

   function Get_Type        return Gtk.Gtk_Type;
   function Hruler_Get_Type return Gtk.Gtk_Type;
   function Vruler_Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Ruler.

   procedure Set_Metric
     (Ruler  : access Gtk_Ruler_Record;
      Metric : Gtk_Metric_Type);
   function Get_Metric
     (Ruler  : access Gtk_Ruler_Record) return Gtk_Metric_Type;
   --  Set or get the units used for a Gtk_Ruler. See Set_Metric.

   procedure Set_Range
     (Ruler    : access Gtk_Ruler_Record;
      Lower    : Gdouble;
      Upper    : Gdouble;
      Position : Gdouble;
      Max_Size : Gdouble);
   procedure Get_Range
     (Ruler    : access Gtk_Ruler_Record;
      Lower    : out Gdouble;
      Upper    : out Gdouble;
      Position : out Gdouble;
      Max_Size : out Gdouble);
   --  Retrieve values indicating the range and current position of a Ruler.
   --  See Set_Range.
   --  Lower: Lower limit of the ruler.
   --  Upper: Upper limit of the ruler.
   --  Position: Current position of the mark on the ruler.
   --  Max_Size: Maximum size of the ruler used when calculating the space to
   --            leave for the text.

   procedure Draw_Ticks (Ruler : access Gtk_Ruler_Record);
   procedure Draw_Pos (Ruler : access Gtk_Ruler_Record);
   --  ???

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Lower_Property
   --  Type:  Double
   --  Descr: Lower limit of ruler
   --
   --  Name:  Max_Size_Property
   --  Type:  Double
   --  Descr: Maximum size of the ruler
   --
   --  Name:  Metric_Property
   --  Type:  Enum
   --  Descr: The metric used for the ruler
   --
   --  Name:  Position_Property
   --  Type:  Double
   --  Descr: Position of mark on the ruler
   --
   --  Name:  Upper_Property
   --  Type:  Double
   --  Descr: Upper limit of ruler
   --
   --  </properties>

   Lower_Property    : constant Glib.Properties.Property_Double;
   Max_Size_Property : constant Glib.Properties.Property_Double;
   Metric_Property   : constant Gtk.Enums.Property_Metric_Type;
   Position_Property : constant Glib.Properties.Property_Double;
   Upper_Property    : constant Glib.Properties.Property_Double;

private
   type Gtk_Ruler_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

   Lower_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("lower");
   Max_Size_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("max-size");
   Metric_Property   : constant Gtk.Enums.Property_Metric_Type :=
     Gtk.Enums.Build ("metric");
   Position_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("position");
   Upper_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("upper");

   pragma Import (C, Get_Type, "gtk_ruler_get_type");
   pragma Import (C, Vruler_Get_Type, "gtk_vruler_get_type");
   pragma Import (C, Hruler_Get_Type, "gtk_hruler_get_type");
end Gtk.Ruler;
