-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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
--  <c_version>1.3.11</c_version>

with Gdk.Color;
with Gtk.Enums;
with Gtk.Box;

package Gtk.Color_Selection is

   type Gtk_Color_Selection_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Color_Selection is access all Gtk_Color_Selection_Record'Class;

   type Color_Index is (Red, Green, Blue, Opacity);
   --  Used as an index to the table used to set and get the currently
   --  selected color.

   type Color_Array is array (Color_Index) of Gdouble;
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

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Color_Selection.

   function Get_Has_Opacity_Control
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  Return True if the Colorsel has an opacity control.

   procedure Set_Has_Opacity_Control
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Opacity : Boolean);
   --  Set the Colorsel to use or not use opacity.

   function Get_Has_Palette
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  Return True if the Colorsel has a palette.

   procedure Set_Has_Palette
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Palette : Boolean);
   --  If Has_Palette is True, then set the Colorsel to show the palette.
   --  Hide the palette otherwise.

   procedure Set_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color);
   --  Set the current color of the Colorsel. When called for the first time,
   --  the original color will the set to Color as well.

   procedure Set_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16);
   --  Set the current opacity to be Alpha. When called for the first time,
   --  the original opacity will be set too.

   procedure Get_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color);
   --  Set Color to the value of the current color in the Colorsel.

   function Get_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16;
   --  Return the current opacity.

   procedure Set_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color);
   --  Set the previous color. This procedure should not be called without
   --  analysis, as it might seem confusing to see that color change.
   --  Calling Set_Current_Color for the first time will also set this
   --  color.

   procedure Set_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16);
   --  Set the previous opacity to Alpha. This procedure should not be called
   --  without analysis, as it might seem confusing to see that value change.

   procedure Get_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color);
   --  Set Color to the original color value.

   function Get_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16;
   --  Return the previous alpha value.

   function Is_Adjusting
      (Colorsel : access Gtk_Color_Selection_Record) return Boolean;
   --  Get the current state of the Colorsel.
   --  Return TRue if the user is currently dragging a color around, False if
   --  the selection has stopped.

   --  procedure Palette_From_String
   --  ??? Not implemented yet.

   --  function Palette_To_String
   --  ??? Not implemented yet.

   --  Set_Change_Palette_Hook
   --  ??? Not implemented yet.

   function To_Absolute (Color : Gdouble) return Gushort;
   --  Convert from a percentage value as returned by Get_Color to an
   --  absolute value as can be used with Gdk_Color.

   function To_Percent (Color : Gushort) return Gdouble;
   --  Convert from an absolute value as used in Gdk_Color to a percentage
   --  value as used in Set_Color.

   --  <doc_ignore>
   procedure Set_Update_Policy
     (Colorsel : access Gtk_Color_Selection_Record;
      Policy   : Enums.Gtk_Update_Type);
   --  Set the behavior of the scales used to select a value (red, green,...)
   --  Set Policy to Update_Continuous if you want to update the color
   --  continuously as the slider is mode, Update_Discontinuous to update the
   --  color only when the mouse is released and Update_Delayed to update when
   --  the mouse is released or has been motionless for a while.
   --  This procedure is deprecated.

   procedure Set_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Color_Array);
   --  Modify the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.
   --  This procedure is deprecated.

   procedure Get_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Color_Array);
   --  Get the current color.
   --  Note that Color is an array of percentages, between 0.0 and 1.0, not
   --  absolute values.
   --  This procedure is deprecated.
   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

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
