-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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
--  A Gtk_Color_Combo is a widget that ease the selection of colors
--  by the user. It is a special form of a Gtk_Combo_Box, that displays
--  a special popup window, with a list of colors.
--
--  Note that nothing appears in the button, this your responsibility to
--  update it when the user selects a new color (see the "changed" signal).
--  </description>
--  <c_version>gtk+extra 0.99.1</c_version>

with Gdk.Color;
with Gtk.Extra.Combo_Box;
with Gtkada.Types;

package Gtk.Extra.Color_Combo is

   type Gtk_Color_Combo_Record is new Gtk.Extra.Combo_Box.Gtk_Combo_Box_Record
     with private;
   type Gtk_Color_Combo is access all Gtk_Color_Combo_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Color_Combo);
   --  Create a new default combo box.
   --  It shows a list of 40 default colors.

   procedure Initialize (Widget : access Gtk_Color_Combo_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Gtk_New (Widget      : out Gtk_Color_Combo;
                      Nrows       : in Gint;
                      Ncols       : in Gint;
                      Color_Names : in Gtkada.Types.Chars_Ptr_Array);
   --  Create a new combo box with a specific list of colors.
   --  Note that Color_Names must contain at least Nrows * Ncols elements.

   procedure Initialize (Widget      : access Gtk_Color_Combo_Record;
                         Nrows       : in Gint;
                         Ncols       : in Gint;
                         Color_Names : in Gtkada.Types.Chars_Ptr_Array);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Color_Combo.

   function Get_Color_At (Widget : access Gtk_Color_Combo_Record;
                          Row    : Gint;
                          Col    : Gint)
                         return String;
   --  Return the name of the color at specific coordinates.

   procedure Find_Color (Color_Combo : access Gtk_Color_Combo_Record;
                         Color       : in Gdk.Color.Gdk_Color;
                         Row         : out Gint;
                         Col         : out Gint);
   --  Return the coordinates in which a color appear in the popup window.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --  procedure Handler (Color_Combo : access Gtk_Color_Combo_Record'Class;
   --                     Selection   : Gint;
   --                     Color_Name  : String);
   --
   --  Emitted when the color has selected a new color.
   --  Selection is the number of the selection (this is the total
   --  row * Ncols + col). Color_Name is the name of the selected color.
   --  </signals>

private
   type Gtk_Color_Combo_Record is new Gtk.Extra.Combo_Box.Gtk_Combo_Box_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_color_combo_get_type");
end Gtk.Extra.Color_Combo;
