------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

--  <description>
--  A special kind of child that can be put in a Gtk_Plot_Canvas.
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gdk.Color;
with Gtk.Enums;
with Gtk.Extra.Plot_Data;

package Gtk.Extra.Plot_Canvas.Text is

   type Gtk_Plot_Canvas_Text_Record is new Gtk_Plot_Canvas_Child_Record
     with private;
   type Gtk_Plot_Canvas_Text is access all Gtk_Plot_Canvas_Text_Record'Class;

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Text;
      Text     : String;
      Font     : String := "";
      Height   : Gint := 0;
      Angle    : Gtk.Extra.Plot_Data.Plot_Angle := Gtk.Extra.Plot_Data.Angle_0;
      Fg       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean := True;
      Justification : Gtk.Enums.Gtk_Justification := Gtk.Enums.Justify_Center);
   --  Create a new text child.
   --  If default values are used, the corresponding attribute isn't set

   procedure Set_Attributes
     (Child         : access Gtk_Plot_Canvas_Text_Record;
      Font          : String := "";
      Height        : Gint;
      Angle         : Gtk.Extra.Plot_Data.Plot_Angle;
      Fg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Bg            : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Transparent   : Boolean;
      Justification : Gtk.Enums.Gtk_Justification;
      Text          : String);
   --  Change the attribute of Text. The attributes with their default
   --  values are not changed.

private
   type Gtk_Plot_Canvas_Text_Record is new Gtk_Plot_Canvas_Child_Record
     with null record;
end Gtk.Extra.Plot_Canvas.Text;
