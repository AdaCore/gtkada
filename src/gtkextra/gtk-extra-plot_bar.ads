------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2013, AdaCore                     --
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
--  This special type of data set displays itself with bar (also known
--  as histograms).
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gtk.Extra.Plot_Data;
with Gtk.Enums;

package Gtk.Extra.Plot_Bar is

   type Gtk_Plot_Bar_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with private;
   type Gtk_Plot_Bar is access all Gtk_Plot_Bar_Record'Class;

   procedure Gtk_New
     (Bar         : out Gtk_Plot_Bar;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Create a new Plot bar.

   procedure Initialize
     (Bar         : access Gtk_Plot_Bar_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Bar.

   procedure Set_Width
     (Bar   : access Gtk_Plot_Bar_Record'Class;
      Width : Gdouble);
   --  Set the width of the bars

   function Get_Width (Bar : access Gtk_Plot_Bar_Record'Class) return Gdouble;
   --  Return the width used to draw the bars

private
   type Gtk_Plot_Bar_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_bar_get_type");
end Gtk.Extra.Plot_Bar;
