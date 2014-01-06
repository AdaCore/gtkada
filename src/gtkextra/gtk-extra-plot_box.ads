------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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
--  This special type of data set displays itself with boxes
--  </description>
--  <c_version>gtkextra 2.1.1</c_version>
--  <group>Plotting Data</group>

with Gtk.Extra.Plot_Data;
with Gtk.Enums;

package Gtk.Extra.Plot_Box is

   type Gtk_Plot_Box_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with private;
   type Gtk_Plot_Box is access all Gtk_Plot_Box_Record'Class;

   procedure Gtk_New
     (Box         : out Gtk_Plot_Box;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Create a new Plot bar.

   procedure Initialize
     (Box         : access Gtk_Plot_Box_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Box

private
   type Gtk_Plot_Box_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_box_get_type");
end Gtk.Extra.Plot_Box;
