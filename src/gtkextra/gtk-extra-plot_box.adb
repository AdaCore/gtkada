------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
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

with Gtk.Extra.Plot_Data;  use Gtk.Extra.Plot_Data;
with Gtk.Enums;            use Gtk.Enums;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Plot_Box is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plot_Box_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Box         : out Gtk_Plot_Box;
      Orientation : Gtk.Enums.Gtk_Orientation) is
   begin
      Box := new Gtk_Plot_Box_Record;
      Initialize (Box, Orientation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Box         : access Gtk_Plot_Box_Record'Class;
      Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal (Orientation : Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_plot_box_new");
   begin
      Set_Object (Box, Internal (Orientation));
   end Initialize;
end Gtk.Extra.Plot_Box;
