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

with System;

package body Gtk.Extra.Combo_Box is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Combo_Box)
   is
   begin
      Widget := new Gtk_Combo_Box_Record;
      Gtk.Extra.Combo_Box.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Combo_Box_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combobox_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------------------
   -- Hide_Popdown_Window --
   -------------------------

   procedure Hide_Popdown_Window (Combobox : access Gtk_Combo_Box_Record)
   is
      procedure Internal (Combobox : in System.Address);
      pragma Import (C, Internal, "gtk_combobox_hide_popdown_window");
   begin
      Internal (Get_Object (Combobox));
   end Hide_Popdown_Window;

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button (Combobox : access Gtk_Combo_Box_Record)
                       return Gtk.Button.Gtk_Button
   is
      function Internal (Combobox : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_box_get_button");
      Stub : Gtk.Button.Gtk_Button_Record;
   begin
      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Combobox)), Stub));
   end Get_Button;

   ---------------
   -- Get_Arrow --
   ---------------

   function Get_Arrow (Combobox : access Gtk_Combo_Box_Record)
                      return Gtk.Arrow.Gtk_Arrow
   is
      function Internal (Combobox : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_box_get_arrow");
      Stub : Gtk.Arrow.Gtk_Arrow_Record;
   begin
      return Gtk.Arrow.Gtk_Arrow
        (Get_User_Data (Internal (Get_Object (Combobox)), Stub));
   end Get_Arrow;

   ---------------
   -- Get_Frame --
   ---------------

   function Get_Frame (Combobox : access Gtk_Combo_Box_Record)
                      return Gtk.Frame.Gtk_Frame
   is
      function Internal (Combobox : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "ada_gtk_combo_box_get_frame");
      Stub : Gtk.Frame.Gtk_Frame_Record;
   begin
      return Gtk.Frame.Gtk_Frame
        (Get_User_Data (Internal (Get_Object (Combobox)), Stub));
   end Get_Frame;

end Gtk.Extra.Combo_Box;
