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

with Gdk; use Gdk;
with System;

package body Gtk.Extra.Font_Combo is

   -----------------------
   -- Font_Combo_Select --
   -----------------------

   procedure Font_Combo_Select (Font_Combo : access Gtk_Font_Combo_Record;
                                Family     : in String;
                                Bold       : in Boolean;
                                Italic     : in Boolean;
                                Height     : in Gint)
   is
      procedure Internal (Font_Combo : in System.Address;
                          Family     : in String;
                          Bold       : in Gint;
                          Italic     : in Gint;
                          Height     : in Gint);
      pragma Import (C, Internal, "gtk_font_combo_select");
   begin
      Internal (Get_Object (Font_Combo),
                Family & ASCII.Nul,
                Boolean'Pos (Bold),
                Boolean'Pos (Italic),
                Height);
   end Font_Combo_Select;

   ---------------------------
   -- Font_Combo_Select_Nth --
   ---------------------------

   procedure Font_Combo_Select_Nth (Font_Combo : access Gtk_Font_Combo_Record;
                                    N          : in Gint;
                                    Bold       : in Boolean;
                                    Italic     : in Boolean;
                                    Height     : in Gint)
   is
      procedure Internal (Font_Combo : in System.Address;
                          N          : in Gint;
                          Bold       : in Gint;
                          Italic     : in Gint;
                          Height     : in Gint);
      pragma Import (C, Internal, "gtk_font_combo_select_nth");
   begin
      Internal (Get_Object (Font_Combo),
                N,
                Boolean'Pos (Bold),
                Boolean'Pos (Italic),
                Height);
   end Font_Combo_Select_Nth;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Font_Combo)
   is
   begin
      Widget := new Gtk_Font_Combo_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Font_Combo_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_font_combo_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   --------------------
   -- Get_Name_Combo --
   --------------------

   function Get_Name_Combo (Font_Combo : access Gtk_Font_Combo_Record)
                           return Gtk.Combo.Gtk_Combo
   is
      function Internal (Font_Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_combo_get_name_combo");
      Stub : Gtk.Combo.Gtk_Combo_Record;
   begin
      return Gtk.Combo.Gtk_Combo
        (Get_User_Data (Internal (Get_Object (Font_Combo)), Stub));
   end Get_Name_Combo;

   --------------------
   -- Get_Size_Combo --
   --------------------

   function Get_Size_Combo (Font_Combo : access Gtk_Font_Combo_Record)
                           return Gtk.Combo.Gtk_Combo
   is
      function Internal (Font_Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_combo_get_size_combo");
      Stub : Gtk.Combo.Gtk_Combo_Record;
   begin
      return Gtk.Combo.Gtk_Combo
        (Get_User_Data (Internal (Get_Object (Font_Combo)), Stub));
   end Get_Size_Combo;

   ---------------------
   -- Get_Bold_Button --
   ---------------------

   function Get_Bold_Button (Font_Combo : access Gtk_Font_Combo_Record)
                            return Gtk.Toggle_Button.Gtk_Toggle_Button
   is
      function Internal (Font_Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_combo_get_bold_button");
      Stub : Gtk.Toggle_Button.Gtk_Toggle_Button_Record;
   begin
      return Gtk.Toggle_Button.Gtk_Toggle_Button
        (Get_User_Data (Internal (Get_Object (Font_Combo)), Stub));
   end Get_Bold_Button;

   -----------------------
   -- Get_Italic_Button --
   -----------------------

   function Get_Italic_Button (Font_Combo : access Gtk_Font_Combo_Record)
                              return Gtk.Toggle_Button.Gtk_Toggle_Button
   is
      function Internal (Font_Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_combo_get_italic_button");
      Stub : Gtk.Toggle_Button.Gtk_Toggle_Button_Record;
   begin
      return Gtk.Toggle_Button.Gtk_Toggle_Button
        (Get_User_Data (Internal (Get_Object (Font_Combo)), Stub));
   end Get_Italic_Button;

   --------------
   -- Get_Font --
   --------------

   function Get_Font (Font_Combo : access Gtk_Font_Combo_Record)
                     return Gdk.Font.Gdk_Font
   is
      function Internal (Font_Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_font_combo_get_font");
      F : Gdk.Font.Gdk_Font;
   begin
      Set_Object (F, Internal (Get_Object (Font_Combo)));
      return F;
   end Get_Font;

end Gtk.Extra.Font_Combo;
