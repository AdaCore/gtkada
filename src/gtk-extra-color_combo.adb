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

with Gdk.Color;
with System;
with Interfaces.C.Strings;

package body Gtk.Extra.Color_Combo is

   ----------------
   -- Find_Color --
   ----------------

   procedure Find_Color (Color_Combo : access Gtk_Color_Combo_Record;
                         Color       : in Gdk.Color.Gdk_Color;
                         Row         : out Gint;
                         Col         : out Gint)
   is
      procedure Internal (Color_Combo : in System.Address;
                          Color       : in System.Address;
                          Row         : out Gint;
                          Col         : out Gint);
      pragma Import (C, Internal, "gtk_color_combo_find_color");
      use type Gdk.Color.Gdk_Color;

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Color_Combo), Ca, Row, Col);
   end Find_Color;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Combo) is
   begin
      Widget := new Gtk_Color_Combo_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Color_Combo_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_combo_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      : out Gtk_Color_Combo;
                      Nrows       : in Gint;
                      Ncols       : in Gint;
                      Color_Names : in Gtkada.Types.Chars_Ptr_Array)
   is
   begin
      Widget := new Gtk_Color_Combo_Record;
      Initialize (Widget, Nrows, Ncols, Color_Names);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget      : access Gtk_Color_Combo_Record;
                         Nrows       : in Gint;
                         Ncols       : in Gint;
                         Color_Names : in Gtkada.Types.Chars_Ptr_Array)
   is
      function Internal (Nrows       : in Gint;
                         Ncols       : in Gint;
                         Color_Names : in System.Address)
                        return           System.Address;
      pragma Import (C, Internal, "gtk_color_combo_new_with_values");
   begin
      Set_Object (Widget, Internal (Nrows,
                                    Ncols,
                                    Color_Names (Color_Names'First)'Address));
      Initialize_User_Data (Widget);
   end Initialize;

   ------------------
   -- Get_Color_At --
   ------------------

   function Get_Color_At (Widget : access Gtk_Color_Combo_Record;
                          Row    : Gint;
                          Col    : Gint)
                         return String
   is
      function Internal (Widget : System.Address;
                         Row    : Gint;
                         Col    : Gint)
                        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_color_combo_get_color_at");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (Widget), Row, Col));
   end Get_Color_At;

end Gtk.Extra.Color_Combo;
