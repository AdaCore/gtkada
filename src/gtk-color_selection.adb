-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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
with Gdk; use Gdk;

package body Gtk.Color_Selection is

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : out Color_Array)
   is
      procedure Internal (Colorsel : in System.Address;
                          Color    : out Color_Array);
      pragma Import (C, Internal, "gtk_color_selection_get_color");
   begin
      Color (Opacity) := 0.0;
      Internal (Get_Object (Colorsel), Color);
   end Get_Color;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Selection) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Colorsel : in Gtk_Color_Selection'Class;
                        Color    : in Color_Array)
   is
      procedure Internal (Colorsel : in System.Address;
                          Color    : in System.Address);
      pragma Import (C, Internal, "gtk_color_selection_set_color");
   begin
      Internal (Get_Object (Colorsel), Color'Address);
   end Set_Color;

   -----------------
   -- Set_Opacity --
   -----------------

   procedure Set_Opacity (Colorsel    : in Gtk_Color_Selection'Class;
                          Use_Opacity : in Boolean)
   is
      procedure Internal (Colorsel    : in System.Address;
                          Use_Opacity : in Gint);
      pragma Import (C, Internal, "gtk_color_selection_set_opacity");
   begin
      Internal (Get_Object (Colorsel), Boolean'Pos (Use_Opacity));
   end Set_Opacity;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy (Colorsel : in Gtk_Color_Selection'Class;
                                Policy   : in Enums.Gtk_Update_Type)
   is
      procedure Internal (Colorsel : in System.Address;
                          Policy   : in Gint);
      pragma Import (C, Internal, "gtk_color_selection_set_update_policy");
   begin
      Internal (Get_Object (Colorsel), Enums.Gtk_Update_Type'Pos (Policy));
   end Set_Update_Policy;

end Gtk.Color_Selection;
