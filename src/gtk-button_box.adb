-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gtk.Button_Box is

   ------------------------
   -- Get_Child_Ipadding --
   ------------------------

   procedure Get_Child_Ipadding
     (Button_Box : access Gtk_Button_Box_Record;
      Ipad_X     : out Gint;
      Ipad_Y     : out Gint)
   is
      procedure Internal
        (Widget : in System.Address;
         Ipad_X : in out Gint;
         Ipad_Y : in out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding");

   begin
      Internal (Get_Object (Button_Box), Ipad_X, Ipad_Y);
   end Get_Child_Ipadding;

   --------------------
   -- Get_Child_Size --
   --------------------

   procedure Get_Child_Size
     (Button_Box : access Gtk_Button_Box_Record;
      Min_Width  : out Gint;
      Min_Height : out Gint)
   is
      procedure Internal
        (Widget     : in System.Address;
         Min_Width  : out Gint;
         Min_Height : out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size");

   begin
      Internal (Get_Object (Button_Box), Min_Width, Min_Height);
   end Get_Child_Size;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Button_Box : access Gtk_Button_Box_Record)
     return Enums.Gtk_Button_Box_Style
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_box_get_layout");

   begin
      return Enums.Gtk_Button_Box_Style'Val
        (Internal (Get_Object (Button_Box)));
   end Get_Layout;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Button_Box : access Gtk_Button_Box_Record)
     return Gint
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_box_get_spacing");

   begin
      return Internal (Get_Object (Button_Box));
   end Get_Spacing;

   ------------------------
   -- Set_Child_Ipadding --
   ------------------------

   procedure Set_Child_Ipadding
     (Button_Box : access Gtk_Button_Box_Record;
      Ipad_X     : in Gint;
      Ipad_Y     : in Gint)
   is
      procedure Internal
        (Widget : in System.Address;
         Ipad_X : in Gint;
         Ipad_Y : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding");

   begin
      Internal (Get_Object (Button_Box), Ipad_X, Ipad_Y);
   end Set_Child_Ipadding;

   --------------------
   -- Set_Child_Size --
   --------------------

   procedure Set_Child_Size
     (Button_Box : access Gtk_Button_Box_Record;
      Min_Width  : in Gint;
      Min_Height : in Gint)
   is
      procedure Internal
        (Widget     : in System.Address;
         Min_Width  : in Gint;
         Min_Height : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size");

   begin
      Internal (Get_Object (Button_Box), Min_Width, Min_Height);
   end Set_Child_Size;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
     (Button_Box   : access Gtk_Button_Box_Record;
      Layout_Style : in Enums.Gtk_Button_Box_Style)
   is
      procedure Internal
        (Widget       : in System.Address;
         Layout_Style : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_layout");

   begin
      Internal (Get_Object (Button_Box),
                Enums.Gtk_Button_Box_Style'Pos (Layout_Style));
   end Set_Layout;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Button_Box : access Gtk_Button_Box_Record;
                          Spacing    : in Gint)
   is
      procedure Internal (Widget  : in System.Address; Spacing : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_spacing");

   begin
      Internal (Get_Object (Button_Box), Spacing);
   end Set_Spacing;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Box.Generate (N, File);
      Gen_Set (N, "Button_Box", "spacing", File);
      Gen_Set (N, "Button_Box", "Layout", "layout_style", "", "", "", File);
      Gen_Set (N, "Button_Box", "Child_Size",
        "child_min_width", "child_min_height", "", "", File);
      Gen_Set (N, "Button_Box", "Child_Ipadding",
        "child_ipad_x", "child_ipad_y", "", "", File);
   end Generate;

   procedure Generate
     (Button_Box : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S, S2 : String_Ptr;
   begin
      Box.Generate (Button_Box, N);
      S := Get_Field (N, "spacing");

      if S /= null then
         Set_Spacing (Gtk_Button_Box (Button_Box), Gint'Value (S.all));
      end if;

      S := Get_Field (N, "layout_style");

      if S /= null then
         Set_Layout
           (Gtk_Button_Box (Button_Box),
            Enums.Gtk_Button_Box_Style'Value (S (S'First + 4 .. S'Last)));
      end if;

      S  := Get_Field (N, "child_min_width");
      S2 := Get_Field (N, "child_min_height");

      if S /= null and then S2 /= null then
         Set_Child_Size
           (Gtk_Button_Box (Button_Box),
            Gint'Value (S.all),
            Gint'Value (S2.all));
      end if;

      S  := Get_Field (N, "child_ipad_x");
      S2 := Get_Field (N, "child_ipad_y");

      if S /= null and then S2 /= null then
         Set_Child_Ipadding
           (Gtk_Button_Box (Button_Box),
            Gint'Value (S.all),
            Gint'Value (S2.all));
      end if;
   end Generate;

end Gtk.Button_Box;
