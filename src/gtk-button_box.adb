-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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
with Gdk; use Gdk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package body Gtk.Button_Box is

   -----------------------
   -- Child_Requisition --
   -----------------------

   procedure Child_Requisition
      (Widget        : in Gtk.Widget.Gtk_Widget'Class;
       Nvis_Children : in out Integer;
       Width         : in out Integer;
       Height        : in out Integer)
   is
      procedure Internal
         (Widget        : in System.Address;
          Nvis_Children : in out Integer;
          Width         : in out Integer;
          Height        : in out Integer);
      pragma Import (C, Internal, "gtk_button_box_child_requisition");
   begin
      Internal (Get_Object (Widget),
                Nvis_Children,
                Width,
                Height);
   end Child_Requisition;

   ------------------------
   -- Get_Child_Ipadding --
   ------------------------

   procedure Get_Child_Ipadding
      (Widget : in Gtk_Button_Box'Class;
       Ipad_X : in out Gint;
       Ipad_Y : in out Gint)
   is
      procedure Internal
         (Widget : in System.Address;
          Ipad_X : in out Gint;
          Ipad_Y : in out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding");
   begin
      Internal (Get_Object (Widget),
                Ipad_X,
                Ipad_Y);
   end Get_Child_Ipadding;

   --------------------------------
   -- Get_Child_Ipadding_Default --
   --------------------------------

   procedure Get_Child_Ipadding_Default
      (Ipad_X : in out Gint;
       Ipad_Y : in out Gint)
   is
      procedure Internal
         (Ipad_X : in out Gint;
          Ipad_Y : in out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_ipadding_default");
   begin
      Internal (Ipad_X,
                Ipad_Y);
   end Get_Child_Ipadding_Default;

   --------------------
   -- Get_Child_Size --
   --------------------

   procedure Get_Child_Size
      (Widget     : in Gtk_Button_Box'Class;
       Min_Width  : in out Gint;
       Min_Height : in out Gint)
   is
      procedure Internal
         (Widget     : in System.Address;
          Min_Width  : in out Gint;
          Min_Height : in out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size");
   begin
      Internal (Get_Object (Widget),
                Min_Width,
                Min_Height);
   end Get_Child_Size;

   ----------------------------
   -- Get_Child_Size_Default --
   ----------------------------

   procedure Get_Child_Size_Default
      (Min_Width  : in out Gint;
       Min_Height : in out Gint)
   is
      procedure Internal
         (Min_Width  : in out Gint;
          Min_Height : in out Gint);
      pragma Import (C, Internal, "gtk_button_box_get_child_size_default");
   begin
      Internal (Min_Width,
                Min_Height);
   end Get_Child_Size_Default;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Widget : in Gtk_Button_Box'Class)
                        return      Gtk_Button_Box_Style
   is
      function Internal (Widget : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_button_box_get_layout");
   begin
      return Gtk_Button_Box_Style'Val (Internal (Get_Object (Widget)));
   end Get_Layout;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Widget : in Gtk_Button_Box'Class)
                         return      Gint
   is
      function Internal (Widget : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_button_box_get_spacing");
   begin
      return Internal (Get_Object (Widget));
   end Get_Spacing;

   ------------------------
   -- Set_Child_Ipadding --
   ------------------------

   procedure Set_Child_Ipadding
      (Widget : in Gtk_Button_Box'Class;
       Ipad_X : in Gint;
       Ipad_Y : in Gint)
   is
      procedure Internal
         (Widget : in System.Address;
          Ipad_X : in Gint;
          Ipad_Y : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding");
   begin
      Internal (Get_Object (Widget),
                Ipad_X,
                Ipad_Y);
   end Set_Child_Ipadding;

   --------------------------------
   -- Set_Child_Ipadding_Default --
   --------------------------------

   procedure Set_Child_Ipadding_Default
      (Ipad_X : in Gint;
       Ipad_Y : in Gint)
   is
      procedure Internal
         (Ipad_X : in Gint;
          Ipad_Y : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_ipadding_default");
   begin
      Internal (Ipad_X,
                Ipad_Y);
   end Set_Child_Ipadding_Default;

   --------------------
   -- Set_Child_Size --
   --------------------

   procedure Set_Child_Size
      (Widget     : in Gtk_Button_Box'Class;
       Min_Width  : in Gint;
       Min_Height : in Gint)
   is
      procedure Internal
         (Widget     : in System.Address;
          Min_Width  : in Gint;
          Min_Height : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size");
   begin
      Internal (Get_Object (Widget),
                Min_Width,
                Min_Height);
   end Set_Child_Size;

   ----------------------------
   -- Set_Child_Size_Default --
   ----------------------------

   procedure Set_Child_Size_Default
      (Min_Width  : in Gint;
       Min_Height : in Gint)
   is
      procedure Internal
         (Min_Width  : in Gint;
          Min_Height : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_child_size_default");
   begin
      Internal (Min_Width,
                Min_Height);
   end Set_Child_Size_Default;

   ----------------
   -- Set_Layout --
   ----------------

   procedure Set_Layout
      (Widget       : in Gtk_Button_Box'Class;
       Layout_Style : in Gtk_Button_Box_Style)
   is
      procedure Internal
         (Widget       : in System.Address;
          Layout_Style : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_layout");
   begin
      Internal (Get_Object (Widget),
                Gtk_Button_Box_Style'Pos (Layout_Style));
   end Set_Layout;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Widget  : in Gtk_Button_Box'Class;
       Spacing : in Gint)
   is
      procedure Internal
         (Widget  : in System.Address;
          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_button_box_set_spacing");
   begin
      Internal (Get_Object (Widget),
                Spacing);
   end Set_Spacing;

end Gtk.Button_Box;
