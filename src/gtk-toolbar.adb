-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Toolbar is

   --------------------
   -- Append_Element --
   --------------------

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_element");

      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      W    : System.Address;
      I    : System.Address;

      use type Gtk.Widget.Gtk_Widget;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Widget = null then
         W := System.Null_Address;
      else
         W := Get_Object (Widget);
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           W, TA, TTA, TPTA, I, System.Null_Address,
           System.Null_Address));
   end Append_Element;

   -----------------
   -- Append_Item --
   -----------------

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_append_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;
      I    : System.Address;

      use type Gtk.Widget.Gtk_Widget;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      if Icon = null then
         I := System.Null_Address;
      else
         I := Get_Object (Icon);
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data (Internal (Get_Object (Toolbar),
                                  TA, TTA, TPTA, I, System.Null_Address,
                                  System.Null_Address),
                        Stub));
   end Append_Item;

   ------------------
   -- Append_Space --
   ------------------

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_space");

   begin
      Internal (Get_Object (Toolbar));
   end Append_Space;

   -------------------
   -- Append_Widget --
   -------------------

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "")
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_append_widget");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Append_Widget;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Toolbar) is
   begin
      Widget := new Gtk_Toolbar_Record;
      Gtk.Toolbar.Initialize (Widget);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style) is
   begin
      Widget := new Gtk_Toolbar_Record;
      Initialize (Widget, Orientation, Style);
   end Gtk_New;


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Toolbar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_new");

   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style) is
   begin
      Gtk.Toolbar.Initialize (Widget);
      Set_Orientation (Widget, Orientation);
      Set_Style (Widget, Style);
   end Initialize;

   --------------------
   -- Insert_Element --
   --------------------

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address;
         Position             : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_element");

      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           Get_Object (Widget),
           TA, TTA, TPTA, Get_Object (Icon),
           System.Null_Address,
           System.Null_Address,
           Position));
   end Insert_Element;

   -----------------
   -- Insert_Item --
   -----------------

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : Gint) return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address;
         Position             : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_insert_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA,
             Get_Object (Icon),
             System.Null_Address, System.Null_Address,
             Position),
           Stub));
   end Insert_Item;

   ------------------
   -- Insert_Space --
   ------------------

   procedure Insert_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint)
   is
      procedure Internal (Toolbar : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_space");

   begin
      Internal (Get_Object (Toolbar), Position);
   end Insert_Space;

   ------------------
   -- Remove_Space --
   ------------------

   procedure Remove_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint)
   is
      procedure Internal (Toolbar : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_toolbar_remove_space");

   begin
      Internal (Get_Object (Toolbar), Position);
   end Remove_Space;

   ------------------
   -- Insert_Stock --
   ------------------

   procedure Insert_Stock
     (Toolbar              : access Gtk_Toolbar_Record;
      Stock_Id             : String;
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Position             : Gint)
   is
      procedure Internal
        (Toolbar              : System.Address;
         Stock_Id             : String;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Position             : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_stock");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal
        (Get_Object (Toolbar), Stock_Id & ASCII.NUL, TTA, TPTA, Position);
   end Insert_Stock;

   -------------------
   -- Insert_Widget --
   -------------------

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Position             : Gint)
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Position             : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert_widget");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal
        (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA, Position);
   end Insert_Widget;

   ---------------------
   -- Prepend_Element --
   ---------------------

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Toolbar              : System.Address;
         The_Type             : Gtk_Toolbar_Child_Type;
         Widget               : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_element");

      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Widget.Convert
        (Internal
          (Get_Object (Toolbar),
           The_Type,
           Get_Object (Widget),
           TA, TTA, TPTA,
           Get_Object (Icon),
           System.Null_Address, System.Null_Address));
   end Prepend_Element;

   ------------------
   -- Prepend_Item --
   ------------------

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : String := "";
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Button.Gtk_Button
   is
      function Internal
        (Toolbar              : System.Address;
         Text                 : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address;
         Icon                 : System.Address;
         Callback             : System.Address;
         User_Data            : System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_prepend_item");

      Stub : Gtk.Button.Gtk_Button_Record;
      T    : aliased constant String := Text & ASCII.NUL;
      TA   : System.Address := T'Address;
      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Text'Length = 0 then
         TA := System.Null_Address;
      end if;

      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      return Gtk.Button.Gtk_Button
        (Get_User_Data
          (Internal
            (Get_Object (Toolbar),
             TA, TTA, TPTA,
             Get_Object (Icon),
             System.Null_Address, System.Null_Address),
           Stub));
   end Prepend_Item;

   -------------------
   -- Prepend_Space --
   -------------------

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_space");

   begin
      Internal (Get_Object (Toolbar));
   end Prepend_Space;

   --------------------
   -- Prepend_Widget --
   --------------------

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : String := "";
      Tooltip_Private_Text : String := "")
   is
      procedure Internal
        (Toolbar              : System.Address;
         Widget               : System.Address;
         Tooltip_Text         : System.Address;
         Tooltip_Private_Text : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_prepend_widget");

      TT   : aliased constant String := Tooltip_Text & ASCII.NUL;
      TTA  : System.Address := TT'Address;
      TPT  : aliased constant String := Tooltip_Private_Text & ASCII.NUL;
      TPTA : System.Address := TPT'Address;

   begin
      if Tooltip_Text'Length = 0 then
         TTA := System.Null_Address;
      end if;

      if Tooltip_Private_Text'Length = 0 then
         TPTA := System.Null_Address;
      end if;

      Internal (Get_Object (Toolbar), Get_Object (Widget), TTA, TPTA);
   end Prepend_Widget;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
     (Toolbar   : access Gtk_Toolbar_Record;
      Icon_Size : Gtk_Icon_Size)
   is
      procedure Internal
        (Toolbar : System.Address; Icon_Size : Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_toolbar_set_icon_size");

   begin
      Internal (Get_Object (Toolbar), Icon_Size);
   end Set_Icon_Size;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : Gtk_Orientation)
   is
      procedure Internal
        (Toolbar     : System.Address;
         Orientation : Gtk_Orientation);
      pragma Import (C, Internal, "gtk_toolbar_set_orientation");

   begin
      Internal (Get_Object (Toolbar), Orientation);
   end Set_Orientation;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : Gtk_Toolbar_Style)
   is
      procedure Internal (Toolbar : System.Address; Style : Gtk_Toolbar_Style);
      pragma Import (C, Internal, "gtk_toolbar_set_style");

   begin
      Internal (Get_Object (Toolbar), Style);
   end Set_Style;

   ------------------
   -- Set_Tooltips --
   ------------------

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record; Enable : Boolean)
   is
      procedure Internal (Toolbar : System.Address; Enable : Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_tooltips");

   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Enable));
   end Set_Tooltips;

   -----------------
   -- Unset_Style --
   -----------------

   procedure Unset_Style (Toolbar : access Gtk_Toolbar_Record)
   is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_style");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Style;

   ---------------------
   -- Unset_Icon_Size --
   ---------------------

   procedure Unset_Icon_Size (Toolbar : access Gtk_Toolbar_Record)
   is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_icon_size");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Icon_Size;

end Gtk.Toolbar;
