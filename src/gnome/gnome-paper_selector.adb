-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--               Copyright (C) 2000 Helix Code, Inc.                 --
--               Copyright (C) 2000-2001 ACT-Europe                  --
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
with Gtk; use Gtk;
with Interfaces.C.Strings;

package body Gnome.Paper_Selector is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Paper_Selector : out Gnome_Paper_Selector) is
   begin
      Paper_Selector := new Gnome_Paper_Selector_Record;
      Gnome.Paper_Selector.Initialize (Paper_Selector);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Paper_Selector : access Gnome_Paper_Selector_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_paper_selector_new");

   begin
      Set_Object (Paper_Selector, Internal);
      Initialize_User_Data (Paper_Selector);
   end Initialize;

   -----------------------
   -- Get_Bottom_Margin --
   -----------------------

   function Get_Bottom_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_bottom_margin");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Bottom_Margin;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_height");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Height;

   ---------------------
   -- Get_Left_Margin --
   ---------------------

   function Get_Left_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_left_margin");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Left_Margin;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Paper_Selector : access Gnome_Paper_Selector_Record) return String
   is
      function Internal (Gspaper : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_paper_selector_get_name");
   begin
      return
        Interfaces.C.Strings.Value (Internal (Get_Object (Paper_Selector)));
   end Get_Name;

   ----------------------
   -- Get_Right_Margin --
   ----------------------

   function Get_Right_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_right_margin");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Right_Margin;

   --------------------
   -- Get_Top_Margin --
   --------------------

   function Get_Top_Margin
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_top_margin");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Top_Margin;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
     (Paper_Selector : access Gnome_Paper_Selector_Record) return Gfloat
   is
      function Internal (Paper_Selector : System.Address) return Gfloat;
      pragma Import (C, Internal, "gnome_paper_selector_get_width");

   begin
      return Internal (Get_Object (Paper_Selector));
   end Get_Width;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Height         : Gfloat)
   is
      procedure Internal
        (Paper_Selector : System.Address;
         Height         : Gfloat);
      pragma Import (C, Internal, "gnome_paper_selector_set_height");

   begin
      Internal (Get_Object (Paper_Selector), Height);
   end Set_Height;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Name           : String)
   is
      procedure Internal
        (Paper_Selector : System.Address;
         Name           : String);
      pragma Import (C, Internal, "gnome_paper_selector_set_name");

   begin
      Internal (Get_Object (Paper_Selector), Name & ASCII.NUL);
   end Set_Name;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
     (Paper_Selector : access Gnome_Paper_Selector_Record;
      Width          : Gfloat)
   is
      procedure Internal
        (Paper_Selector : System.Address;
         Width          : Gfloat);
      pragma Import (C, Internal, "gnome_paper_selector_set_width");

   begin
      Internal (Get_Object (Paper_Selector), Width);
   end Set_Width;

end Gnome.Paper_Selector;
