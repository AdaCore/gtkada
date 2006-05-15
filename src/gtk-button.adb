-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtk.Widget;           use Gtk.Widget;

package body Gtk.Button is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Button : access Gtk_Button_Record) is
      procedure Internal (W : in System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Button));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Button : access Gtk_Button_Record) is
      procedure Internal (W : System.Address);
      pragma Import (C, Internal, "gtk_button_enter");

   begin
      Internal (Get_Object (Button));
   end Enter;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Button : access Gtk_Button_Record) return UTF8_String
   is
      function Internal (Button : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_button_get_label");

   begin
      return Value (Internal (Get_Object (Button)));
   end Get_Label;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief
     (Button : access Gtk_Button_Record)
      return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal (Button : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_button_get_relief");

   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Get_Object (Button)));
   end Get_Relief;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
     (Button : access Gtk_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_button_get_use_underline");

   begin
      return Internal (Get_Object (Button)) /= 0;
   end Get_Use_Underline;

   -------------------
   -- Get_Use_Stock --
   -------------------

   function Get_Use_Stock
     (Button : access Gtk_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_button_get_use_stock");

   begin
      return Internal (Get_Object (Button)) /= 0;
   end Get_Use_Stock;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Button; Label : UTF8_String := "") is
   begin
      Button := new Gtk_Button_Record;
      Initialize (Button, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Button : out Gtk_Button; Stock_Id : String) is
   begin
      Button := new Gtk_Button_Record;
      Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Button : out Gtk_Button; Label : UTF8_String) is
   begin
      Button := new Gtk_Button_Record;
      Initialize_With_Mnemonic (Button, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Button : access Gtk_Button_Record'Class;
      Label  : UTF8_String)
   is
      function Internal (S : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");

      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gtk_button_new");

   begin
      if Label = "" then
         Set_Object (Button, Internal2);
      else
         Set_Object (Button, Internal (Label & ASCII.NUL));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Button   : access Gtk_Button_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_from_stock");

   begin
      Set_Object (Button, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Button : access Gtk_Button_Record'Class;
      Label  : UTF8_String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_mnemonic");

   begin
      Set_Object (Button, Internal (Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

   -----------
   -- Leave --
   -----------

   procedure Leave (Button : access Gtk_Button_Record) is
      procedure Internal (W : System.Address);
      pragma Import (C, Internal, "gtk_button_leave");

   begin
      Internal (Get_Object (Button));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Button : access Gtk_Button_Record) is
      procedure Internal (W : System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");

   begin
      Internal (Get_Object (Button));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Button : access Gtk_Button_Record) is
      procedure Internal (W : System.Address);
      pragma Import (C, Internal, "gtk_button_released");

   begin
      Internal (Get_Object (Button));
   end Released;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Button : access Gtk_Button_Record;
      Label  : UTF8_String)
   is
      procedure Internal
        (Button : System.Address;
         Label  : String);
      pragma Import (C, Internal, "gtk_button_set_label");

   begin
      Internal (Get_Object (Button), Label & ASCII.NUL);
   end Set_Label;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief
     (Button    : access Gtk_Button_Record;
      New_Style : Gtk.Enums.Gtk_Relief_Style)
   is
      procedure Internal
        (Button    : System.Address;
         New_Style : Gtk.Enums.Gtk_Relief_Style);
      pragma Import (C, Internal, "gtk_button_set_relief");

   begin
      Internal (Get_Object (Button), New_Style);
   end Set_Relief;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
     (Button        : access Gtk_Button_Record;
      Use_Underline : Boolean)
   is
      procedure Internal
        (Button        : System.Address;
         Use_Underline : Gboolean);
      pragma Import (C, Internal, "gtk_button_set_use_underline");

   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   -------------------
   -- Set_Use_Stock --
   -------------------

   procedure Set_Use_Stock
     (Button    : access Gtk_Button_Record;
      Use_Stock : Boolean)
   is
      procedure Internal
        (Button    : System.Address;
         Use_Stock : Gboolean);
      pragma Import (C, Internal, "gtk_button_set_use_stock");

   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Stock));
   end Set_Use_Stock;

   -------------------
   -- Get_Alignment --
   -------------------

   procedure Get_Alignment
     (Button : access Gtk_Button_Record;
      Xalign : out Gfloat;
      Yalign : out Gfloat)
   is
      procedure Internal
        (Button : System.Address;
         Xalign : out Gfloat;
         Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_button_get_alignment");
   begin
      Internal (Get_Object (Button), Xalign, Yalign);
   end Get_Alignment;

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
     (Button : access Gtk_Button_Record)
     return Boolean
   is
      function Internal
        (Button : System.Address)
        return Gboolean;
      pragma Import (C, Internal, "gtk_button_get_focus_on_click");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Focus_On_Click;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Button : access Gtk_Button_Record)
     return Gtk_Widget
   is
      function Internal
        (Button : System.Address)
        return System.Address;
      pragma Import (C, Internal, "gtk_button_get_image");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data
          (Internal (Get_Object (Button)), Stub));
   end Get_Image;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
     (Button : access Gtk_Button_Record;
      Xalign : Gfloat := 0.5;
      Yalign : Gfloat := 0.5)
   is
      procedure Internal
        (Button : System.Address;
         Xalign : Gfloat;
         Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_button_set_alignment");
   begin
      Internal (Get_Object (Button), Xalign, Yalign);
   end Set_Alignment;

   ------------------------
   -- Set_Focus_On_Click --
   ------------------------

   procedure Set_Focus_On_Click
     (Button         : access Gtk_Button_Record;
      Focus_On_Click : Boolean := True)
   is
      procedure Internal
        (Button         : System.Address;
         Focus_On_Click : Gboolean);
      pragma Import (C, Internal, "gtk_button_set_focus_on_click");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Button : access Gtk_Button_Record;
      Image  : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Button : System.Address;
         Image  : System.Address);
      pragma Import (C, Internal, "gtk_button_set_image");
   begin
      Internal (Get_Object (Button), Get_Object (Image));
   end Set_Image;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkButton" then
         return new Gtk_Button_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Button;
