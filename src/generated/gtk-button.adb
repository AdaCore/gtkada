-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Button_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Button) is
   begin
      Self := new Gtk_Button_Record;
      Gtk.Button.Initialize (Self);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Button; Label : UTF8_String) is
   begin
      Self := new Gtk_Button_Record;
      Gtk.Button.Initialize (Self, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Self     : out Gtk_Button;
       Stock_Id : UTF8_String)
   is
   begin
      Self := new Gtk_Button_Record;
      Gtk.Button.Initialize_From_Stock (Self, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Button;
       Label : UTF8_String)
   is
   begin
      Self := new Gtk_Button_Record;
      Gtk.Button.Initialize_With_Mnemonic (Self, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Button_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_button_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : access Gtk_Button_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      Tmp_Label := New_String (Label);
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Self, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Self     : access Gtk_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr;
      Tmp_Return   : System.Address;
   begin
      Tmp_Stock_Id := New_String (Stock_Id);
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      Set_Object (Self, Tmp_Return);
   end Initialize_From_Stock;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Self  : access Gtk_Button_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      Tmp_Label := New_String (Label);
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Self, Tmp_Return);
   end Initialize_With_Mnemonic;

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Self : access Gtk_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Self));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Self : access Gtk_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Self));
   end Enter;

   -------------------
   -- Get_Alignment --
   -------------------

   procedure Get_Alignment
      (Self   : access Gtk_Button_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : out Gfloat;
          Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_button_get_alignment");
   begin
      Internal (Get_Object (Self), Xalign, Yalign);
   end Get_Alignment;

   ----------------------
   -- Get_Event_Window --
   ----------------------

   function Get_Event_Window
      (Self : access Gtk_Button_Record) return Gdk.Window.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_button_get_event_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Event_Window;

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
      (Self : access Gtk_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_focus_on_click");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Focus_On_Click;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image (Self : access Gtk_Button_Record) return Gtk_Widget is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_button_get_image");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub));
   end Get_Image;

   ------------------------
   -- Get_Image_Position --
   ------------------------

   function Get_Image_Position
      (Self : access Gtk_Button_Record) return Gtk.Enums.Gtk_Position_Type
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_image_position");
   begin
      return Gtk.Enums.Gtk_Position_Type'Val (Internal (Get_Object (Self)));
   end Get_Image_Position;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Self : access Gtk_Button_Record) return UTF8_String is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_button_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Self)));
   end Get_Label;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief
      (Self : access Gtk_Button_Record) return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_relief");
   begin
      return Gtk.Enums.Gtk_Relief_Style'Val (Internal (Get_Object (Self)));
   end Get_Relief;

   -------------------
   -- Get_Use_Stock --
   -------------------

   function Get_Use_Stock (Self : access Gtk_Button_Record) return Boolean is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_use_stock");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Stock;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Self : access Gtk_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Underline;

   -----------
   -- Leave --
   -----------

   procedure Leave (Self : access Gtk_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_button_leave");
   begin
      Internal (Get_Object (Self));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Self : access Gtk_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Self));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Self : access Gtk_Button_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Self));
   end Released;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Self   : access Gtk_Button_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Gfloat;
          Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_button_set_alignment");
   begin
      Internal (Get_Object (Self), Xalign, Yalign);
   end Set_Alignment;

   ------------------------
   -- Set_Focus_On_Click --
   ------------------------

   procedure Set_Focus_On_Click
      (Self           : access Gtk_Button_Record;
       Focus_On_Click : Boolean)
   is
      procedure Internal (Self : System.Address; Focus_On_Click : Integer);
      pragma Import (C, Internal, "gtk_button_set_focus_on_click");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
      (Self  : access Gtk_Button_Record;
       Image : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Image : System.Address);
      pragma Import (C, Internal, "gtk_button_set_image");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Image)));
   end Set_Image;

   ------------------------
   -- Set_Image_Position --
   ------------------------

   procedure Set_Image_Position
      (Self     : access Gtk_Button_Record;
       Position : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal (Self : System.Address; Position : Integer);
      pragma Import (C, Internal, "gtk_button_set_image_position");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Position_Type'Pos (Position));
   end Set_Image_Position;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Self  : access Gtk_Button_Record;
       Label : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Label : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_button_set_label");
      Tmp_Label : Interfaces.C.Strings.chars_ptr;
   begin
      Tmp_Label := New_String (Label);
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief
      (Self     : access Gtk_Button_Record;
       Newstyle : Gtk.Enums.Gtk_Relief_Style)
   is
      procedure Internal (Self : System.Address; Newstyle : Integer);
      pragma Import (C, Internal, "gtk_button_set_relief");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Relief_Style'Pos (Newstyle));
   end Set_Relief;

   -------------------
   -- Set_Use_Stock --
   -------------------

   procedure Set_Use_Stock
      (Self      : access Gtk_Button_Record;
       Use_Stock : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Stock : Integer);
      pragma Import (C, Internal, "gtk_button_set_use_stock");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Stock));
   end Set_Use_Stock;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Self          : access Gtk_Button_Record;
       Use_Underline : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Underline : Integer);
      pragma Import (C, Internal, "gtk_button_set_use_underline");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

end Gtk.Button;
