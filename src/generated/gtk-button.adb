------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Button is

   package Type_Conversion_Gtk_Button is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Button_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Button);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Button : out Gtk_Button; Label : UTF8_String := "") is
   begin
      Button := new Gtk_Button_Record;
      Gtk.Button.Initialize (Button, Label);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Button;
       Stock_Id : UTF8_String)
   is
   begin
      Button := new Gtk_Button_Record;
      Gtk.Button.Initialize_From_Stock (Button, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Button : out Gtk_Button;
       Label  : UTF8_String)
   is
   begin
      Button := new Gtk_Button_Record;
      Gtk.Button.Initialize_With_Mnemonic (Button, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String := "")
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_label");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr;
      Tmp_Return : System.Address;
   begin
      if Label = "" then
         Tmp_Label := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Label := New_String (Label);
      end if;
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Button, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Button_Record'Class;
       Stock_Id : UTF8_String)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      Set_Object (Button, Tmp_Return);
   end Initialize_From_Stock;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String)
   is
      function Internal
         (Label : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_button_new_with_mnemonic");
      Tmp_Label  : Interfaces.C.Strings.chars_ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Label);
      Free (Tmp_Label);
      Set_Object (Button, Tmp_Return);
   end Initialize_With_Mnemonic;

   -------------
   -- Clicked --
   -------------

   procedure Clicked (Button : not null access Gtk_Button_Record) is
      procedure Internal (Button : System.Address);
      pragma Import (C, Internal, "gtk_button_clicked");
   begin
      Internal (Get_Object (Button));
   end Clicked;

   -----------
   -- Enter --
   -----------

   procedure Enter (Button : not null access Gtk_Button_Record) is
      procedure Internal (Button : System.Address);
      pragma Import (C, Internal, "gtk_button_enter");
   begin
      Internal (Get_Object (Button));
   end Enter;

   -------------------
   -- Get_Alignment --
   -------------------

   procedure Get_Alignment
      (Button : not null access Gtk_Button_Record;
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

   ----------------------
   -- Get_Event_Window --
   ----------------------

   function Get_Event_Window
      (Button : not null access Gtk_Button_Record)
       return Gdk.Window.Gdk_Window
   is
      function Internal
         (Button : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_button_get_event_window");
   begin
      return Internal (Get_Object (Button));
   end Get_Event_Window;

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
      (Button : not null access Gtk_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_focus_on_click");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Focus_On_Click;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
      (Button : not null access Gtk_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_button_get_image");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Image;

   ------------------------
   -- Get_Image_Position --
   ------------------------

   function Get_Image_Position
      (Button : not null access Gtk_Button_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Button : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_button_get_image_position");
   begin
      return Internal (Get_Object (Button));
   end Get_Image_Position;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Button : not null access Gtk_Button_Record) return UTF8_String
   is
      function Internal
         (Button : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_button_get_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Button)));
   end Get_Label;

   ----------------
   -- Get_Relief --
   ----------------

   function Get_Relief
      (Button : not null access Gtk_Button_Record)
       return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal
         (Button : System.Address) return Gtk.Enums.Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_button_get_relief");
   begin
      return Internal (Get_Object (Button));
   end Get_Relief;

   -------------------
   -- Get_Use_Stock --
   -------------------

   function Get_Use_Stock
      (Button : not null access Gtk_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_use_stock");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Use_Stock;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Button : not null access Gtk_Button_Record) return Boolean
   is
      function Internal (Button : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_button_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Button)));
   end Get_Use_Underline;

   -----------
   -- Leave --
   -----------

   procedure Leave (Button : not null access Gtk_Button_Record) is
      procedure Internal (Button : System.Address);
      pragma Import (C, Internal, "gtk_button_leave");
   begin
      Internal (Get_Object (Button));
   end Leave;

   -------------
   -- Pressed --
   -------------

   procedure Pressed (Button : not null access Gtk_Button_Record) is
      procedure Internal (Button : System.Address);
      pragma Import (C, Internal, "gtk_button_pressed");
   begin
      Internal (Get_Object (Button));
   end Pressed;

   --------------
   -- Released --
   --------------

   procedure Released (Button : not null access Gtk_Button_Record) is
      procedure Internal (Button : System.Address);
      pragma Import (C, Internal, "gtk_button_released");
   begin
      Internal (Get_Object (Button));
   end Released;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Button : not null access Gtk_Button_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
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
      (Button         : not null access Gtk_Button_Record;
       Focus_On_Click : Boolean)
   is
      procedure Internal (Button : System.Address; Focus_On_Click : Integer);
      pragma Import (C, Internal, "gtk_button_set_focus_on_click");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
      (Button : not null access Gtk_Button_Record;
       Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Button : System.Address; Image : System.Address);
      pragma Import (C, Internal, "gtk_button_set_image");
   begin
      Internal (Get_Object (Button), Get_Object (Image));
   end Set_Image;

   ------------------------
   -- Set_Image_Position --
   ------------------------

   procedure Set_Image_Position
      (Button   : not null access Gtk_Button_Record;
       Position : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Button   : System.Address;
          Position : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_button_set_image_position");
   begin
      Internal (Get_Object (Button), Position);
   end Set_Image_Position;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Button : not null access Gtk_Button_Record;
       Label  : UTF8_String)
   is
      procedure Internal
         (Button : System.Address;
          Label  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_button_set_label");
      Tmp_Label : Interfaces.C.Strings.chars_ptr := New_String (Label);
   begin
      Internal (Get_Object (Button), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ----------------
   -- Set_Relief --
   ----------------

   procedure Set_Relief
      (Button   : not null access Gtk_Button_Record;
       Newstyle : Gtk.Enums.Gtk_Relief_Style)
   is
      procedure Internal
         (Button   : System.Address;
          Newstyle : Gtk.Enums.Gtk_Relief_Style);
      pragma Import (C, Internal, "gtk_button_set_relief");
   begin
      Internal (Get_Object (Button), Newstyle);
   end Set_Relief;

   -------------------
   -- Set_Use_Stock --
   -------------------

   procedure Set_Use_Stock
      (Button    : not null access Gtk_Button_Record;
       Use_Stock : Boolean)
   is
      procedure Internal (Button : System.Address; Use_Stock : Integer);
      pragma Import (C, Internal, "gtk_button_set_use_stock");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Stock));
   end Set_Use_Stock;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Button        : not null access Gtk_Button_Record;
       Use_Underline : Boolean)
   is
      procedure Internal (Button : System.Address; Use_Underline : Integer);
      pragma Import (C, Internal, "gtk_button_set_use_underline");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Button_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Action));
   end Get_Related_Action;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Action_Appearance;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Set_Related_Action;

   -------------------------------
   -- Set_Use_Action_Appearance --
   -------------------------------

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Button_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Appearance : Integer);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Button;
