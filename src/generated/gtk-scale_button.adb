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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Scale_Button is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scale_Button_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Button : out Gtk_Scale_Button;
       Size   : Gtk.Enums.Gtk_Icon_Size;
       Min    : Gdouble;
       Max    : Gdouble;
       Step   : Gdouble;
       Icons  : GNAT.Strings.String_List)
   is
   begin
      Button := new Gtk_Scale_Button_Record;
      Gtk.Scale_Button.Initialize (Button, Size, Min, Max, Step, Icons);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Button : not null access Gtk_Scale_Button_Record'Class;
       Size   : Gtk.Enums.Gtk_Icon_Size;
       Min    : Gdouble;
       Max    : Gdouble;
       Step   : Gdouble;
       Icons  : GNAT.Strings.String_List)
   is
      function Internal
         (Size  : Gtk.Enums.Gtk_Icon_Size;
          Min   : Gdouble;
          Max   : Gdouble;
          Step  : Gdouble;
          Icons : Interfaces.C.Strings.chars_ptr_array)
          return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_new");
      Tmp_Icons  : Interfaces.C.Strings.chars_ptr_array := From_String_List (Icons);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Size, Min, Max, Step, Tmp_Icons);
      GtkAda.Types.Free (Tmp_Icons);
      Set_Object (Button, Tmp_Return);
   end Initialize;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_adjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Adjustment));
   end Get_Adjustment;

   ----------------------
   -- Get_Minus_Button --
   ----------------------

   function Get_Minus_Button
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_minus_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Minus_Button;

   ---------------------
   -- Get_Plus_Button --
   ---------------------

   function Get_Plus_Button
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_plus_button");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Plus_Button;

   ---------------
   -- Get_Popup --
   ---------------

   function Get_Popup
      (Button : not null access Gtk_Scale_Button_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Button : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scale_button_get_popup");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Button)), Stub_Gtk_Widget));
   end Get_Popup;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Button : not null access Gtk_Scale_Button_Record) return Gdouble
   is
      function Internal (Button : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_scale_button_get_value");
   begin
      return Internal (Get_Object (Button));
   end Get_Value;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Button     : not null access Gtk_Scale_Button_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
      
   is
      procedure Internal
         (Button     : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_scale_button_set_adjustment");
   begin
      Internal (Get_Object (Button), Get_Object (Adjustment));
   end Set_Adjustment;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons
      (Button : not null access Gtk_Scale_Button_Record;
       Icons  : GNAT.Strings.String_List)
   is
      procedure Internal
         (Button : System.Address;
          Icons  : Interfaces.C.Strings.chars_ptr_array);
      pragma Import (C, Internal, "gtk_scale_button_set_icons");
      Tmp_Icons : Interfaces.C.Strings.chars_ptr_array := From_String_List (Icons);
   begin
      Internal (Get_Object (Button), Tmp_Icons);
      GtkAda.Types.Free (Tmp_Icons);
   end Set_Icons;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Button : not null access Gtk_Scale_Button_Record;
       Value  : Gdouble)
   is
      procedure Internal (Button : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_scale_button_set_value");
   begin
      Internal (Get_Object (Button), Value);
   end Set_Value;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Scale_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Scale_Button_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Scale_Button_Record)
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
      (Self : not null access Gtk_Scale_Button_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Get_Use_Action_Appearance;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Scale_Button_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Scale_Button_Record;
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
      (Self           : not null access Gtk_Scale_Button_Record;
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
      (Self   : not null access Gtk_Scale_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

end Gtk.Scale_Button;
