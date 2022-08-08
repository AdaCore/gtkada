------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Settings is

   function Get_Settings
     (Widget   : not null access Gtk_Widget_Record'Class)
   return Gtk_Settings
   is
      function Internal (W : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_settings");
      Stub : Gtk_Settings_Record;
   begin
      return Gtk_Settings (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Settings;

   procedure Set_Property_Value
     (Settings : not null access Gtk_Settings_Record;
      Name     : String;
      Value    : GValue;
      Origin   : String)
   is
      type Property_Value is record
         Origin : Gtkada.Types.Chars_Ptr;
         Value  : GValue;
      end record;
      pragma Convention (C, Property_Value);

      procedure Internal
        (Settings : System.Address;
         Name     : String;
         Svalue   : System.Address);
      pragma Import (C, Internal, "gtk_settings_set_property_value");

      Val : aliased Property_Value :=
        (Origin => New_String (Origin),
         Value  => Value);
   begin
      Internal (Get_Object (Settings), Name & ASCII.NUL, Val'Address);
      g_free (Val.Origin);
   end Set_Property_Value;

   package Type_Conversion_Gtk_Settings is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Settings_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Settings);

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self : not null access Gtk_Settings_Record;
       Name : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_settings_reset_property");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Reset_Property;

   -------------------------
   -- Set_Double_Property --
   -------------------------

   procedure Set_Double_Property
      (Self     : not null access Gtk_Settings_Record;
       Name     : UTF8_String;
       V_Double : Gdouble;
       Origin   : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Name     : Gtkada.Types.Chars_Ptr;
          V_Double : Gdouble;
          Origin   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_settings_set_double_property");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Origin : Gtkada.Types.Chars_Ptr := New_String (Origin);
   begin
      Internal (Get_Object (Self), Tmp_Name, V_Double, Tmp_Origin);
      Free (Tmp_Origin);
      Free (Tmp_Name);
   end Set_Double_Property;

   -----------------------
   -- Set_Long_Property --
   -----------------------

   procedure Set_Long_Property
      (Self   : not null access Gtk_Settings_Record;
       Name   : UTF8_String;
       V_Long : Glong;
       Origin : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Name   : Gtkada.Types.Chars_Ptr;
          V_Long : Glong;
          Origin : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_settings_set_long_property");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Origin : Gtkada.Types.Chars_Ptr := New_String (Origin);
   begin
      Internal (Get_Object (Self), Tmp_Name, V_Long, Tmp_Origin);
      Free (Tmp_Origin);
      Free (Tmp_Name);
   end Set_Long_Property;

   -------------------------
   -- Set_String_Property --
   -------------------------

   procedure Set_String_Property
      (Self     : not null access Gtk_Settings_Record;
       Name     : UTF8_String;
       V_String : UTF8_String;
       Origin   : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Name     : Gtkada.Types.Chars_Ptr;
          V_String : Gtkada.Types.Chars_Ptr;
          Origin   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_settings_set_string_property");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_V_String : Gtkada.Types.Chars_Ptr := New_String (V_String);
      Tmp_Origin   : Gtkada.Types.Chars_Ptr := New_String (Origin);
   begin
      Internal (Get_Object (Self), Tmp_Name, Tmp_V_String, Tmp_Origin);
      Free (Tmp_Origin);
      Free (Tmp_V_String);
      Free (Tmp_Name);
   end Set_String_Property;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Settings_Record;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean)
   is
      function Internal
         (Self      : System.Address;
          Path      : Gtk.Widget.Gtk_Widget_Path;
          State     : Gtk.Enums.Gtk_State_Flags;
          Acc_Pspec : access Glib.Param_Spec;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_provider_get_style_property");
      Acc_Pspec  : aliased Glib.Param_Spec := Pspec;
      Acc_Value  : aliased Glib.Values.GValue;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Path, State, Acc_Pspec'Access, Acc_Value'Access);
      Pspec := Acc_Pspec;
      Value := Acc_Value;
      Found := Tmp_Return /= 0;
   end Get_Style_Property;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Settings is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_default");
      Stub_Gtk_Settings : Gtk_Settings_Record;
   begin
      return Gtk.Settings.Gtk_Settings (Get_User_Data (Internal, Stub_Gtk_Settings));
   end Get_Default;

   --------------------
   -- Get_For_Screen --
   --------------------

   function Get_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Settings
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_settings_get_for_screen");
      Stub_Gtk_Settings : Gtk_Settings_Record;
   begin
      return Gtk.Settings.Gtk_Settings (Get_User_Data (Internal (Get_Object (Screen)), Stub_Gtk_Settings));
   end Get_For_Screen;

   ----------------------
   -- Install_Property --
   ----------------------

   procedure Install_Property (Pspec : in out Glib.Param_Spec) is
      procedure Internal (Pspec : in out Glib.Param_Spec);
      pragma Import (C, Internal, "gtk_settings_install_property");
   begin
      Internal (Pspec);
   end Install_Property;

end Gtk.Settings;
