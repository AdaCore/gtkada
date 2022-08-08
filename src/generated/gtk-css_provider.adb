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
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Css_Provider is

   package Type_Conversion_Gtk_Css_Provider is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Css_Provider_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Css_Provider);

   --------------------------
   -- Gtk_Css_Provider_New --
   --------------------------

   function Gtk_Css_Provider_New return Gtk_Css_Provider is
      Self : constant Gtk_Css_Provider := new Gtk_Css_Provider_Record;
   begin
      Gtk.Css_Provider.Initialize (Self);
      return Self;
   end Gtk_Css_Provider_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Css_Provider) is
   begin
      Self := new Gtk_Css_Provider_Record;
      Gtk.Css_Provider.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Css_Provider_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_css_provider_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------
   -- Load_From_Data --
   --------------------

   function Load_From_Data
      (Self  : not null access Gtk_Css_Provider_Record;
       Data  : UTF8_String;
       Error : access Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Data      : Gtkada.Types.Chars_Ptr;
          Length    : Gssize;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_css_provider_load_from_data");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Data   : Gtkada.Types.Chars_Ptr := New_String (Data);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Data, -1, Acc_Error'Access);
      Free (Tmp_Data);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return /= 0;
   end Load_From_Data;

   --------------------
   -- Load_From_Path --
   --------------------

   function Load_From_Path
      (Self  : not null access Gtk_Css_Provider_Record;
       Path  : UTF8_String;
       Error : access Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Path      : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_css_provider_load_from_path");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Path, Acc_Error'Access);
      Free (Tmp_Path);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return /= 0;
   end Load_From_Path;

   ------------------------
   -- Load_From_Resource --
   ------------------------

   procedure Load_From_Resource
      (Self          : not null access Gtk_Css_Provider_Record;
       Resource_Path : UTF8_String)
   is
      procedure Internal
         (Self          : System.Address;
          Resource_Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_css_provider_load_from_resource");
      Tmp_Resource_Path : Gtkada.Types.Chars_Ptr := New_String (Resource_Path);
   begin
      Internal (Get_Object (Self), Tmp_Resource_Path);
      Free (Tmp_Resource_Path);
   end Load_From_Resource;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Self : not null access Gtk_Css_Provider_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_css_provider_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Css_Provider_Record;
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

   function Get_Default return Gtk_Css_Provider is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_css_provider_get_default");
      Stub_Gtk_Css_Provider : Gtk_Css_Provider_Record;
   begin
      return Gtk.Css_Provider.Gtk_Css_Provider (Get_User_Data (Internal, Stub_Gtk_Css_Provider));
   end Get_Default;

   ---------------
   -- Get_Named --
   ---------------

   function Get_Named
      (Name    : UTF8_String;
       Variant : UTF8_String := "") return Gtk_Css_Provider
   is
      function Internal
         (Name    : Gtkada.Types.Chars_Ptr;
          Variant : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_css_provider_get_named");
      Tmp_Name              : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Variant           : Gtkada.Types.Chars_Ptr;
      Stub_Gtk_Css_Provider : Gtk_Css_Provider_Record;
      Tmp_Return            : System.Address;
   begin
      if Variant = "" then
         Tmp_Variant := Gtkada.Types.Null_Ptr;
      else
         Tmp_Variant := New_String (Variant);
      end if;
      Tmp_Return := Internal (Tmp_Name, Tmp_Variant);
      Free (Tmp_Variant);
      Free (Tmp_Name);
      return Gtk.Css_Provider.Gtk_Css_Provider (Get_User_Data (Tmp_Return, Stub_Gtk_Css_Provider));
   end Get_Named;

end Gtk.Css_Provider;
