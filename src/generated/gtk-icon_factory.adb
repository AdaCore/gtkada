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

package body Gtk.Icon_Factory is

   function Get_Icon_Factory
     (Self : Gtk_Style_Provider;
      Path : Gtk.Widget.Gtk_Widget_Path)
   return Gtk.Icon_Factory.Gtk_Icon_Factory
   is
      function Internal
        (Self : Gtk_Style_Provider;
         Path : Gtk.Widget.Gtk_Widget_Path) return System.Address;
      pragma Import (C, Internal, "gtk_style_provider_get_icon_factory");
      Stub_Gtk_Icon_Factory : Gtk.Icon_Factory.Gtk_Icon_Factory_Record;
   begin
      return Gtk.Icon_Factory.Gtk_Icon_Factory (Get_User_Data (Internal (Self, Path), Stub_Gtk_Icon_Factory));
   end Get_Icon_Factory;

   package Type_Conversion_Gtk_Icon_Factory is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Factory_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Factory);

   --------------------------
   -- Gtk_Icon_Factory_New --
   --------------------------

   function Gtk_Icon_Factory_New return Gtk_Icon_Factory is
      Self : constant Gtk_Icon_Factory := new Gtk_Icon_Factory_Record;
   begin
      Gtk.Icon_Factory.Initialize (Self);
      return Self;
   end Gtk_Icon_Factory_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Icon_Factory) is
   begin
      Self := new Gtk_Icon_Factory_Record;
      Gtk.Icon_Factory.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Icon_Factory_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_factory_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
      (Self     : not null access Gtk_Icon_Factory_Record;
       Stock_Id : UTF8_String;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set)
   is
      procedure Internal
         (Self     : System.Address;
          Stock_Id : Gtkada.Types.Chars_Ptr;
          Icon_Set : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_add");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Self), Tmp_Stock_Id, Get_Object (Icon_Set));
      Free (Tmp_Stock_Id);
   end Add;

   -----------------
   -- Add_Default --
   -----------------

   procedure Add_Default (Self : not null access Gtk_Icon_Factory_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_add_default");
   begin
      Internal (Get_Object (Self));
   end Add_Default;

   ------------
   -- Lookup --
   ------------

   function Lookup
      (Self     : not null access Gtk_Icon_Factory_Record;
       Stock_Id : UTF8_String) return Gtk.Icon_Set.Gtk_Icon_Set
   is
      function Internal
         (Self     : System.Address;
          Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_icon_factory_lookup");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      return From_Object (Tmp_Return);
   end Lookup;

   --------------------
   -- Remove_Default --
   --------------------

   procedure Remove_Default (Self : not null access Gtk_Icon_Factory_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_icon_factory_remove_default");
   begin
      Internal (Get_Object (Self));
   end Remove_Default;

   ----------------------
   -- Icon_Size_Lookup --
   ----------------------

   procedure Icon_Size_Lookup
      (Size   : Gtk.Enums.Gtk_Icon_Size;
       Width  : out Glib.Gint;
       Height : out Glib.Gint;
       Result : out Boolean)
   is
      function Internal
         (Size       : Gtk.Enums.Gtk_Icon_Size;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_size_lookup");
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Size, Acc_Width'Access, Acc_Height'Access);
      Width := Acc_Width;
      Height := Acc_Height;
      Result := Tmp_Return /= 0;
   end Icon_Size_Lookup;

   -----------------------------------
   -- Icon_Size_Lookup_For_Settings --
   -----------------------------------

   procedure Icon_Size_Lookup_For_Settings
      (Settings : not null access Gtk.Settings.Gtk_Settings_Record'Class;
       Size     : Gtk.Enums.Gtk_Icon_Size;
       Width    : out Glib.Gint;
       Height   : out Glib.Gint;
       Result   : out Boolean)
   is
      function Internal
         (Settings   : System.Address;
          Size       : Gtk.Enums.Gtk_Icon_Size;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_size_lookup_for_settings");
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Settings), Size, Acc_Width'Access, Acc_Height'Access);
      Width := Acc_Width;
      Height := Acc_Height;
      Result := Tmp_Return /= 0;
   end Icon_Size_Lookup_For_Settings;

   ------------------------
   -- Icon_Size_Register --
   ------------------------

   function Icon_Size_Register
      (Name   : UTF8_String;
       Width  : Glib.Gint;
       Height : Glib.Gint) return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Name   : Gtkada.Types.Chars_Ptr;
          Width  : Glib.Gint;
          Height : Glib.Gint) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_icon_size_register");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Gtk.Enums.Gtk_Icon_Size;
   begin
      Tmp_Return := Internal (Tmp_Name, Width, Height);
      Free (Tmp_Name);
      return Tmp_Return;
   end Icon_Size_Register;

   ------------------------------
   -- Icon_Size_Register_Alias --
   ------------------------------

   procedure Icon_Size_Register_Alias
      (Alias  : UTF8_String;
       Target : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Alias  : Gtkada.Types.Chars_Ptr;
          Target : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_icon_size_register_alias");
      Tmp_Alias : Gtkada.Types.Chars_Ptr := New_String (Alias);
   begin
      Internal (Tmp_Alias, Target);
      Free (Tmp_Alias);
   end Icon_Size_Register_Alias;

   --------------------
   -- Lookup_Default --
   --------------------

   function Lookup_Default
      (Stock_Id : UTF8_String) return Gtk.Icon_Set.Gtk_Icon_Set
   is
      function Internal
         (Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_icon_factory_lookup_default");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      return From_Object (Tmp_Return);
   end Lookup_Default;

end Gtk.Icon_Factory;
