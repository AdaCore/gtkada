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

package body Gtk.Style_Properties is

   package Type_Conversion_Gtk_Style_Properties is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Style_Properties_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Style_Properties);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Style_Properties) is
   begin
      Self := new Gtk_Style_Properties_Record;
      Gtk.Style_Properties.Initialize (Self);
   end Gtk_New;

   ------------------------------
   -- Gtk_Style_Properties_New --
   ------------------------------

   function Gtk_Style_Properties_New return Gtk_Style_Properties is
      Self : constant Gtk_Style_Properties := new Gtk_Style_Properties_Record;
   begin
      Gtk.Style_Properties.Initialize (Self);
      return Self;
   end Gtk_Style_Properties_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Style_Properties_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_properties_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Gtk_Style_Properties_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_style_properties_clear");
   begin
      Internal (Get_Object (Self));
   end Clear;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue;
       Exists   : out Boolean)
   is
      function Internal
         (Self      : System.Address;
          Property  : Gtkada.Types.Chars_Ptr;
          State     : Gtk.Enums.Gtk_State_Flags;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_properties_get_property");
      Acc_Value    : aliased Glib.Values.GValue;
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Property, State, Acc_Value'Access);
      Free (Tmp_Property);
      Value := Acc_Value;
      Exists := Tmp_Return /= 0;
   end Get_Property;

   ------------------
   -- Lookup_Color --
   ------------------

   function Lookup_Color
      (Self : not null access Gtk_Style_Properties_Record;
       Name : UTF8_String) return Gtk.Symbolic_Color.Gtk_Symbolic_Color
   is
      function Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_style_properties_lookup_color");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
      return From_Object (Tmp_Return);
   end Lookup_Color;

   ---------------
   -- Map_Color --
   ---------------

   procedure Map_Color
      (Self  : not null access Gtk_Style_Properties_Record;
       Name  : UTF8_String;
       Color : Gtk.Symbolic_Color.Gtk_Symbolic_Color)
   is
      procedure Internal
         (Self  : System.Address;
          Name  : Gtkada.Types.Chars_Ptr;
          Color : System.Address);
      pragma Import (C, Internal, "gtk_style_properties_map_color");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name, Get_Object (Color));
      Free (Tmp_Name);
   end Map_Color;

   -----------
   -- Merge --
   -----------

   procedure Merge
      (Self           : not null access Gtk_Style_Properties_Record;
       Props_To_Merge : not null access Gtk_Style_Properties_Record'Class;
       Replace        : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Props_To_Merge : System.Address;
          Replace        : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_style_properties_merge");
   begin
      Internal (Get_Object (Self), Get_Object (Props_To_Merge), Boolean'Pos (Replace));
   end Merge;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          State    : Gtk.Enums.Gtk_State_Flags;
          Value    : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_style_properties_set_property");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Property, State, Value);
      Free (Tmp_Property);
   end Set_Property;

   --------------------
   -- Unset_Property --
   --------------------

   procedure Unset_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          State    : Gtk.Enums.Gtk_State_Flags);
      pragma Import (C, Internal, "gtk_style_properties_unset_property");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Property, State);
      Free (Tmp_Property);
   end Unset_Property;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Style_Properties_Record;
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

end Gtk.Style_Properties;
