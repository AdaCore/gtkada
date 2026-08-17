------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.File_Filter is

   package Type_Conversion_Gtk_File_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_File_Filter);

   -------------------------
   -- Gtk_File_Filter_New --
   -------------------------

   function Gtk_File_Filter_New return Gtk_File_Filter is
      Self : constant Gtk_File_Filter := new Gtk_File_Filter_Record;
   begin
      Gtk.File_Filter.Initialize (Self);
      return Self;
   end Gtk_File_Filter_New;

   ---------------------------------------
   -- Gtk_File_Filter_New_From_Gvariant --
   ---------------------------------------

   function Gtk_File_Filter_New_From_Gvariant
      (Variant : Glib.Variant.Gvariant) return Gtk_File_Filter
   is
      Self : constant Gtk_File_Filter := new Gtk_File_Filter_Record;
   begin
      Gtk.File_Filter.Initialize_From_Gvariant (Self, Variant);
      return Self;
   end Gtk_File_Filter_New_From_Gvariant;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_File_Filter) is
   begin
      Self := new Gtk_File_Filter_Record;
      Gtk.File_Filter.Initialize (Self);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_From_Gvariant --
   ---------------------------

   procedure Gtk_New_From_Gvariant
      (Self    : out Gtk_File_Filter;
       Variant : Glib.Variant.Gvariant)
   is
   begin
      Self := new Gtk_File_Filter_Record;
      Gtk.File_Filter.Initialize_From_Gvariant (Self, Variant);
   end Gtk_New_From_Gvariant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_File_Filter_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_From_Gvariant --
   ------------------------------

   procedure Initialize_From_Gvariant
      (Self    : not null access Gtk_File_Filter_Record'Class;
       Variant : Glib.Variant.Gvariant)
   is
      function Internal (Variant : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_new_from_gvariant");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Variant)));
      end if;
   end Initialize_From_Gvariant;

   -------------------
   -- Add_Mime_Type --
   -------------------

   procedure Add_Mime_Type
      (Self      : not null access Gtk_File_Filter_Record;
       Mime_Type : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Mime_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_add_mime_type");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
   begin
      Internal (Get_Object (Self), Tmp_Mime_Type);
      Free (Tmp_Mime_Type);
   end Add_Mime_Type;

   --------------------
   -- Add_Mime_Types --
   --------------------

   procedure Add_Mime_Types
      (Self       : not null access Gtk_File_Filter_Record;
       Mime_Types : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self       : System.Address;
          Mime_Types : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_file_filter_add_mime_types");
      Tmp_Mime_Types : Gtkada.Types.chars_ptr_array := From_String_List (Mime_Types);
   begin
      Internal (Get_Object (Self), Tmp_Mime_Types);
      Gtkada.Types.Free (Tmp_Mime_Types);
   end Add_Mime_Types;

   -----------------
   -- Add_Pattern --
   -----------------

   procedure Add_Pattern
      (Self    : not null access Gtk_File_Filter_Record;
       Pattern : UTF8_String)
   is
      procedure Internal
         (Self    : System.Address;
          Pattern : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_add_pattern");
      Tmp_Pattern : Gtkada.Types.Chars_Ptr := New_String (Pattern);
   begin
      Internal (Get_Object (Self), Tmp_Pattern);
      Free (Tmp_Pattern);
   end Add_Pattern;

   ------------------------
   -- Add_Pixbuf_Formats --
   ------------------------

   procedure Add_Pixbuf_Formats
      (Self : not null access Gtk_File_Filter_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_file_filter_add_pixbuf_formats");
   begin
      Internal (Get_Object (Self));
   end Add_Pixbuf_Formats;

   ----------------
   -- Add_Suffix --
   ----------------

   procedure Add_Suffix
      (Self   : not null access Gtk_File_Filter_Record;
       Suffix : UTF8_String)
   is
      procedure Internal
         (Self   : System.Address;
          Suffix : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_add_suffix");
      Tmp_Suffix : Gtkada.Types.Chars_Ptr := New_String (Suffix);
   begin
      Internal (Get_Object (Self), Tmp_Suffix);
      Free (Tmp_Suffix);
   end Add_Suffix;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Self : not null access Gtk_File_Filter_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_file_filter_get_attributes");
   begin
      return To_String_List (Internal (Get_Object (Self)).all);
   end Get_Attributes;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gtk_File_Filter_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_filter_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Self : not null access Gtk_File_Filter_Record;
       Name : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_file_filter_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   -----------------
   -- To_Gvariant --
   -----------------

   function To_Gvariant
      (Self : not null access Gtk_File_Filter_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_file_filter_to_gvariant");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end To_Gvariant;

end Gtk.File_Filter;
