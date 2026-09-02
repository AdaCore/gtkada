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

package body Glib.File_Info is

   package Type_Conversion_Gfile_Info is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gfile_Info_Record);
   pragma Unreferenced (Type_Conversion_Gfile_Info);

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gfile_Info) is
   begin
      Self := new Gfile_Info_Record;
      Glib.File_Info.Initialize (Self);
   end G_New;

   --------------------
   -- Gfile_Info_New --
   --------------------

   function Gfile_Info_New return Gfile_Info is
      Self : constant Gfile_Info := new Gfile_Info_Record;
   begin
      Glib.File_Info.Initialize (Self);
      return Self;
   end Gfile_Info_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gfile_Info_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_file_info_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------
   -- Clear_Status --
   ------------------

   procedure Clear_Status (Self : not null access Gfile_Info_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_file_info_clear_status");
   begin
      Internal (Get_Object (Self));
   end Clear_Status;

   ---------------
   -- Copy_Into --
   ---------------

   procedure Copy_Into
      (Self      : not null access Gfile_Info_Record;
       Dest_Info : not null access Gfile_Info_Record'Class)
   is
      procedure Internal (Self : System.Address; Dest_Info : System.Address);
      pragma Import (C, Internal, "g_file_info_copy_into");
   begin
      Internal (Get_Object (Self), Get_Object (Dest_Info));
   end Copy_Into;

   ---------
   -- Dup --
   ---------

   function Dup (Self : not null access Gfile_Info_Record) return Gfile_Info is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_file_info_dup");
      Stub_Gfile_Info : Gfile_Info_Record;
   begin
      return Glib.File_Info.Gfile_Info (Get_User_Data (Internal (Get_Object (Self)), Stub_Gfile_Info));
   end Dup;

   -----------------------------
   -- Get_Attribute_As_String --
   -----------------------------

   function Get_Attribute_As_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_attribute_as_string");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Get_Attribute_As_String;

   ---------------------------
   -- Get_Attribute_Boolean --
   ---------------------------

   function Get_Attribute_Boolean
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_get_attribute_boolean");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Get_Attribute_Boolean;

   -------------------------------
   -- Get_Attribute_Byte_String --
   -------------------------------

   function Get_Attribute_Byte_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_attribute_byte_string");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_Attribute_Byte_String;

   -----------------------------
   -- Get_Attribute_File_Path --
   -----------------------------

   function Get_Attribute_File_Path
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_attribute_file_path");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_Attribute_File_Path;

   -------------------------
   -- Get_Attribute_Int32 --
   -------------------------

   function Get_Attribute_Int32
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Gint32
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gint32;
      pragma Import (C, Internal, "g_file_info_get_attribute_int32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gint32;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return;
   end Get_Attribute_Int32;

   -------------------------
   -- Get_Attribute_Int64 --
   -------------------------

   function Get_Attribute_Int64
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Gint64
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gint64;
      pragma Import (C, Internal, "g_file_info_get_attribute_int64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gint64;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return;
   end Get_Attribute_Int64;

   --------------------------
   -- Get_Attribute_Object --
   --------------------------

   function Get_Attribute_Object
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_file_info_get_attribute_object");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Stub_GObject  : Glib.Object.GObject_Record;
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Get_Attribute_Object;

   --------------------------
   -- Get_Attribute_String --
   --------------------------

   function Get_Attribute_String
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_attribute_string");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_Attribute_String;

   ---------------------------
   -- Get_Attribute_Stringv --
   ---------------------------

   function Get_Attribute_Stringv
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return GNAT.Strings.String_List
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_file_info_get_attribute_stringv");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return To_String_List (Tmp_Return.all);
   end Get_Attribute_Stringv;

   --------------------------
   -- Get_Attribute_Uint32 --
   --------------------------

   function Get_Attribute_Uint32
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Guint32
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Guint32;
      pragma Import (C, Internal, "g_file_info_get_attribute_uint32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Guint32;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return;
   end Get_Attribute_Uint32;

   --------------------------
   -- Get_Attribute_Uint64 --
   --------------------------

   function Get_Attribute_Uint64
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Guint64
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Guint64;
      pragma Import (C, Internal, "g_file_info_get_attribute_uint64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Guint64;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return;
   end Get_Attribute_Uint64;

   ----------------------
   -- Get_Content_Type --
   ----------------------

   function Get_Content_Type
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_content_type");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Content_Type;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Display_Name;

   -------------------
   -- Get_Edit_Name --
   -------------------

   function Get_Edit_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_edit_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Edit_Name;

   --------------
   -- Get_Etag --
   --------------

   function Get_Etag
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_etag");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Etag;

   -------------------
   -- Get_File_Type --
   -------------------

   function Get_File_Type
      (Self : not null access Gfile_Info_Record) return GFile_Type
   is
      function Internal (Self : System.Address) return GFile_Type;
      pragma Import (C, Internal, "g_file_info_get_file_type");
   begin
      return Internal (Get_Object (Self));
   end Get_File_Type;

   --------------
   -- Get_Icon --
   --------------

   function Get_Icon
      (Self : not null access Gfile_Info_Record) return Glib.G_Icon.G_Icon
   is
      function Internal (Self : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "g_file_info_get_icon");
   begin
      return Internal (Get_Object (Self));
   end Get_Icon;

   -------------------
   -- Get_Is_Backup --
   -------------------

   function Get_Is_Backup
      (Self : not null access Gfile_Info_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_get_is_backup");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Backup;

   -------------------
   -- Get_Is_Hidden --
   -------------------

   function Get_Is_Hidden
      (Self : not null access Gfile_Info_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_get_is_hidden");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Hidden;

   --------------------
   -- Get_Is_Symlink --
   --------------------

   function Get_Is_Symlink
      (Self : not null access Gfile_Info_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_get_is_symlink");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Is_Symlink;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
      (Self : not null access Gfile_Info_Record) return Glib.Gint64
   is
      function Internal (Self : System.Address) return Glib.Gint64;
      pragma Import (C, Internal, "g_file_info_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   --------------------
   -- Get_Sort_Order --
   --------------------

   function Get_Sort_Order
      (Self : not null access Gfile_Info_Record) return Gint32
   is
      function Internal (Self : System.Address) return Gint32;
      pragma Import (C, Internal, "g_file_info_get_sort_order");
   begin
      return Internal (Get_Object (Self));
   end Get_Sort_Order;

   -----------------------
   -- Get_Symbolic_Icon --
   -----------------------

   function Get_Symbolic_Icon
      (Self : not null access Gfile_Info_Record) return Glib.G_Icon.G_Icon
   is
      function Internal (Self : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "g_file_info_get_symbolic_icon");
   begin
      return Internal (Get_Object (Self));
   end Get_Symbolic_Icon;

   ------------------------
   -- Get_Symlink_Target --
   ------------------------

   function Get_Symlink_Target
      (Self : not null access Gfile_Info_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_file_info_get_symlink_target");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Symlink_Target;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_has_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return /= 0;
   end Has_Attribute;

   -------------------
   -- Has_Namespace --
   -------------------

   function Has_Namespace
      (Self       : not null access Gfile_Info_Record;
       Name_Space : UTF8_String) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Name_Space : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_file_info_has_namespace");
      Tmp_Name_Space : Gtkada.Types.Chars_Ptr := New_String (Name_Space);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name_Space);
      Free (Tmp_Name_Space);
      return Tmp_Return /= 0;
   end Has_Namespace;

   ---------------------
   -- List_Attributes --
   ---------------------

   function List_Attributes
      (Self       : not null access Gfile_Info_Record;
       Name_Space : UTF8_String := "") return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Name_Space : Gtkada.Types.Chars_Ptr) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_file_info_list_attributes");
      Tmp_Name_Space : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : chars_ptr_array_access;
   begin
      Tmp_Name_Space :=
        (if Name_Space = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Name_Space));
      Tmp_Return := Internal (Get_Object (Self), Tmp_Name_Space);
      Free (Tmp_Name_Space);
      return To_String_List_And_Free (Tmp_Return);
   end List_Attributes;

   ----------------------
   -- Remove_Attribute --
   ----------------------

   procedure Remove_Attribute
      (Self      : not null access Gfile_Info_Record;
       Attribute : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_remove_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute);
      Free (Tmp_Attribute);
   end Remove_Attribute;

   ---------------------------
   -- Set_Attribute_Boolean --
   ---------------------------

   procedure Set_Attribute_Boolean
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Boolean)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Glib.Gboolean);
      pragma Import (C, Internal, "g_file_info_set_attribute_boolean");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Boolean'Pos (Attr_Value));
      Free (Tmp_Attribute);
   end Set_Attribute_Boolean;

   -------------------------------
   -- Set_Attribute_Byte_String --
   -------------------------------

   procedure Set_Attribute_Byte_String
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_attribute_byte_string");
      Tmp_Attribute  : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Attr_Value : Gtkada.Types.Chars_Ptr := New_String (Attr_Value);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Tmp_Attr_Value);
      Free (Tmp_Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Byte_String;

   -----------------------------
   -- Set_Attribute_File_Path --
   -----------------------------

   procedure Set_Attribute_File_Path
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_attribute_file_path");
      Tmp_Attribute  : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Attr_Value : Gtkada.Types.Chars_Ptr := New_String (Attr_Value);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Tmp_Attr_Value);
      Free (Tmp_Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_File_Path;

   -------------------------
   -- Set_Attribute_Int32 --
   -------------------------

   procedure Set_Attribute_Int32
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Gint32)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gint32);
      pragma Import (C, Internal, "g_file_info_set_attribute_int32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Int32;

   -------------------------
   -- Set_Attribute_Int64 --
   -------------------------

   procedure Set_Attribute_Int64
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Gint64)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gint64);
      pragma Import (C, Internal, "g_file_info_set_attribute_int64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Int64;

   --------------------------
   -- Set_Attribute_Object --
   --------------------------

   procedure Set_Attribute_Object
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : not null access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : System.Address);
      pragma Import (C, Internal, "g_file_info_set_attribute_object");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Get_Object (Attr_Value));
      Free (Tmp_Attribute);
   end Set_Attribute_Object;

   --------------------------
   -- Set_Attribute_String --
   --------------------------

   procedure Set_Attribute_String
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_attribute_string");
      Tmp_Attribute  : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Attr_Value : Gtkada.Types.Chars_Ptr := New_String (Attr_Value);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Tmp_Attr_Value);
      Free (Tmp_Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_String;

   ---------------------------
   -- Set_Attribute_Stringv --
   ---------------------------

   procedure Set_Attribute_Stringv
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "g_file_info_set_attribute_stringv");
      Tmp_Attribute  : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Attr_Value : Gtkada.Types.chars_ptr_array := From_String_List (Attr_Value);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Tmp_Attr_Value);
      Gtkada.Types.Free (Tmp_Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Stringv;

   --------------------------
   -- Set_Attribute_Uint32 --
   --------------------------

   procedure Set_Attribute_Uint32
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Guint32)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Guint32);
      pragma Import (C, Internal, "g_file_info_set_attribute_uint32");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Uint32;

   --------------------------
   -- Set_Attribute_Uint64 --
   --------------------------

   procedure Set_Attribute_Uint64
      (Self       : not null access Gfile_Info_Record;
       Attribute  : UTF8_String;
       Attr_Value : Guint64)
   is
      procedure Internal
         (Self       : System.Address;
          Attribute  : Gtkada.Types.Chars_Ptr;
          Attr_Value : Guint64);
      pragma Import (C, Internal, "g_file_info_set_attribute_uint64");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Tmp_Attribute, Attr_Value);
      Free (Tmp_Attribute);
   end Set_Attribute_Uint64;

   ----------------------
   -- Set_Content_Type --
   ----------------------

   procedure Set_Content_Type
      (Self         : not null access Gfile_Info_Record;
       Content_Type : UTF8_String)
   is
      procedure Internal
         (Self         : System.Address;
          Content_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_content_type");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
   begin
      Internal (Get_Object (Self), Tmp_Content_Type);
      Free (Tmp_Content_Type);
   end Set_Content_Type;

   ----------------------
   -- Set_Display_Name --
   ----------------------

   procedure Set_Display_Name
      (Self         : not null access Gfile_Info_Record;
       Display_Name : UTF8_String)
   is
      procedure Internal
         (Self         : System.Address;
          Display_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_display_name");
      Tmp_Display_Name : Gtkada.Types.Chars_Ptr := New_String (Display_Name);
   begin
      Internal (Get_Object (Self), Tmp_Display_Name);
      Free (Tmp_Display_Name);
   end Set_Display_Name;

   -------------------
   -- Set_Edit_Name --
   -------------------

   procedure Set_Edit_Name
      (Self      : not null access Gfile_Info_Record;
       Edit_Name : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Edit_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_edit_name");
      Tmp_Edit_Name : Gtkada.Types.Chars_Ptr := New_String (Edit_Name);
   begin
      Internal (Get_Object (Self), Tmp_Edit_Name);
      Free (Tmp_Edit_Name);
   end Set_Edit_Name;

   -------------------
   -- Set_File_Type --
   -------------------

   procedure Set_File_Type
      (Self     : not null access Gfile_Info_Record;
       The_Type : GFile_Type)
   is
      procedure Internal (Self : System.Address; The_Type : GFile_Type);
      pragma Import (C, Internal, "g_file_info_set_file_type");
   begin
      Internal (Get_Object (Self), The_Type);
   end Set_File_Type;

   --------------
   -- Set_Icon --
   --------------

   procedure Set_Icon
      (Self : not null access Gfile_Info_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "g_file_info_set_icon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_Icon;

   -------------------
   -- Set_Is_Hidden --
   -------------------

   procedure Set_Is_Hidden
      (Self      : not null access Gfile_Info_Record;
       Is_Hidden : Boolean)
   is
      procedure Internal (Self : System.Address; Is_Hidden : Glib.Gboolean);
      pragma Import (C, Internal, "g_file_info_set_is_hidden");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Is_Hidden));
   end Set_Is_Hidden;

   --------------------
   -- Set_Is_Symlink --
   --------------------

   procedure Set_Is_Symlink
      (Self       : not null access Gfile_Info_Record;
       Is_Symlink : Boolean)
   is
      procedure Internal (Self : System.Address; Is_Symlink : Glib.Gboolean);
      pragma Import (C, Internal, "g_file_info_set_is_symlink");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Is_Symlink));
   end Set_Is_Symlink;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Self : not null access Gfile_Info_Record;
       Name : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Self), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Self : not null access Gfile_Info_Record;
       Size : Glib.Gint64)
   is
      procedure Internal (Self : System.Address; Size : Glib.Gint64);
      pragma Import (C, Internal, "g_file_info_set_size");
   begin
      Internal (Get_Object (Self), Size);
   end Set_Size;

   --------------------
   -- Set_Sort_Order --
   --------------------

   procedure Set_Sort_Order
      (Self       : not null access Gfile_Info_Record;
       Sort_Order : Gint32)
   is
      procedure Internal (Self : System.Address; Sort_Order : Gint32);
      pragma Import (C, Internal, "g_file_info_set_sort_order");
   begin
      Internal (Get_Object (Self), Sort_Order);
   end Set_Sort_Order;

   -----------------------
   -- Set_Symbolic_Icon --
   -----------------------

   procedure Set_Symbolic_Icon
      (Self : not null access Gfile_Info_Record;
       Icon : Glib.G_Icon.G_Icon)
   is
      procedure Internal (Self : System.Address; Icon : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "g_file_info_set_symbolic_icon");
   begin
      Internal (Get_Object (Self), Icon);
   end Set_Symbolic_Icon;

   ------------------------
   -- Set_Symlink_Target --
   ------------------------

   procedure Set_Symlink_Target
      (Self           : not null access Gfile_Info_Record;
       Symlink_Target : UTF8_String)
   is
      procedure Internal
         (Self           : System.Address;
          Symlink_Target : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_file_info_set_symlink_target");
      Tmp_Symlink_Target : Gtkada.Types.Chars_Ptr := New_String (Symlink_Target);
   begin
      Internal (Get_Object (Self), Tmp_Symlink_Target);
      Free (Tmp_Symlink_Target);
   end Set_Symlink_Target;

   --------------------------
   -- Unset_Attribute_Mask --
   --------------------------

   procedure Unset_Attribute_Mask (Self : not null access Gfile_Info_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_file_info_unset_attribute_mask");
   begin
      Internal (Get_Object (Self));
   end Unset_Attribute_Mask;

end Glib.File_Info;
