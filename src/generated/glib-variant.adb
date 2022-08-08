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
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Glib.Variant is

   function From_Object_Free
     (B : access Gvariant'Class) return Gvariant
   is
      Result : constant Gvariant := Gvariant (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gvariant is
      S : Gvariant;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gvariant_Type; Type_String : UTF8_String) is
      function Internal
         (Type_String : Gtkada.Types.Chars_Ptr) return Gvariant_Type;
      pragma Import (C, Internal, "g_variant_type_new");
      Tmp_Type_String : Gtkada.Types.Chars_Ptr := New_String (Type_String);
      Tmp_Return      : Gvariant_Type;
   begin
      Tmp_Return := Internal (Tmp_Type_String);
      Free (Tmp_Type_String);
      Self := Tmp_Return;
   end G_New;

   -------------------
   -- G_New_Boolean --
   -------------------

   procedure G_New_Boolean (Self : out Gvariant; Value : Boolean) is
      function Internal (Value : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "g_variant_new_boolean");
   begin
      Self.Set_Object (Internal (Boolean'Pos (Value)));
   end G_New_Boolean;

   ----------------
   -- G_New_Byte --
   ----------------

   procedure G_New_Byte (Self : out Gvariant; Value : Guchar) is
      function Internal (Value : Guchar) return System.Address;
      pragma Import (C, Internal, "g_variant_new_byte");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Byte;

   ----------------------
   -- G_New_Bytestring --
   ----------------------

   procedure G_New_Bytestring (Self : out Gvariant; String : Gint_Array) is
      function Internal (String : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_bytestring");
   begin
      Self.Set_Object (Internal (String (String'First)'Address));
   end G_New_Bytestring;

   ----------------------------
   -- G_New_Bytestring_Array --
   ----------------------------

   procedure G_New_Bytestring_Array
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize)
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_bytestring_array");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
   end G_New_Bytestring_Array;

   ----------------------
   -- G_New_Dict_Entry --
   ----------------------

   procedure G_New_Dict_Entry
      (Self  : out Gvariant;
       Key   : Gvariant;
       Value : Gvariant)
   is
      function Internal
         (Key   : System.Address;
          Value : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_dict_entry");
   begin
      Self.Set_Object (Internal (Get_Object (Key), Get_Object (Value)));
   end G_New_Dict_Entry;

   ----------------------
   -- G_New_Dict_Entry --
   ----------------------

   procedure G_New_Dict_Entry
      (Self  : out Gvariant_Type;
       Key   : Gvariant_Type;
       Value : Gvariant_Type)
   is
      function Internal
         (Key   : Gvariant_Type;
          Value : Gvariant_Type) return Gvariant_Type;
      pragma Import (C, Internal, "g_variant_type_new_dict_entry");
   begin
      Self := Internal (Key, Value);
   end G_New_Dict_Entry;

   ------------------
   -- G_New_Double --
   ------------------

   procedure G_New_Double (Self : out Gvariant; Value : Gdouble) is
      function Internal (Value : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_variant_new_double");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Double;

   ------------------
   -- G_New_Handle --
   ------------------

   procedure G_New_Handle (Self : out Gvariant; Value : Gint32) is
      function Internal (Value : Gint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_handle");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Handle;

   -----------------
   -- G_New_Int16 --
   -----------------

   procedure G_New_Int16 (Self : out Gvariant; Value : Gint16) is
      function Internal (Value : Gint16) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int16");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Int16;

   -----------------
   -- G_New_Int32 --
   -----------------

   procedure G_New_Int32 (Self : out Gvariant; Value : Gint32) is
      function Internal (Value : Gint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int32");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Int32;

   -----------------
   -- G_New_Int64 --
   -----------------

   procedure G_New_Int64 (Self : out Gvariant; Value : Gint64) is
      function Internal (Value : Gint64) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int64");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Int64;

   -----------------------
   -- G_New_Object_Path --
   -----------------------

   procedure G_New_Object_Path
      (Self        : out Gvariant;
       Object_Path : UTF8_String)
   is
      function Internal
         (Object_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_object_path");
      Tmp_Object_Path : Gtkada.Types.Chars_Ptr := New_String (Object_Path);
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Object_Path);
      Free (Tmp_Object_Path);
      Self.Set_Object (Tmp_Return);
   end G_New_Object_Path;

   ----------------
   -- G_New_Objv --
   ----------------

   procedure G_New_Objv
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize)
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_objv");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
   end G_New_Objv;

   ---------------------
   -- G_New_Signature --
   ---------------------

   procedure G_New_Signature (Self : out Gvariant; Signature : UTF8_String) is
      function Internal
         (Signature : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_signature");
      Tmp_Signature : Gtkada.Types.Chars_Ptr := New_String (Signature);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Signature);
      Free (Tmp_Signature);
      Self.Set_Object (Tmp_Return);
   end G_New_Signature;

   ------------------
   -- G_New_String --
   ------------------

   procedure G_New_String (Self : out Gvariant; String : UTF8_String) is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      Self.Set_Object (Tmp_Return);
   end G_New_String;

   ----------------
   -- G_New_Strv --
   ----------------

   procedure G_New_Strv
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize)
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_strv");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
   end G_New_Strv;

   -----------------------
   -- G_New_Take_String --
   -----------------------

   procedure G_New_Take_String (Self : out Gvariant; String : UTF8_String) is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_take_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      Self.Set_Object (Tmp_Return);
   end G_New_Take_String;

   ------------------
   -- G_New_Uint16 --
   ------------------

   procedure G_New_Uint16 (Self : out Gvariant; Value : Guint16) is
      function Internal (Value : Guint16) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint16");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Uint16;

   ------------------
   -- G_New_Uint32 --
   ------------------

   procedure G_New_Uint32 (Self : out Gvariant; Value : Guint32) is
      function Internal (Value : Guint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint32");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Uint32;

   ------------------
   -- G_New_Uint64 --
   ------------------

   procedure G_New_Uint64 (Self : out Gvariant; Value : Guint64) is
      function Internal (Value : Guint64) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint64");
   begin
      Self.Set_Object (Internal (Value));
   end G_New_Uint64;

   -------------------
   -- G_New_Variant --
   -------------------

   procedure G_New_Variant (Self : out Gvariant; Value : Gvariant) is
      function Internal (Value : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_variant");
   begin
      Self.Set_Object (Internal (Get_Object (Value)));
   end G_New_Variant;

   --------------------------
   -- Gvariant_New_Boolean --
   --------------------------

   function Gvariant_New_Boolean (Value : Boolean) return Gvariant is
      function Internal (Value : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "g_variant_new_boolean");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Boolean'Pos (Value)));
      return Self;
   end Gvariant_New_Boolean;

   -----------------------
   -- Gvariant_New_Byte --
   -----------------------

   function Gvariant_New_Byte (Value : Guchar) return Gvariant is
      function Internal (Value : Guchar) return System.Address;
      pragma Import (C, Internal, "g_variant_new_byte");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Byte;

   -----------------------------
   -- Gvariant_New_Bytestring --
   -----------------------------

   function Gvariant_New_Bytestring (String : Gint_Array) return Gvariant is
      function Internal (String : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_bytestring");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (String (String'First)'Address));
      return Self;
   end Gvariant_New_Bytestring;

   -----------------------------------
   -- Gvariant_New_Bytestring_Array --
   -----------------------------------

   function Gvariant_New_Bytestring_Array
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_bytestring_array");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
      Self       : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Bytestring_Array;

   -----------------------------
   -- Gvariant_New_Dict_Entry --
   -----------------------------

   function Gvariant_New_Dict_Entry
      (Key   : Gvariant;
       Value : Gvariant) return Gvariant
   is
      function Internal
         (Key   : System.Address;
          Value : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_dict_entry");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Get_Object (Key), Get_Object (Value)));
      return Self;
   end Gvariant_New_Dict_Entry;

   -------------------------
   -- Gvariant_New_Double --
   -------------------------

   function Gvariant_New_Double (Value : Gdouble) return Gvariant is
      function Internal (Value : Gdouble) return System.Address;
      pragma Import (C, Internal, "g_variant_new_double");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Double;

   -------------------------
   -- Gvariant_New_Handle --
   -------------------------

   function Gvariant_New_Handle (Value : Gint32) return Gvariant is
      function Internal (Value : Gint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_handle");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Handle;

   ------------------------
   -- Gvariant_New_Int16 --
   ------------------------

   function Gvariant_New_Int16 (Value : Gint16) return Gvariant is
      function Internal (Value : Gint16) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int16");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Int16;

   ------------------------
   -- Gvariant_New_Int32 --
   ------------------------

   function Gvariant_New_Int32 (Value : Gint32) return Gvariant is
      function Internal (Value : Gint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int32");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Int32;

   ------------------------
   -- Gvariant_New_Int64 --
   ------------------------

   function Gvariant_New_Int64 (Value : Gint64) return Gvariant is
      function Internal (Value : Gint64) return System.Address;
      pragma Import (C, Internal, "g_variant_new_int64");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Int64;

   ------------------------------
   -- Gvariant_New_Object_Path --
   ------------------------------

   function Gvariant_New_Object_Path
      (Object_Path : UTF8_String) return Gvariant
   is
      function Internal
         (Object_Path : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_object_path");
      Tmp_Object_Path : Gtkada.Types.Chars_Ptr := New_String (Object_Path);
      Tmp_Return      : System.Address;
      Self            : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_Object_Path);
      Free (Tmp_Object_Path);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Object_Path;

   -----------------------
   -- Gvariant_New_Objv --
   -----------------------

   function Gvariant_New_Objv
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_objv");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
      Self       : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Objv;

   ----------------------------
   -- Gvariant_New_Signature --
   ----------------------------

   function Gvariant_New_Signature (Signature : UTF8_String) return Gvariant is
      function Internal
         (Signature : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_signature");
      Tmp_Signature : Gtkada.Types.Chars_Ptr := New_String (Signature);
      Tmp_Return    : System.Address;
      Self          : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_Signature);
      Free (Tmp_Signature);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Signature;

   -------------------------
   -- Gvariant_New_String --
   -------------------------

   function Gvariant_New_String (String : UTF8_String) return Gvariant is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
      Self       : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_String;

   -----------------------
   -- Gvariant_New_Strv --
   -----------------------

   function Gvariant_New_Strv
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant
   is
      function Internal
         (Strv   : Gtkada.Types.chars_ptr_array;
          Length : Gssize) return System.Address;
      pragma Import (C, Internal, "g_variant_new_strv");
      Tmp_Strv   : Gtkada.Types.chars_ptr_array := From_String_List (Strv);
      Tmp_Return : System.Address;
      Self       : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_Strv, Length);
      Gtkada.Types.Free (Tmp_Strv);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Strv;

   ------------------------------
   -- Gvariant_New_Take_String --
   ------------------------------

   function Gvariant_New_Take_String (String : UTF8_String) return Gvariant is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_variant_new_take_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
      Self       : Gvariant;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gvariant_New_Take_String;

   -------------------------
   -- Gvariant_New_Uint16 --
   -------------------------

   function Gvariant_New_Uint16 (Value : Guint16) return Gvariant is
      function Internal (Value : Guint16) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint16");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Uint16;

   -------------------------
   -- Gvariant_New_Uint32 --
   -------------------------

   function Gvariant_New_Uint32 (Value : Guint32) return Gvariant is
      function Internal (Value : Guint32) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint32");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Uint32;

   -------------------------
   -- Gvariant_New_Uint64 --
   -------------------------

   function Gvariant_New_Uint64 (Value : Guint64) return Gvariant is
      function Internal (Value : Guint64) return System.Address;
      pragma Import (C, Internal, "g_variant_new_uint64");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Value));
      return Self;
   end Gvariant_New_Uint64;

   --------------------------
   -- Gvariant_New_Variant --
   --------------------------

   function Gvariant_New_Variant (Value : Gvariant) return Gvariant is
      function Internal (Value : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_new_variant");
      Self : Gvariant;
   begin
      Self.Set_Object (Internal (Get_Object (Value)));
      return Self;
   end Gvariant_New_Variant;

   -----------------------
   -- Gvariant_Type_New --
   -----------------------

   function Gvariant_Type_New
      (Type_String : UTF8_String) return Gvariant_Type
   is
      function Internal
         (Type_String : Gtkada.Types.Chars_Ptr) return Gvariant_Type;
      pragma Import (C, Internal, "g_variant_type_new");
      Tmp_Type_String : Gtkada.Types.Chars_Ptr := New_String (Type_String);
      Tmp_Return      : Gvariant_Type;
      Self            : Gvariant_Type;
   begin
      Tmp_Return := Internal (Tmp_Type_String);
      Free (Tmp_Type_String);
      Self := Tmp_Return;
      return Self;
   end Gvariant_Type_New;

   ----------------------------------
   -- Gvariant_Type_New_Dict_Entry --
   ----------------------------------

   function Gvariant_Type_New_Dict_Entry
      (Key   : Gvariant_Type;
       Value : Gvariant_Type) return Gvariant_Type
   is
      function Internal
         (Key   : Gvariant_Type;
          Value : Gvariant_Type) return Gvariant_Type;
      pragma Import (C, Internal, "g_variant_type_new_dict_entry");
      Self : Gvariant_Type;
   begin
      Self := Internal (Key, Value);
      return Self;
   end Gvariant_Type_New_Dict_Entry;

   --------------
   -- Byteswap --
   --------------

   function Byteswap (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_byteswap");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Byteswap;

   -------------------------
   -- Check_Format_String --
   -------------------------

   function Check_Format_String
      (Self          : Gvariant;
       Format_String : UTF8_String;
       Copy_Only     : Boolean) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Format_String : Gtkada.Types.Chars_Ptr;
          Copy_Only     : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_check_format_string");
      Tmp_Format_String : Gtkada.Types.Chars_Ptr := New_String (Format_String);
      Tmp_Return        : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Format_String, Boolean'Pos (Copy_Only));
      Free (Tmp_Format_String);
      return Tmp_Return /= 0;
   end Check_Format_String;

   --------------
   -- Classify --
   --------------

   function Classify (Self : Gvariant) return GVariant_Class is
      function Internal (Self : System.Address) return GVariant_Class;
      pragma Import (C, Internal, "g_variant_classify");
   begin
      return Internal (Get_Object (Self));
   end Classify;

   --------------------------
   -- Dup_Bytestring_Array --
   --------------------------

   function Dup_Bytestring_Array
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_dup_bytestring_array");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Dup_Bytestring_Array;

   --------------
   -- Dup_Objv --
   --------------

   function Dup_Objv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_dup_objv");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Dup_Objv;

   ----------------
   -- Dup_String --
   ----------------

   function Dup_String
      (Self   : Gvariant;
       Length : access Gsize) return UTF8_String
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_dup_string");
      Acc_Length : aliased Gsize;
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      Length.all := Acc_Length;
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Dup_String;

   ----------------
   -- Dup_String --
   ----------------

   function Dup_String (Self : Gvariant_Type) return UTF8_String is
      function Internal (Self : Gvariant_Type) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_type_dup_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end Dup_String;

   --------------
   -- Dup_Strv --
   --------------

   function Dup_Strv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_dup_strv");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Dup_Strv;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Self : Gvariant) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_get_boolean");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Boolean;

   --------------
   -- Get_Byte --
   --------------

   function Get_Byte (Self : Gvariant) return Guchar is
      function Internal (Self : System.Address) return Guchar;
      pragma Import (C, Internal, "g_variant_get_byte");
   begin
      return Internal (Get_Object (Self));
   end Get_Byte;

   --------------------------
   -- Get_Bytestring_Array --
   --------------------------

   function Get_Bytestring_Array
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_get_bytestring_array");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Get_Bytestring_Array;

   ---------------------
   -- Get_Child_Value --
   ---------------------

   function Get_Child_Value (Self : Gvariant; Index : Gsize) return Gvariant is
      function Internal
         (Self  : System.Address;
          Index : Gsize) return System.Address;
      pragma Import (C, Internal, "g_variant_get_child_value");
   begin
      return From_Object (Internal (Get_Object (Self), Index));
   end Get_Child_Value;

   ----------------
   -- Get_Double --
   ----------------

   function Get_Double (Self : Gvariant) return Gdouble is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "g_variant_get_double");
   begin
      return Internal (Get_Object (Self));
   end Get_Double;

   ----------------
   -- Get_Handle --
   ----------------

   function Get_Handle (Self : Gvariant) return Gint32 is
      function Internal (Self : System.Address) return Gint32;
      pragma Import (C, Internal, "g_variant_get_handle");
   begin
      return Internal (Get_Object (Self));
   end Get_Handle;

   ---------------
   -- Get_Int16 --
   ---------------

   function Get_Int16 (Self : Gvariant) return Gint16 is
      function Internal (Self : System.Address) return Gint16;
      pragma Import (C, Internal, "g_variant_get_int16");
   begin
      return Internal (Get_Object (Self));
   end Get_Int16;

   ---------------
   -- Get_Int32 --
   ---------------

   function Get_Int32 (Self : Gvariant) return Gint32 is
      function Internal (Self : System.Address) return Gint32;
      pragma Import (C, Internal, "g_variant_get_int32");
   begin
      return Internal (Get_Object (Self));
   end Get_Int32;

   ---------------
   -- Get_Int64 --
   ---------------

   function Get_Int64 (Self : Gvariant) return Gint64 is
      function Internal (Self : System.Address) return Gint64;
      pragma Import (C, Internal, "g_variant_get_int64");
   begin
      return Internal (Get_Object (Self));
   end Get_Int64;

   ---------------
   -- Get_Maybe --
   ---------------

   function Get_Maybe (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_get_maybe");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Maybe;

   ---------------------
   -- Get_Normal_Form --
   ---------------------

   function Get_Normal_Form (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_get_normal_form");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Normal_Form;

   --------------
   -- Get_Objv --
   --------------

   function Get_Objv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_get_objv");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Get_Objv;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Self : Gvariant) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "g_variant_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
      (Self   : Gvariant;
       Length : access Gsize) return UTF8_String
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_get_string");
      Acc_Length : aliased Gsize;
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return Gtkada.Bindings.Value_Allowing_Null (Tmp_Return);
   end Get_String;

   --------------
   -- Get_Strv --
   --------------

   function Get_Strv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self       : System.Address;
          Acc_Length : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_variant_get_strv");
      Acc_Length : aliased Gsize;
      Tmp_Return : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Length'Access);
      if Length /= null then
         Length.all := Acc_Length;
      end if;
      return To_String_List_And_Free (Tmp_Return);
   end Get_Strv;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Self : Gvariant) return Gvariant_Type is
      function Internal (Self : System.Address) return Gvariant_Type;
      pragma Import (C, Internal, "g_variant_get_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Type;

   ---------------------
   -- Get_Type_String --
   ---------------------

   function Get_Type_String (Self : Gvariant) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_get_type_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Type_String;

   ----------------
   -- Get_Uint16 --
   ----------------

   function Get_Uint16 (Self : Gvariant) return Guint16 is
      function Internal (Self : System.Address) return Guint16;
      pragma Import (C, Internal, "g_variant_get_uint16");
   begin
      return Internal (Get_Object (Self));
   end Get_Uint16;

   ----------------
   -- Get_Uint32 --
   ----------------

   function Get_Uint32 (Self : Gvariant) return Guint32 is
      function Internal (Self : System.Address) return Guint32;
      pragma Import (C, Internal, "g_variant_get_uint32");
   begin
      return Internal (Get_Object (Self));
   end Get_Uint32;

   ----------------
   -- Get_Uint64 --
   ----------------

   function Get_Uint64 (Self : Gvariant) return Guint64 is
      function Internal (Self : System.Address) return Guint64;
      pragma Import (C, Internal, "g_variant_get_uint64");
   begin
      return Internal (Get_Object (Self));
   end Get_Uint64;

   -----------------
   -- Get_Variant --
   -----------------

   function Get_Variant (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_get_variant");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Variant;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Gvariant) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_variant_hash");
   begin
      return Internal (Get_Object (Self));
   end Hash;

   ----------
   -- Init --
   ----------

   function Init (Self : Gvariant_Iter; Value : Gvariant) return Gsize is
      function Internal
         (Self  : Gvariant_Iter;
          Value : System.Address) return Gsize;
      pragma Import (C, Internal, "g_variant_iter_init");
   begin
      return Internal (Self, Get_Object (Value));
   end Init;

   --------------
   -- Is_Array --
   --------------

   function Is_Array (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_array");
   begin
      return Internal (Self) /= 0;
   end Is_Array;

   --------------
   -- Is_Basic --
   --------------

   function Is_Basic (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_basic");
   begin
      return Internal (Self) /= 0;
   end Is_Basic;

   ------------------
   -- Is_Container --
   ------------------

   function Is_Container (Self : Gvariant) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_container");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Container;

   ------------------
   -- Is_Container --
   ------------------

   function Is_Container (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_container");
   begin
      return Internal (Self) /= 0;
   end Is_Container;

   -----------------
   -- Is_Definite --
   -----------------

   function Is_Definite (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_definite");
   begin
      return Internal (Self) /= 0;
   end Is_Definite;

   -------------------
   -- Is_Dict_Entry --
   -------------------

   function Is_Dict_Entry (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_dict_entry");
   begin
      return Internal (Self) /= 0;
   end Is_Dict_Entry;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating (Self : Gvariant) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_floating");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Floating;

   --------------
   -- Is_Maybe --
   --------------

   function Is_Maybe (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_maybe");
   begin
      return Internal (Self) /= 0;
   end Is_Maybe;

   --------------------
   -- Is_Normal_Form --
   --------------------

   function Is_Normal_Form (Self : Gvariant) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_normal_form");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Normal_Form;

   ----------------
   -- Is_Of_Type --
   ----------------

   function Is_Of_Type
      (Self     : Gvariant;
       The_Type : Gvariant_Type) return Boolean
   is
      function Internal
         (Self     : System.Address;
          The_Type : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_of_type");
   begin
      return Internal (Get_Object (Self), The_Type) /= 0;
   end Is_Of_Type;

   -------------------
   -- Is_Subtype_Of --
   -------------------

   function Is_Subtype_Of
      (Self      : Gvariant_Type;
       Supertype : Gvariant_Type) return Boolean
   is
      function Internal
         (Self      : Gvariant_Type;
          Supertype : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_subtype_of");
   begin
      return Internal (Self, Supertype) /= 0;
   end Is_Subtype_Of;

   --------------
   -- Is_Tuple --
   --------------

   function Is_Tuple (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_tuple");
   begin
      return Internal (Self) /= 0;
   end Is_Tuple;

   ----------------
   -- Is_Variant --
   ----------------

   function Is_Variant (Self : Gvariant_Type) return Boolean is
      function Internal (Self : Gvariant_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_is_variant");
   begin
      return Internal (Self) /= 0;
   end Is_Variant;

   --------------
   -- Iter_New --
   --------------

   function Iter_New (Self : Gvariant) return Gvariant_Iter is
      function Internal (Self : System.Address) return Gvariant_Iter;
      pragma Import (C, Internal, "g_variant_iter_new");
   begin
      return Internal (Get_Object (Self));
   end Iter_New;

   ------------------
   -- Lookup_Value --
   ------------------

   function Lookup_Value
      (Self          : Gvariant;
       Key           : UTF8_String;
       Expected_Type : Gvariant_Type) return Gvariant
   is
      function Internal
         (Self          : System.Address;
          Key           : Gtkada.Types.Chars_Ptr;
          Expected_Type : Gvariant_Type) return System.Address;
      pragma Import (C, Internal, "g_variant_lookup_value");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key, Expected_Type);
      Free (Tmp_Key);
      return From_Object (Tmp_Return);
   end Lookup_Value;

   ----------------
   -- N_Children --
   ----------------

   function N_Children (Self : Gvariant) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "g_variant_n_children");
   begin
      return Internal (Get_Object (Self));
   end N_Children;

   ----------------
   -- Next_Value --
   ----------------

   function Next_Value (Self : Gvariant_Iter) return Gvariant is
      function Internal (Self : Gvariant_Iter) return System.Address;
      pragma Import (C, Internal, "g_variant_iter_next_value");
   begin
      return From_Object (Internal (Self));
   end Next_Value;

   -----------------
   -- Peek_String --
   -----------------

   function Peek_String (Self : Gvariant_Type) return UTF8_String is
      function Internal (Self : Gvariant_Type) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_type_peek_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Peek_String;

   -----------
   -- Print --
   -----------

   function Print
      (Self          : Gvariant;
       Type_Annotate : Boolean) return UTF8_String
   is
      function Internal
         (Self          : System.Address;
          Type_Annotate : Glib.Gboolean) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_print");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Boolean'Pos (Type_Annotate)));
   end Print;

   ------------------
   -- Print_String --
   ------------------

   function Print_String
      (Self          : Gvariant;
       String        : Glib.String.Gstring;
       Type_Annotate : Boolean) return Glib.String.Gstring
   is
      function Internal
         (Self          : System.Address;
          String        : Glib.String.Gstring;
          Type_Annotate : Glib.Gboolean) return access Glib.String.Gstring;
      pragma Import (C, Internal, "g_variant_print_string");
   begin
      return From_Object_Free (Internal (Get_Object (Self), String, Boolean'Pos (Type_Annotate)));
   end Print_String;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   --------------
   -- Ref_Sink --
   --------------

   function Ref_Sink (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_ref_sink");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref_Sink;

   -----------
   -- Store --
   -----------

   procedure Store (Self : Gvariant; Data : System.Address) is
      procedure Internal (Self : System.Address; Data : System.Address);
      pragma Import (C, Internal, "g_variant_store");
   begin
      Internal (Get_Object (Self), Data);
   end Store;

   --------------
   -- Take_Ref --
   --------------

   function Take_Ref (Self : Gvariant) return Gvariant is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_variant_take_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Take_Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gvariant) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_variant_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   --------------------
   -- Is_Object_Path --
   --------------------

   function Is_Object_Path (String : UTF8_String) return Boolean is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_object_path");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      return Tmp_Return /= 0;
   end Is_Object_Path;

   ------------------
   -- Is_Signature --
   ------------------

   function Is_Signature (String : UTF8_String) return Boolean is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_is_signature");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      return Tmp_Return /= 0;
   end Is_Signature;

   -----------
   -- Parse --
   -----------

   function Parse
      (The_Type : Gvariant_Type;
       Text     : UTF8_String;
       Limit    : UTF8_String := "";
       Endptr   : GNAT.Strings.String_List) return Gvariant
   is
      function Internal
         (The_Type : Gvariant_Type;
          Text     : Gtkada.Types.Chars_Ptr;
          Limit    : Gtkada.Types.Chars_Ptr;
          Endptr   : Gtkada.Types.chars_ptr_array) return System.Address;
      pragma Import (C, Internal, "g_variant_parse");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Limit  : Gtkada.Types.Chars_Ptr;
      Tmp_Endptr : Gtkada.Types.chars_ptr_array := From_String_List (Endptr);
      Tmp_Return : System.Address;
   begin
      if Limit = "" then
         Tmp_Limit := Gtkada.Types.Null_Ptr;
      else
         Tmp_Limit := New_String (Limit);
      end if;
      Tmp_Return := Internal (The_Type, Tmp_Text, Tmp_Limit, Tmp_Endptr);
      Gtkada.Types.Free (Tmp_Endptr);
      Free (Tmp_Limit);
      Free (Tmp_Text);
      return From_Object (Tmp_Return);
   end Parse;

   -------------------------------
   -- Parse_Error_Print_Context --
   -------------------------------

   function Parse_Error_Print_Context
      (Error      : Glib.Error.GError;
       Source_Str : UTF8_String) return UTF8_String
   is
      function Internal
         (Error      : Glib.Error.GError;
          Source_Str : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_variant_parse_error_print_context");
      Tmp_Source_Str : Gtkada.Types.Chars_Ptr := New_String (Source_Str);
      Tmp_Return     : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Error, Tmp_Source_Str);
      Free (Tmp_Source_Str);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Parse_Error_Print_Context;

   -----------------------
   -- Parse_Error_Quark --
   -----------------------

   function Parse_Error_Quark return Glib.GQuark is
      function Internal return Glib.GQuark;
      pragma Import (C, Internal, "g_variant_parse_error_quark");
   begin
      return Internal;
   end Parse_Error_Quark;

   ----------------------------
   -- Parser_Get_Error_Quark --
   ----------------------------

   function Parser_Get_Error_Quark return Glib.GQuark is
      function Internal return Glib.GQuark;
      pragma Import (C, Internal, "g_variant_parser_get_error_quark");
   begin
      return Internal;
   end Parser_Get_Error_Quark;

   ---------------------
   -- String_Is_Valid --
   ---------------------

   function String_Is_Valid (Type_String : UTF8_String) return Boolean is
      function Internal
         (Type_String : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "g_variant_type_string_is_valid");
      Tmp_Type_String : Gtkada.Types.Chars_Ptr := New_String (Type_String);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Type_String);
      Free (Tmp_Type_String);
      return Tmp_Return /= 0;
   end String_Is_Valid;

end Glib.Variant;
