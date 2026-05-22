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
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Content_Formats is

   function From_Object_Free
     (B : access Gdk_Content_Formats'Class) return Gdk_Content_Formats
   is
      Result : constant Gdk_Content_Formats := Gdk_Content_Formats (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Content_Formats is
      S : Gdk_Content_Formats;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------------
   -- Gdk_Content_Formats_New --
   -----------------------------

   function Gdk_Content_Formats_New
      (Mime_Types   : GNAT.Strings.String_List;
       N_Mime_Types : Guint) return Gdk_Content_Formats
   is
      function Internal
         (Mime_Types   : Gtkada.Types.chars_ptr_array;
          N_Mime_Types : Guint) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_new");
      Tmp_Mime_Types : Gtkada.Types.chars_ptr_array := From_String_List (Mime_Types);
      Tmp_Return     : System.Address;
      Self           : Gdk_Content_Formats;
   begin
      Tmp_Return := Internal (Tmp_Mime_Types, N_Mime_Types);
      Gtkada.Types.Free (Tmp_Mime_Types);
      Self.Set_Object (Tmp_Return);
      return Self;
   end Gdk_Content_Formats_New;

   ---------------------------------------
   -- Gdk_Content_Formats_New_For_Gtype --
   ---------------------------------------

   function Gdk_Content_Formats_New_For_Gtype
      (The_Type : GType) return Gdk_Content_Formats
   is
      function Internal (The_Type : GType) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_new_for_gtype");
      Self : Gdk_Content_Formats;
   begin
      Self.Set_Object (Internal (The_Type));
      return Self;
   end Gdk_Content_Formats_New_For_Gtype;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Self         : out Gdk_Content_Formats;
       Mime_Types   : GNAT.Strings.String_List;
       N_Mime_Types : Guint)
   is
      function Internal
         (Mime_Types   : Gtkada.Types.chars_ptr_array;
          N_Mime_Types : Guint) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_new");
      Tmp_Mime_Types : Gtkada.Types.chars_ptr_array := From_String_List (Mime_Types);
      Tmp_Return     : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Mime_Types, N_Mime_Types);
      Gtkada.Types.Free (Tmp_Mime_Types);
      Self.Set_Object (Tmp_Return);
   end Gdk_New;

   -----------------------
   -- Gdk_New_For_Gtype --
   -----------------------

   procedure Gdk_New_For_Gtype
      (Self     : out Gdk_Content_Formats;
       The_Type : GType)
   is
      function Internal (The_Type : GType) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_new_for_gtype");
   begin
      Self.Set_Object (Internal (The_Type));
   end Gdk_New_For_Gtype;

   -------------------
   -- Contain_Gtype --
   -------------------

   function Contain_Gtype
      (Self     : Gdk_Content_Formats;
       The_Type : GType) return Boolean
   is
      function Internal
         (Self     : System.Address;
          The_Type : GType) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_formats_contain_gtype");
   begin
      return Internal (Get_Object (Self), The_Type) /= 0;
   end Contain_Gtype;

   -----------------------
   -- Contain_Mime_Type --
   -----------------------

   function Contain_Mime_Type
      (Self      : Gdk_Content_Formats;
       Mime_Type : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Mime_Type : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_formats_contain_mime_type");
      Tmp_Mime_Type : Gtkada.Types.Chars_Ptr := New_String (Mime_Type);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Mime_Type);
      Free (Tmp_Mime_Type);
      return Tmp_Return /= 0;
   end Contain_Mime_Type;

   --------------------
   -- Get_Mime_Types --
   --------------------

   function Get_Mime_Types
      (Self         : Gdk_Content_Formats;
       N_Mime_Types : access Gsize) return GNAT.Strings.String_List
   is
      function Internal
         (Self             : System.Address;
          Acc_N_Mime_Types : access Gsize) return chars_ptr_array_access;
      pragma Import (C, Internal, "gdk_content_formats_get_mime_types");
      Acc_N_Mime_Types : aliased Gsize;
      Tmp_Return       : chars_ptr_array_access;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_N_Mime_Types'Access);
      if N_Mime_Types /= null then
         N_Mime_Types.all := Acc_N_Mime_Types;
      end if;
      return To_String_List (Tmp_Return.all);
   end Get_Mime_Types;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Gdk_Content_Formats) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_formats_is_empty");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Empty;

   -----------
   -- Match --
   -----------

   function Match
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Second : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_content_formats_match");
   begin
      return Internal (Get_Object (Self), Get_Object (Second)) /= 0;
   end Match;

   -----------------
   -- Match_Gtype --
   -----------------

   function Match_Gtype
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return GType
   is
      function Internal
         (Self   : System.Address;
          Second : System.Address) return GType;
      pragma Import (C, Internal, "gdk_content_formats_match_gtype");
   begin
      return Internal (Get_Object (Self), Get_Object (Second));
   end Match_Gtype;

   ---------------------
   -- Match_Mime_Type --
   ---------------------

   function Match_Mime_Type
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return UTF8_String
   is
      function Internal
         (Self   : System.Address;
          Second : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_content_formats_match_mime_type");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Get_Object (Second)));
   end Match_Mime_Type;

   -----------
   -- Print --
   -----------

   procedure Print
      (Self   : Gdk_Content_Formats;
       String : Glib.String.Gstring)
   is
      procedure Internal
         (Self   : System.Address;
          String : Glib.String.Gstring);
      pragma Import (C, Internal, "gdk_content_formats_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Content_Formats) return Gdk_Content_Formats is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gdk_Content_Formats) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_content_formats_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -----------
   -- Union --
   -----------

   function Union
      (Self   : Gdk_Content_Formats;
       Second : Gdk_Content_Formats) return Gdk_Content_Formats
   is
      function Internal
         (Self   : System.Address;
          Second : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_union");
   begin
      return From_Object (Internal (Get_Object (Self), Get_Object (Second)));
   end Union;

   ------------------------------
   -- Union_Deserialize_Gtypes --
   ------------------------------

   function Union_Deserialize_Gtypes
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_union_deserialize_gtypes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Union_Deserialize_Gtypes;

   ----------------------------------
   -- Union_Deserialize_Mime_Types --
   ----------------------------------

   function Union_Deserialize_Mime_Types
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_union_deserialize_mime_types");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Union_Deserialize_Mime_Types;

   ----------------------------
   -- Union_Serialize_Gtypes --
   ----------------------------

   function Union_Serialize_Gtypes
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_union_serialize_gtypes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Union_Serialize_Gtypes;

   --------------------------------
   -- Union_Serialize_Mime_Types --
   --------------------------------

   function Union_Serialize_Mime_Types
      (Self : Gdk_Content_Formats) return Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_union_serialize_mime_types");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Union_Serialize_Mime_Types;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Content_Formats) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_content_formats_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   -----------
   -- Parse --
   -----------

   function Parse (String : UTF8_String) return Gdk_Content_Formats is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gdk_content_formats_parse");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      return From_Object (Tmp_Return);
   end Parse;

end Gdk.Content_Formats;
