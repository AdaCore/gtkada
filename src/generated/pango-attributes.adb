------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
with Ada.Unchecked_Conversion;
with Glib.Object;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;     use Interfaces.C.Strings;
pragma Warnings(On);

package body Pango.Attributes is

   function From_Object_Free (B : access Pango_Attribute) return Pango_Attribute is
      Result : constant Pango_Attribute := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free
     (B : access Pango_Attr_List'Class) return Pango_Attr_List
   is
      Result : constant Pango_Attr_List := Pango_Attr_List (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Attr_List is
      S : Pango_Attr_List;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function C_Pango_Attr_List_Filter
      (Self : System.Address;
       Func : System.Address;
       Data : System.Address) return System.Address;
   pragma Import (C, C_Pango_Attr_List_Filter, "pango_attr_list_filter");
   --  Given a Pango.Attributes.Pango_Attr_List and callback function, removes
   --  any elements of List for which Func returns True and inserts them into a
   --  new list.
   --  Since: gtk+ 1.2
   --  "func": callback function; returns True if an attribute should be
   --  filtered out.
   --  "data": Data to be passed to Func

   function To_Pango_Attr_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Pango_Attr_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Pango_Attr_Filter_Func, System.Address);

   function Internal_Pango_Attr_Filter_Func
      (Attribute : access Pango.Attributes.Pango_Attribute;
       User_Data : System.Address) return Integer;
   pragma Convention (C, Internal_Pango_Attr_Filter_Func);
   --  "attribute": a Pango attribute
   --  "user_data": user data passed to the function

   -------------------------------------
   -- Internal_Pango_Attr_Filter_Func --
   -------------------------------------

   function Internal_Pango_Attr_Filter_Func
      (Attribute : access Pango.Attributes.Pango_Attribute;
       User_Data : System.Address) return Integer
   is
      Func : constant Pango_Attr_Filter_Func := To_Pango_Attr_Filter_Func (User_Data);
   begin
      return Boolean'Pos (Func (Attribute.all));
   end Internal_Pango_Attr_Filter_Func;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Pango_Attr_List) is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_attr_list_new");
   begin
      Self.Set_Object (Internal);
   end Gdk_New;

   -------------------------
   -- Pango_Attr_List_New --
   -------------------------

   function Pango_Attr_List_New return Pango_Attr_List is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_attr_list_new");
      Self : Pango_Attr_List;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Pango_Attr_List_New;

   ------------
   -- Change --
   ------------

   procedure Change (Self : Pango_Attr_List; Attr : Pango_Attribute) is
      procedure Internal (Self : System.Address; Attr : Pango_Attribute);
      pragma Import (C, Internal, "pango_attr_list_change");
   begin
      Internal (Get_Object (Self), Attr);
   end Change;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Pango_Attr_List) return Pango_Attr_List is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_attr_list_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Pango_Attribute;
       Attr2 : Pango_Attribute) return Boolean
   is
      function Internal
         (Self  : Pango_Attribute;
          Attr2 : Pango_Attribute) return Integer;
      pragma Import (C, Internal, "pango_attribute_equal");
   begin
      return Internal (Self, Attr2) /= 0;
   end Equal;

   ------------
   -- Filter --
   ------------

   function Filter
      (Self : Pango_Attr_List;
       Func : Pango_Attr_Filter_Func) return Pango_Attr_List
   is
   begin
      if Func = null then
         return From_Object (C_Pango_Attr_List_Filter (Get_Object (Self), System.Null_Address, System.Null_Address));
      else
         return From_Object (C_Pango_Attr_List_Filter (Get_Object (Self), Internal_Pango_Attr_Filter_Func'Address, To_Address (Func)));
      end if;
   end Filter;

   package body Filter_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Pango_Attr_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Pango_Attr_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Pango_Attr_Filter_Func, System.Address);

      function Internal_Cb
         (Attribute : access Pango.Attributes.Pango_Attribute;
          User_Data : System.Address) return Integer;
      pragma Convention (C, Internal_Cb);
      --  Type of a function filtering a list of attributes.
      --  "attribute": a Pango attribute
      --  "user_data": user data passed to the function

      ------------
      -- Filter --
      ------------

      function Filter
         (Self : Pango.Attributes.Pango_Attr_List;
          Func : Pango_Attr_Filter_Func;
          Data : User_Data_Type) return Pango.Attributes.Pango_Attr_List
      is
      begin
         if Func = null then
            return From_Object (C_Pango_Attr_List_Filter (Get_Object (Self), System.Null_Address, System.Null_Address));
         else
            return From_Object (C_Pango_Attr_List_Filter (Get_Object (Self), Internal_Cb'Address, Users.Build (To_Address (Func), Data)));
         end if;
      end Filter;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Attribute : access Pango.Attributes.Pango_Attribute;
          User_Data : System.Address) return Integer
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return Boolean'Pos (To_Pango_Attr_Filter_Func (D.Func) (Attribute.all, D.Data.all));
      end Internal_Cb;

   end Filter_User_Data;

   ------------
   -- Insert --
   ------------

   procedure Insert (Self : Pango_Attr_List; Attr : Pango_Attribute) is
      procedure Internal (Self : System.Address; Attr : Pango_Attribute);
      pragma Import (C, Internal, "pango_attr_list_insert");
   begin
      Internal (Get_Object (Self), Attr);
   end Insert;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before (Self : Pango_Attr_List; Attr : Pango_Attribute) is
      procedure Internal (Self : System.Address; Attr : Pango_Attribute);
      pragma Import (C, Internal, "pango_attr_list_insert_before");
   begin
      Internal (Get_Object (Self), Attr);
   end Insert_Before;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Pango_Attr_List) return Pango_Attr_List is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_attr_list_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ------------
   -- Splice --
   ------------

   procedure Splice
      (Self  : Pango_Attr_List;
       Other : Pango_Attr_List;
       Pos   : Gint;
       Len   : Gint)
   is
      procedure Internal
         (Self  : System.Address;
          Other : System.Address;
          Pos   : Gint;
          Len   : Gint);
      pragma Import (C, Internal, "pango_attr_list_splice");
   begin
      Internal (Get_Object (Self), Get_Object (Other), Pos, Len);
   end Splice;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Pango_Attr_List) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_attr_list_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   ---------------------
   -- Attr_Family_New --
   ---------------------

   function Attr_Family_New (Family : UTF8_String) return Pango_Attribute is
      function Internal
         (Family : Interfaces.C.Strings.chars_ptr)
          return access Pango_Attribute;
      pragma Import (C, Internal, "pango_attr_family_new");
      Tmp_Family : Interfaces.C.Strings.chars_ptr := New_String (Family);
      Tmp_Return : access Pango_Attribute;
   begin
      Tmp_Return := Internal (Tmp_Family);
      Free (Tmp_Family);
      return From_Object_Free (Tmp_Return);
   end Attr_Family_New;

   ----------------------------
   -- Attr_Strikethrough_New --
   ----------------------------

   function Attr_Strikethrough_New
      (Strikethrough : Boolean) return Pango_Attribute
   is
      function Internal
         (Strikethrough : Integer) return access Pango_Attribute;
      pragma Import (C, Internal, "pango_attr_strikethrough_new");
   begin
      return From_Object_Free (Internal (Boolean'Pos (Strikethrough)));
   end Attr_Strikethrough_New;

end Pango.Attributes;
