------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Gtkada.Types;         use Gtkada.Types;
with System;               use System;

package body Glib is

   ----------------------
   -- To_Boolean_Array --
   ----------------------

   function To_Boolean_Array (A : Gboolean_Array) return Boolean_Array is
      Result : Boolean_Array (A'Range);
   begin
      for Index in A'Range loop
         Result (Index) := A (Index) /= 0;
      end loop;

      return Result;
   end To_Boolean_Array;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (Bool : Boolean) return Gint is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Gint;

   -----------------------
   -- Quark_From_String --
   -----------------------

   function Quark_From_String (Id : String) return GQuark is
      function Internal (Id : String) return GQuark;
      pragma Import (C, Internal, "g_quark_from_string");
   begin
      return Internal (Id & ASCII.NUL);
   end Quark_From_String;

   ----------------------
   -- Quark_Try_String --
   ----------------------

   function Quark_Try_String (Id : String) return GQuark is
      function Internal (Id : String) return GQuark;
      pragma Import (C, Internal, "g_quark_try_string");
   begin
      return Internal (Id & ASCII.NUL);
   end Quark_Try_String;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : GType) return String is
      function Internal (Type_Num : GType) return Chars_Ptr;
      pragma Import (C, Internal, "g_type_name");
      Ret : constant Chars_Ptr := Internal (Type_Num);
   begin
      if Ret = Null_Ptr then
         return "";
      else
         return Value (Ret);
      end if;
   end Type_Name;

   --------------------
   -- Type_From_Name --
   --------------------

   function Type_From_Name (Name : String) return GType is
      function Internal (Name : String) return GType;
      pragma Import (C, Internal, "g_type_from_name");
   begin
      return Internal (Name & ASCII.NUL);
   end Type_From_Name;

   -----------
   -- Build --
   -----------

   function Build (Name : String) return Property is
   begin
      if Name (Name'Last) /= ASCII.NUL then
         return Property (Name & ASCII.NUL);
      else
         return Property (Name);
      end if;
   end Build;

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Prop : Property) return String is
   begin
      return String (Prop);
   end Property_Name;

   --------------------------------
   -- Boxed_Type_Register_Static --
   --------------------------------

   function Boxed_Type_Register_Static
     (Name : String;
      Copy : Boxed_Copy;
      Free : Boxed_Free) return GType
   is
      function Internal
        (N : String; Copy : Boxed_Copy; Free : Boxed_Free) return GType;
      pragma Import (C, Internal, "g_boxed_type_register_static");
   begin
      return Internal (Name & ASCII.NUL, Copy, Free);
   end Boxed_Type_Register_Static;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Self : C_Boxed'Class) return System.Address is
   begin
      return Self.Ptr;
   end Get_Object;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object (Self : in out C_Boxed'Class; Ptr : System.Address) is
   begin
      Self.Ptr := Ptr;
   end Set_Object;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : C_Boxed'Class) return Boolean is
   begin
      return Self.Ptr = System.Null_Address;
   end Is_Null;
end Glib;
