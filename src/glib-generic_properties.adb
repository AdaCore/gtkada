-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;
with Unchecked_Conversion;
with System;      use System;

package body Glib.Generic_Properties is

   type Byte is range 0 .. 255;
   for Byte'Size use 8;

   function C_Enum_Value_Size return Natural;
   pragma Import (C, C_Enum_Value_Size, "ada_c_enum_value_size");
   type C_Enum_Value is array (1 .. C_Enum_Value_Size) of Byte;
   pragma Pack (C_Enum_Value);

   type Enum_Value_Array is array (Guint range <>) of C_Enum_Value;
   type Enum_Value_Array_Access is access all Enum_Value_Array;

   procedure C_Create_Enum_Value
     (Value : Gint; Name, Nick : System.Address; Enum : out C_Enum_Value);
   pragma Import (C, C_Create_Enum_Value, "ada_genum_create_enum_value");
   --  The strings Name and Nick are duplicated.

   function C_Register_Static
     (Name   : String; Static_Values : System.Address) return GType;
   pragma Import (C, C_Register_Static, "g_enum_register_static");

   ----------------------------------
   -- Generic_Enumeration_Property --
   ----------------------------------

   package body Generic_Enumeration_Property is

      function Conv is new Unchecked_Conversion (Enumeration, Gint);
      --  Note: this will raise a warning at compilation-time if no
      --  representation clause has been defined for Enumeration. As
      --  documented, Set_Property will not work anyway, so a warning
      --  is even a good thing!

      --------------
      -- Get_Type --
      --------------

      function Get_Type return GType is
         Arr : Enum_Value_Array_Access;
         --  The memory for Arr is voluntarily not freed, since we need
         --  to use if at any time in the life of the GtkAda application.

         J : Guint := 1;
      begin
         if The_Type /= GType_Invalid then
            return The_Type;
         end if;

         Arr := new Enum_Value_Array
           (1 .. Enumeration'Pos (Enumeration'Last) + 2);
         for E in Enumeration'Range loop
            declare
               S : aliased String := Enumeration'Image (E) & ASCII.Nul;
            begin
               C_Create_Enum_Value (Conv (E), S'Address, S'Address, Arr (J));
            end;
            J := J + 1;
         end loop;
         C_Create_Enum_Value
           (0, System.Null_Address, System.Null_Address, Arr (Arr'Last));

         The_Type := C_Register_Static (Name & ASCII.Nul, Arr.all'Address);
         return The_Type;
      end Get_Type;

      ---------------
      -- Gnew_Enum --
      ---------------

      function Gnew_Enum
        (Name, Nick, Blurb : String;
         Default           : Enumeration := Enumeration'First;
         Flags : Param_Flags := Param_Readable or Param_Writable)
         return Param_Spec
      is
         function Internal
           (Name, Nick, Blurb : String;
            Enum_Type         : GType;
            Default           : Enumeration;
            Flags             : Param_Flags) return Param_Spec;
         pragma Import (C, Internal, "g_param_spec_enum");
      begin
         return Internal
           (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
            Enum_Type => Get_Type,
            Default   => Default,
            Flags     => Flags);
      end Gnew_Enum;

      --------------
      -- Set_Enum --
      --------------

      procedure Set_Enum (Value : in out GValue; Enum : Enumeration) is
         procedure Internal (Value : in out GValue; Enum : Enumeration);
         pragma Import (C, Internal, "g_value_set_enum");
      begin
         Init (Value, Get_Type);
         Internal (Value, Enum);
      end Set_Enum;
   end Generic_Enumeration_Property;


   ----------------------------------------
   -- Generic_Internal_Discrete_Property --
   ----------------------------------------

   package body Generic_Internal_Discrete_Property is
      ------------------
      -- Get_Property --
      ------------------

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property_RO) return Discrete_Type
      is
         procedure Get
           (Object : System.Address;
            Name : Property;
            Value : out Discrete_Type;
            Last : System.Address := System.Null_Address);
         pragma Import (C, Get, "g_object_get");
         Ret : Discrete_Type;
      begin
         Get (Get_Object (Object), Property (Name), Ret);
         return Ret;
      end Get_Property;

      ------------------
      -- Get_Property --
      ------------------

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property) return Discrete_Type is
      begin
         return Get_Property (Object, Property_RO (Name));
      end Get_Property;

      ------------------
      -- Set_Property --
      ------------------

      procedure Set_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property;
         Value  : Discrete_Type)
      is
         procedure Internal
           (Object   : System.Address;
            Name     : Property;
            Value    : Discrete_Type;
            Null_Arg : System.Address := System.Null_Address);
         pragma Import (C, Internal, "g_object_set");
      begin
         Internal (Get_Object (Object), Name, Value);
      end Set_Property;
   end Generic_Internal_Discrete_Property;

   -------------------------------------
   -- Generic_Internal_Boxed_Property --
   -------------------------------------

   package body Generic_Internal_Boxed_Property is

      type Boxed_Access is access all Boxed_Type;
      function Conv is new Unchecked_Conversion
        (System.Address, Boxed_Access);

      ------------------
      -- Set_Property --
      ------------------

      procedure Set_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property;
         Value  : Boxed_Type)
      is
         procedure Internal
           (Object : System.Address;
            Name   : Property;
            Value  : in out GValue);
         pragma Import (C, Internal, "g_object_set_property");

         Boxed : Boxed_Access;
         Val : GValue;
      begin
         Init (Val, Get_Type);
         Boxed := new Boxed_Type' (Value);
         --  Memory is freed automatically by gtk+, see g_value_unset and
         --  gdk_color_get_type, where the free() function is registered

         Set_Address (Val, Boxed.all'Address);
         Internal (Get_Object (Object), Name, Val);
      end Set_Property;

      ------------------
      -- Get_Property --
      ------------------

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property) return Boxed_Type is
      begin
         return Get_Property (Object, Property_RO (Name));
      end Get_Property;

      ------------------
      -- Get_Property --
      ------------------

      function Get_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property_RO) return Boxed_Type
      is
         procedure Internal
           (Object : System.Address;
            Name   : Property_RO;
            Value  : in out GValue);
         pragma Import (C, Internal, "g_object_get_property");

         Boxed : Boxed_Access;
         Value : GValue;
      begin
         Init (Value, Get_Type);
         Internal (Get_Object (Object), Name, Value);
         Boxed := Conv (Get_Address (Value));
         return Boxed.all;
      end Get_Property;
   end Generic_Internal_Boxed_Property;

end Glib.Generic_Properties;
