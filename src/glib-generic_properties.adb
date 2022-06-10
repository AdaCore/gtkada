------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2022, AdaCore                     --
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

with Glib.Object;     use Glib.Object;
with Glib.Values;     use Glib.Values;
with Ada.Unchecked_Conversion;
with System;          use System;

--  Can't with because of elaboration order issues
--  with Glib.Properties;

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

      function Conv is new Ada.Unchecked_Conversion (Enumeration, Gint);
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
               S : aliased String := Enumeration'Image (E) & ASCII.NUL;
            begin
               C_Create_Enum_Value (Conv (E), S'Address, S'Address, Arr (J));
            end;
            J := J + 1;
         end loop;
         C_Create_Enum_Value
           (0, System.Null_Address, System.Null_Address, Arr (Arr'Last));

         The_Type := C_Register_Static (Name & ASCII.NUL, Arr.all'Address);
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
           (Name & ASCII.NUL, Nick & ASCII.NUL, Blurb & ASCII.NUL,
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
            Name   : Property;
            Value  : out Gulong);
         pragma Import (C, Get, "ada_g_object_get_ulong");

         pragma Warnings (Off);
         function To_Discrete_Type is new
           Ada.Unchecked_Conversion (Gulong, Discrete_Type);
         pragma Warnings (On);
         Ret : Gulong;

      begin
         Get (Get_Object (Object), Property (Name), Ret);
         return To_Discrete_Type (Ret);
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
            Value    : Gulong);
         pragma Import (C, Internal, "ada_g_object_set_ulong");

         pragma Warnings (Off);
         function To_Gulong is new
           Ada.Unchecked_Conversion (Discrete_Type, Gulong);
         pragma Warnings (On);

      begin
         Internal (Get_Object (Object), Name, To_Gulong (Value));
      end Set_Property;
   end Generic_Internal_Discrete_Property;

   -------------------------------------
   -- Generic_Internal_Boxed_Property --
   -------------------------------------

   package body Generic_Internal_Boxed_Property is

      type Boxed_Access is access all Boxed_Type;
      function Conv is new Ada.Unchecked_Conversion
        (System.Address, Boxed_Access);

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Value  : in out Glib.Values.GValue;
         Val    : Boxed_Type)
      is
         V : constant System.Address := To_Address (Val, Val'Address);
         T : constant Glib.GType := Type_Of (Value);
         T2 : constant Glib.GType := Get_Type;
      begin
         --  Do not do an actual copy with gdk_color_copy, or
         --  pango_font_description_copy: it is in fact done
         --  automatically by gdk, when it defines (in gdkcolor.c) the type as
         --  g_boxed_type_register_static
         --   ("GdkColor",(GBoxedCopyFunc)gdk_color_copy,
         --    (GBoxedFreeFunc)gdk_color_free);

         if T = GType_Invalid then
            Init (Value, T2);
         elsif T /= T2 then
            Unset (Value);
            Init (Value, Get_Type);
         end if;

         Set_Boxed (Value, V);
      end Set_Value;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value (Value : Glib.Values.GValue) return Boxed_Type is
         Boxed : constant Boxed_Access := Conv (Get_Boxed (Value));
      begin
         if Boxed = null then
            raise Unset_Value;
         else
            return Boxed.all;
         end if;
      end Get_Value;

      ------------------
      -- Set_Property --
      ------------------

      procedure Set_Property
        (Object : access Glib.Object.GObject_Record'Class;
         Name   : Property;
         Value  : Boxed_Type)
      is
         procedure Internal
           (Object : System.Address; Name : String; Value : in out GValue);
         pragma Import (C, Internal, "g_object_set_property");
         --  Internal function to set the properties (can't use the one from
         --  glib.properties because of elaboration order issues

         Val : GValue;
      begin
         Set_Value (Value => Val, Val => Value);
         Internal (Get_Object (Object), Property_Name (Name) & ASCII.NUL, Val);
         Unset (Val);
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

         Value : GValue;
         Result : Boxed_Type;
      begin
         Init (Value, Get_Type);
         Internal (Get_Object (Object), Name, Value);
         Result := Get_Value (Value);
         Unset (Value);
         return Result;
      end Get_Property;
   end Generic_Internal_Boxed_Property;

end Glib.Generic_Properties;
