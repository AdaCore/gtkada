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

--  <description>
--  This package provides an interface to generic values as used in the
--  Glib object model.
--
--  The main type in this package is GValues, which is the
--  equivalent of the C's (GValue*) array, i.e an array of unions.  This
--  package provides functions to extract the values from this type.
--  </description>


with System;

package Glib.Values is

   --  <doc_ignore>Do not create automatic documentation for this package

   type GValue is private;
   --  A generic value that can hold any of the types as provided in the
   --  Set and Get functions below.

   type GValues is private;
   --  This type represents a table of values. Each argument of the
   --  table can be of any type.
   --  The index of the first element is always 1.

   function Make_Values (Nb : Guint; Val : System.Address) return GValues;
   --  Build a Gvalues structure from the given C array. Nb should be the
   --  number of elements in the Values array.

   function Nth (Val : GValues; Num : Guint) return GValue;
   --  Return the Num-th element from Values.

   --------------------------------------------------
   -- Conversion functions, interfacing to GValues --
   --------------------------------------------------

   procedure Set_Char (Value : GValue; V_Char : Gchar);
   function  Get_Char (Value : GValue) return Gchar;

   procedure Set_Uchar (Value : GValue; V_Uchar : Guchar);
   function  Get_Uchar (Value : GValue) return Guchar;

   procedure Set_Boolean (Value : GValue; V_Boolean : Boolean);
   function  Get_Boolean (Value : GValue) return Boolean;

   procedure Set_Int (Value : GValue; V_Int : Gint);
   function  Get_Int (Value : GValue) return Gint;

   procedure Set_Uint (Value : GValue; V_Uint : Guint);
   function  Get_Uint (Value : GValue) return Guint;

   procedure Set_Long (Value : GValue; V_Long : Glong);
   function  Get_Long (Value : GValue) return Glong;

   procedure Set_Ulong (Value : GValue; V_Ulong : Gulong);
   function  Get_Ulong (Value : GValue) return Gulong;

   procedure Set_Float (Value : GValue; V_Float : Gfloat);
   function  Get_Float (Value : GValue) return Gfloat;

   procedure Set_Double (Value : GValue; V_Double : Gdouble);
   function  Get_Double (Value : GValue) return Gdouble;

   procedure Set_String (Value : GValue; V_String : String);
   function  Get_String (Value : GValue) return String;
   function  Get_String (Value : GValue; Length : Gint) return String;

   procedure Set_Proxy (Value : GValue; V_Proxy : C_Proxy);
   function  Get_Proxy (Value : GValue) return C_Proxy;

   procedure Set_Address (Value : GValue; V_Address : System.Address);
   function  Get_Address (Value : GValue) return System.Address;

private
   type GValue is new System.Address;

   type GValues is record
      Nb  : Guint;
      Arr : System.Address;
   end record;

   pragma Import (C, Set_Char, "g_value_set_char");
   pragma Import (C, Get_Char, "g_value_get_char");
   pragma Import (C, Set_Uchar, "g_value_set_uchar");
   pragma Import (C, Get_Uchar, "g_value_get_uchar");
   pragma Import (C, Set_Int, "g_value_set_int");
   pragma Import (C, Get_Int, "g_value_get_int");
   pragma Import (C, Set_Uint, "g_value_set_uint");
   pragma Import (C, Get_Uint, "g_value_get_uint");
   pragma Import (C, Set_Long, "g_value_set_long");
   pragma Import (C, Get_Long, "g_value_get_long");
   pragma Import (C, Set_Ulong, "g_value_set_ulong");
   pragma Import (C, Get_Ulong, "g_value_get_ulong");
   pragma Import (C, Set_Float, "g_value_set_float");
   pragma Import (C, Get_Float, "g_value_get_float");
   pragma Import (C, Set_Double, "g_value_set_double");
   pragma Import (C, Get_Double, "g_value_get_double");
   pragma Import (C, Set_Proxy, "g_value_set_pointer");
   pragma Import (C, Get_Proxy, "ada_gvalue_get_pointer");
   pragma Import (C, Set_Address, "g_value_set_pointer");
   pragma Import (C, Get_Address, "ada_gvalue_get_pointer");

   pragma Inline (Make_Values);
   pragma Inline (Set_Boolean);
   pragma Inline (Get_Boolean);
   pragma Inline (Set_String);
   pragma Inline (Get_String);

   --  </doc_ignore>
end Glib.Values;
