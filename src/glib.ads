-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Ada.Unchecked_Deallocation;
with Interfaces.C;
with System;

package Glib is

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   ----------------------------------------
   --  The basic types  defined by glib  --
   ----------------------------------------

   type Gboolean is new C.char;

   type Gshort is new C.short;
   type Glong  is new C.long;
   type Gint   is new C.int;
   type Gchar  is new C.char;

   type Gushort is new C.unsigned_short;
   type Gulong  is new C.unsigned_long;
   type Guint   is new C.unsigned;
   type Guchar  is new C.unsigned_char;

   type Gfloat  is new C.C_float;
   type Gdouble is new C.double;

   subtype Gint8  is Gint range -(2 ** 7) .. (2 ** 7 - 1);
   subtype Gint16 is Gint range -(2 ** 15) .. (2 ** 15 - 1);
   subtype Gint32 is Gint range -(2 ** 31) .. (2 ** 31 - 1);

   subtype Guint8  is Guint range Guint'First .. (2 ** 8 - 1);
   subtype Guint16 is Guint range Guint'First .. (2 ** 16 - 1);
   subtype Guint32 is Guint range Guint'First .. (2 ** 32 - 1);


   ------------------------
   --  Some Array types  --
   ------------------------

   type Gboolean_Array is array (Natural range <>) of Gboolean;
   type Gshort_Array is array (Natural range <>) of Gshort;
   type Glong_Array is array (Natural range <>) of Glong;
   type Gint_Array is array (Natural range <>) of Gint;
   type Guint_Array is array (Natural range <>) of Guint;
   type Gushort_Array is array (Natural range <>) of Gushort;
   type Gulong_Array is array (Natural range <>) of Gulong;
   type Gfloat_Array is array (Natural range <>) of Gfloat;
   type Guchar_Array is array (Natural range <>) of Guchar;

   type Boolean_Array is array (Natural range <>) of Boolean;

   type Short_Array is array (Natural range <>) of C.short;
   type Long_Array is array (Natural range <>) of C.long;

   ---------------------------
   --  Conversion services  --
   ---------------------------

   function To_Boolean_Array (A : in Gboolean_Array) return Boolean_Array;
   function To_Boolean (Value : in Gboolean) return Boolean;
   function To_Boolean (Value : in Gint) return Boolean;
   function To_Boolean (Value : in Guint) return Boolean;
   function To_Gboolean (Bool : in Boolean) return Gboolean;
   function To_Gint (Bool : in Boolean) return Gint;


   -------------------------
   --  Some Access types  --
   -------------------------

   type Guchar_Array_Access is access Guchar_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Guchar_Array, Name => Guchar_Array_Access);

   type String_Ptr is access all String;

   -------------------
   --  Object_Type  --
   -------------------

   type Object_Type is private;
   Null_Object_Type : constant Object_Type;


   function Is_Created (Object : in Object_Type) return Boolean;
   --  Returns True if the associated C object has been created.

   --  The following services are for INTERNAL use only. They are not
   --  declared inside the private part for visibility issues. Do NOT
   --  use them outside of the binding.
   function Get_Object (Object : in Object_Type) return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Object_Type;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

private

   type Object_Type is
     record
        Ptr : System.Address := System.Null_Address;
     end record;

   Null_Object_Type : constant Object_Type := (Ptr => System.Null_Address);

end Glib;






