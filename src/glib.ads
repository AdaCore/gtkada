-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

--  <description>
--
--  This package provides definitions for the basic types used in Glib,
--  Gdk and Gtk.
--
--  </description>

with Ada.Unchecked_Deallocation;
with Interfaces.C;

package Glib is

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   ----------------------------------------
   --  The basic types  defined by glib  --
   ----------------------------------------

   type Gshort is new C.short;
   type Glong  is new C.long;
   type Gint   is new C.int;
   type Gchar  is new C.char;
   type Gboolean is new Gint;

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
   type Gshort_Array   is array (Natural range <>) of Gshort;
   type Glong_Array    is array (Natural range <>) of Glong;
   type Gint_Array     is array (Natural range <>) of Gint;
   type Guint_Array    is array (Natural range <>) of Guint;
   type Guint32_Array  is array (Natural range <>) of Guint32;
   type Gushort_Array  is array (Natural range <>) of Gushort;
   type Gulong_Array   is array (Natural range <>) of Gulong;
   type Gfloat_Array   is array (Natural range <>) of Gfloat;
   type Guchar_Array   is array (Natural range <>) of Guchar;
   type Gdouble_Array  is array (Natural range <>) of Gdouble;

   type Boolean_Array  is array (Natural range <>) of Boolean;

   type Short_Array    is array (Natural range <>) of C.short;
   type Long_Array     is array (Natural range <>) of C.long;

   ---------------------------
   --  Conversion services  --
   ---------------------------

   function To_Boolean_Array (A : in Gboolean_Array) return Boolean_Array;
   --  Convert a C-style boolean array into an Ada-style array.

   function To_Boolean (Value : in Gboolean) return Boolean;
   --  Convert a C boolean into an Ada boolean.

   function To_Boolean (Value : in Gint) return Boolean;
   --  Convert a C int into an Ada boolean.

   function To_Boolean (Value : in Guint) return Boolean;
   --  Convert a C uint into an Ada boolean.

   function To_Gboolean (Bool : in Boolean) return Gboolean;
   --  Convert an Ada boolean into a C boolean.

   function To_Gint (Bool : in Boolean) return Gint;
   --  Convert an Ada boolean into a C int.

   -------------------------
   --  Some Access types  --
   -------------------------

   type Guchar_Array_Access is access Guchar_Array;

   --  <doc_ignore>
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Guchar_Array, Name => Guchar_Array_Access);
   --  </doc_ignore>

   type String_Ptr is access all String;

   ------------
   -- Quarks --
   ------------

   type GQuark is new Guint32;
   --  Represents a string internally in GtkAda. Once you know the
   --  equivalent for a string, you can always use it instead of the string,
   --  which provides a faster access for all the functions that use htables
   --  in GtkAda.
   --  There is a global htable that contains all the quarks defined in
   --  your application and GtkAda itself.

   Unknown_Quark : constant GQuark := 0;

   function Quark_From_String (Id : in String) return GQuark;
   --  Return, or create the quark associated with the string.
   --  Note that if the quark does not already exist, an entry is created for
   --  it in the global htable for quarks.

   function Quark_Try_String (Id : in String) return GQuark;
   --  Return the quark associated with the string, if it exists.
   --  If it does not exist, return Unknown_Quark.

end Glib;
