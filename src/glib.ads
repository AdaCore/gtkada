-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

package Glib is
   pragma Preelaborate;

   package C renames Interfaces.C;
   use type C.int;
   use type C.unsigned;

   -------------------------------------
   -- The basic types defined by glib --
   -------------------------------------

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

   type Gint8  is range -(2 ** 7) .. (2 ** 7 - 1);
   type Gint16 is range -(2 ** 15) .. (2 ** 15 - 1);
   type Gint32 is range -(2 ** 31) .. (2 ** 31 - 1);

   type Guint8  is mod 2 ** 8;
   type Guint16 is mod 2 ** 16;
   type Guint32 is mod 2 ** 32;

   ----------------------
   -- Some Array types --
   ----------------------

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

   -------------------------
   -- Conversion services --
   -------------------------

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

   -----------------------
   -- Some Access types --
   -----------------------

   type Guchar_Array_Access is access Guchar_Array;

   type String_Ptr is access all String;

   --  <doc_ignore>
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Guchar_Array, Name => Guchar_Array_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String, Name => String_Ptr);
   --  </doc_ignore>

   --  <doc_ignore>
   type C_Dummy is limited private;
   --  </doc_ignore>

   type C_Proxy is access C_Dummy;
   --  General proxy for C structures.
   --  This type is used instead of System.Address so that the variables are
   --  automatically initialized to 'null'.
   --  The value pointed to is irrelevant, and in fact should not be accessed.
   --  It has thus been made limited private with no subprogram to access it.
   --  C_Proxy is a public type so that one can compare directly the value
   --  of the variables with 'null'.

   --  <doc_ignore>
   pragma Convention (C, C_Proxy);

   function Convert is new Ada.Unchecked_Conversion (System.Address, C_Proxy);
   function Convert is new Ada.Unchecked_Conversion (C_Proxy, System.Address);
   --  Converts from a System.Address returned by a C function to an
   --  internal C_Proxy.

   --  </doc_ignore>

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

   -----------
   -- GType --
   -----------

   type GType is new Guint32;
   --  This type describes an internal type in Glib.
   --  You shouldn't have to use it in your own applications, however it might
   --  be useful sometimes.
   --  Every object type is associated with a specific value, created
   --  dynamically at run time the first time you instantiate an object of that
   --  type (thus if you have never used e.g a Gtk_File_Selection, it won't
   --  have any GType associated with it).
   --  You can get the exact type value for each type by using the functions
   --  Get_Type provided in all the packages in GtkAda.
   --  You can get the specific value for an existing widget by using the
   --  function Gtk.Object.Get_Type.

   GType_Invalid   : constant GType := 0;
   GType_None      : constant GType := 1;
   GType_Interface : constant GType := 2;

   --  Glib type ids
   GType_Char      : constant GType := 3;
   GType_Uchar     : constant GType := 4;
   GType_Bool      : constant GType := 5;
   GType_Int       : constant GType := 6;
   GType_Uint      : constant GType := 7;
   GType_Long      : constant GType := 8;
   GType_Ulong     : constant GType := 9;
   GType_Enum      : constant GType := 10;
   GType_Flags     : constant GType := 11;

   GType_Float     : constant GType := 12;
   GType_Double    : constant GType := 13;
   GType_String    : constant GType := 14; --  Null terminated string.
   GType_Pointer   : constant GType := 15; --  a general pointer type.
   GType_Boxed     : constant GType := 16;
   GType_Param     : constant GType := 17;
   GType_Object    : constant GType := 18; --  One of the widgets/objects

private
   type C_Dummy is null record;
   --  This array can contain anything, since it is never used on the Ada side
   --  anyway.

end Glib;
