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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Glib.Object; use Glib.Object;
with Glib.Values; use Glib.Values;

package body Glib.Properties.Creation is

   procedure Internal_Set_Property_Handler
     (Object        : System.Address;
      Prop_Id       : Property_Id;
      Value         : GValue;
      Property_Spec : Param_Spec);
   --  Internal handler for Set_Property. This is the one called directly by
   --  gtk+, and that, in turns, calls the one defined by the user, after
   --  converting Object to a valid Ada object.

   procedure Internal_Get_Property_Handler
     (Object        : System.Address;
      Prop_Id       : Property_Id;
      Value         : out GValue;
      Property_Spec : Param_Spec);
   --  Same as above for the Get_Property handler.

   ----------------
   -- Pspec_Name --
   ----------------

   function Pspec_Name (Param : Param_Spec) return String is
      function Internal (Param : Param_Spec)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gparam_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Param));
   end Pspec_Name;

   -----------------
   -- Description --
   -----------------

   function Description (Param : Param_Spec) return String is
      function Internal (Param : Param_Spec)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_param_get_blurb");
   begin
      return Interfaces.C.Strings.Value (Internal (Param));
   end Description;

   ---------------
   -- Nick_Name --
   ---------------

   function Nick_Name (Param : Param_Spec) return String is
      function Internal (Param : Param_Spec)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_param_get_nick");
   begin
      return Interfaces.C.Strings.Value (Internal (Param));
   end Nick_Name;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Char) return Glib.Gint8 is
      function Internal (Param : Param_Spec_Char) return Gint8;
      pragma Import (C, Internal, "ada_gparam_get_minimum_char");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Char) return Glib.Gint8 is
      function Internal (Param : Param_Spec_Char) return Gint8;
      pragma Import (C, Internal, "ada_gparam_get_maximum_char");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Char) return Glib.Gint8 is
      function Internal (Param : Param_Spec_Char) return Gint8;
      pragma Import (C, Internal, "ada_gparam_get_default_char");
   begin
      return Internal (Param);
   end Default;

   ---------------
   -- Gnew_Char --
   ---------------

   function Gnew_Char
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gint8;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Glib.Gint8;
         Flags                     : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_char");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Char;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Uchar) return Glib.Guint8 is
      function Internal (Param : Param_Spec_Uchar) return Guint8;
      pragma Import (C, Internal, "ada_gparam_get_minimum_uchar");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Uchar) return Glib.Guint8 is
      function Internal (Param : Param_Spec_Uchar) return Guint8;
      pragma Import (C, Internal, "ada_gparam_get_maximum_uchar");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Uchar) return Glib.Guint8 is
      function Internal (Param : Param_Spec_Uchar) return Guint8;
      pragma Import (C, Internal, "ada_gparam_get_default_uchar");
   begin
      return Internal (Param);
   end Default;

   ----------------
   -- Gnew_Uchar --
   ----------------

   function Gnew_Uchar
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Guint8;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Glib.Guint8;
         Flags                     : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_uchar");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Uchar;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Boolean) return Boolean is
      function Internal (Param : Param_Spec_Boolean) return Boolean;
      pragma Import (C, Internal, "ada_gparam_get_default_boolean");
   begin
      return Internal (Param);
   end Default;

   ------------------
   -- Gnew_Boolean --
   ------------------

   function Gnew_Boolean
     (Name, Nick, Blurb : String;
      Default           : Boolean;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Default           : Boolean;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_uchar");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Default, Flags);
   end Gnew_Boolean;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Int) return Glib.Gint is
      function Internal (Param : Param_Spec_Int) return Gint;
      pragma Import (C, Internal, "ada_gparam_get_minimum_int");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Int) return Glib.Gint is
      function Internal (Param : Param_Spec_Int) return Gint;
      pragma Import (C, Internal, "ada_gparam_get_maximum_int");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Int) return Glib.Gint is
      function Internal (Param : Param_Spec_Int) return Gint;
      pragma Import (C, Internal, "ada_gparam_get_default_int");
   begin
      return Internal (Param);
   end Default;

   --------------
   -- Gnew_Int --
   --------------

   function Gnew_Int
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Gint;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_int");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Int;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Uint) return Glib.Guint is
      function Internal (Param : Param_Spec_Uint) return Guint;
      pragma Import (C, Internal, "ada_gparam_get_minimum_uint");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Uint) return Glib.Guint is
      function Internal (Param : Param_Spec_Uint) return Guint;
      pragma Import (C, Internal, "ada_gparam_get_maximum_uint");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Uint) return Glib.Guint is
      function Internal (Param : Param_Spec_Uint) return Guint;
      pragma Import (C, Internal, "ada_gparam_get_default_uint");
   begin
      return Internal (Param);
   end Default;

   ---------------
   -- Gnew_Uint --
   ---------------

   function Gnew_Uint
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Guint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Guint;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_uint");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Uint;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Long) return Glib.Glong is
      function Internal (Param : Param_Spec_Long) return Glong;
      pragma Import (C, Internal, "ada_gparam_get_minimum_long");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Long) return Glib.Glong is
      function Internal (Param : Param_Spec_Long) return Glong;
      pragma Import (C, Internal, "ada_gparam_get_maximum_long");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Long) return Glib.Glong is
      function Internal (Param : Param_Spec_Long) return Glong;
      pragma Import (C, Internal, "ada_gparam_get_default_long");
   begin
      return Internal (Param);
   end Default;

   ---------------
   -- Gnew_Long --
   ---------------

   function Gnew_Long
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Glong;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Glong;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_long");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Long;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Ulong) return Glib.Gulong is
      function Internal (Param : Param_Spec_Ulong) return Gulong;
      pragma Import (C, Internal, "ada_gparam_get_minimum_ulong");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Ulong) return Glib.Gulong is
      function Internal (Param : Param_Spec_Ulong) return Gulong;
      pragma Import (C, Internal, "ada_gparam_get_maximum_ulong");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Ulong) return Glib.Gulong is
      function Internal (Param : Param_Spec_Ulong) return Gulong;
      pragma Import (C, Internal, "ada_gparam_get_default_ulong");
   begin
      return Internal (Param);
   end Default;

   ----------------
   -- Gnew_Ulong --
   ----------------

   function Gnew_Ulong
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gulong;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Gulong;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_ulong");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Ulong;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Unichar) return Gunichar is
      function Internal (Param : Param_Spec_Unichar) return Gunichar;
      pragma Import (C, Internal, "ada_gparam_get_default_unichar");
   begin
      return Internal (Param);
   end Default;

   ------------------
   -- Gnew_Unichar --
   ------------------

   function Gnew_Unichar
     (Name, Nick, Blurb : String;
      Default           : Gunichar;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Default           : Gunichar;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_unichar");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Default, Flags);
   end Gnew_Unichar;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Enum) return Glib.Glong is
      function Internal (Param : Param_Spec_Enum) return Glong;
      pragma Import (C, Internal, "ada_gparam_get_default_enum");
   begin
      return Internal (Param);
   end Default;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Flags) return Glong is
      function Internal (Param : Param_Spec_Flags) return Glong;
      pragma Import (C, Internal, "ada_gparam_get_default_flags");
   begin
      return Internal (Param);
   end Default;

   ----------------
   -- Gnew_Flags --
   ----------------

   function Gnew_Flags
     (Name, Nick, Blurb : String;
      Flags_Type        : Glib.GType;
      Default           : Guint;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Flags_Type        : GType;
         Default           : Guint;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_flags");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Flags_Type, Default, Flags);
   end Gnew_Flags;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Float) return Gfloat is
      function Internal (Param : Param_Spec_Float) return Gfloat;
      pragma Import (C, Internal, "ada_gparam_get_minimum_gfloat");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Float) return Gfloat is
      function Internal (Param : Param_Spec_Float) return Gfloat;
      pragma Import (C, Internal, "ada_gparam_get_maximum_gfloat");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Float) return Gfloat is
      function Internal (Param : Param_Spec_Float) return Gfloat;
      pragma Import (C, Internal, "ada_gparam_get_default_gfloat");
   begin
      return Internal (Param);
   end Default;

   -------------
   -- Epsilon --
   -------------

   function Epsilon (Param : Param_Spec_Float) return Gfloat is
      function Internal (Param : Param_Spec_Float) return Gfloat;
      pragma Import (C, Internal, "ada_gparam_get_epsilon_gfloat");
   begin
      return Internal (Param);
   end Epsilon;

   ----------------
   -- Gnew_Float --
   ----------------

   function Gnew_Float
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gfloat;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Glib.Gfloat;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_float");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Float;

   -------------
   -- Minimum --
   -------------

   function Minimum (Param : Param_Spec_Double) return Gdouble is
      function Internal (Param : Param_Spec_Double) return Gdouble;
      pragma Import (C, Internal, "ada_gparam_get_minimum_gdouble");
   begin
      return Internal (Param);
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Param : Param_Spec_Double) return Gdouble is
      function Internal (Param : Param_Spec_Double) return Gdouble;
      pragma Import (C, Internal, "ada_gparam_get_maximum_gdouble");
   begin
      return Internal (Param);
   end Maximum;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_Double) return Gdouble is
      function Internal (Param : Param_Spec_Double) return Gdouble;
      pragma Import (C, Internal, "ada_gparam_get_default_gdouble");
   begin
      return Internal (Param);
   end Default;

   -------------
   -- Epsilon --
   -------------

   function Epsilon (Param : Param_Spec_Double) return Gdouble is
      function Internal (Param : Param_Spec_Double) return Gdouble;
      pragma Import (C, Internal, "ada_gparam_get_epsilon_gdouble");
   begin
      return Internal (Param);
   end Epsilon;

   -----------------
   -- Gnew_Double --
   -----------------

   function Gnew_Double
     (Name, Nick, Blurb         : String;
      Minimum, Maximum, Default : Glib.Gdouble;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Minimum, Maximum, Default : Glib.Gdouble;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_double");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Minimum, Maximum, Default, Flags);
   end Gnew_Double;

   -------------
   -- Default --
   -------------

   function Default (Param : Param_Spec_String) return String is
      function Internal (Param : Param_Spec_String) return chars_ptr;
      pragma Import (C, Internal, "ada_gparam_default_string");
      C : chars_ptr := Internal (Param);
   begin
      if C /= Null_Ptr then
         return Value (C);
      else
         return "";
      end if;
   end Default;

   ----------------
   -- Cset_First --
   ----------------

   function Cset_First (Param : Param_Spec_String) return String is
      function Internal (Param : Param_Spec_String) return chars_ptr;
      pragma Import (C, Internal, "ada_gparam_cset_first_string");
      C : chars_ptr := Internal (Param);
   begin
      if C /= Null_Ptr then
         return Value (C);
      else
         return "";
      end if;
   end Cset_First;

   --------------
   -- Cset_Nth --
   --------------

   function Cset_Nth (Param : Param_Spec_String) return String is
      function Internal (Param : Param_Spec_String) return chars_ptr;
      pragma Import (C, Internal, "ada_gparam_cset_nth_string");
      C : chars_ptr := Internal (Param);
   begin
      if C /= Null_Ptr then
         return Value (C);
      else
         return "";
      end if;
   end Cset_Nth;

   -----------------
   -- Substitutor --
   -----------------

   function Substitutor (Param : Param_Spec_String) return Character is
      function Internal (Param : Param_Spec_String) return Character;
      pragma Import (C, Internal, "ada_gparam_substitutor_string");
   begin
      return Internal (Param);
   end Substitutor;

   ---------------------
   -- Ensure_Non_Null --
   ---------------------

   function Ensure_Non_Null (Param : Param_Spec_String) return Boolean is
      function Internal (Param : Param_Spec_String) return Boolean;
      pragma Import (C, Internal, "ada_gparam_ensure_non_null_string");
   begin
      return Internal (Param);
   end Ensure_Non_Null;

   -----------------
   -- Gnew_String --
   -----------------

   function Gnew_String
     (Name, Nick, Blurb : String;
      Default           : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Default           : String;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_string");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Default & ASCII.Nul, Flags);
   end Gnew_String;

   ----------------
   -- Gnew_Param --
   ----------------

   function Gnew_Param
     (Name, Nick, Blurb : String;
      Param_Type        : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Param_Type        : GType;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_param");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Param_Type, Flags);
   end Gnew_Param;

   ----------------
   -- Gnew_Boxed --
   ----------------

   function Gnew_Boxed
     (Name, Nick, Blurb : String;
      Boxed_Type        : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Boxed_Type        : GType;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_boxed");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Boxed_Type, Flags);
   end Gnew_Boxed;

   ------------------
   -- Gnew_Pointer --
   ------------------

   function Gnew_Pointer
     (Name, Nick, Blurb : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_pointer");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul, Flags);
   end Gnew_Pointer;

   ------------------
   -- Gnew_Closure --
   ------------------

   function Gnew_Closure
     (Name, Nick, Blurb : String;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_closure");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul, Flags);
   end Gnew_Closure;

   -----------------
   -- Gnew_Object --
   -----------------

   function Gnew_Object
     (Name, Nick, Blurb : String;
      Object_Type       : Glib.GType;
      Flags : Param_Flags := Param_Readable or Param_Writable)
      return Param_Spec
   is
      function Internal
        (Name, Nick, Blurb : String;
         Object_Type       : GType;
         Flags             : Param_Flags) return Param_Spec;
      pragma Import (C, Internal, "g_param_spec_object");
   begin
      return Internal
        (Name & ASCII.Nul, Nick & ASCII.Nul, Blurb & ASCII.Nul,
         Object_Type, Flags);
   end Gnew_Object;

   -----------
   -- Value --
   -----------

   function Value (Val : Enum_Value) return Gint is
      function Internal (Val : Enum_Value) return Gint;
      pragma Import (C, Internal, "ada_genum_get_value");
   begin
      return Internal (Val);
   end Value;

   ----------
   -- Name --
   ----------

   function Name (Val : Enum_Value) return String is
      function Internal (Val : Enum_Value) return chars_ptr;
      pragma Import (C, Internal, "ada_genum_get_name");
   begin
      return Value (Internal (Val));
   end Name;

   ----------
   -- Nick --
   ----------

   function Nick (Val : Enum_Value) return String is
      function Internal (Val : Enum_Value) return chars_ptr;
      pragma Import (C, Internal, "ada_genum_get_nick");
   begin
      return Value (Internal (Val));
   end Nick;

   ---------------
   -- Nth_Value --
   ---------------

   function Nth_Value (Klass : Enum_Class; Nth : Glib.Guint)
      return Enum_Value
   is
      function Internal (Klass : Enum_Class; Nth : Guint) return Enum_Value;
      pragma Import (C, Internal, "ada_genum_nth_value");
   begin
      return Internal (Klass, Nth);
   end Nth_Value;

   -----------
   -- Value --
   -----------

   function Value (Val : Flags_Value) return Flags_Int_Value is
      function Internal (Val : Flags_Value) return Flags_Int_Value;
      pragma Import (C, Internal, "ada_gflags_get_value");
   begin
      return Internal (Val);
   end Value;

   ----------
   -- Name --
   ----------

   function Name (Val : Flags_Value) return String is
      function Internal (Val : Flags_Value) return chars_ptr;
      pragma Import (C, Internal, "ada_gflags_get_name");
   begin
      return Value (Internal (Val));
   end Name;

   ----------
   -- Nick --
   ----------

   function Nick (Val : Flags_Value) return String is
      function Internal (Val : Flags_Value) return chars_ptr;
      pragma Import (C, Internal, "ada_gflags_get_nick");
   begin
      return Value (Internal (Val));
   end Nick;

   ---------------
   -- Nth_Value --
   ---------------

   function Nth_Value (Klass : Flags_Class; Nth : Glib.Guint)
      return Flags_Value
   is
      function Internal (Klass : Flags_Class; Nth : Guint) return Flags_Value;
      pragma Import (C, Internal, "ada_gflags_nth_value");
   begin
      return Internal (Klass, Nth);
   end Nth_Value;

   -----------------------------------
   -- Internal_Set_Property_Handler --
   -----------------------------------

   procedure Internal_Set_Property_Handler
     (Object        : System.Address;
      Prop_Id       : Property_Id;
      Value         : GValue;
      Property_Spec : Param_Spec)
   is
      function Real (Object : System.Address) return Set_Property_Handler;
      pragma Import (C, Real, "ada_real_set_property_handler");
      Stub : GObject_Record;
      Obj : GObject := Get_User_Data (Object, Stub);
   begin
      Real (Object) (Obj, Prop_Id, Value, Property_Spec);
   end Internal_Set_Property_Handler;

   -----------------------------------
   -- Internal_Get_Property_Handler --
   -----------------------------------

   procedure Internal_Get_Property_Handler
     (Object        : System.Address;
      Prop_Id       : Property_Id;
      Value         : out GValue;
      Property_Spec : Param_Spec)
   is
      function Real (Object : System.Address) return Get_Property_Handler;
      pragma Import (C, Real, "ada_real_get_property_handler");
      Stub : GObject_Record;
      Obj : GObject := Get_User_Data (Object, Stub);
   begin
      Real (Object) (Obj, Prop_Id, Value, Property_Spec);
   end Internal_Get_Property_Handler;

   -----------------------------
   -- Set_Properties_Handlers --
   -----------------------------

   procedure Set_Properties_Handlers
     (Class_Record : GObject_Class;
      Set_Property : Set_Property_Handler;
      Get_Property : Get_Property_Handler)
   is
      procedure Set_Set
        (Class_Record : GObject_Class; Set_Prop : Set_Property_Handler);
      pragma Import (C, Set_Set, "ada_set_real_set_property_handler");

      procedure Set_Get
        (Class_Record : GObject_Class; Set_Prop : Get_Property_Handler);
      pragma Import (C, Set_Get, "ada_set_real_get_property_handler");

      procedure Internal
        (Class_Record : GObject_Class;
         Set_Property : System.Address;
         Get_Property : System.Address);
      pragma Import (C, Internal, "ada_set_properties_handlers");

   begin
      Set_Set (Class_Record, Set_Property);
      Set_Get (Class_Record, Get_Property);
      Internal (Class_Record,
                Internal_Set_Property_Handler'Address,
                Internal_Get_Property_Handler'Address);
   end Set_Properties_Handlers;

end Glib.Properties.Creation;
