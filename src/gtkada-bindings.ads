------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  This is a unit purely internal to GtkAda, to ease binding and avoid code
--  duplication.
--  Do not use in your own applications, since the interface might change from
--  release to release.
--  See also Gtkada.Types

--  with Gdk.Types;
with Glib;
--  with Glib.Object;
with Gtkada.C;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

package Gtkada.Bindings is
   package ICS renames Interfaces.C.Strings;

   generic
      type T is private;
      Null_T : T;
   function Generic_To_Address_Or_Null
     (Val : System.Address) return System.Address;
   --  Return either a Null_Address or a pointer to Val, depending on
   --  whether Val is the null value for the type.
   --  In all cases, Val is supposed to be an access to T.
   --  In Ada2012, these could be replaced with expression functions instead.

   function Value_And_Free
     (Str : Interfaces.C.Strings.chars_ptr) return String;
   --  Returns the value stored in Str, and free the memory occupied by Str.

   -------------
   -- Strings --
   -------------

   function String_Or_Null (S : String) return ICS.chars_ptr;
   --  Return Null_Ptr if S is the empty string, or a newly allocated string
   --  otherwise. This is intended mostly for the binding itself.

   type chars_ptr_array_access
     is access ICS.chars_ptr_array (Interfaces.C.size_t);
   pragma Convention (C, chars_ptr_array_access);
   --  Suitable for a C function that returns a gchar**

   procedure g_strfreev (Str_Array : in out chars_ptr_array_access);
   --  Thin binding to C function of the same name.  Frees a null-terminated
   --  array of strings, and the array itself.  If called on a null value,
   --  simply return.

   function To_String_List
     (C : ICS.chars_ptr_array) return GNAT.Strings.String_List;
   --  Converts C into a String_List. Returned value must be freed by caller,
   --  as well as C. C is NULL terminated.

   function To_String_List_And_Free
     (C : chars_ptr_array_access) return GNAT.Strings.String_List;
   --  Converts C into a String_List, and frees C.
   --  Returned value must be freed by caller.

   function To_String_List
     (C : ICS.chars_ptr_array; N : Glib.Gint)
      return GNAT.Strings.String_List;
   --  Converts C into a String_List. N is the number of elements in C.
   --  Returned value must be freed by caller, as well as C.

   function From_String_List
     (C : GNAT.Strings.String_List) return ICS.chars_ptr_array;
   --  Converts C into a chars_ptr_array. Returned value must be freed by
   --  caller, as well as C.

   function To_Chars_Ptr
     (C : chars_ptr_array_access) return ICS.chars_ptr_array;
   --  Return a bounded array that contains the same strings as C (so you
   --  shouldn't free C). 'Last applies to the result, whereas it doesn't to C.

   ------------
   -- Arrays --
   ------------
   --  See Gtkada.C for more information.
   --  The packages that are commented out are instanciated in various,
   --  possibly duplicated places. This is because of elaboration circularity
   --  issues.

   package Gint_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Gint, 0, Natural, Glib.Gint_Array);
   package Pspec_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Param_Spec, null, Natural, Glib.Param_Spec_Array);
   package GType_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.GType, Glib.GType_None, Glib.Guint, Glib.GType_Array);

   function To_Gint_Array_Zero_Terminated
     (Arr : Gint_Arrays.Unbounded_Array_Access)
      return Glib.Gint_Array;
   --  Converts Arr, stopping at the first 0 encountered

private
   pragma Import (C, g_strfreev, "g_strfreev");
end Gtkada.Bindings;
