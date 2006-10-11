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
--  This package provides GtkAda specific types and their associated functions.
--
--  </description>

with GNAT.Strings;
with Interfaces.C.Strings;

package Gtkada.Types is
   pragma Preelaborate;

   package ICS renames Interfaces.C.Strings;

   Data_Error : exception;

   subtype Chars_Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Chars_Ptr_Array is Interfaces.C.Strings.chars_ptr_array;
   --  Removed types, since this package will in general be used within a
   --  package that already does a "use Interfaces.C.Strings", and then we have
   --  multiple definitions of Chars_Ptr.
   --  The definition is left in comment for the time being to help transition
   --  for code that depended on those.

   procedure g_free (Mem : ICS.chars_ptr);
   --  Free a C string returned from Gtk.

   Null_Ptr : Chars_Ptr renames Interfaces.C.Strings.Null_Ptr;

   function Null_Array return ICS.chars_ptr_array;
   --  Return a null array.
   pragma Inline (Null_Array);

   -------------------------------------
   --  Handling of arrays of Strings  --
   -------------------------------------
   --  The following functions provide a very convenient way to create
   --  C arrays of null terminated strings in Ada.
   --
   --  You can either create such a String on the fly, or declare a variable:
   --
   --     Signals : Chars_Ptr_Array := "clicked" + "missed" + "new signal";
   --
   --  which corresponds to the C declaration:
   --
   --     char *signals[] = @{"clicked", "missed", "new signal"@};
   --
   --  Note that you still need to manually call Free (Signals) if you want to
   --  release the memory dynamically allocated by the "+" functions.

   function "+" (S1, S2 : String) return ICS.chars_ptr_array;
   --  Create an array containing S1 and S2.
   --  Note that this function allocates memory to store S1 and S2 as null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+"
     (S1 : ICS.chars_ptr_array; S2 : String) return ICS.chars_ptr_array;
   --  Append S2 to S1.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+"
     (S1 : ICS.chars_ptr_array; S2 : ICS.chars_ptr) return ICS.chars_ptr_array;
   --  Append S2 to S1.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+" (S1 : ICS.chars_ptr; S2 : String) return ICS.chars_ptr_array;
   --  Create an array containing S1 and S2.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated string. The user is responsible for calling Free on the
   --  resulting array.

   function String_Or_Null (S : String) return ICS.chars_ptr;
   --  Return Null_Ptr if S is the empty string, or a newly allocated string
   --  otherwise. This is intended mostly for the binding itself.

   type chars_ptr_array_access
     is access ICS.chars_ptr_array (Interfaces.C.size_t);
   --  Suitable for a C function that returns a gchar**

   function To_String_List
     (C : ICS.chars_ptr_array) return GNAT.Strings.String_List;
   --  Converts C into a String_List. Returned value must be freed by caller,
   --  as well as C.

   function From_String_List
     (C : GNAT.Strings.String_List) return ICS.chars_ptr_array;
   --  Converts C into a chars_ptr_array. Returned value must be freed by
   --  caller, as well as C.

   procedure Free (C : in out ICS.chars_ptr_array);
   --  Free the strings in C

private
   pragma Import (C, g_free, "g_free");
end Gtkada.Types;
