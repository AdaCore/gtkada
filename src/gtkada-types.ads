------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

--  <description>
--
--  This package provides GtkAda specific types and their associated functions.
--
--  </description>

with Interfaces.C.Strings;

package Gtkada.Types is

   pragma Preelaborate;

   Data_Error : exception;

   subtype Chars_Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Chars_Ptr_Array is Interfaces.C.Strings.chars_ptr_array;

   procedure g_free (Mem : Chars_Ptr);
   --  Free a C string returned from Gtk

   Null_Ptr : Chars_Ptr renames Interfaces.C.Strings.Null_Ptr;

   function Null_Array return Chars_Ptr_Array;
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

   function "+" (S1, S2 : String) return Chars_Ptr_Array;
   --  Create an array containing S1 and S2.
   --  Note that this function allocates memory to store S1 and S2 as null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+" (S1 : Chars_Ptr_Array; S2 : String) return Chars_Ptr_Array;
   --  Append S2 to S1.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+" (S1 : Chars_Ptr_Array; S2 : Chars_Ptr) return Chars_Ptr_Array;
   --  Append S2 to S1.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated Strings. The user is responsible for calling Free on the
   --  resulting array.

   function "+" (S1 : Chars_Ptr; S2 : String) return Chars_Ptr_Array;
   --  Create an array containing S1 and S2.
   --  Note that this function allocates memory to store S2 as a null
   --  terminated string. The user is responsible for calling Free on the
   --  resulting array.

   procedure Free (A : in out Chars_Ptr_Array);
   --  Free all the strings in A.

private
   pragma Import (C, g_free, "g_free");
end Gtkada.Types;
