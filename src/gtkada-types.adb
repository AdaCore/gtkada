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

package body Gtkada.Types is

   use Interfaces.C.Strings;

   ---------
   -- "+" --
   ---------

   function "+" (S1, S2 : String) return Chars_Ptr_Array is
   begin
      return (0 => New_String (S1), 1 => New_String (S2));
   end "+";

   function "+" (S1 : Chars_Ptr_Array; S2 : String) return Chars_Ptr_Array is
   begin
      return S1 + New_String (S2);
   end "+";

   function "+" (S1 : Chars_Ptr_Array; S2 : Chars_Ptr)
     return Chars_Ptr_Array
   is
      use type Interfaces.C.size_t;

      Result : Chars_Ptr_Array (S1'First .. S1'Last + 1);

   begin
      Result (S1'Range) := S1;
      Result (S1'Last + 1) := S2;
      return Result;
   end "+";

   function "+" (S1 : Chars_Ptr; S2 : String) return Chars_Ptr_Array is
   begin
      return (0 => S1, 1 => New_String (S2));
   end "+";

   ----------
   -- Free --
   ----------

   procedure Free (A : in out Chars_Ptr_Array) is
   begin
      for J in A'Range loop
         Interfaces.C.Strings.Free (A (J));
      end loop;
   end Free;

   function Null_Array return Chars_Ptr_Array is
   begin
      return (1 .. 0 => Null_Ptr);
   end Null_Array;

end Gtkada.Types;
