-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2000-2006, AdaCore              --
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

with GNAT.Strings;         use GNAT.Strings;
with Interfaces.C.Strings; use Interfaces.C, Interfaces.C.Strings;

package body Gtkada.Types is

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

   function Null_Array return Chars_Ptr_Array is
   begin
      return (1 .. 0 => Null_Ptr);
   end Null_Array;

   --------------------
   -- String_Or_Null --
   --------------------

   function String_Or_Null
     (S : String) return Interfaces.C.Strings.chars_ptr is
   begin
      if S = "" then
         return Null_Ptr;
      else
         return New_String (S);
      end if;
   end String_Or_Null;

   ----------
   -- Free --
   ----------

   procedure Free (C : in out Interfaces.C.Strings.chars_ptr_array) is
   begin
      for S in C'Range loop
         Free (C (S));
      end loop;
   end Free;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (C : Interfaces.C.Strings.chars_ptr_array) return String_List
   is
      Count : Natural := 0;
   begin
      while C (size_t (Count)) /= Null_Ptr loop
         Count := Count + 1;
      end loop;

      declare
         Result : String_List (1 .. Count);
      begin
         Count := 0;
         while C (size_t (Count)) /= Null_Ptr loop
            Result (Count + 1) := new String'(Value (C (size_t (Count))));
            Count := Count + 1;
         end loop;
         return Result;
      end;
   end To_String_List;

   ----------------------
   -- From_String_List --
   ----------------------

   function From_String_List
     (C : String_List) return Interfaces.C.Strings.chars_ptr_array
   is
      Result : Interfaces.C.Strings.chars_ptr_array (0 .. C'Length);
   begin
      for S in C'Range loop
         Result (size_t (S - C'First)) := New_String (C (S).all);
      end loop;
      Result (Result'Last) := Null_Ptr;
      return Result;
   end From_String_List;

end Gtkada.Types;
