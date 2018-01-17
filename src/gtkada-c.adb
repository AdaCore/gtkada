------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

package body Gtkada.C is

   ----------------------
   -- Unbounded_Arrays --
   ----------------------

   package body Unbounded_Arrays is

      --------------
      -- To_Array --
      --------------

      function To_Array
        (Arr : Unbounded_Array_Access; N : Index) return T_Array
      is
      begin
         if Arr = null then
            return (Index'Val (1) .. Index'Val (0) => Null_T);
         else
            declare
               Result : T_Array (Index'Val (1) .. N);
            begin
               for R in Index'Val (1) .. N loop
                  Result (R) := Arr (R);
               end loop;
               return Result;
            end;
         end if;
      end To_Array;
   end Unbounded_Arrays;

end Gtkada.C;
