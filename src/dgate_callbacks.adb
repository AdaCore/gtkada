-----------------------------------------------------------------------
--                       DGATE Components                            --
--                                                                   --
--                      Copyright (C) 1999                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- Dynagate is free software;  you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation; either version 2 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
 
--  This package provides to DGATE a generic callback

with Ada.Text_IO;
with Unchecked_Conversion;
 
package body DGate_Callbacks is
 
   type String_Ptr is access all String;
   for String_Ptr'Size use System.Word_Size;
 
   function To_String is new Unchecked_Conversion
     (System.Address, String_Ptr);
 
   procedure Generic_Callback
     (Object : in Private_Object;
      Data   : in System.Address) is
   begin
      Ada.Text_IO.Put_Line ("Callback called.");
   end Generic_Callback;
 
end DGate_Callbacks;
