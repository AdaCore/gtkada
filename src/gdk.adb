-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

package body Gdk is

   ------------------
   --  Get_Object  --
   ------------------

   function Get_Object (Object : in Root_Type'Class) return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   function Get_Object (Object : access Root_Type'Class) return System.Address
   is
   begin
      return Object.Ptr;
   end Get_Object;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (Object : in Root_Type'Class) return Boolean is
      use type System.Address;
   begin
      return Object.Ptr /= System.Null_Address;
   end Is_Created;

   ------------------
   --  Set_Object  --
   ------------------

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address) is
   begin
      Object.Ptr := Value;
   end Set_Object;

   procedure Set_Object (Object : access Root_Type'Class;
                         Value  : in     System.Address) is
   begin
      Object.Ptr := Value;
   end Set_Object;

   --------------------
   -- Unchecked_Cast --
   --------------------

   package body Unchecked_Cast is
      function Convert (From : access Root_Type'Class) return To_Access is
      begin
         Set_Object (Returned, Get_Object (From));
         return Returned'Access;
      end Convert;
   end Unchecked_Cast;

end Gdk;
