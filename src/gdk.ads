-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

with System;

package Gdk is

   type Root_Type is abstract tagged private;


   function Is_Created (Object : in Root_Type) return Boolean;
   --  Test if the corresponding object has been created (ie the associated
   --  pointer is not null and the object is not tagged as destroyed.

   generic
      type To is new Root_Type with private;
   function Unchecked_Cast (From : in Root_Type'Class)
                            return To;
   --  This function allows the conversion any two widget types.
   --  Warning : no verification is done at Ada level. The only verification
   --  are done by gtk itself. Whenever possible, avoir using this function


   --  The following 2 services are for INTERNAL use only. They are not
   --  declared inside the private part for visibilty issues. Do NOT use them
   --  outside of the binding.
   --
   function Get_Object (Object : in Root_Type'Class) return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

private

   type Root_Type is abstract tagged
     record
        Ptr : System.Address := System.Null_Address;
     end record;

end Gdk;
