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

with System;

package Gdk is
   pragma Elaborate_Body;

   type Root_Type is abstract tagged private;
   type Root_Type_Access is access all Root_Type'Class;

   generic
      type To is new Root_Type with private;
      type To_Access is access all To'Class;
   package Unchecked_Cast is
      function Convert (From : access Root_Type'Class) return To_Access;
   private
      Returned : aliased To;
   end Unchecked_Cast;
   --  This function allows the conversion to any widget type.
   --  Note:
   --   The access type returned points to a global variable in this package,
   --   that will be overidden the next time you call Convert. You should make
   --   a copy of it if you need to reuse it.
   --  Warning : no verification is done at Ada level. The only verification
   --  are done by gtk itself. Whenever possible, avoid using this function
   --  See testgtk/create_notebook.adb for an example how to use this package.

   function Is_Created (Object : in Root_Type'Class) return Boolean;
   --  Return True if the associated C object has been created, False if no
   --  C object is associated with Object.


   --  The following services are for INTERNAL use only. They are not
   --  declared inside the private part for visibility issues. Do NOT
   --  use them outside of the binding.
   function Get_Object (Object : in Root_Type'Class) return System.Address;
   function Get_Object (Object : access Root_Type'Class) return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Object : in out Root_Type'Class;
                         Value  : in     System.Address);
   procedure Set_Object (Object : access Root_Type'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

private

   type Root_Type is abstract tagged
     record
        Ptr : System.Address := System.Null_Address;
     end record;

end Gdk;
