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
--  This is the top level package of the Gdk widget hierarchy.
--  It provides a single type used to access the underlying C structures.
--
--  </description>

with System;
with Unchecked_Conversion;

package Gdk is
   pragma Preelaborate;

   type C_Dummy is limited private;
   type C_Proxy is access C_Dummy;
   pragma Convention (C, C_Proxy);
   --  General proxy for C structures.
   --  This type is used instead of System.Address so that the variables are
   --  automatically initialized to 'null'.
   --  The value pointed to is irrelevant, and in fact should not be accessed.
   --  It has thus been made limited private with no subprogram to access it.
   --  C_Proxy is a public type so that one can compare directly the value
   --  of the variables with 'null'.

   --  <doc_ignore>

   function Convert is new Unchecked_Conversion (System.Address, C_Proxy);
   function Convert is new Unchecked_Conversion (C_Proxy, System.Address);
   --  Converts from a System.Address returned by a C function to an
   --  internal C_Proxy.

   --  </doc_ignore>

private
   type C_Dummy is null record;
   --  This array can contain anything, since it is never used on the Ada side
   --  anyway.

end Gdk;
