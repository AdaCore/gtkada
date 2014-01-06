------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Pango.Enums; use Pango.Enums;

package Pango.Coverage is

   type Pango_Coverage is new Glib.C_Boxed with null record;
   Null_Pango_Coverage : constant Pango_Coverage;

   function From_Object (Object : System.Address) return Pango_Coverage;
   function From_Object_Free (B : access Pango_Coverage'Class) return Pango_Coverage;
   pragma Inline (From_Object_Free, From_Object);

   -------------
   -- Methods --
   -------------

   function Copy (Self : Pango_Coverage) return Pango_Coverage;
   --  Copy an existing Pango.Coverage.Pango_Coverage. (This function may now
   --  be unnecessary since we refcount the structure. File a bug if you use
   --  it.)

   function Get
      (Self  : Pango_Coverage;
       Index : Gint) return Pango.Enums.Coverage_Level;
   --  Determine whether a particular index is covered by Coverage
   --  "index_": the index to check

   procedure Max (Self : Pango_Coverage; Other : Pango_Coverage);
   --  Set the coverage for each index in Coverage to be the max (better)
   --  value of the current coverage for the index and the coverage for the
   --  corresponding index in Other.
   --  "other": another Pango.Coverage.Pango_Coverage

   function Ref (Self : Pango_Coverage) return Pango_Coverage;
   --  Increase the reference count on the Pango.Coverage.Pango_Coverage by
   --  one

   procedure Set
      (Self  : Pango_Coverage;
       Index : Gint;
       Level : Pango.Enums.Coverage_Level);
   --  Modify a particular index within Coverage
   --  "index_": the index to modify
   --  "level": the new level for Index_

   procedure Unref (Self : Pango_Coverage);
   --  Decrease the reference count on the Pango.Coverage.Pango_Coverage by
   --  one. If the result is zero, free the coverage and all associated memory.

   ---------------
   -- Functions --
   ---------------

   function Pango_New return Pango_Coverage;
   --  Create a new Pango.Coverage.Pango_Coverage

private

   Null_Pango_Coverage : constant Pango_Coverage := (Glib.C_Boxed with null record);

end Pango.Coverage;
