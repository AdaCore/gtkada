------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
--  The Pango.Coverage.Pango_Coverage structure represents a map from Unicode
--  characters to Pango.Enums.Coverage_Level. It is an opaque structure with no
--  public fields.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Pango.Enums; use Pango.Enums;

package Pango.Coverage is

   type Pango_Coverage_Record is new GObject_Record with null record;
   type Pango_Coverage is access all Pango_Coverage_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Pango_Coverage);
   --  Create a new Pango.Coverage.Pango_Coverage

   procedure Initialize (Self : not null access Pango_Coverage_Record'Class);
   --  Create a new Pango.Coverage.Pango_Coverage
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Pango_Coverage_New return Pango_Coverage;
   --  Create a new Pango.Coverage.Pango_Coverage

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_coverage_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Self : not null access Pango_Coverage_Record) return Pango_Coverage;
   --  Copy an existing Pango.Coverage.Pango_Coverage. (This function may now
   --  be unnecessary since we refcount the structure. File a bug if you use
   --  it.)

   function Get
      (Self  : not null access Pango_Coverage_Record;
       Index : Glib.Gint) return Pango.Enums.Coverage_Level;
   --  Determine whether a particular index is covered by Coverage
   --  "index_": the index to check

   procedure Max
      (Self  : not null access Pango_Coverage_Record;
       Other : not null access Pango_Coverage_Record'Class);
   pragma Obsolescent (Max);
   --  Set the coverage for each index in Coverage to be the max (better)
   --  value of the current coverage for the index and the coverage for the
   --  corresponding index in Other.
   --  Deprecated since 1.44, 1
   --  "other": another Pango.Coverage.Pango_Coverage

   function Ref
      (Self : not null access Pango_Coverage_Record) return Pango_Coverage;
   --  Increase the reference count on the Pango.Coverage.Pango_Coverage by
   --  one

   procedure Set
      (Self  : not null access Pango_Coverage_Record;
       Index : Glib.Gint;
       Level : Pango.Enums.Coverage_Level);
   --  Modify a particular index within Coverage
   --  "index_": the index to modify
   --  "level": the new level for Index_

   procedure Unref (Self : not null access Pango_Coverage_Record);
   --  Decrease the reference count on the Pango.Coverage.Pango_Coverage by
   --  one. If the result is zero, free the coverage and all associated memory.

end Pango.Coverage;
