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

with System;

package Gdk is
   pragma Elaborate_Body;

   type Root_Type is tagged private;
   type Root_Type_Access is access all Root_Type'Class;
   --  The base type of the hierarchy in GtkAda. It basically gives access
   --  to an underlying C object. This is not a controlled type, for efficiency
   --  reasons, and because gtk+ takes care of memory management on its own.

   type C_Dummy_Record is limited private;
   type C_Proxy is access C_Dummy_Record;
   pragma Convention (C, C_Proxy);
   --  General proxy for C structures.
   --  This type is used instead of System.Address so that the variables are
   --  automatically initialized to 'null'.
   --  The value pointed to is irrelevant, and in fact should not be accessed.
   --  It has thus been made limited private with no subprogram to access it.
   --  C_Proxy is a public type so that one can compare directly the value
   --  of the variables with 'null'.

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
   --  are done by gtk itself.
   --  See also the package Gtk.Type_Conversion for help on how to convert
   --  from C widgets to Ada widgets.
   --
   --  This package should not be required at all. If you absolutely need to
   --  use it, please report it to the GtkAda team (gtkada@ada.eu.org) so that
   --  we try and get rid of this case if possible.

   function Is_Created (Object : in Root_Type'Class) return Boolean;
   --  Return True if the associated C object has been created, False if no
   --  C object is associated with Object.
   --  This is not the same as testing whether an access type (for instance
   --  any of the widgets) is "null", since this relates to the underlying
   --  C object.

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

   type Root_Type is tagged record
      Ptr : System.Address := System.Null_Address;
   end record;

   type C_Dummy_Record is null record;
   --  This array can contain anything, since it is never used on the Ada side
   --  anyway.
end Gdk;
