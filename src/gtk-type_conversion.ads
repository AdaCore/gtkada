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

package Gtk.Type_Conversion is

   procedure Init;
   --  This function has to be called to enable the full capacity for type
   --  conversions in GtkAda. If this function is not called, then
   --  converting a C widget to an Ada type will not be as exact (for
   --  instance, most C widget will get converted to a Gtk.Object, instead
   --  of the matching Ada widget).
   --  On the other hand, if you call this function (or with this package),
   --  then your application will 'with' all the GtkAda packages, and the
   --  initialization will be a little bit slower).

   type File_Conversion_Hook_Type is
     access function (Type_Name : String) return Root_Type_Access;
   File_Conversion_Hook : File_Conversion_Hook_Type := null;
   --  This variable can be point to one of your functions.
   --  It gets the name of a C widget (ex/ "GtkButton") and should return
   --  a newly allocated Ada widget.
   --  Its intended use is to provide a way to easily import new C widgets,
   --  that are not part of the standard gtk+ distribution.
   --  You do not need to set this variable for widgets that you created
   --  directly in Ada.

private
   function Full_Conversion (Obj  : System.Address; Stub : Root_Type'Class)
                             return Root_Type_Access;
   --  This function converts a C widget type to the correct Ada type.
   --  It has to be in a separate package so that its use is not mandatory
   --  (users who need this feature will simply 'with' this package).

end Gtk.Type_Conversion;
