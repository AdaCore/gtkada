-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                   Copyright (C) 2001 ACT-Europe                   --
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
--  This package provides an implementation for hooks used in
--  Gtk.Type_Conversion. These hooks should be used when you import a new
--  C GObject, so that GtkAda can recreate the Ada structure from the
--  underlying C structure.
--  Note that when you create a GObject directly in Ada, you do not need to
--  provide any hook.
--
--  Implementation note: This is a separate package from Gtk.Type_Conversion
--  so that adding a hook does not necessarily mean the user has to 'with'
--  Gtk.Type_Conversion, and thus all the packages from GtkAda.
--
--  Note that this package is not thread safe. You should call the
--  function Add_Hook from the elaboration part of your packages.
--
--  </description>

with Glib.Object;

package Glib.Type_Conversion_Hooks is

   type File_Conversion_Hook_Type is
     access function (Type_Name : String) return Glib.Object.GObject;
   --  This variable can be point to one of your functions.
   --  It gets the name of a C widget (ex/ "GtkButton") and should return
   --  a newly allocated Ada widget.

   type Hook_List;
   type Hook_List_Access is access Hook_List;
   type Hook_List is record
      Func : File_Conversion_Hook_Type;
      Next : Hook_List_Access := null;
   end record;
   --  Internal structure used for the list.

   procedure Add_Hook (Func : File_Conversion_Hook_Type);
   --  Add a new function to the list of hooks for file conversions.
   --  All the hooks are called when GtkAda finds a type which is not one of
   --  the standard types.

   function Conversion_Hooks return Hook_List_Access;
   --  Return the head of the hook list.

private
   File_Conversion_Hook : Hook_List_Access := null;

end Glib.Type_Conversion_Hooks;
