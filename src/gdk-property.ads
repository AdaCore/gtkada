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

with Glib; use Glib;

with Gdk.Types;
with Gdk.Window;

package Gdk.Property is

   function Atom_Intern (Atom_Name      : in String;
                         Only_If_Exists : in Boolean := True)
                         return              Gdk.Types.Gdk_Atom;

   function Atom_Name (Atom   : in Gdk.Types.Gdk_Atom) return String;

   procedure Get (Window               : in     Gdk.Window.Gdk_Window;
                  Property             : in     Gdk.Types.Gdk_Atom;
                  The_Type             : in     Gdk.Types.Gdk_Atom;
                  Offset               : in     Gulong;
                  Length               : in     Gulong;
                  Pdelete              : in     Boolean;
                  Actual_Property_Type :    out Gdk.Types.Gdk_Atom;
                  Actual_Format        :    out Gint;
                  Data                 :    out Guchar_Array_Access;
                  Success              :    out Boolean);

   procedure Change (Window    : in Gdk.Window.Gdk_Window;
                     Property  : in Gdk.Types.Gdk_Atom;
                     The_Type  : in Gdk.Types.Gdk_Atom;
                     Format    : in Gint;
                     Mode      : in Gdk.Types.Gdk_Prop_Mode;
                     Data      : in Guchar_Array);

   procedure Delete (Window   : in Gdk.Window.Gdk_Window;
                     Property : in Gdk.Types.Gdk_Atom);

private
   pragma Import (C, Delete, "gdk_property_delete");
end Gdk.Property;
