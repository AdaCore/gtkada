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

with Gtk.Drawing_Area;
with Gdk.GL; use Gdk.GL;

package Gtk.GLArea is

   type Gtk_GLArea_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type Gtk_GLArea is access all Gtk_GLArea_Record'Class;

   type Attributes_Array is array (Natural range <>) of GL_Configs;
   --  Note: as opposed to what exists in C, you don't need to have
   --  the last element in the array be GDK_GL_NONE. This is done
   --  transparently by GtkAda itself.

   procedure Gtk_New (Widget    : out Gtk_GLArea;
                      Attr_List : in  Attributes_Array);
   procedure Initialize (Widget    : access Gtk_GLArea_Record;
                         Attr_List : in     Attributes_Array);
   procedure Gtk_New
     (Widget    :    out Gtk_GLArea;
      Attr_List : in     Attributes_Array;
      Share     : access Gtk_GLArea_Record'Class);
   procedure Initialize
     (Widget    : access Gtk_GLArea_Record;
      Attr_List : in     Attributes_Array;
      Share     : access Gtk_GLArea_Record'Class);

   function Make_Current
     (Glarea : access Gtk_GLArea_Record'Class)
     return Boolean;

   procedure Swap_Buffers (Glarea : access Gtk_GLArea_Record'Class);

private
   type Gtk_GLArea_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with null record;

end Gtk.GLArea;
