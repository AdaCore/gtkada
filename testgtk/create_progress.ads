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

with Gtk.Button;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Widget;      use Gtk.Widget;
with Common;          use Common;

package Create_Progress is

   procedure Run (Widget : in out Gtk.Button.Gtk_Button);

   type String10 is new String (1 .. 10);
   type Array_Of_String is array (Natural range <>) of String10;

   procedure Build_Option_Menu (Omenu   : out Gtk_Option_Menu;
                                Gr      : out Widget_Slist.GSlist;
                                Items   : Array_Of_String;
                                History : Natural;
                                Cb      : Widget_Cb.Callback);

end Create_Progress;
