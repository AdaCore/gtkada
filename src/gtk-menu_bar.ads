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

with Gtk.Enums; use Gtk.Enums;
with Gtk.Object; use Gtk.Object;
with Gtk.Menu_Shell;
with Gtk.Menu_Item;

package Gtk.Menu_Bar is

   type Gtk_Menu_Bar_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with private;
   type Gtk_Menu_Bar is access all Gtk_Menu_Bar_Record'Class;

   procedure Gtk_New (Menu_Bar : out Gtk_Menu_Bar);

   procedure Initialize (Menu_Bar : access Gtk_Menu_Bar_Record'Class);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Option_Menu.

   procedure Append
     (Menu_Bar : access Gtk_Menu_Bar_Record;
      Child    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);

   procedure Insert
     (Menu_Bar : access Gtk_Menu_Bar_Record;
      Child    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Position : in Gint);

   procedure Prepend
     (Menu_Bar : access Gtk_Menu_Bar_Record;
      Child    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class);

   procedure Set_Shadow_Type
     (Menu_Bar : access Gtk_Menu_Bar_Record;
      The_Type : in Gtk_Shadow_Type);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N : in Node_Ptr; File : in File_Type);

   procedure Generate (Menu_Bar : in out Gtk_Object; N : in Node_Ptr);

private

   type Gtk_Menu_Bar_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_menu_bar_get_type");

end Gtk.Menu_Bar;
