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

with Gtk.Object; use Gtk.Object;
with Gtk.Enums;
with Gtk.Item;
with Gtk.Widget;

package Gtk.Menu_Item is

   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with private;
   type Gtk_Menu_Item is access all Gtk_Menu_Item_Record'Class;

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item;
                      Label     : in  String := "");

   procedure Initialize (Menu_Item : access Gtk_Menu_Item_Record;
                         Label     : in  String);

   procedure Set_Submenu (Menu_Item : access Gtk_Menu_Item_Record;
                          Submenu   : access Widget.Gtk_Widget_Record'Class);

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Set_Placement (Menu_Item : access Gtk_Menu_Item_Record;
                            Placement : in     Enums.Gtk_Submenu_Placement);

   procedure Configure (Menu_Item              : access Gtk_Menu_Item_Record;
                        Show_Toggle_Indicator  : in     Boolean;
                        Show_Submenu_Indicator : in     Boolean);

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record);

   procedure Set_Right_Justify
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean);
   --  Call Right_Justify when Justify. Noop otherwise
   --  This procedure is needed by Gate to automate the code generation.

   --  The two following procedures are used to generate and create widgets
   --  from a Node.
 
   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
 
   procedure Generate (Menu_Item : in out Gtk_Object; N : in Node_Ptr);

private

   type Gtk_Menu_Item_Record is new Item.Gtk_Item_Record with null record;

end Gtk.Menu_Item;
