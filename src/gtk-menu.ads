-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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


with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu is new Gtk.Menu_Shell.Gtk_Menu_Shell with private;

   type Gtk_Menu_Detach_Func is access procedure
     (Attach_Widget : in Gtk.Widget.Gtk_Widget'Class;
      Menu          : in Gtk_Menu'Class);

   procedure Append
     (Menu  : in Gtk_Menu'Class;
      Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Attach_To_Widget
     (Menu          : in Gtk_Menu'Class;
      Attach_Widget : in Gtk.Widget.Gtk_Widget'Class;
      Detacher      : in Gtk_Menu_Detach_Func);
   procedure Detach (Menu : in Gtk_Menu'Class);
   function Get_Active (Menu : in Gtk_Menu'Class)
                        return Gtk.Menu_Item.Gtk_Menu_Item;
   function Get_Attach_Widget (Menu : in Gtk_Menu'Class)
                               return Gtk.Widget.Gtk_Widget;
   procedure Gtk_New (Widget : out Gtk_Menu);
   procedure Insert
     (Menu     : in Gtk_Menu'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class;
      Position : in Gint);


   generic
      type Data_Type is private;
   package Menu_Popup is
      type Gtk_Menu_Position_Func is access procedure
        (Menu      : in Gtk_Menu'Class;
         X         : in out Gint;
         Y         : in out Gint;
         User_Data : access Data_Type);
      procedure Popup
        (Menu              : in Gtk_Menu'Class;
         Parent_Menu_Shell : in Gtk.Menu_Shell.Gtk_Menu_Shell'Class;
         Parent_Menu_Item  : in Gtk.Menu_Item.Gtk_Menu_Item;
         Func              : in Gtk_Menu_Position_Func;
         Data              : access Data_Type;
         Button            : in Guint;
         Activate_Time     : in Guint32);
   end Menu_Popup;

   procedure Popdown (Menu : in Gtk_Menu'Class);
   procedure Prepend
     (Menu  : in Gtk_Menu'Class;
      Child : in Gtk.Widget.Gtk_Widget'Class);
   procedure Set_Active
     (Menu  : in Gtk_Menu'Class;
      Index : in Guint);
--    procedure Set_Accelerator_Table
--      (Menu  : in Gtk_Menu'Class;
--       Table : in Gtk_Accelerator_Table);


private
   type Gtk_Menu is new Gtk.Menu_Shell.Gtk_Menu_Shell with null record;

   --  mapping: NOT_IMPLEMENTED gtkmenu.h gtk_menu_get_type
   --  mapping: Gtk_New gtkmenu.h gtk_menu_new
   --  mapping: Append gtkmenu.h gtk_menu_append
   --  mapping: Prepend gtkmenu.h gtk_menu_prepend
   --  mapping: Insert gtkmenu.h gtk_menu_insert
   --  mapping: Popup gtkmenu.h gtk_menu_popup
   --  mapping: Popdown gtkmenu.h gtk_menu_popdown
   --  mapping: Get_Active gtkmenu.h gtk_menu_get_active
   --  mapping: Set_Active gtkmenu.h gtk_menu_set_active
   --  mapping: Set_Accelerator_Table gtkmenu.h gtk_menu_set_accelerator_table
   --  mapping: Attach_To_Widget gtkmenu.h gtk_menu_attach_to_widget
   --  mapping: Get_Attach_Widget gtkmenu.h gtk_menu_get_attach_widget
   --  mapping: Detach gtkmenu.h gtk_menu_detach

end Gtk.Menu;
