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
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Widget;

package Gtk.Menu is

   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with private;
   type Gtk_Menu is access all Gtk_Menu_Record'Class;

   type Gtk_Menu_Detach_Func is access procedure
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record;
      Menu          : in Gtk_Menu);

   procedure Append
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Attach_To_Widget
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Detacher      : in Gtk_Menu_Detach_Func);
   procedure Detach (Menu : access Gtk_Menu_Record);
   function Get_Active (Menu : access Gtk_Menu_Record)
                        return Gtk.Menu_Item.Gtk_Menu_Item;
   function Get_Attach_Widget (Menu : access Gtk_Menu_Record)
                               return Gtk.Widget.Gtk_Widget;
   procedure Gtk_New (Widget : out Gtk_Menu);
   procedure Initialize (Widget : access Gtk_Menu_Record'Class);
   procedure Insert
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : in Gint);

   generic
      type Data_Type is private;
   package Menu_Popup is
      type Gtk_Menu_Position_Func is access procedure
        (Menu      : access Gtk_Menu_Record;
         X         : in out Gint;
         Y         : in out Gint;
         User_Data : access Data_Type);
      procedure Popup
        (Menu              : access Gtk_Menu_Record;
         Parent_Menu_Shell : access Gtk.Menu_Shell.Gtk_Menu_Shell_Record'Class;
         Parent_Menu_Item  : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
         Func              : in Gtk_Menu_Position_Func;
         Data              : access Data_Type;
         Button            : in Guint;
         Activate_Time     : in Guint32);
   end Menu_Popup;

   procedure Popdown (Menu : access Gtk_Menu_Record);
   procedure Prepend
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Set_Active
     (Menu  : access Gtk_Menu_Record;
      Index : in Guint);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Menu : in out Gtk_Object; N : in Node_Ptr);

private
   type Gtk_Menu_Record is new Gtk.Menu_Shell.Gtk_Menu_Shell_Record
     with null record;

end Gtk.Menu;
