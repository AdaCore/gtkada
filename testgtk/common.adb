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

with Gtk.Menu;             use Gtk.Menu;
with Gtk.Radio_Menu_Item;  use Gtk.Radio_Menu_Item;

with Ada.Strings.Fixed;

package body Common is

   -------------------------
   --  Build_Option_Menu  --
   -------------------------

   procedure Build_Option_Menu (Omenu   : out Gtk.Option_Menu.Gtk_Option_Menu;
                                Gr      : out Widget_Slist.GSlist;
                                Items   : Chars_Ptr_Array;
                                History : Gint;
                                Cb      : Widget_Cb.Callback)
   is
      Menu      : Gtk_Menu;
      Menu_Item : Gtk_Radio_Menu_Item;
      Id        : Guint;
   begin
      Gtk.Option_Menu.Gtk_New (OMenu);
      Gtk_New (Menu);

      for I in Items'Range loop
         Gtk_New (Menu_Item, Gr, ICS.Value (Items (I)));
         Id := Widget_Cb.Connect (Menu_Item, "activate", Cb, Menu_Item);
         Gr := Group (Menu_Item);
         Append (Menu, Menu_Item);
         if I = History then
            Set_Active (Menu_Item, True);
         end if;
         Show (Menu_Item);
      end loop;
      Gtk.Option_Menu.Set_Menu (Omenu, Menu);
      Gtk.Option_Menu.Set_History (Omenu, Gint (History));
   end Build_Option_Menu;


   --------------------
   -- Destroy_Window --
   --------------------

   procedure Destroy_Window (Win : access Gtk.Window.Gtk_Window_Record;
                             Ptr : in Gtk_Window_Access) is
   begin
      Ptr.all := null;
   end Destroy_Window;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog (Win : access Gtk.Dialog.Gtk_Dialog_Record;
                             Ptr : in Gtk_Dialog_Access) is
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   --------------
   -- Image_Of --
   --------------

   function Image_Of (I : in Gint) return String is
   begin
      return Ada.Strings.Fixed.Trim (Gint'Image (I), Ada.Strings.Left);
   end Image_Of;

end Common;
