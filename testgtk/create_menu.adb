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
--         General Public License for more details.                  --
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
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Object; use Gtk.Object;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Menu is


   Window : aliased Gtk_Window;

   function Create_Menu (Depth : Integer) return Gtk_Menu is
      Menu      : Gtk_Menu;
      Group     : Widget_SList.GSlist;
      Menu_Item : Gtk_Radio_Menu_Item;
   begin
      if Depth < 1 then
         return Menu;
      end if;

      Gtk_New (Menu);

      for I in 0 .. 5 loop
         Gtk_New (Menu_Item, Group, "Item" & Integer'Image (Depth)
                  & " -" & Integer'Image (I + 1));
         Group := Gtk.Radio_Menu_Item.Group (Menu_Item);
         if Depth mod 2 /= 0 then
            Set_Show_Toggle (Menu_Item, True);
         end if;
         Append (Menu, Menu_Item);
         Show (Menu_Item);
         if I = 3 then
            Set_Sensitive (Menu_Item, False);
         end if;
         Set_Submenu (Menu_Item, Create_Menu (Depth - 1));
      end loop;
      return Menu;
   end Create_Menu;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id   : Guint;
      Box1 : Gtk_Box;
      Box2 : Gtk_Box;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Option_Menu : Gtk_Option_Menu;
      Separator   : Gtk_Separator;
      Button : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Menus");
         Border_Width (Window, 0);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New (Menu_Bar);
         Pack_Start (Box1, Menu_Bar, False, True, 0);
         Show (Menu_Bar);

         Menu := Create_Menu (2);

         Gtk_New (Menu_Item, "test" & Ascii.LF & "line2");
         Set_Submenu (Menu_Item, Menu);
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, "foo");
         Set_Submenu (Menu_Item, Create_Menu (3));
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New (Menu_Item, "bar");
         Set_Submenu (Menu_Item, Create_Menu (4));
         Append (Menu_Bar, Menu_Item);
         Show (Menu_Item);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Option_Menu);
         Set_Menu (Option_Menu, Create_Menu (1));
         Set_History (Option_Menu, 4);
         Pack_Start (Box2, Option_Menu, True, True, 0);
         Show (Option_Menu);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, True, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "Close");
         Id := Widget_Cb.Connect (Button, "clicked", Gtk.Widget.Destroy'Access,
                                  Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Menu;

