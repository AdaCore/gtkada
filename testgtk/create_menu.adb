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

with Gtk.Box; use Gtk.Box;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Tearoff_Menu_Item; use Gtk.Tearoff_Menu_Item;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

package body Create_Menu is

   function Help return String is
   begin
      return
        "There are several widgets involved in displaying menus. The"
        & " @bGtk_Menu_Bar@B widget is a horizontal menu bar, which normally"
        & " appears at the top of an application. The @bGtk_Menu@B widget is"
        & " the actual menu that pops up. Both @bGtk_Menu_Bar@B and"
        & " @bGtk_Menu@B are subclasses of @bGtk_Menu_Shell@B; a"
        & " @bGtk_Menu_Shell@B contains menu items (@bGtk_Menu_Item@B)."
        & " Each menu item contains text and/or images and can be selected"
        & " by the user."
        & ASCII.LF
        & "This demo shows how to create a @bGtk_Menu_Bar@B, with multiple"
        & " @bGtk_Menu@Bs. Each of this submenu is actually a @btearoff@B menu"
        & ", which means by that clicking on the dashed line, you can simply"
        & " glue the submenu to another place on your desktop, and keep it"
        & " around. To hide it, simply click on the dashed line again."
        & ASCII.LF
        & "There are several kinds of menu item, including plain"
        & " @bGtk_Menu_Item@B, @bGtk_Check_Menu_Item@B which can be"
        & " checked/unchecked, @bGtk_Radio_Menu_Item@B which is a check menu"
        & " item that's in a mutually exclusive group,"
        & " @bGtk_Separator_Menu_Item@B which is a separator bar,"
        & " @bGtk_Tearoff_Menu_Item@B which allows a @bGtk_Menu@B to be torn"
        & " off, and @bGtk_Image_Menu_Item@B which can place a @bGtk_Image@B"
        & " or other widget next to the menu text. A @bGtk_Menu_Item can have"
        & " a submenu, which is simply a @bGtk_Menu@B to pop up when the menu"
        & " item is selected. Typically, all menu items in a menu bar have"
        & " submenus."
        & ASCII.LF
        & "The @bGtk_Option_Menu@B widget is a button that pops up a"
        & " @bGtk_Menu@B when clicked. It's used inside dialogs and such."
        & " This is different from the @bGtk_Combo_Box@B that you can see"
        & " in the @bEntry@B demo, since a @bGtk_Option_Menu@B does not have"
        & " any editable entry associated with it.";
   end Help;

   function Create_Menu
     (Depth : Integer; Tearoff : Boolean) return Gtk_Menu is
      Menu      : Gtk_Menu;
      Group     : Widget_SList.GSlist;
      Menu_Item : Gtk_Radio_Menu_Item;
   begin
      Gtk_New (Menu);

      if Tearoff then
         declare
            Tear_Menu : Gtk_Tearoff_Menu_Item;
         begin
            Gtk_New (Tear_Menu);
            Append (Menu, Tear_Menu);
            Show (Tear_Menu);
         end;
      end if;

      for J in 0 .. 5 loop
         Gtk_New (Menu_Item, Group, "Item" & Integer'Image (Depth)
                  & " -" & Integer'Image (J + 1));
         Group := Gtk.Radio_Menu_Item.Get_Group (Menu_Item);
         Append (Menu, Menu_Item);
         Show (Menu_Item);

         if J = 3 then
            Set_Sensitive (Menu_Item, False);
         end if;

         if Depth > 1 then
            Set_Submenu (Menu_Item, Create_Menu (Depth - 1, Tearoff));
         end if;
      end loop;
      return Menu;
   end Create_Menu;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1 : Gtk_Box;
      Box2 : Gtk_Box;
      Menu_Bar  : Gtk_Menu_Bar;
      Menu : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
      Option_Menu : Gtk_Option_Menu;

   begin

      Set_Label (Frame, "Menus");
      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Menu_Bar);
      Pack_Start (Box1, Menu_Bar, False, False, 0);

      Menu := Create_Menu (2, True);

      Gtk_New (Menu_Item, "test" & Ascii.LF & "line2");
      Set_Submenu (Menu_Item, Menu);
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "foo");
      Set_Submenu (Menu_Item, Create_Menu (3, True));
      Append (Menu_Bar, Menu_Item);

      Gtk_New (Menu_Item, "bar");
      Set_Submenu (Menu_Item, Create_Menu (4, true));

      Set_Right_Justified (Menu_Item, True);
      Append (Menu_Bar, Menu_Item);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Option_Menu);
      Set_Menu (Option_Menu, Create_Menu (1, False));
      Set_History (Option_Menu, 3);
      Pack_Start (Box2, Option_Menu, False, False, 0);

      Show_All (Frame);
   end Run;

end Create_Menu;

