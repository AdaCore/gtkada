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
-----------------------------------------------------------------------

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.List; use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

package body Create_List is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package List_Cb is new Signal.Object_Callback (Gtk_List);

   Window : Gtk.Window.Gtk_Window;

   type String10 is new String (1 .. 10);
   List_Items : array (Positive range <>) of String10 :=
     ("hello     ",
      "world     ",
      "blah      ",
      "foo       ",
      "bar       ",
      "argh      ",
      "spencer   ",
      "is a      ",
      "wussy     ",
      "programmer");

   Num_Item : Natural := 0;

   procedure List_Add (List : in out Gtk_List'Class) is
      Item : Gtk_List_Item;
   begin
      Gtk_New (Item, "added item" & Natural'Image (Num_Item));
      Num_Item := Num_Item + 1;
      Add (List, Item);
      Show (Item);
   end List_Add;

   procedure List_Remove (List : in out Gtk_List'Class) is
      use Widget_List;
      Tmp_List,
        Clear_List : Widget_List.Glist;
   begin
      Tmp_List := Get_Selection (List);
      while Tmp_List /= Widget_List.Null_List loop
         Prepend (Clear_List, Get_Data (Tmp_List));
         Tmp_List := Next (Tmp_List);
      end loop;

      List_Reverse (Clear_List);
      Remove_Items (List, Clear_List);
      Free (Clear_List);
   end List_Remove;

   procedure List_Clear (List : in out Gtk_List'Class) is
   begin
      Clear_Items (List, 3 - 1, 5 - 1);
   end List_Clear;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id       : Guint;
      Close    : Gtk_Button;
      Box1,
        Box2   : Gtk_Box;
      Scrolled : Gtk_Scrolled_Window;
      List     : Gtk_List;
      Button   : Gtk_Button;
      Item     : Gtk_List_Item;
      Sep      : Gtk_Separator;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget_Cb.Connect (Window, "destroy", Destroy'Access, Window);
         Set_Title (Window, "list");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Scrolled);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
         Pack_Start (Box2, Scrolled, True, True, 0);
         Show (Scrolled);

         Gtk_New (List);
         Set_Selection_Mode (List, Selection_Browse);
         Add (Scrolled, List);
         Set_Focus_Vadjustment (List, Get_Vadjustment (Scrolled));
         Show (List);

         for I in List_Items'Range loop
            Gtk_New (Item, String (List_Items (I)));
            Add (List, Item);
            Show (Item);
         end loop;

         Gtk_New (Button, "add");
         Unset_Flags (Button, Can_Focus);
         Id := List_Cb.Connect (Button, "clicked", List_Add'Access, List);
         Pack_Start (Box2, Button, False, True, 0);
         Show (Button);

         Gtk_New (Button, "Clear items 3-5");
         Unset_Flags (Button, Can_Focus);
         Id := List_Cb.Connect (Button, "clicked", List_Clear'Access, List);
         Pack_Start (Box2, Button, False, True, 0);
         Show (Button);

         Gtk_New (Button, "Remove");
         Unset_Flags (Button, Can_Focus);
         Id := List_Cb.Connect (Button, "clicked", List_Remove'Access, List);
         Pack_Start (Box2, Button, False, True, 0);
         Show (Button);

         Gtk_New_Hseparator (Sep);
         Pack_Start (Box1, Sep, False, True, 0);
         Show (Sep);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;


      if not Gtk.Widget.Visible_Is_Set (Window) then
         Gtk.Widget.Show (Window);
      else
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_List;
