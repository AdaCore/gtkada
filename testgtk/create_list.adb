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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Label; use Gtk.Label;
with Gtk.List; use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.Option_Menu;  use Gtk.Option_Menu;
with Gtk.Radio_Menu_Item;  use Gtk.Radio_Menu_Item;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Common; use Common;
with Gtk; use Gtk;
with Ada.Text_IO;   use Ada.Text_IO;

with Create_Progress;

package body Create_List is

   package List_Cb is new Signal.Object_Callback (Gtk_List_Record);

   Window : aliased Gtk.Window.Gtk_Window;

   Num_Item : Natural := 0;

   Items : constant Create_Progress.Array_Of_String :=
     ("Single    ",
      "Browse    ",
      "Multiple  ",
      "Extended  ");

   List : Gtk_List;
   Omenu_Group  : Widget_Slist.GSlist;

   procedure Toggle_Sel_Mode (Widget : access Gtk_Widget_Record) is
   begin
      if not Mapped_Is_Set (Widget) then
         return;
      end if;
      Set_Selection_Mode
        (List,
         Gtk_Selection_Mode'Val (3 - Selected_Button (Omenu_Group)));
   end Toggle_Sel_Mode;

   procedure List_Add (List : access Gtk_List_Record) is
      Item : Gtk_List_Item;
   begin
      Gtk_New (Item, Label => "added item" & Natural'Image (Num_Item));
      Num_Item := Num_Item + 1;
      Show (Item);
      Add (List, Item);
   end List_Add;

   procedure List_Remove (List : access Gtk_List_Record) is
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

   procedure List_Clear (List : access Gtk_List_Record) is
   begin
      Clear_Items (List, 0, -1);
   end List_Clear;

   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Id           : Guint;
      Vbox,
      Hbox,
      Cbox         : Gtk_Box;
      Button       : Gtk_Button;
      Sep          : Gtk_Separator;
      Scrolled_Win : Gtk_Scrolled_Window;
      Label        : Gtk_Label;
      Infile       : Ada.Text_IO.File_Type;
      List_Omenu   : Gtk_Option_Menu;
   begin

      if Window = null then
         Gtk_New (Window, Window_Toplevel);
         Id := Destroy_Cb.Connect (Window, "destroy",
                                   Destroy_Window'Access, Window'Access);
         Set_Title (Window, "list");
         Set_Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (VBox, Homogeneous => False, Spacing => 0);
         Add (Window, VBox);

         Gtk_New (Scrolled_Win);
         Set_Border_Width (Scrolled_Win, Border_Width => 5);
         Set_Usize (Scrolled_Win, Width => -1, Height => 300);
         Pack_Start (Vbox, Scrolled_Win,
                     Expand => True,
                     Fill => True,
                     Padding => 0);
         Set_Policy (Scrolled_Win,
                     H_Scrollbar_Policy => Policy_Automatic,
                     V_Scrollbar_Policy => Policy_Automatic);

         Gtk_New (List);
         Set_Selection_Mode (List, Mode => Selection_Single);
         Add_With_Viewport (Scrolled_Win, List);
         Set_Focus_Vadjustment (List, Get_Vadjustment (Scrolled_Win));
         Set_Focus_Hadjustment (List, Get_Hadjustment (Scrolled_Win));

         Open (Infile, In_File, "create_list.adb");
         declare
            S    : String (1 .. 1024);
            Last : Natural;
            Item : Gtk_List_Item;
         begin
            while not End_Of_File (Infile) loop
               Get_Line (File => Infile, Item => S, Last => Last);
               Gtk_New (Item, Label => S (S'First .. Last));
               Add (List, Item);
            end loop;
         end;
         Close (Infile);

         Gtk_New_Hbox (Hbox, Homogeneous => True, Spacing => 5);
         Set_Border_Width (Hbox, Border_Width => 5);
         Pack_Start (Vbox, Hbox,
                     Expand => False,
                     Fill => True,
                     Padding => 0);

         Gtk_New (Button, Label => "Insert Row");
         Pack_Start (Hbox, BUtton,
                     Expand  => True,
                     Fill    => True,
                     Padding => 0);
         Id := List_Cb.Connect (Button, "clicked", List_Add'Access, List);

         Gtk_New (Button, Label => "Clear List");
         Pack_Start (Hbox, BUtton,
                     Expand  => True,
                     Fill    => True,
                     Padding => 0);
         Id := List_Cb.Connect (Button, "clicked", List_Clear'Access, List);

         Gtk_New (Button, Label => "Remove Selection");
         Pack_Start (Hbox, Button,
                     Expand  => True,
                     Fill    => True,
                     Padding => 0);
         Id := List_Cb.Connect (Button, "clicked", List_Remove'Access, List);

         Gtk_New_Hbox (Cbox, Homogeneous => False, Spacing => 0);
         Pack_Start (Vbox, Cbox,
                     Expand  => False,
                     Fill    => True,
                     Padding => 0);

         Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 5);
         Set_Border_Width (Hbox, Border_Width => 5);
         Pack_Start (Cbox, Hbox,
                     Expand  => True,
                     Fill    => False,
                     Padding => 0);

         Gtk_New (Label, Str => "Selection Mode :");
         Pack_Start (Hbox, Label,
                     Expand  => False,
                     Fill    => True,
                     Padding => 0);

         Create_Progress.Build_Option_Menu (List_Omenu,
                                            Omenu_Group,
                                            Items,
                                            0,
                                            Toggle_Sel_Mode'Access);
         Pack_Start (Hbox, List_Omenu,
                     Expand  => False,
                     Fill    => True,
                     Padding => 0);

         Gtk_New_Hseparator (Sep);
         Pack_Start (Vbox, Sep,
                     Expand  => False,
                     Fill    => True,
                     Padding => 0);

         Gtk_New_Hbox (Cbox, Homogeneous => False, Spacing => 0);
         Pack_Start (Vbox, Cbox,
                     Expand  => False,
                     Fill    => True,
                     Padding => 0);

         Gtk_New (Button, Label => "Close");
         Set_Border_Width (Button, Border_Width => 10);
         Pack_Start (Cbox, Button,
                     Expand  => True,
                     Fill    => True,
                     Padding => 0);
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show_All (Window);
      else
         Destroy (Window);
      end if;

   exception
      when Name_Error =>
         Ada.Text_Io.Put_Line ("File not found: create_list.adb");
   end Run;

end Create_List;
