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
-- Library General Public License for more details.                  --
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
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Bitmap; use Gdk.Bitmap;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Clist; use Gtk.Clist;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Common; use Common;
with Gtk; use Gtk;

with Interfaces.C.Strings;

package body Create_Clist is
   package ICS renames Interfaces.C.Strings;

   package Clist_Cb  is new Signal.Object_Callback (Gtk_Clist);

   Window : aliased Gtk.Window.Gtk_Window;
   Clist_Columns      : constant Gint := 7;
   Clist_Rows         : Integer := 0;
   Clist_Selected_Row : Gint:= 0;

   procedure Clear_List (List : in out Gtk_Clist'Class) is
   begin
      Clear (List);
      Clist_Rows := 0;
   end Clear_List;

   procedure Remove_Row (List : in out Gtk_Clist'Class) is
   begin
      Remove (List, Clist_Selected_Row);
      Clist_Rows := Clist_Rows - 1;
   end Remove_Row;

   procedure Show_Titles (List : in out Gtk_Clist'Class) is
   begin
      Column_Titles_Show (List);
   end Show_Titles;

   procedure Hide_Titles (List : in out Gtk_Clist'Class) is
   begin
      Column_Titles_Hide (List);
   end Hide_Titles;

   procedure Add1000 (List : in out Gtk_Clist'Class) is
      Pixmap : Gdk_Pixmap;
      Mask   : Gdk_Bitmap;
      Texts  : Line_Data (0 .. Clist_Columns - 1);
      Row    : Gint;
   begin
      Create_From_Xpm (Pixmap, Get_Clist_Window (List),
                       Mask, Get_White (Get_Style (List)),
                       "test.xpm");
      for I in 4 .. Clist_Columns - 1 loop
         Texts (I) := ICS.New_String ("Column" & Gint'Image (I));
      end loop;
      Texts (3) := ICS.Null_Ptr;
      Texts (1) := ICS.New_String ("Right");
      Texts (2) := ICS.New_String ("Center");
      Texts (0) := ICS.Null_Ptr;
      Freeze (List);

      for I in 0 .. 999 loop
         ICS.Free (Texts (0));
         Texts (0) := ICS.New_String ("Row" & Integer'Image (Clist_Rows));
         Clist_Rows := Clist_Rows + 1;
         Row := Append (List, Texts);
         Set_Pixtext (List, Row, 3, "Testing", 5, Pixmap, Mask);
      end loop;

      Free_Line_Data (Texts);

      Thaw (List);
   end Add1000;

   procedure Add10000 (List : in out Gtk_Clist'Class) is
      Texts  : Line_Data (0 .. Clist_Columns - 1);
      Row    : Gint;
   begin
      for I in 3 .. Clist_Columns - 1 loop
         Texts (I) := ICS.New_String ("Column" & Gint'Image (I));
      end loop;
      Texts (1) := ICS.New_String ("Right");
      Texts (2) := ICS.New_String ("Center");
      Texts (0) := ICs.Null_Ptr;

      Freeze (List);
      for I in 0 .. 9999 loop
         Ics.Free (Texts (0));
         Texts (0) := ICS.New_String ("Row" & Integer'Image (Clist_Rows));
         Clist_Rows := Clist_Rows + 1;
         Row := Append (List, Texts);
      end loop;
      Free_Line_Data (Texts);
      Thaw (List);
   end Add10000;

   procedure Insert_Row (List : in out Gtk_Clist'Class) is
      Texts  : Line_Data (0 .. Clist_Columns - 1)
        := (ICS.New_String ("This"),
            ICS.New_String ("is"),
            ICS.New_String ("an"),
            ICS.New_String ("inserted"),
            ICS.New_String ("row"),
            ICS.New_String ("la la la la la"),
            ICS.New_String ("la la la la"));
   begin
      Insert (List, Clist_Selected_Row, Texts);
      Clist_Rows := Clist_Rows + 1;
      Free_Line_Data (Texts);
   end Insert_Row;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Titles : Line_Data (1 .. Clist_Columns) := (ICS.New_String ("Title 0"),
                                                  ICS.New_String ("Title 1"),
                                                  ICS.New_String ("Title 2"),
                                                  ICS.New_String ("Title 3"),
                                                  ICS.New_String ("Title 4"),
                                                  ICS.New_String ("Title 5"),
                                                  ICS.New_String ("Title 6"));
      Texts     : Line_Data (0 .. Clist_Columns - 1);
      Id        : Guint;
      Box1,
        Box2    : Gtk_Box;
      Clist     : Gtk_CList;
      Button    : Gtk_Button;
      Separator : Gtk_Separator;
      New_Row   : Gint;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "clist");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Hbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, False, 0);
         Show (Box2);

         Gtk_New (Clist, Clist_Columns, Titles);

         Gtk_New (Button, "Add 1000 Rows with Pixmaps");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Add1000'Access, Clist);
         Show (Button);

         Gtk_New (Button, "Add 10000 Rows");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Add10000'Access, Clist);
         Show (Button);

         Gtk_New (Button, "Clear List");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Clear_List'Access, Clist);
         Show (Button);

         Gtk_New (Button, "Remove Row");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Remove_Row'Access, Clist);
         Show (Button);

         Gtk_New_Hbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, False, 0);
         Show (Box2);

         Gtk_New (Button, "Insert Row");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Insert_Row'Access, Clist);
         Show (Button);

         Gtk_New (Button, "Show title Buttons");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Show_Titles'Access, Clist);
         Show (Button);

         Gtk_New (Button, "Hide title Buttons");
         Pack_Start (Box2, Button, True, True, 0);
         Id := Clist_Cb.Connect (Button, "clicked", Hide_Titles'Access, Clist);
         Show (Button);


         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Set_Row_Height (Clist, 20);
--       FIXME Id := Connect (Clist, "select_row", Select_Clist'Access);
--       FIXME Id := Connect (Clist, "unselect_row", Select_Clist'Access);

         Set_Column_Width (Clist, 0, 100);
         for I in 1 .. Clist_Columns - 1 loop
            Set_Column_Width (Clist, I, 80);
         end loop;

         Set_Selection_Mode (Clist, Selection_Browse);
         Set_Policy (Clist, Policy_Automatic, Policy_Automatic);
         Set_Column_Justification (Clist, 1, Justify_Right);
         Set_Column_Justification (Clist, 2, Justify_Center);

         for I in 3 .. Clist_Columns - 1 loop
            Texts (I) := ICS.New_String ("Column" & Gint'Image (I));
         end loop;
         Texts (0) := ICS.Null_Ptr;
         Texts (1) := ICS.New_String ("Right");
         Texts (2) := ICS.New_String ("Center");

         for I in 0 .. 99 loop
            ICS.Free (Texts (0));
            Texts (0) := ICS.New_String ("Row" & Integer'Image (I));
            New_Row := Append (Clist, Texts);
         end loop;

         Free_Line_Data (Texts);

         Border_Width (Clist, 5);
         Pack_Start (Box2, Clist, True, True, 0);
         Show (Clist);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

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
         Clist_Rows := 0;
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Clist;

