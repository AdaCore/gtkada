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
with Gdk.Color; use Gdk.Color;
with Gdk.Font; use Gdk.Font;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Style; use Gtk.Style;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Scrollbar; use Gtk.Scrollbar;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Table; use Gtk.Table;
with Gtk.Text; use Gtk.Text;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

with Ada.Text_IO; use Ada.Text_IO;

package body Create_Text is

   package Text_Cb is new Signal.Callback (Gtk_Toggle_Button, Gtk_Text);

   Window : aliased Gtk_Window;

   procedure Toggle_Editable (Toggle : in out Gtk_Toggle_Button;
                              Text   : in out Gtk_Text)
   is
   begin
      Set_Editable (Text, Is_Active (Toggle));
   end Toggle_Editable;

   procedure Word_Wrap (Toggle : in out Gtk_Toggle_Button;
                        Text   : in out Gtk_Text)
   is
   begin
      Set_Word_Wrap (Text, Is_Active (Toggle));
   end Word_Wrap;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id           : Guint;
      Box1,
        Box2       : Gtk_Box;
      Hbox         : Gtk_HButton_Box;
      Table        : Gtk_Table;
      Text         : Gtk_Text;
      Hscrollbar,
        Vscrollbar : Gtk_Scrollbar;
      Infile       : File_Type;
      Check        : Gtk_Check_Button;
      Separator    : Gtk_Separator;
      Button       : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                  Window'Access);
         Set_Name (Window, "text window");
         Set_Title (Window, "test");
         Set_Usize (Window, 500, 500);
         Set_Policy (Window, True, True, False);
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Table, 2, 2, False);
         Set_Row_Spacing (Table, 0, 2);
         Set_Col_Spacing (Table, 0, 2);
         Pack_Start (Box2, Table, True, True, 0);
         Show (Table);

         Gtk_New (Text);
         Set_Editable (Text, True);
         Attach (Table, Text, 0, 1, 0, 1,
                 Expand or Shrink or Fill,
                 Expand or Shrink or Fill, 0, 0);
         Show (Text);

         Gtk_New_Hscrollbar (Hscrollbar, Get_Hadj (Text));
         Attach (Table, Hscrollbar, 0, 1, 1, 2,
                 Expand or Fill or Shrink,
                 Fill, 0, 0);
         Show (Hscrollbar);

         Gtk_New_Vscrollbar (Vscrollbar, Get_Vadj (Text));
         Attach (Table, Vscrollbar, 1, 2, 0, 1,
                 Fill, Expand or Fill or Shrink,
                 0, 0);
         Show (Vscrollbar);

         Freeze (Text);
         Realize (Text);

         Open (Infile, In_File, "create_text.adb");
         declare
            Buffer : String (1 .. 1024);
            Last   : Natural;
         begin
            while not End_Of_File (Infile) loop
               Get_Line (Infile, Buffer, Last);

               Insert (Text, Null_Font, Null_Color, Null_Color,
                       Buffer (1 .. Last) & Ascii.LF, Gint (Last) + 1);
            end loop;
         end;

         Close (Infile);

         Insert (Text, Null_Font, Get_Black (Get_Style (Text)), Null_Color,
                 "And even ", 9);
         Insert (Text, Null_Font, Get_Bg (Get_Style (Text), State_Normal),
                 Null_Color, "colored", 7);
         Insert (Text, Null_Font, Get_Black (Get_Style (Text)), Null_Color,
                 "text", 4);
         Thaw (Text);

         Gtk_New (Hbox);
         Pack_Start (Box2, Hbox, False, False, 0);
         Show (Hbox);

         Gtk_New (Check, "Editable");
         Pack_Start (Hbox, Check, False, False, 0);
         Id := Text_Cb.Connect (Check, "toggled", Toggle_Editable'Access, Text);
         Set_State (Check, True);
         Show (Check);

         Gtk_New (Check, "Wrap Words");
         Pack_Start (Hbox, Check, False, False, 0);
         Id := Text_Cb.Connect (Check, "toggled", Word_Wrap'Access, Text);
         Set_State (Check, False);
         Show (Check);

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
         Show (Window);
      else
         Destroy (Window);
      end if;

   exception
      when Name_Error =>
         Put_Line ("File create_text.adb not found....");
         Destroy (Window);
   end Run;

end Create_Text;

