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
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Common; use Common;
with Gtk; use Gtk;

package body Create_Entry is

   package Entry_Cb is new Signal.Callback (Widget_Type => Gtk_Check_Button,
                                            Data_Type   => Gtk_Entry);

   Window : aliased Gtk_Window;

   procedure Toggle_Editable (Button : in out Gtk_Check_Button;
                              The_Entry : in out Gtk_Entry)
   is
   begin
      Set_Editable (The_Entry, Is_Active (Button));
   end Toggle_Editable;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      use String_List;

      List      : Glist;
      Id        : Guint;
      Box1,
        Box2    : Gtk_Box;
      The_Entry : Gtk_Entry;
      Combo     : Gtk_Combo;
      Check     : Gtk_Check_Button;
      Separator : Gtk_Separator;
      Button    : Gtk_Button;
   begin
      if not Is_Created (Window) then

         Append (List, "item0");
         Append (List, "item1 item1");
         Append (List, "item2 item2 item2");
         Append (List, "item3 item3 item3 item3");
         Append (List, "item4 item4 item4 item4 item4");
         Append (List, "item5 item5 item5 item5 item5 item5");
         Append (List, "item6 item6 item6 item6 item6");
         Append (List, "item7 item7 item7 item7");
         Append (List, "item8 item8 item8");
         Append (List, "item9 item9");

         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "entry");
         Border_Width (Window, 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (The_Entry);
         Set_Text (The_Entry, "Hello world");
         Gtk.GEntry.Select_Region (The_Entry, 0, -1);
         Pack_Start (Box2, The_Entry, True, True, 0);
         Show (The_Entry);

         Gtk_New (Combo);
         Set_Popdown_Strings (Combo, List);
         Set_Text (Get_Entry (Combo), "hello world");
         Gtk.GEntry.Select_Region (Get_Entry (Combo), 0, -1);
         Pack_Start (Box2, Combo, True, True, 0);
         Show (Combo);

         Gtk_New (Check, "Editable");
         Pack_Start (Box2, Check, False, True, 0);
         Id := Entry_Cb.Connect (Check, "toggled", Toggle_Editable'Access,
                                 The_Entry);
         Set_State (Check, True);
         Show (Check);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, False, True, 0);
         Show (Box2);

         Gtk_New (Button, "Close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box2, Button, True, True, 0);
         Set_Flags (Button, Can_Default);
         Grab_Default (Button);
         Show (Button);
      end if;

      if not Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;
   end Run;

end Create_Entry;

