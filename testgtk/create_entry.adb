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

with Glib;             use Glib;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Combo;        use Gtk.Combo;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk;              use Gtk;

package body Create_Entry is

   package Entry_Cb is new Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Entry);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo demonstrates two types of widgets, a simple"
        & " @bGtk_Entry@B and a @bGtk_Combo@B that is composed that adds a"
        & " window to facilitates the insertion of the text.";
   end Help;

   ---------------------
   -- Toggle_Editable --
   ---------------------

   procedure Toggle_Editable (Button : access Gtk_Check_Button_Record'Class;
                              The_Entry : in Gtk_Entry)
   is
   begin
      Set_Editable (The_Entry, Get_Active (Button));
   end Toggle_Editable;

   ----------------------
   -- Toggle_Sensitive --
   ----------------------

   procedure Toggle_Sensitive (Button : access Gtk_Check_Button_Record'Class;
                               The_Entry : in Gtk_Entry)
   is
   begin
      Set_Sensitive (The_Entry, Get_Active (Button));
   end Toggle_Sensitive;

   -----------------------
   -- Toggle_Visibility --
   -----------------------

   procedure Toggle_Visibility (Button : access Gtk_Check_Button_Record'Class;
                                The_Entry : in Gtk_Entry)
   is
   begin
      Set_Visibility (The_Entry, Get_Active (Button));
   end Toggle_Visibility;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      use String_List;

      List      : String_List.Glist;
      Box1,
        Box2    : Gtk_Box;
      The_Entry : Gtk_Entry;
      Combo     : Gtk_Combo;
      Check     : Gtk_Check_Button;

   begin
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

      Set_Label (Frame, "Entry");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (The_Entry);
      Set_Text (The_Entry, "Hello world");
      Select_Region (The_Entry, 0, 5);
      Pack_Start (Box2, The_Entry, True, True, 0);

      Gtk_New (Combo);
      Set_Popdown_Strings (Combo, List);
      Set_Text (Get_Entry (Combo), "hello world");
      Gtk.GEntry.Select_Region (Get_Entry (Combo), 0, -1);
      Pack_Start (Box2, Combo, True, True, 0);

      Gtk_New (Check, "Editable");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Editable'Access), The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Visible");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Visibility'Access), The_Entry);
      Set_Active (Check, True);

      Gtk_New (Check, "Sensitive");
      Pack_Start (Box2, Check, False, True, 0);
      Entry_Cb.Connect
        (Check, "toggled",
         Entry_Cb.To_Marshaller (Toggle_Sensitive'Access), The_Entry);
      Set_Active (Check, True);

      Show_All (Frame);
   end Run;

end Create_Entry;

