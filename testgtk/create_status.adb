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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk; use Gtk;

with Ada.Text_IO;
with Interfaces.C.Strings;

package body Create_Status is

   type Do_Not_Use_This_Type is new Gtk_Status_Bar_Record with null record;
   package Do_Not_Use_This_Package is
     new Signal.Object_Callback (Do_Not_Use_This_Type);
   --
   -- FIXME : The previous two lines are there to prevent a small technical
   -- FIMXE : problem found in GNAT-3.11p (a.k.a a bug) during compilation.
   -- FIXME : Remove them when GNAT-3.12p is released.

   package Status_Cb is new Signal.Object_Callback (Gtk_Status_Bar_Record);

   Counter : Gint := 1;

   procedure Push (Status : access Gtk_Status_Bar_Record) is
      Id : Message_Id;
   begin
      Id := Push (Status, 1, "Something" & Gint'Image (Counter));
      Counter := Counter + 1;
   end Push;

   procedure Pop (Status : access Gtk_Status_Bar_Record) is
   begin
      Pop (Status, 1);
   end Pop;

   procedure Popped (Status : access Gtk_Status_Bar_Record) is
      use type Messages_List.GSlist;
   begin
      if Get_Messages (Status) = Messages_List.Null_List then
         Counter := 1;
      end if;
   end Popped;

   procedure Steal (Status : access Gtk_Status_Bar_Record) is
   begin
      Remove (Status, 1, 4);
   end Steal;

   procedure Contexts (Status : access Gtk_Status_Bar_Record) is
   begin
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "any context"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "any context")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "idle messages"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "idle message")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "some text"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "some text")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "hit the mouse")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse2"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "hit the mouse2")));
   end Contexts;

   procedure Dump (Status : access Gtk_Status_Bar_Record) is
      List : Messages_List.GSlist := Get_Messages (Status);
      use type Messages_List.GSlist;
   begin
      while List /= Messages_List.Null_List loop
         declare
            Msg : Status_Bar_Msg := Messages_List.Get_Data (List);
         begin
            Ada.Text_IO.Put_Line ("Context Id = "
                                  & Context_Id'Image (Msg.Context)
                                  & " Message_Id = "
                                  & Message_Id'Image (Msg.Message)
                                  & " Text = "
                                  & Interfaces.C.Strings.Value (Msg.Text));
         end;
         List := Messages_List.Next (List);
      end loop;
   end Dump;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id        : Guint;
      Box1,
        Box2    : Gtk_Box;
      Status    : Gtk_Status_Bar;
      Button    : Gtk_Button;

   begin
      Set_Label (Frame, "Status Bar");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Status);
      Pack_End (Box1, Status, False, False, 0);
      Id := Status_Cb.Connect (Status, "text_popped", Popped'Access,
                               Status);

      --  FIXME : the C testgtk uses gtk_widget_new here, which are
      --  functions with multiple arguments

      Gtk_New (Button, "Push Something");
      Pack_Start (Box2, Button, False, False, 0);
      Id := Status_Cb.Connect (Button, "clicked", Push'Access, Status);

      Gtk_New (Button, "Pop");
      Pack_Start (Box2, Button, False, False, 0);
      Id := Status_Cb.Connect (Button, "clicked", Pop'Access, Status);

      Gtk_New (Button, "Steal #4");
      Pack_Start (Box2, Button, False, False, 0);
      Id := Status_Cb.Connect (Button, "clicked", Steal'Access, Status);

      Gtk_New (Button, "Dump stack");
      Pack_Start (Box2, Button, False, False, 0);
      Id := Status_Cb.Connect (Button, "clicked", Dump'Access, Status);

      Gtk_New (Button, "Test contexts");
      Pack_Start (Box2, Button, False, False, 0);
      Id := Status_Cb.Connect (Button, "clicked", Contexts'Access, Status);

      Show_All (Frame);
   end Run;

end Create_Status;

