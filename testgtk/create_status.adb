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
with Gtk.Container; use Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

with Ada.Text_IO;
with Interfaces.C.Strings;

package body Create_Status is

   package Status_Cb is new Signal.Object_Callback (Gtk_Status_Bar);

   Window  : aliased Gtk.Window.Gtk_Window;
   Counter : Gint := 1;

   procedure Push (Status : in out Gtk_Status_Bar'Class) is
      Id : Message_Id;
   begin
      Id := Push (Status, 1, "Something" & Gint'Image (Counter));
      Counter := Counter + 1;
   end Push;

   procedure Pop (Status : in out Gtk_Status_Bar'Class) is
   begin
      Pop (Status, 1);
   end Pop;

   procedure Popped (Status : in out Gtk_Status_Bar'Class) is
      use type Messages_List.GSlist;
   begin
      if Get_Messages (Status) = Messages_List.Null_List then
         Counter := 1;
      end if;
   end Popped;

   procedure Steal (Status : in out Gtk_Status_Bar'Class) is
   begin
      Remove (Status, 1, 4);
   end Steal;

   procedure Contexts (Status : in out Gtk_Status_Bar'Class) is
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

   procedure Dump (Status : in out Gtk_Status_Bar'Class) is
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

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id        : Guint;
      Box1,
        Box2    : Gtk_Box;
      Status    : Gtk_Status_Bar;
      Button    : Gtk_Button;
      Separator : Gtk_Separator;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "Status");
         Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Vbox (Box2, False, 10);
         Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Status);
         Pack_End (Box1, Status, True, True, 0);
         Show (Status);
         Id := Status_Cb.Connect (Status, "text_popped", Popped'Access,
                                  Status);

         --  FIXME : the C testgtk uses gtk_widget_new here, which are
         --  functions with multiple arguments

         Gtk_New (Button, "Push Something");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Id := Status_Cb.Connect (Button, "clicked", Push'Access, Status);

         Gtk_New (Button, "Pop");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Id := Status_Cb.Connect (Button, "clicked", Pop'Access, Status);

         Gtk_New (Button, "Steal #4");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Id := Status_Cb.Connect (Button, "clicked", Steal'Access, Status);

         Gtk_New (Button, "Dump stack");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Id := Status_Cb.Connect (Button, "clicked", Dump'Access, Status);

         Gtk_New (Button, "Test contexts");
         Show (Button);
         Pack_Start (Box2, Button, False, False, 0);
         Id := Status_Cb.Connect (Button, "clicked", Contexts'Access, Status);

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
         Gtk.Widget.Destroy (Window);
      end if;

   end Run;

end Create_Status;

