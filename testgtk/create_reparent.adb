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
with Gtk.Frame; use Gtk.Frame;
with Gtk.Label; use Gtk.Label;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

with Ada.Text_IO;

package body Create_Reparent is

   package Box_Cb is new Signal.Callback (Gtk_Button, Gtk_Box);
   package Int_Cb is new Signal.Two_Callback (Gtk_Label, Gint, Gtk_Widget);
   package Label_User is new User_Data (Gtk_Label);

   Window : aliased Gtk.Window.Gtk_Window;

   procedure Set_Parent_Signal (Child      : in out Gtk_Label;
                                Old_Parent : in out Gtk_Widget;
                                Data       : in out Gint)
   is
   begin
      Ada.Text_IO.Put ("Set_Parent for ");
      if Is_Created (Child) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Child))
                          & " : new parent : ");
         if Is_Created (Get_Parent (Child)) then
            Ada.Text_IO.Put (Type_Name (Get_Type (Get_Parent (Child))));
         else
            Ada.Text_IO.Put ("NULL");
         end if;
      else
         Ada.Text_IO.Put ("NULL ");
      end if;
      Ada.Text_IO.Put ("  old parent : ");
      if Is_Created (Old_Parent) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Old_Parent)));
      else
         Ada.Text_IO.Put ("NULL");
      end if;
      Ada.Text_IO.Put_Line (" data = " & Gint'Image (Data));
   end Set_Parent_Signal;

   procedure Reparent_Label (Widget     : in out Gtk_Button;
                             New_Parent : in out Gtk_Box)
   is
      Label : Gtk_Label;
   begin
      Label := Label_User.Get (Widget);
      Reparent (Label, New_Parent);
   end Reparent_Label;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id        : Guint;
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Box3      : Gtk_Box;
      Label     : Gtk_Label;
      Frame     : Gtk_Frame;
      Button    : Gtk_Button;
      Separator : Gtk_Separator;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Set_Title (Window, "reparent");
         Set_Border_Width (Window, Border_Width => 0);

         Gtk_New_Vbox (Box1, False, 0);
         Add (Window, Box1);
         Show (Box1);

         Gtk_New_Hbox (Box2, False, 5);
         Set_Border_Width (Box2, 10);
         Pack_Start (Box1, Box2, True, True, 0);
         Show (Box2);

         Gtk_New (Label, "hello world");

         Gtk_New (Frame, "Frame 1");
         Pack_Start (Box2, Frame, True, True, 0);
         Show (Frame);
         Gtk_New_Vbox (Box3, False, 5);
         Set_Border_Width (Box3, 5);
         Add (Frame, Box3);
         Show (Box3);
         Gtk_New (Button, "switch");
         Id := Box_Cb.Connect (Button, "clicked", Reparent_Label'Access, Box3);
         Label_User.Set (Button, Label);
         Pack_Start (Box3, Button, False, True, 0);
         Show (Button);

         Pack_Start (Box3, Label, False, True, 0);
         Id := Int_Cb.Connect (Label, "parent_set", Set_Parent_Signal'Access,
                               42);
         Show (Label);

         Gtk_New (Frame, "Frame 2");
         Pack_Start (Box2, Frame, True, True, 0);
         Show (Frame);
         Gtk_New_vbox (Box3, False, 5);
         Set_Border_Width (Box3, 5);
         Add (Frame, Box3);
         Show (Box3);
         Gtk_New (Button, "switch");
         Id := Box_Cb.Connect (Button, "clicked", Reparent_Label'Access, Box3);
         Label_User.Set (Button, Label);
         Pack_Start (Box3, Button, False, True, 0);
         Show (Button);

         Gtk_New_Hseparator (Separator);
         Pack_Start (Box1, Separator, False, True, 0);
         Show (Separator);

         Gtk_New_Vbox (Box3, False, 10);
         Set_Border_Width (Box3, 10);
         Pack_Start (Box1, Box3, False, True, 0);
         Show (Box3);

         Gtk_New (Button, "close");
         Id := Widget_Cb.Connect (Button, "clicked", Destroy'Access, Window);
         Pack_Start (Box3, Button, True, True, 0);
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

end Create_Reparent;

