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
with Gtk.Handle_Box; use Gtk.Handle_Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Object; use Gtk.Object;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;

with Ada.Text_IO;
with Create_Toolbar;

package body Create_Handle_Box is

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget, Gtk_Widget_Access);
   package Handle_Cb is new Signal.Two_Callback (Gtk_Handle_Box,
                                                 String,
                                                 Gtk_Widget);
   Window : aliased Gtk_Window;


   procedure Child_Signal (Handle : in out Gtk_Handle_Box'Class;
                           Child  : in out Gtk_Widget;
                           Data   : in out String) is
   begin
      Ada.Text_IO.Put_Line (Type_Name (Get_Type (Handle))
                            & ": child <"
                            & Type_Name (Get_Type (Child))
                            & "> "
                            & Data);
   end Child_Signal;


   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id        : Guint;
      Vbox      : Gtk_Box;
      Hbox      : Gtk_Box;
      Label     : Gtk_Label;
      Separator : Gtk_Separator;
      Handle    : Gtk_Handle_Box;
      Handle2   : Gtk_Handle_Box;
      Toolbar   : Gtk_Toolbar;
   begin
      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Handle Box Test");
         Set_Policy (Window, Allow_Shrink => True,
                     Allow_Grow => True, Auto_Shrink => False);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Border_Width (Window, 20);

         Gtk_New_Vbox (Vbox, False, 0);
         Add (Window, Vbox);
         Show (Vbox);

         Gtk_New (Label, "Above");
         Add (Vbox, Label);
         Show (Label);

         Gtk_New_Hseparator (Separator);
         Add (Vbox, Separator);
         Show (Separator);

         Gtk_New_Hbox (Hbox, False, 10);
         Add (Vbox, Hbox);
         Show (Hbox);

         Gtk_New_Hseparator (Separator);
         Add (Vbox, Separator);
         Show (Separator);

         Gtk_New (Label, "Below");
         Add (Vbox, Label);
         Show (Label);

         Gtk_New (Handle);
         Add (Hbox, Handle);
         Id := Handle_Cb.Connect (Handle, "child_attached",
                                  Child_Signal'Access, "attached");
         Id := Handle_Cb.Connect (Handle, "child_detached",
                                  Child_Signal'Access, "detached");
         Show (Handle);

         Create_Toolbar.Make_Toolbar (Toolbar, Window);
         Add (Handle, Toolbar);
         Show (Toolbar);

         Gtk_New (Handle);
         Add (Hbox, Handle);
         Id := Handle_Cb.Connect (Handle, "child_attached",
                                  Child_Signal'Access, "attached");
         Id := Handle_Cb.Connect (Handle, "child_detached",
                                  Child_Signal'Access, "detached");
         Show (Handle);

         Gtk_New (Handle2);
         Add (Handle, Handle2);
         Id := Handle_Cb.Connect (Handle2, "child_attached",
                                  Child_Signal'Access, "attached");
         Id := Handle_Cb.Connect (Handle2, "child_detached",
                                  Child_Signal'Access, "detached");
         Show (Handle2);

         Gtk_New (Label, "Fooo!");
         Add (Handle2, Label);
         Show (Label);
      end if;

      if Visible_Is_Set (Window) then
         Gtk.Widget.Destroy (Window);
      else
         Show (Window);
      end if;
   end Run;

end Create_Handle_Box;
