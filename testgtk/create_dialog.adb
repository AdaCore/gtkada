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
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Misc; use Gtk.Misc;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

package body Create_Dialog is

   package Label_Cb is new Signal.Object_Callback (Gtk_Label);
   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget);

   Dialog       : Gtk.Dialog.Gtk_Dialog;
   Global_Label : Gtk_Label;

   procedure Label_Toggle (Label : in out Gtk_Label'Class) is
      Id : Guint;
   begin
      if not Is_Created (Label) then
         Gtk_New (Label, "Dialog Test");
         Id := Widget_Cb.Connect (Label, "destroy", Destroy'Access, Label);
         Set_Padding (Label, 10, 10);
         Pack_Start (Get_Vbox (Dialog), Label, True, True, 0);
         Show (Label);
      else
         Gtk.Widget.Destroy (Label);
      end if;
   end Label_Toggle;


   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id     : Guint;
      Button : Gtk_Button;
   begin
      if not Is_Created (Dialog) then
         Gtk_New (Dialog);
         Id := Widget_Cb.Connect (Dialog, "destroy", Destroy'Access, Dialog);
         Set_Title (Dialog, "dialog");
         Border_Width (Dialog, 0);

         Gtk_New (Button, "OK");
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Dialog), Button, True, True, 0);
         Grab_Default (Button);
         Show (Button);

         Gtk_New (Button, "Toggle");
         Id := Label_Cb.Connect (Button, "clicked", Label_Toggle'Access,
                                 Global_Label);
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Dialog), Button, True, True, 0);
         Show (Button);
      end if;

      if Visible_Is_Set (Dialog) then
         Gtk.Widget.Destroy (Dialog);
      else
         Show (Dialog);
      end if;

   end Run;

end Create_Dialog;



