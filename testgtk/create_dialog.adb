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
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Label; use Gtk.Label;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Common; use Common;
with Gtk; use Gtk;

package body Create_Dialog is
   type Gtk_Label_access is access all Gtk_Label;
   package Label_Destroy is new Signal.Callback (Gtk_Label_Record,
                                                 Gtk_Label_Access);
   procedure Destroyed (Lab : access Gtk_Label_Record;
                        Ptr : in Gtk_Label_Access);

   type Gtk_Dialog_access is access all Gtk_Dialog;
   package Dialog_Destroy is new Signal.Callback (Gtk_Dialog_Record,
                                                  Gtk_Dialog_Access);
   procedure Destroyed (Lab : access Gtk_Dialog_Record;
                        Ptr : in Gtk_Dialog_Access);

   Dialog       : aliased Gtk.Dialog.Gtk_Dialog;
   Global_Label : aliased Gtk_Label;

   procedure Destroyed (Lab : access Gtk_Label_Record;
                        Ptr : in Gtk_Label_Access) is
   begin
      Ptr.all := null;
   end Destroyed;

   procedure Destroyed (Lab : access Gtk_Dialog_Record;
                        Ptr : in Gtk_Dialog_Access) is
   begin
      Ptr.all := null;
   end Destroyed;

   procedure Label_Toggle (Label : access Gtk_Label_Record) is
      pragma Warnings (Off, Label);
      Id : Guint;
   begin
      if Global_Label = null then
         Gtk_New (Global_Label, "Dialog Test");
         Id := Label_Destroy.Connect
           (Global_Label, "destroy", Destroyed'Access, Global_Label'Access);
         Set_Padding (Global_Label, 10, 10);
         Pack_Start (Get_Vbox (Dialog), Global_Label, True, True, 0);
         Show (Global_Label);
      else
         Destroy (Global_Label);
      end if;
   end Label_Toggle;


   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Id     : Guint;
      Button : Gtk_Button;
   begin
      if Dialog = null then
         Gtk_New (Dialog);
         Id := Dialog_Destroy.Connect
           (Dialog, "destroy", Destroyed'Access, Dialog'Access);
         Set_Title (Dialog, "dialog");
         Set_Border_Width (Dialog, 0);

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
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;

   end Run;

end Create_Dialog;



