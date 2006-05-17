-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2003 ACT Europe                 --
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

with Common;       use Common;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Dialog;   use Gtk.Dialog;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Label;    use Gtk.Label;
with Gtk.Widget;   use Gtk.Widget;
with Gtk;          use Gtk;

package body Create_Dialog is
   type Gtk_Label_Access is access all Gtk_Label;
   package Label_Destroy is new Handlers.User_Callback
     (Gtk_Label_Record, Gtk_Label_Access);

   procedure Destroyed (Lab : access Gtk_Label_Record'Class;
                        Ptr : in Gtk_Label_Access);

   Dialog       : aliased Gtk.Dialog.Gtk_Dialog;
   Global_Label : aliased Gtk_Label;

   function Help return String is
   begin
      return "A @bGtk_dialog@B is a separate window, usually used to print"
        & " a message for the user, or signal an error." & ASCII.LF
        & "A @bGtk_Dialog@B is split into two boxes, its @bVbox@B that will"
        & " contain the message, and an @bAction_Area@B that contains a"
        & " series of button, like OK, Cancel or Help.";
   end Help;

   procedure Destroyed (Lab : access Gtk_Label_Record'Class;
                        Ptr : in Gtk_Label_Access)
   is
      pragma Warnings (Off, Lab);
   begin
      Ptr.all := null;
   end Destroyed;

   procedure Label_Toggle (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);

   begin
      if Global_Label = null then
         Gtk_New (Global_Label, "Dialog Test");
         Label_Destroy.Connect
           (Global_Label, "destroy",
            Label_Destroy.To_Marshaller (Destroyed'Access),
            Global_Label'Access);
         Set_Padding (Global_Label, 10, 10);
         Pack_Start (Get_Vbox (Dialog), Global_Label, True, True, 0);
         Show (Global_Label);
      else
         Destroy (Global_Label);
      end if;
   end Label_Toggle;


   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button : Gtk_Button;

   begin
      Set_Label (Frame, "Dialog");
      if Dialog = null then
         Gtk_New (Dialog);
         Destroy_Dialog_Handler.Connect
           (Dialog, "destroy",
            Destroy_Dialog_Handler.To_Marshaller (Destroy_Dialog'Access),
            Dialog'Access);
         Set_Title (Dialog, "Gtk_Dialog");
         Set_Border_Width (Dialog, 0);
         Set_USize (Dialog, 200, 110);

         Gtk_New (Button, "OK");
         Set_Flags (Button, Can_Default);
         Pack_Start (Get_Action_Area (Dialog), Button, True, True, 0);
         Grab_Default (Button);
         Show (Button);

         Gtk_New (Button, "Toggle");
         Widget_Handler.Connect
           (Button, "clicked",
            Widget_Handler.To_Marshaller (Label_Toggle'Access));
         Pack_Start (Get_Action_Area (Dialog), Button, True, True, 0);
         Show (Button);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;

   end Run;

end Create_Dialog;



