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

with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Label;        use Gtk.Label;
with Gtk.Paned;        use Gtk.Paned;
with Gtk.Table;        use Gtk.Table;
with Gtk.Widget;       use Gtk.Widget;
with Gtk;              use Gtk;
with Common;           use Common;

package body Create_Paned is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Paned@B splits a container in two parts, that can be"
        & " resized by the user." & ASCII.LF
        & "They have two children, one for each side."
        & ASCII.LF
        & "If @bShrink@B is set to True for one of the children, then the user"
        & " can resize it to any size. If it is set to False, then the"
        & " minimum size set for the child by a call to @bSet_Usize@B is"
        & " enforced, and the child can never be shrinked more than that."
        & ASCII.LF
        & "If @bResize@B is set to True for one of the children only, then"
        & " that child gets the exact size it requested, the other gets the"
        & " remaining space. In the example, the two buttons have required"
        & " the same size, and you can see the effect of the resize buttons"
        & " by clicking on them."
        & ASCII.LF
        & "If both children have the same value for resize, their allocated"
        & " size is a ratio between their respective sizes."
        & ASCII.LF
        & "Note that moving the handle to manually resize the widgets"
        & " cancels the effect of the resize buttons, so you should click on"
        & " them before playing with the handles...";
   end Help;

   -------------------
   -- Toggle_Resize --
   -------------------

   procedure Toggle_Resize (Child : access Gtk_Widget_Record'Class) is
      Paned : Gtk_Paned := Gtk_Paned (Get_Parent (Child));
      Is_Child1 : Boolean := Gtk_Widget (Child) = Get_Child1 (Paned);
      Resize : Boolean;
      Shrink : Boolean;
   begin
      if Is_Child1 then
         Resize := Get_Child1_Resize (Paned);
         Shrink := Get_Child1_Shrink (Paned);
      else
         Resize := Get_Child2_Resize (Paned);
         Shrink := Get_Child2_Shrink (Paned);
      end if;

      Ref (Child);
      --  Since we are going to remove it, we do not want
      --  to delete it.
      Remove (Paned, Child);
      if Is_Child1 then
         Pack1 (Paned, Child, not Resize, Shrink);
      else
         Pack2 (Paned, Child, not Resize, Shrink);
      end if;
      Unref (Child);
   end Toggle_Resize;

   -------------------
   -- Toggle_Shrink --
   -------------------

   procedure Toggle_Shrink (Child : access Gtk_Widget_Record'Class) is
      Paned : Gtk_Paned := Gtk_Paned (Get_Parent (Child));
      Is_Child1 : Boolean := Gtk_Widget (Child) = Get_Child1 (Paned);
      Resize : Boolean;
      Shrink : Boolean;
   begin
      if Is_Child1 then
         Resize := Get_Child1_Resize (Paned);
         Shrink := Get_Child1_Shrink (Paned);
      else
         Resize := Get_Child2_Resize (Paned);
         Shrink := Get_Child2_Shrink (Paned);
      end if;

      Ref (Child);
      --  Since we are going to remove it, we do not want
      --  to delete it.
      Remove (Paned, Child);
      if Is_Child1 then
         Pack1 (Paned, Child, Resize, not Shrink);
      else
         Pack2 (Paned, Child, Resize, not Shrink);
      end if;
      Unref (Child);
   end Toggle_Shrink;

   -------------------------
   -- Create_Pane_Options --
   -------------------------

   function Create_Pane_Options (Paned : access Gtk_Paned_Record'Class;
                                 Frame_Label : String;
                                 Label1 : String;
                                 Label2 : String)
                                 return Gtk_Frame
   is
      Frame : Gtk_Frame;
      Table : Gtk_Table;
      Label : Gtk_Label;
      Check : Gtk_Check_Button;

   begin
      Gtk_New (Frame, Frame_Label);
      Set_Border_Width (Frame, 4);

      Gtk_New (Table, 3, 2, False);
      Add (Frame, Table);

      Gtk_New (Label, Label1);
      Attach_Defaults (Table, Label, 0, 1, 0, 1);

      Gtk_New (Check, "Resize");
      Attach_Defaults (Table, Check, 0, 1, 1, 2);
      Set_Active (Check, Get_Child1_Resize (Paned));
      Widget_Handler.Object_Connect
        (Check, "toggled",
         Widget_Handler.To_Marshaller (Toggle_Resize'Access),
         Slot_Object => Get_Child1 (Paned));

      Gtk_New (Check, "Shrink");
      Attach_Defaults (Table, Check, 0, 1, 2, 3);
      Set_Active (Check, Get_Child1_Shrink (Paned));
      Widget_Handler.Object_Connect
        (Check, "toggled",
         Widget_Handler.To_Marshaller (Toggle_Shrink'Access),
         Slot_Object => Get_Child1 (Paned));

      Gtk_New (Label, Label2);
      Attach_Defaults (Table, Label, 1, 2, 0, 1);

      Gtk_New (Check, "Resize");
      Attach_Defaults (Table, Check, 1, 2, 1, 2);
      Set_Active (Check, Get_Child2_Resize (Paned));
      Widget_Handler.Object_Connect
        (Check, "toggled",
         Widget_Handler.To_Marshaller (Toggle_Resize'Access),
         Slot_Object => Get_Child2 (Paned));

      Gtk_New (Check, "Shrink");
      Attach_Defaults (Table, Check, 1, 2, 2, 3);
      Set_Active (Check, Get_Child2_Shrink (Paned));
      Widget_Handler.Object_Connect
        (Check, "toggled",
         Widget_Handler.To_Marshaller (Toggle_Shrink'Access),
         Slot_Object => Get_Child2 (Paned));

      return Frame;
   end Create_Pane_Options;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      VPaned : Gtk_Paned;
      HPaned : Gtk_Paned;
      Frame2  : Gtk_Frame;
      Button : Gtk_Button;
      Vbox   : Gtk_Box;

   begin

      Set_Label (Frame, "Panes");

      Gtk_New_Vbox (Vbox, False, 0);
      Add (Frame, Vbox);

      Gtk_New_Vpaned (VPaned);
      Pack_Start (Vbox, Vpaned, True, True, 0);
      Set_Border_Width (VPaned, 5);

      Gtk_New_Hpaned (HPaned);
      Add1 (Vpaned, HPaned);

      Gtk_New (Frame2);

      Gtk_New (Button, "Hi There");
      Add (Frame2, Button);

      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Usize (Frame2, 60, 60);
      Pack1 (HPaned, Frame2, False, False);


      Gtk_New (Frame2);

      Gtk_New (Button, "erehT iH");
      Add (Frame2, Button);

      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Usize (Frame2, 80, 60);
      Add2 (HPaned, Frame2);


      Gtk_New (Frame2);
      Set_Shadow_Type (Frame2, Shadow_In);
      Set_Usize (Frame2, 60, 280);
      Add2 (VPaned, Frame2);

      Pack_Start (Vbox,
                  Create_Pane_Options (Hpaned, "Horizontal",
                                       "Left", "Right"),
                  False, False, 0);
      Pack_Start (Vbox,
                  Create_Pane_Options (Vpaned, "Vertical",
                                       "Top", "Bottom"),
                  False, False, 0);

      Show_All (Frame);

   end Run;

end Create_Paned;
