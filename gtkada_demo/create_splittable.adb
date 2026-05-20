------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib.Object;        use Glib.Object;
with Gtk;                use Gtk;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.Multi_Paned; use Gtkada.Multi_Paned;
with Gtk.Toolbar;        use Gtk.Toolbar;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtk.Vbutton_Box;    use Gtk.Vbutton_Box;
with Gtk.Toggle_Button;  use Gtk.Toggle_Button;
with Gtk.Tool_Button;    use Gtk.Tool_Button;
with Gtk.Enums;          use Gtk.Enums;

package body Create_Splittable is

   function Create_Child
      (Bar : Gtk_Toolbar; Title : String) return Gtk_Widget;
   procedure On_Destroy (Button : access GObject_Record'Class);
   procedure On_Toggle  (Button : access Gtk_Widget_Record'Class);
   procedure On_Resize  (Button : access Gtk_Widget_Record'Class);
   procedure On_Split_V (Button : access Gtk_Widget_Record'Class);
   procedure On_Split_H (Button : access Gtk_Widget_Record'Class);
   procedure On_Fixed   (Button : access Gtk_Widget_Record'Class);
   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class);

   Pane   : Gtkada_Multi_Paned;
   Item   : Natural := 6;
   Opaque : Boolean := False;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A Gtkada-specific widget, where children can be resized"
        & " interactively by the user, as well as splitted." & ASCII.LF
        & ASCII.LF
        & "It is similar to a Gtk.Paned widget, but with a number of"
        & " differences: there are multiple children (only two for a"
        & " Gtk_Paned), which cannot be emulated by nested multiple"
        & " Gtk_Paned widgets. For instance, if you drag the right-most"
        & " vertical separator towards the left, it will eventually also"
        & " move the left-most separator. This would not happen if you"
        & " had two nested Gtk_Paned." & ASCII.LF
        & ASCII.LF
        & "It provides non-opaque resizing, whereas a Gtk_Paned only"
        & " provides opaque resizing. The latter can be slow when the"
        & " application is running on a remote machine with a local"
        & " display.";
   end Help;

   ---------------
   -- On_Opaque --
   ---------------

   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Opaque := not Opaque;
      Set_Opaque_Resizing (Pane, Opaque);
   end On_Opaque;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Button : access GObject_Record'Class) is
   begin
      Destroy (Gtk_Widget (Button));
   end On_Destroy;

   ---------------
   -- On_Toggle --
   ---------------

   procedure On_Toggle (Button : access Gtk_Widget_Record'Class) is
   begin
      if Button.Get_Visible then
         Hide (Button);
      else
         Show (Button);
      end if;
   end On_Toggle;

   ---------------
   -- On_Resize --
   ---------------

   procedure On_Resize (Button : access Gtk_Widget_Record'Class) is
   begin
      Set_Size (Pane, Button, 100, 100);
   end On_Resize;

   ----------------
   -- On_Split_V --
   ----------------

   procedure On_Split_V (Button : access Gtk_Widget_Record'Class) is
      Child : constant Gtk_Widget := Create_Child (null, Integer'Image (Item));
   begin
      Item := Item + 1;
      Split (Pane, Button, Child, Orientation_Vertical);
   end On_Split_V;

   ----------------
   -- On_Split_H --
   ----------------

   procedure On_Split_H (Button : access Gtk_Widget_Record'Class) is
      Child : constant Gtk_Widget := Create_Child (null, Integer'Image (Item));
   begin
      Item := Item + 1;
      Split (Pane, Button, Child, Orientation_Horizontal);
   end On_Split_H;

   --------------
   -- On_Fixed --
   --------------

   procedure On_Fixed (Button : access Gtk_Widget_Record'Class) is
   begin
      Set_Size (Pane, Button,
                Get_Allocated_Width (Button),
                Get_Allocated_Height (Button),
                Fixed_Size => True);
   end On_Fixed;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child
      (Bar : Gtk_Toolbar; Title : String) return Gtk_Widget
   is
      Frame  : Gtk_Frame;
      Box    : Gtk_Vbutton_Box;
      Button : Gtk_Button;
      Item   : Gtk_Tool_Button;
   begin
      Gtk_New (Frame);

      Gtk_New (Box);
      Add (Frame, Box);
      Set_Layout (Box, Buttonbox_Start);

      Gtk_New (Button, "Destroy_" & Title);
      Pack_Start (Box, Button, Expand => False);
      Button.On_Clicked (On_Destroy'Access, Slot => Frame);

      Gtk_New (Button, "Resize_" & Title);
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Resize'Unrestricted_Access, Frame);

      Gtk_New (Button, "Split_V " & Title);
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Split_V'Unrestricted_Access, Frame);

      Gtk_New (Button, "Split_H " & Title);
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Split_H'Unrestricted_Access, Frame);

      Gtk_New (Button, "Fixed_Size " & Title);
      Pack_Start (Box, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", On_Fixed'Unrestricted_Access, Frame);

      if Bar /= null then
         Gtk_New (Item, Label => "Toggle_" & Title);
         Add (Bar, Item);
         Widget_Callback.Object_Connect
           (Item, "clicked",
            Widget_Callback.To_Marshaller (On_Toggle'Unrestricted_Access),
            Frame);
      end if;

      Show_All (Frame);
      return Gtk_Widget (Frame);
   end Create_Child;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button, Button1, Button2, Button3, Button4 : Gtk_Widget;
      Bar    : Gtk_Toolbar;
      Box    : Gtk_Box;
      Toggle : Gtk_Toggle_Button;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Bar);
      Pack_Start (Box, Bar, Expand => False);

      Gtk_New (Toggle, "Opaque Resizing");
      Pack_Start (Box, Toggle, Expand => False);
      Widget_Callback.Connect (Toggle, "toggled", On_Opaque'Access);

      Gtk_New (Pane);
      Pack_Start (Box, Pane, Expand => True, Fill => True);

      Button1 := Create_Child (Bar, "1");
      Add_Child (Pane, Button1);

      Button2 := Create_Child (Bar, "2");
      Add_Child (Pane, Button2);  --  Should split horizontally

      Button3 := Create_Child (Bar, "3");
      Add_Child (Pane, Button3);  --  Should split horizontally

      Button4 := Create_Child (Bar, "4");
      Split (Pane, Button2, Button4, Orientation_Vertical);

      Button := Create_Child (Bar, "5");
      Split (Pane, Button4, Button, Orientation_Horizontal);

      Show_All (Frame);
   end Run;

end Create_Splittable;
