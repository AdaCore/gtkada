-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2003                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2004-2010, AdaCore              --
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

with Gdk.Color;          use Gdk.Color;
with Gtk;                use Gtk;
with Gtk.Accel_Group;    use Gtk.Accel_Group;
with Gtk.Box;            use Gtk.Box;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Label;          use Gtk.Label;
with Gtk.Menu;           use Gtk.Menu;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.MDI;         use Gtkada.MDI;
with Gtk.Toolbar;        use Gtk.Toolbar;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtk.Toggle_Tool_Button;  use Gtk.Toggle_Tool_Button;
with Gtk.Enums;          use Gtk.Enums;

package body Create_MDI is

   package Desktops is new Gtkada.MDI.Desktop (Integer);

   function Create_Child return MDI_Child;
   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class);
   procedure Do_Configure (MDI : access MDI_Window_Record'Class);

   MDI    : MDI_Window;
   Item   : Natural := 1;
   Opaque : Boolean := False;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A Gtkada specific widget." & ASCII.LF
        & "This is based on the GtkAda multi panned widget. You should"
        & " try the corresponding demo to find out about other capabilities"
        & " like splitting windows or using fixed sizes." & ASCII.LF
        & "In the MDI, windows can be dragged around to be reorganized."
        & " The MDI also supports the notion of perspectives: when you"
        & " select another perspective, some of the windows (the ones in the"
        & " central area) will be preserved. All other windows will only"
        & " remain visible if they also are in the other perspective."
        & ASCII.LF
        & "The MDI also provides the notion of desktop (the current layout"
        & " of your windows in all the perspectives). Such a desktop can be"
        & " saved to the disk, and restored when the application is"
        & " restarted (not shown in this demo). This is actually how"
        & " the default perspectives themselves can be defined initially"
        & " when you package your application."
        & ASCII.LF
        & "Windows can be @bfloated@B, ie put outside of the MDI, by"
        & " the user. This provides a convenient use for many users who"
        & " prefer having multiple windows. It is also a convenient way"
        & " to use multiple screens. You can either float a window"
        & " programmatically (and provide a menu to the user to do so),"
        & " or the user can drag a window outside of the MDI to float it"
        & ASCII.LF
        & "A contextual menu exists in the notebook tabs to close windows,"
        & " or change the location of tabs.";
   end Help;

   ------------------
   -- Do_Configure --
   ------------------

   procedure Do_Configure (MDI : access MDI_Window_Record'Class) is
      Bg_Color, Title_Color, Focus_Color : Gdk_Color;
   begin
      Bg_Color := Parse ("#8A8A8A");
      Alloc (Get_Colormap (MDI), Bg_Color);

      Title_Color := Parse ("#7D7D7D");
      Alloc (Get_Colormap (MDI), Title_Color);

      Focus_Color := Parse ("#5894FA");
      Alloc (Get_Colormap (MDI), Focus_Color);

      Configure (MDI,
                 Background_Color  => Bg_Color,
                 Title_Bar_Color   => Title_Color,
                 Focus_Title_Color => Focus_Color,
                 Opaque_Resize     => Opaque,
                 Show_Tabs_Policy  => Automatic,
                 Tabs_Position     => Gtk.Enums.Pos_Bottom,
                 Draw_Title_Bars   => Always);
   end Do_Configure;

   ---------------
   -- On_Opaque --
   ---------------

   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Opaque := not Opaque;
      Do_Configure (MDI);
   end On_Opaque;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child return MDI_Child is
      Child : MDI_Child;
      Box    : Gtk_Box;
      Label  : Gtk_Label;
   begin
      Gtk_New_Vbox (Box);
      Gtk_New (Child, Box, Flags => All_Buttons, Group => Group_Default);

      Gtk_New (Label, "This is the" & Integer'Image (Item) & " window");
      Pack_Start (Box, Label);

      Set_Title (Child, "Window" & Integer'Image (Item));

      Item := Item + 1;

      Show_All (Child);
      return Child;
   end Create_Child;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Child : array (1 .. 5) of MDI_Child;
      Pos   : constant array (Child'Range) of Child_Position :=
         (Position_Automatic,
          Position_Automatic,
          Position_Automatic,
          Position_Automatic,
          Position_Automatic);
      Bar    : Gtk_Toolbar;
      Box    : Gtk_Box;
      Toggle : Gtk_Toggle_Tool_Button;
      Group  : Gtk_Accel_Group;

      Menu   : Gtk_Menu;
      Menu_Button : Gtk_Menu_Tool_Button;

      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Bar);
      Pack_Start (Box, Bar, Expand => False);

      Gtk_New (Group);
      Gtk_New (MDI, Group => Group);
      Do_Configure (MDI);
      Pack_End (Box, MDI, Expand => True);

      Menu := Desktops.Create_Menu (MDI, User => 1);  --  User irrelevant here

      Gtk_New (Toggle);
      Set_Label (Toggle, "Opaque Resizing");
      Insert (Bar, Toggle);
      Widget_Callback.Connect (Toggle, "toggled", On_Opaque'Access);

      Gtk_New (Menu_Button, Label => "Menu");
      Set_Menu (Menu_Button, Menu);
      Insert (Bar, Menu_Button);

      --  Setup a minimal desktop. This is a required call to finish the
      --  setup of the MDI
      --  Success := Desktops.Restore_Desktop (MDI, null, null, 1);

      for N in Child'Range loop
         Child (N) := Create_Child;
         Put (MDI, Child (N), Pos (N));
      end loop;

      Show_All (Frame);
   end Run;

end Create_MDI;
