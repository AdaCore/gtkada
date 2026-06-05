------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Glib;              use Glib;
with Gtk;               use Gtk;
with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget;        use Gtk.Widget;

package body Create_Toggle_Buttons is

   Align_Range : constant array (1 .. 3) of Gtk_Align :=
     (Align_Start, Align_Center, Align_End);

   procedure Get_Toggled (Self : access Gtk_Toggle_Button_Record'Class);
   --  Do something cool when pressed.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Toggle_Button@B is a button with two possible states: "
        & "on (active) or off. This state switches each time the user presses "
        & "the button."
        & ASCII.LF
        & "By default, multiple toggle buttons may be active at the same time. "
        & "However, if the toggle buttons are all added to the same group "
        & " then only one toggle button in the group may be active at a time."
        & ASCII.LF
        & "This behaviour mimics the @bGtk_Radio_Button@B widget, "
        & "which has been removed in GTK4.";
   end Help;

   -----------------
   -- Get_Toggled --
   -----------------

   procedure Get_Toggled (Self : access Gtk_Toggle_Button_Record'Class) is
      Idx : constant Positive := Positive (Self.Get_Margin_Start);
   begin
      if Self.Get_Active then
         Self.Set_Label ("BOOM!");
         Self.Set_Halign (Align_Fill);
         Self.Set_Valign (Align_Fill);
      else
         Self.Set_Label ("Toggle " & Idx'Img);
         Self.Set_Valign (Align_Range (Idx));
         Self.Set_Halign (Align_Center);
      end if;
   end Get_Toggled;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Toggle : Gtk_Toggle_Button;
      Outer, Inner : Box.Gtk_Box;

      procedure Add_Toggle_Button
        (Idx : Positive; Container : Box.Gtk_Box);
      --  Add new Gtk_Toggle_Button to Container

      -----------------------
      -- Add_Toggle_Button --
      -----------------------

      procedure Add_Toggle_Button
        (Idx : Positive; Container : Box.Gtk_Box) is
      begin
         Gtk_New_With_Label (Toggle, "Toggle " & Idx'Img);
         --  Place buttons in a central vertical stack
         Toggle.Set_Valign (Align_Range (Idx));
         Toggle.Set_Halign (Align_Center);
         --  Allow buttons to grow in size
         Toggle.Set_Vexpand (True);
         Toggle.Set_Hexpand (True);
         --  This is a terrible way to encode index
         Toggle.Set_Margin_Start (Glib.Gint (Idx));
         Toggle.On_Toggled (Get_Toggled'Access);
         Container.Append (Toggle);
      end Add_Toggle_Button;
   begin
      Set_Label (Frame, "Toggle Buttons");
      Frame.Set_Label_Align (0.5);

      Box.Gtk_New (Outer, Orientation_Vertical, Spacing => 10);
      Outer.Set_Homogeneous (False);
      Outer.Set_Margin_Start (10);
      Outer.Set_Margin_End (10);
      Outer.Set_Margin_Top (10);
      Outer.Set_Margin_Bottom (10);
      Frame.Set_Child (Outer);

      Box.Gtk_New (Inner, Orientation_Vertical, Spacing => 10);
      Inner.Set_Homogeneous (False);
      Inner.Set_Margin_Start (10);
      Inner.Set_Margin_End (10);
      Outer.Set_Margin_Top (10);
      Outer.Set_Margin_Bottom (10);
      Outer.Append (Inner);

      for T in Align_Range'Range loop
         Add_Toggle_Button (T, Inner);
      end loop;

   end Run;

end Create_Toggle_Buttons;
