------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2026, AdaCore                     --
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

with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Widget; use Gtk.Widget;

package body Create_Info_Bar is
   procedure Dismiss
     (Self : access Gtk_Info_Bar_Record'Class; Response_Id : Gint)
   is
      pragma Unreferenced (Response_Id);
   begin
      Self.Set_Revealed (False);
   end Dismiss;

   procedure Restore (Self : access GObject_Record'Class) is
   begin
      Gtk_Info_Bar (Self).Set_Revealed (True);
   end Restore;

   function Help return String is
   begin
      return "A @bGtk_Info_Bar@B displays a message and optional actions"
        & " inside a window. Each row shows a different message type."
        & ASCII.LF
        & "The action and close buttons emit @bresponse@B; this demo"
        & " conceals the bar on any response. Use Show again to restore it."
        & ASCII.LF
        & "The toggle controls the standard close button. Gtk.Info_Bar"
        & " is deprecated in GTK 4 but remains available for existing apps.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 12);
      Bar : Gtk_Info_Bar;
      Row : Gtk_Box;
      Text : Gtk_Label;
      Action : Gtk_Widget;
      Show : Gtk_Button;
      Close : Gtk_Toggle_Button;
   begin
      Frame.Set_Label ("Info Bars");
      Frame.Set_Child (Box);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      for Kind in Gtk_Message_Type loop
         Gtk_New (Bar);
         Bar.Set_Message_Type (Kind);
         Bar.Set_Show_Close_Button (True);
         Gtk_New (Text, "Message type: " & Gtk_Message_Type'Image (Kind));
         Bar.Add_Child (Text);
         Action := Bar.Add_Button ("_Dismiss", 1);
         Action.Set_Tooltip_Text ("Conceal this message");
         Bar.On_Response (Dismiss'Access);
         Box.Append (Bar);
         Gtk_New (Row, Orientation_Horizontal, 6);
         Box.Append (Row);
         Gtk_New (Show, "Show again");
         Show.On_Clicked (Restore'Access, Slot => Bar);
         Row.Append (Show);
         Gtk_New_With_Label (Close, "Show close button");
         Close.Set_Active (True);
         Close.Bind_Property ("active", Bar, "show-close-button");
         Row.Append (Close);
      end loop;
   end Run;
end Create_Info_Bar;
