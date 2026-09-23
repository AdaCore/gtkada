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

with Ada.Text_IO;     use Ada.Text_IO;
with Glib.Object;     use Glib.Object;
with Gtk;             use Gtk;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Widget;      use Gtk.Widget;

package body Create_Link_Buttons is

   function On_Link_Button_Clicked
     (Button : access Gtk_Link_Button_Record'Class) return Boolean;
   --  Report the URI and mark the button visited, returning True so that
   --  Gtk.Link_Button's default handler does not hand the URI to a launcher.

   procedure On_Reset_Button_Clicked (Widget : access GObject_Record'Class);
   --  Clear the visited state of the link button passed as the slot.

   ----------------------------
   -- On_Link_Button_Clicked --
   ----------------------------

   function On_Link_Button_Clicked
     (Button : access Gtk_Link_Button_Record'Class) return Boolean is
   begin
      Put_Line ("Link_Button clicked: " & Button.Get_Uri);
      Set_Visited (Button, True);
      return True;
   end On_Link_Button_Clicked;

   -----------------------------
   -- On_Reset_Button_Clicked --
   -----------------------------

   procedure On_Reset_Button_Clicked (Widget : access GObject_Record'Class) is
   begin
      Set_Visited (Gtk_Link_Button (Widget), False);
   end On_Reset_Button_Clicked;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Link_Button@B is a @bGtk_Button@B associated with a"
        & " URI, styled like a hyperlink in a web browser. Clicking it"
        & " invokes a callback with that URI, and the button remembers"
        & " whether it has been visited.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1         : Gtk_Box;
      Link_Button1 : Gtk_Link_Button;
      Reset_Button : Gtk_Button;
   begin
      Gtk.Frame.Set_Label (Frame, "Link Buttons");

      Gtk_New (Box1, Orientation => Orientation_Vertical, Spacing => 6);
      Box1.Set_Homogeneous (False);
      Frame.Set_Child (Box1);

      Gtk_New_With_Label
        (Self  => Link_Button1,
         URI   => "http://www.example.com/",
         Label => "Click me.");
      Link_Button1.Set_Halign (Align_Start);
      Link_Button1.On_Activate_Link (On_Link_Button_Clicked'Access);
      Box1.Append (Link_Button1);

      Gtk_New (Reset_Button, "Reset Link_Button's ""visited"" state");
      Reset_Button.Set_Halign (Align_Start);
      Reset_Button.On_Clicked (On_Reset_Button_Clicked'Access, Link_Button1);
      Box1.Append (Reset_Button);
   end Run;

end Create_Link_Buttons;
