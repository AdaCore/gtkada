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

with Ada.Text_IO;
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Box;        use Gtk.Box;
with Gtk.Button;     use Gtk.Button;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk;            use Gtk;

package body Create_Status is

   Counter : Gint := 1;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Status_Bar@B is generally found at the bottom of"
        & " application windows. It displays some help text, generally"
        & " depending on the current context in the application."
        & ASCII.LF
        & "In GtkAda, @bGtk_Status_Bar@B are organized as stacks: you push"
        & " some text onto the stack, and the top of the stack is always"
        & " displayed. You can then pop part of the stak to display the older"
        & " content of the @bGtk_Status_Bar@B. This behavior is especially"
        & " for temporarily displaying help in the status bar, but still being"
        & " able to show some other informations.";
   end Help;

   ----------
   -- Push --
   ----------

   procedure Push (Status : access GObject_Record'Class) is
      Id : Message_Id;
      S : constant Gtk_Status_Bar := Gtk_Status_Bar (Status);
      pragma Unreferenced (Id);
   begin
      Id := Push (S, 1, "Something" & Gint'Image (Counter));
      Counter := Counter + 1;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Status : access GObject_Record'Class) is
      S : constant Gtk_Status_Bar := Gtk_Status_Bar (Status);
   begin
      Pop (S, 1);
   end Pop;

   -----------
   -- Steal --
   -----------

   procedure Steal (Status : access GObject_Record'Class) is
      S : constant Gtk_Status_Bar := Gtk_Status_Bar (Status);
   begin
      Remove (S, 1, 4);
   end Steal;

   --------------
   -- Contexts --
   --------------

   procedure Contexts (Status : access GObject_Record'Class) is
      S : constant Gtk_Status_Bar := Gtk_Status_Bar (Status);
   begin
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "any context"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (S, "any context")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "idle messages"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (S, "idle message")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "some text"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (S, "some text")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (S, "hit the mouse")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse2"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (S, "hit the mouse2")));
   end Contexts;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1,
        Box2    : Gtk_Box;
      Status    : Gtk_Status_Bar;
      Button    : Gtk_Button;

   begin
      Set_Label (Frame, "Status Bar");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Status);
      Pack_End (Box1, Status, False, False, 0);

      Gtk_New (Button, "Push Something");
      Pack_Start (Box2, Button, False, False, 0);
      Button.On_Clicked (Push'Access, Status);

      Gtk_New (Button, "Pop");
      Pack_Start (Box2, Button, False, False, 0);
      Button.On_Clicked (Pop'Access, Status);

      Gtk_New (Button, "Steal Message_Id #4");
      Pack_Start (Box2, Button, False, False, 0);
      Button.On_Clicked (Steal'Access, Status);

      Gtk_New (Button, "Test contexts");
      Pack_Start (Box2, Button, False, False, 0);
      Button.On_Clicked (Contexts'Access, Status);

      Show_All (Frame);
   end Run;

end Create_Status;
