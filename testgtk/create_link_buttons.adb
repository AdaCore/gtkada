------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
with Glib;            use Glib;
with Gtk;             use Gtk;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk.Link_Button; use Gtk.Link_Button;
with Gtk.Widget;      use Gtk.Widget;

package body Create_Link_Buttons is

   package Reset_Button_Cb is new Handlers.Callback (Gtk_Link_Button_Record);
   package Uri_Hook is new Generic_Uri_Hook (Integer);
   --  Integer is just a dummy type, we don't actually pass any user data.

   ----------------------------
   -- On_Link_Button_Clicked --
   ----------------------------

   procedure On_Link_Button_Clicked
     (Button : access Gtk_Link_Button_Record'Class;
      Link   : UTF8_String;
      Data   : Integer)
   is
      pragma Unreferenced (Button, Data);
   begin
      Put_Line ("Link_Button clicked: " & Link);
   end On_Link_Button_Clicked;

   -----------------------------
   -- On_Reset_Button_Clicked --
   -----------------------------

   procedure On_Reset_Button_Clicked
     (Widget : access Gtk_Link_Button_Record'Class)
   is
   begin
      Set_Visited (Widget, False);
   end On_Reset_Button_Clicked;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "a @bGtk_Link_Button@B is a @bGtk_Button@B with a hyperlink,"
        & " similar to the kind displayed by web browsers.  It is associated"
        & " with a URI.  The URI is passed to a callback procedure that is"
        & " invoked when the user clicks the button. "
        & " This widget also keeps track of whether it has been visited.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1         : Gtk_Box;
      Link_Button1 : Gtk_Link_Button;
      Reset_Button : Gtk_Button;
      Tmp          : Uri_Func;
      pragma Unreferenced (Tmp);
   begin
      Gtk.Frame.Set_Label (Frame, "Link_Buttons");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Box1);

      Gtk_New_With_Label
        (Widget => Link_Button1,
         Uri    => "http://www.example.com/",
         Label  => "Click me.");
      Uri_Hook.Set_Uri_Hook
        (Handler   => On_Link_Button_Clicked'Access,
         User_Data => 0,
         Destroy   => null);
      Pack_Start
        (Box1, Link_Button1, Expand => False, Fill => False, Padding => 0);

      Gtk_New (Reset_Button, "Reset Link_Button's ""visited"" state");
      Reset_Button_Cb.Object_Connect
        (Reset_Button,
         "clicked",
         Reset_Button_Cb.To_Marshaller (On_Reset_Button_Clicked'Access),
         Link_Button1);
      Pack_Start
        (Box1, Reset_Button, Expand => False, Fill => False, Padding => 0);

      Show_All (Box1);
   end Run;

end Create_Link_Buttons;
