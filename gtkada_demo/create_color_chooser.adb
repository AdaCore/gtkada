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

with Gdk.RGBA;                 use Gdk.RGBA;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Color_Dialog;         use Gtk.Color_Dialog;
with Gtk.Color_Dialog_Button;  use Gtk.Color_Dialog_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;

package body Create_Color_Chooser is

   Dialog       : Gtk_Color_Dialog;
   Color_Button : Gtk_Color_Dialog_Button;
   Result_Label : Gtk_Label;
   --  Shared between Run and the asynchronous callbacks. Demos are re-Run
   --  every time their row is selected, so these are rebuilt on each Run; the
   --  callbacks always act on the most recently built widgets.

   procedure On_Pick (Button : access Gtk_Button_Record'Class);
   --  Drive the asynchronous Gtk.Color_Dialog.Choose_Rgba path explicitly.

   procedure On_Chosen
     (Source_Object : access GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Completion callback for Choose_Rgba: read the chosen colour and show it.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This page demonstrates the Gtk4 colour-selection API."
        & " A @bGtk_Color_Dialog_Button@B is the one-widget, declarative"
        & " path: it renders the currently-selected colour and pops up a"
        & " colour chooser when clicked. The @bPick a colour...@B button"
        & " drives the asynchronous @bGtk_Color_Dialog.Choose_Rgba@B flow"
        & " explicitly; its @bChoose_Rgba_Finish@B callback reads the chosen"
        & " colour and writes its textual form into the label below."
        & " Cancelling the dialog leaves the current colour unchanged.";
   end Help;

   -------------
   -- On_Pick --
   -------------

   procedure On_Pick (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      Dialog.Choose_Rgba
        (Parent        => null,
         Initial_Color => Color_Button.Get_Rgba,
         Cancellable   => null,
         Callback      => On_Chosen'Access);
   end On_Pick;

   ---------------
   -- On_Chosen --
   ---------------

   procedure On_Chosen
     (Source_Object : access GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Warnings (Off, Source_Object);
      Color : constant Gdk_RGBA := Dialog.Choose_Rgba_Finish (Res);
   begin
      if Color = Null_RGBA then
         --  Choose_Rgba_Finish yields a null colour when the user dismisses
         --  the dialog; treat that as "no change".
         Result_Label.Set_Text ("Selection cancelled.");
      else
         Result_Label.Set_Text ("Selected colour: " & To_String (Color));

         --  Keep the colour button in sync with the explicitly-picked colour.
         Color_Button.Set_Rgba (Color);
      end if;
   end On_Chosen;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      Box  : Gtk_Box;
      Pick : Gtk_Button;
   begin
      Frame.Set_Label ("Color Chooser");

      Gtk_New
        (Box, Orientation => Orientation_Vertical, Spacing => 6);
      Frame.Set_Child (Box);

      --  One shared dialog, driving both the button and the explicit path.
      Gtk_New (Dialog);
      Dialog.Set_Title ("Choose a colour");
      Dialog.Set_With_Alpha (True);

      --  The declarative path: a button that shows and updates the colour.
      Gtk_New (Color_Button, Dialog);
      Color_Button.Set_Rgba (Black_RGBA);
      Box.Append (Color_Button);

      --  The explicit asynchronous path.
      Gtk_New (Pick, "Pick a colour...");
      Pick.On_Clicked (On_Pick'Access);
      Box.Append (Pick);

      Gtk_New (Result_Label, "No colour selected yet.");
      Box.Append (Result_Label);
   end Run;

end Create_Color_Chooser;
