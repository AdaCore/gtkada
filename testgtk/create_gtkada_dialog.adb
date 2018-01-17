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

with Gtk.Button;      use Gtk.Button;
with Gtk.Box;         use Gtk.Box;
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;
with Gtkada.Dialogs;  use Gtkada.Dialogs;

package body Create_Gtkada_Dialog is

   type Dialog_Button_Record is new Gtk_Button_Record with record
      Kind : Message_Dialog_Type;
   end record;
   type Dialog_Button is access all Dialog_Button_Record'Class;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "GtkAda provides some custom widgets and convenient wrappers"
        & " on top of existing gtk+ widgets." & ASCII.LF
        & "This demo shows how to create simple informational dialogs.";
   end Help;

   procedure On_Click (Button : access Gtk_Button_Record'Class) is
      B : constant Dialog_Button := Dialog_Button (Button);
      Dialog : Gtk_Dialog;
      Resp   : Gtk_Response_Type;
      Dummy  : Gtk_Widget;
   begin
      Dialog := Create_Gtk_Dialog
         (Msg         => "Some message",
          Dialog_Type => B.Kind,
          Title       => "Dialog's title",
          Parent      => Gtk_Window (Button.Get_Toplevel));

      Dummy := Dialog.Add_Button ("Close", Gtk_Response_Close);

      Dialog.Show_All;

      --  The following call is blocking until the user closes the dialog
      Resp := Dialog.Run;
      Dialog.Destroy;
   end On_Click;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button  : Dialog_Button;
      Box     : Gtk_Box;

   begin
      Gtk.Frame.Set_Label (Frame, "Gtkada Dialogs");

      Gtk_New_Vbox (Box);
      Frame.Add (Box);

      Button := new Dialog_Button_Record'
         (Gtk_Button_Record with Kind => Information);
      Initialize (Button, "Create information dialog");
      Box.Pack_Start (Button, Expand => False, Fill => False);
      Button.On_Clicked (On_Click'Access);

      Button := new Dialog_Button_Record'
         (Gtk_Button_Record with Kind => Error);
      Initialize (Button, "Create error dialog");
      Box.Pack_Start (Button, Expand => False, Fill => False);
      Button.On_Clicked (On_Click'Access);

      Show_All (Frame);
   end Run;

end Create_Gtkada_Dialog;
