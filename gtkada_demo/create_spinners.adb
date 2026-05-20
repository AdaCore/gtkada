------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Glib.Main;       use Glib.Main;
with Glib.Properties; use Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Label;       use Gtk.Label;
with Gtk.Spinner;     use Gtk.Spinner;
with Gtk.Grid;        use Gtk.Grid;
with Gtk.Widget;      use Gtk.Widget;

with Common;          use Common;

package body Create_Spinners is

   --  Timer for pulsing activity of one of our spinners.
   package Time_Cb is new Glib.Main.Generic_Sources (Gtk_Spinner);

   --  Function passed to Time_Cb.Timeout_Add, to be invoked periodically.
   function Spinner_Timeout (Spinner : Gtk_Spinner) return Boolean;

   --  A handle referencing our timeout
   Timer : G_Source_Id := No_Source_Id;

   procedure Stop_Timeout (Widget : access Gtk_Widget_Record'Class);
   --  Callback invoked when our spinner widget is destroyed.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A %bGtk_Spinner%B widget displays an icon-size spinning"
        & " animation. It is often used as an alternative to a"
        & " %bGtk_Progress%B for displaying indefinite activity, instead"
        & " of actual progress.  To start the animation, use"
        & " %bGtk.Spinner.Start%B; to stop it use $bGtk.Spinner.Stop$B.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Active_Spinner, Transition_Spinner, Inactive_Spinner : Gtk_Spinner;
      Active_Label,   Transition_Label,   Inactive_Label   : Gtk_Label;
      Table1 : Gtk_Grid;
   begin
      Set_Label (Frame, "Spinners");

      Gtk_New (Table1);
      Frame.Add (Table1);

      Gtk_New (Active_Label, "Active spinner:");
      Gtk_New (Active_Spinner);
      Table1.Attach (Active_Label, 0, 0);
      Table1.Attach (Active_Spinner, 1, 0);

      Gtk_New (Transition_Label, "On/Off spinner:");
      Gtk_New (Transition_Spinner);
      Table1.Attach (Transition_Label, 0, 1);
      Table1.Attach (Transition_Spinner, 1, 1);

      Gtk_New (Inactive_Label, "Inactive spinner:");
      Gtk_New (Inactive_Spinner);
      Table1.Attach (Inactive_Label, 0, 2);
      Table1.Attach (Inactive_Spinner, 1, 2);

      --  Start one spinner, set another pulsing, and don't touch the
      --  third (so that it stays off).
      Gtk.Spinner.Start (Active_Spinner);

      Timer := Time_Cb.Timeout_Add
        (1_000, Spinner_Timeout'Access, Transition_Spinner);

      --  Make sure to disengage the timer if the spinner is destroyed,
      --  otherwise when Spinner_Timeout is called we'll raise an
      --  exception.
      Widget_Handler.Connect
        (Transition_Spinner, "destroy",
         Widget_Handler.To_Marshaller (Stop_Timeout'Access));

      Show_All (Frame);
   end Run;

   ---------------------
   -- Spinner_Timeout --
   ---------------------

   function Spinner_Timeout (Spinner : Gtk_Spinner) return Boolean is
   begin
      case Get_Property (Spinner, Active_Property) is
         when True  => Stop  (Spinner);
         when False => Start (Spinner);
      end case;

      return True;
   end Spinner_Timeout;

   ------------------
   -- Stop_Timeout --
   ------------------

   procedure Stop_Timeout (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      if Timer /= No_Source_Id then
         Remove (Timer);
         Timer := No_Source_Id;
      end if;
   end Stop_Timeout;

end Create_Spinners;
