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

with Glib;             use Glib;
with Gtk;              use Gtk;
with Gtk.Box;          use Gtk.Box;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Label;
with Gtk.Widget;

package body Create_Check_Buttons is

   procedure Rename_Button (Self : access Gtk_Check_Button_Record'Class);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Check_Button@B has two possible states, either activated"
        & " or deactivated. A callback can be set each time the state is"
        & " modified.";
   end Help;

   -------------------
   -- Rename_Button --
   -------------------

   procedure Rename_Button (Self : access Gtk_Check_Button_Record'Class) is
      function Rev (S : String) return String is
         T : String (S'Range);
      begin
         for I in reverse S'Range loop
            T (S'Last - I + S'First) := S (I);
         end loop;
         return T;
      end Rev;

      L : constant UTF8_String := Self.Get_Label;
   begin
      Self.Set_Label (Rev (L));
   end Rename_Button;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Outer_Box, Inner_Box : Gtk_Box;
      Check                : Gtk_Check_Button;
      Button_Labels        : constant String := "ABC";
   begin
      Frame.Set_Label ("Check Buttons");

      Box.Gtk_New (Outer_Box, Orientation_Vertical, Spacing => 0);
      Outer_Box.Set_Homogeneous (False);
      Outer_Box.Set_Hexpand (False);
      Outer_Box.Set_Margin_Start (10);
      Outer_Box.Set_Margin_End (10);
      Outer_Box.Set_Margin_Top (10);
      Outer_Box.Set_Margin_Bottom (10);
      Frame.Set_Child (Outer_Box);

      Box.Gtk_New (Inner_Box, Orientation_Vertical, Spacing => 10);
      Inner_Box.Set_Homogeneous (False);
      Inner_Box.Set_Hexpand (False);

      for Button_Name of Button_Labels loop
         Check_Button.Gtk_New (Check);
         Check.Set_Label ("Button " & Button_Name);
         Check.On_Toggled (Rename_Button'Access);
         Inner_Box.Append (Check);
      end loop;
      Outer_Box.Append (Inner_Box);
   end Run;

end Create_Check_Buttons;
