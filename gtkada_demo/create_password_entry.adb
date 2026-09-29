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

with Glib.Properties;    use Glib.Properties;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Editable;       use Gtk.Editable;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Label;          use Gtk.Label;
with Gtk.Password_Entry; use Gtk.Password_Entry;
with Gtk.Widget;         use Gtk.Widget;

package body Create_Password_Entry is

   Password : Gtk_Password_Entry;
   Confirm  : Gtk_Password_Entry;
   Done     : Gtk_Button;
   Status   : Gtk_Label;
   --  The widgets the callbacks work on

   function Acceptable return Boolean;
   --  Whether the two entries hold the same, non-empty, password

   procedure Update;
   --  Bring Done and Status in line with what the entries hold

   procedure On_Changed (Self : Gtk_Editable);
   procedure On_Activate (Self : access Gtk_Password_Entry_Record'Class);
   procedure On_Done (Self : access Gtk_Button_Record'Class);

   ----------------
   -- Acceptable --
   ----------------

   function Acceptable return Boolean is
     (Password.Get_Text /= "" and then Password.Get_Text = Confirm.Get_Text);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "A @bGtk_Password_Entry@B is an entry for secrets. It never shows"
        & " its contents in clear text and does not let them be copied to"
        & " the clipboard, and it warns when Caps Lock is on."
        & ASCII.LF
        & "@bSet_Show_Peek_Icon@B adds the icon at the end of the first"
        & " entry, which reveals the text for as long as it is toggled."
        & " Everything else comes from @bGtk_Editable@B, which is how"
        & " @bOn_Changed@B tells us to recheck the two entries, and from the"
        & " @bactivates-default@B and @bplaceholder-text@B properties."
        & ASCII.LF
        & "Done only becomes available when both entries hold the same"
        & " password. Pressing Enter in an entry does the same as clicking"
        & " it, once that is so.";
   end Help;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Done.Set_Sensitive (Acceptable);
      if Confirm.Get_Text = "" or else Acceptable then
         Status.Set_Text ("");
      else
         Status.Set_Text ("The passwords do not match");
      end if;
   end Update;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed (Self : Gtk_Editable) is
      pragma Unreferenced (Self);
   begin
      Update;
   end On_Changed;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Self : access Gtk_Password_Entry_Record'Class) is
      pragma Unreferenced (Self);
   begin
      if Acceptable then
         Status.Set_Text ("Password accepted");
      end if;
   end On_Activate;

   -------------
   -- On_Done --
   -------------

   procedure On_Done (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Status.Set_Text ("Password accepted");
   end On_Done;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Page : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 12);
   begin
      Frame.Set_Label ("Password Entry");

      Page.Set_Margin_Top (12);
      Page.Set_Margin_Bottom (12);
      Page.Set_Margin_Start (12);
      Page.Set_Margin_End (12);
      Frame.Set_Child (Page);

      Gtk_New (Password);
      Password.Set_Show_Peek_Icon (True);
      Set_Property (Password, Placeholder_Text_Property, "Password");
      Password.On_Activate (On_Activate'Access);
      On_Changed (+Password, On_Changed'Access);
      Page.Append (Password);

      Gtk_New (Confirm);
      Set_Property (Confirm, Placeholder_Text_Property, "Confirm password");
      Confirm.On_Activate (On_Activate'Access);
      On_Changed (+Confirm, On_Changed'Access);
      Page.Append (Confirm);

      Gtk_New (Status, "");
      Status.Set_Xalign (0.0);
      Page.Append (Status);

      Gtk_New (Done, "Done");
      Done.Set_Halign (Align_End);
      Done.Set_Sensitive (False);
      Done.On_Clicked (On_Done'Access);
      Page.Append (Done);
   end Run;

end Create_Password_Entry;
