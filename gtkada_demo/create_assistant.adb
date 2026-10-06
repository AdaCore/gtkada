------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Assistant;      use Gtk.Assistant;
with Gtk.Assistant_Page; use Gtk.Assistant_Page;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Editable;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.Label;          use Gtk.Label;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.Window;         use Gtk.Window;

package body Create_Assistant is

   procedure Open_Assistant (Self : access Gtk_Button_Record'Class);
   procedure Finish (Self : access Gtk_Assistant_Record'Class);
   procedure Apply (Self : access Gtk_Assistant_Record'Class);
   procedure Prepare
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk_Widget_Record'Class);
   procedure Input_Changed (Self : access GObject_Record'Class);

   function Help return String is
   begin
      return
        "@bGtk_Assistant@B guides the user through a sequence of pages."
        & " Each child has a @bGtk_Assistant_Page@B holding its title,"
        & " type and completeness. The assistant chooses its navigation"
        & " buttons from these properties."
        & ASCII.LF
        & "Enter a name to complete the first page and enable Next."
        & " The @bprepare@B signal fills the confirmation page;"
        & " @bapply@B commits the choice and moves to the summary."
        & " Close or Cancel destroys the assistant.";
   end Help;

   procedure Finish (Self : access Gtk_Assistant_Record'Class) is
   begin
      Self.Destroy;
   end Finish;

   procedure Input_Changed (Self : access GObject_Record'Class) is
      Page : constant Gtk_Assistant_Page := Gtk_Assistant_Page (Self);
      Input : constant Gtk_Entry :=
        Gtk_Entry (Page.Get_Child.Get_Last_Child);
   begin
      Set_Property (Page, Complete_Property, Input.Get_Text /= "");
   end Input_Changed;

   procedure Prepare
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk_Widget_Record'Class)
   is
      Input : constant Gtk_Entry :=
        Gtk_Entry (Self.Get_Nth_Page (0).Get_Last_Child);
   begin
      if Gtk_Widget (Page) = Self.Get_Nth_Page (1) then
         Gtk_Label (Page).Set_Text
           ("Create a profile for " & Input.Get_Text & "?");
      end if;
   end Prepare;

   procedure Apply (Self : access Gtk_Assistant_Record'Class) is
      Input : constant Gtk_Entry :=
        Gtk_Entry (Self.Get_Nth_Page (0).Get_Last_Child);
   begin
      Gtk_Label (Self.Get_Nth_Page (2)).Set_Text
        ("Profile created for " & Input.Get_Text & ".");
      Self.Commit;
   end Apply;

   procedure Open_Assistant (Self : access Gtk_Button_Record'Class) is
      A : Gtk_Assistant;
      Box : Gtk_Box;
      Label : Gtk_Label;
      Input : Gtk_Entry;
      Page : Gtk_Assistant_Page;
      Position : Gint;
   begin
      Gtk_New (A);
      A.Set_Title ("Create a profile");
      A.Set_Default_Size (480, 240);
      A.Set_Transient_For
        (Gtk_Window (Self.Get_Ancestor (Gtk.Window.Get_Type)));
      A.Set_Modal (True);
      A.Set_Destroy_With_Parent (True);

      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Top (24);
      Box.Set_Margin_Bottom (24);
      Box.Set_Margin_Start (24);
      Box.Set_Margin_End (24);
      Gtk_New (Label, "Enter a name for the new profile:");
      Box.Append (Label);
      Gtk_New (Input);
      Box.Append (Input);
      Position := A.Append_Page (Box);
      Page := A.Get_Page (Box);
      Set_Property (Page, Gtk.Assistant_Page.Title_Property, "Profile name");
      Set_Property (Page, Page_Type_Property, Intro);
      Gtk.Editable.On_Changed
        (+Input, Input_Changed'Access, Slot => Page);

      Gtk_New (Label, "");
      Position := A.Append_Page (Label);
      A.Set_Page_Title (Label, "Confirm");
      A.Set_Page_Type (Label, Confirm);
      A.Set_Page_Complete (Label, True);

      Gtk_New (Label, "");
      Position := A.Append_Page (Label);
      A.Set_Page_Title (Label, "Finished");
      A.Set_Page_Type (Label, Summary);
      A.Set_Page_Complete (Label, True);

      A.On_Prepare (Prepare'Access);
      A.On_Apply (Apply'Access);
      A.On_Cancel (Finish'Access);
      A.On_Close (Finish'Access);
      A.Present;
   end Open_Assistant;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box : Gtk_Box;
      Button : Gtk_Button;
      Label : Gtk_Label;
   begin
      Frame.Set_Label ("Assistant");
      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Frame.Set_Child (Box);
      Gtk_New (Label, "A three-page assistant with input validation.");
      Box.Append (Label);
      Gtk_New (Button, "Create a profile…");
      Button.Set_Halign (Align_Start);
      Button.On_Clicked (Open_Assistant'Access);
      Box.Append (Button);
   end Run;

end Create_Assistant;
