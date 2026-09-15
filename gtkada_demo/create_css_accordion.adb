------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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

with Ada.Directories;
with Ada.Text_IO;

with Glib.Object;        use Glib.Object;
with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Css_Provider;   use Gtk.Css_Provider;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Style_Context;  use Gtk.Style_Context;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Widget;         use Gtk.Widget;

package body Create_Css_Accordion is

   Css_Filename : constant String := "css_accordion.css";

   Css_Installation_Attempted : Boolean := False;
   --  The provider is display-wide, so install it only once. Its selectors
   --  are scoped to the accordion's CSS class and cannot affect other demos.

   procedure Install_Css (Widget : not null access Gtk_Widget_Record'Class);
   --  Load and install the accordion stylesheet. A missing stylesheet is
   --  non-fatal, matching the behaviour of the main demo stylesheet.

   -----------------
   -- Install_Css --
   -----------------

   procedure Install_Css (Widget : not null access Gtk_Widget_Record'Class) is
      Provider : Gtk_Css_Provider;
   begin
      if Css_Installation_Attempted then
         return;
      end if;

      Css_Installation_Attempted := True;

      if not Ada.Directories.Exists (Css_Filename) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: cannot find "
            & Css_Filename
            & "; continuing without CSS Accordion styling");
         return;
      end if;

      Gtk_New (Provider);
      Provider.Load_From_Path (Css_Filename);
      Add_Provider_For_Display
        (Get_Style_Context (Widget).Get_Display,
         +Provider,
         Priority_Application);
      Unref (Provider);
   end Install_Css;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "This demo showcases CSS transitions and multiple backgrounds in"
        & " Gtk4. Hover over the buttons to expand the accordion.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box          : Gtk_Box;
      Styled_Frame : Gtk_Frame;
   begin
      Gtk_New (Styled_Frame);
      Styled_Frame.Add_Css_Class ("accordion");
      Frame.Set_Child (Styled_Frame);

      Gtk_New (Box, Orientation_Horizontal, Spacing => 0);
      Box.Set_Halign (Align_Center);
      Box.Set_Valign (Align_Center);
      Styled_Frame.Set_Child (Box);

      Box.Append (Gtk_Button_New_With_Label ("This"));
      Box.Append (Gtk_Button_New_With_Label ("Is"));
      Box.Append (Gtk_Button_New_With_Label ("A"));
      Box.Append (Gtk_Button_New_With_Label ("CSS"));
      Box.Append (Gtk_Button_New_With_Label ("Accordion"));
      Box.Append (Gtk_Button_New_With_Label (":-)"));

      Install_Css (Styled_Frame);
   end Run;

end Create_Css_Accordion;
