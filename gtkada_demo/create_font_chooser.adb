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

with Gtk.Box;                use Gtk.Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Font_Dialog;        use Gtk.Font_Dialog;
with Gtk.Font_Dialog_Button; use Gtk.Font_Dialog_Button;
with Gtk.Label;              use Gtk.Label;
with Pango.Font;             use Pango.Font;

package body Create_Font_Chooser is
   function Help return String is
   begin
      return "Gtk_Font_Dialog_Button opens Gtk_Font_Dialog and keeps the"
        & " selected font description. This button displays its name using"
        & " the chosen font and size. Applications can read Get_Font_Desc"
        & " and listen to notify::font-desc to update their text.";
   end Help;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Dialog : Gtk_Font_Dialog;
      Button : Gtk_Font_Dialog_Button;
      Font   : Pango_Font_Description := From_String ("Sans 14");
   begin
      Frame.Set_Label ("Font Chooser");
      Gtk_New (Box, Orientation_Vertical, 12);
      Box.Set_Margin_Start (12);
      Box.Set_Margin_End (12);
      Box.Set_Margin_Top (12);
      Box.Set_Margin_Bottom (12);
      Frame.Set_Child (Box);
      Box.Append (Gtk_Label_New ("Choose a font to change the button's text:"));
      Gtk_New (Dialog);
      Dialog.Set_Title ("Choose a font");
      --  Gtk_Font_Dialog_Button_New takes ownership of Dialog.
      Gtk_New (Button, Dialog);
      Button.Set_Font_Desc (Font);
      Free (Font);
      Button.Set_Use_Font (True);
      Button.Set_Use_Size (True);
      Box.Append (Button);
   end Run;
end Create_Font_Chooser;
