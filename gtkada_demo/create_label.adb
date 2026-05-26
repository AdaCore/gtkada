------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 2000-2026, AdaCore                      --
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

with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Label;    use Gtk.Label;
with Glib.Convert; use Glib.Convert;
with Pango.Enums;  use Pango.Enums;

package body Create_Label is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Label@B is used to display any kind of text on the"
        & " screen, even on multiple line."
        & ASCII.LF
        & "This is a passive widget, in that it does not react to any event.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox, Hbox : Gtk_Box;
      Label      : Gtk_Label;
      Frame2     : Gtk_Frame;
   begin
      Set_Label (Frame, "Label");
      Gtk_New (Hbox, Orientation_Horizontal, 5);
      Hbox.Set_Homogeneous (False);
      Frame.Set_Child (Hbox);

      Gtk_New (Vbox, Orientation_Vertical, 5);
      Vbox.Set_Homogeneous (False);
      Hbox.Append (Vbox);

      Gtk_New (Frame2, "Normal Label");
      Gtk_New (Label, "This is a Normal Label");
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Multi-line Label");
      Gtk_New (Label, "This is a Multi-line label."
               & ASCII.LF
               & "Second Line"
               & ASCII.LF
               & "Third line");
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Left Justified Label");
      Gtk_New (Label, "This is a Left-Justified"
               & ASCII.LF & "Multi-line label." & ASCII.LF
               & "Third      line");
      Label.Set_Justify (Justify_Left);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Right Justified Label");
      Gtk_New (Label, "This is a Right-Justified"
               & ASCII.LF & "Multi-line label." & ASCII.LF
               & "Fourth      line");
      Label.Set_Justify (Justify_Right);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Selectable Label");
      Gtk_New (Label, "This is a selectable label "
               & ASCII.LF
               & "you can select the text with the "
               & ASCII.LF
               & "mouse and paste it.");
      Label.Set_Selectable (True);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Vbox, Orientation_Vertical, 5);
      Vbox.Set_Homogeneous (False);
      Hbox.Append (Vbox);

      Gtk_New (Frame2, "Line wrapped Label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  It should not "
               & "be taking up the entire             "
               & "width allocated to it, but automatically wraps the words "
               & "to fit.  The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Label.Set_Wrap (True);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Character mode wrapped label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  Like the above "
               & "example, it will not be taking up the entire width "
               & "allocated to it, but instead of wrapping words to fit, it "
               & "wraps characters to fit.  "
               & "The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Label.Set_Wrap (True);
      Label.Set_Wrap_Mode (Pango_Wrap_Char);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Filled, wrapped Label");
      Gtk_New (Label,
               "This is an example of a line-wrapped label.  It should "
               & "be taking up the entire             "
               & "width allocated to it, but automatically wraps the words "
               & "to fit.  The time has come, for all good men, to come to "
               & "the aid of their party.  The sixth sheik's six sheep's sick."
               & ASCII.LF
               & "     It supports multiple paragraphs correctly, and  "
               & "correctly   adds many          extra  spaces. ");
      Label.Set_Wrap (True);
      Label.Set_Justify (Justify_Fill);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      --  Gtk4 has dropped Gtk.Label.Set_Pattern; emulate the original
      --  "Underlined Label" sub-frame with Pango markup instead, which is the
      --  idiomatic Gtk4 way of underlining ranges of characters.
      Gtk_New (Frame2, "Underlined Label");
      Gtk_New (Label);
      Label.Set_Markup
        (Locale_To_UTF8
           ("<u>This label is underlined!</u>"
            & ASCII.LF
            & "<u>This one</u> is <u>underlined</u> <u>in</u>"
            & " ���ܸ������<u>quite</u> <u>a</u> <u>funky</u>"
            & " <u>fashion</u>"));
      Label.Set_Justify (Justify_Left);
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);

      Gtk_New (Frame2, "Markup Label");
      Gtk_New (Label);

      Set_Markup (Label,
         ("<span size=""x-large"" weight=""ultrabold"""
          & "background=""darkblue"" color=""white"">"
          & "This label has <i>markup</i>!</span>"
          & ASCII.LF
          & "You can make text <b>bold</b> <i>italic</i> "
          & "<u>underline</u> <s>striken</s> <big>big</big>. "
          & ASCII.LF
          & "<tt>You can also use mono spaced font</tt>"
          & " and write H<sub><small>2</small></sub>O "
          & "and "
          & ASCII.LF
          & "<span weight=""light"" color=""red"" size=""xx-large"">"
          & "y=Σx<sub><small>i</small></sub>"
          & "<sup><small>2</small></sup> "
          & "+ 3</span>"
          & " besides other things..."));
      Frame2.Set_Child (Label);
      Vbox.Append (Frame2);
   end Run;

end Create_Label;
