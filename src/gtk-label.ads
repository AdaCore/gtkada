-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_Label is a light widget associated with some text you want
--  to display on the screen. You can change the text dynamically if
--  needed.
--
--  The text can be on multiple lines if you separate each line with
--  the ASCII.LF character. However, this is not the recommended way
--  to display long texts (see the Gtk_Text widget instead)
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Object; use Gtk.Object;
with Gtk.Enums;
with Gtk.Misc;

package Gtk.Label is

   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with private;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String);
   --  Creates a new label.
   --  STR is the string to be displayed.

   procedure Initialize (Label : access Gtk_Label_Record'Class;
                         Str   : in     String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Text (Label : access Gtk_Label_Record;
                       Str   : in String);
   --  Changes the text of the label
   --  The new text is visible on the screen at once. Note that the underline
   --  pattern is not modified.

   procedure Set_Justify (Label : access Gtk_Label_Record;
                          Jtype : in Enums.Gtk_Justification);
   --  Sets the justification for the label.
   --  The default value is Justify_Center, which means that the text will be
   --  centered in the label. Note that this setting has an impact only when
   --  the Gtk_Label is larger than the text (its default width is the same
   --  as the text).

   procedure Set_Pattern (Label   : access Gtk_Label_Record;
                          Pattern : in String);
   --  Changes the underlines pattern.
   --  PATTERN is a simple string made of underscore and space characters,
   --  matching the ones in the string. GtkAda will underline every letter
   --  that matches an underscore.
   --  An empty string disables the underlines.
   --  example: If the text is FooBarBaz and the PATTERN is "___   ___"
   --  then both "Foo" and "Baz" will be underlines, but not "Bar".

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record;
                            Wrap  : in Boolean);
   --  Toggles line wrapping within LABEL.
   --  if WRAP is True, then Label will break lines if the text is larger
   --  then the widget's size. If WRAP is False, then the text is simply
   --  cut off.

   function Get (Label : access Gtk_Label_Record) return String;
   --  Gets the current value of the text displayed in the label.

   procedure Parse_Uline (Label : access Gtk_Label_Record;
                          Text  : in     String);
   --  Creates both the text and the underscore pattern from a single string.
   --  TEXT is parsed for underscores. The next character is converted to
   --  an underlined character.

   --  Note: as opposed to the C version, this subprogram does not return the
   --  accelerator keyval associated with the last character underlined. This
   --  feature is only used internally by gtk+ to create menus, and is not
   --  useful for end-users.

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Label : in out Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private
   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with null record;

end Gtk.Label;
