-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
--  <c_version>1.3.11</c_version>

with Gdk.Types;
with Glib.Properties;
with Gtk.Enums;
with Gtk.Misc;
with Gtk.Widget;

package Gtk.Label is

   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with private;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "");
   --  Create a new label.
   --  Str is the string to be displayed.

   procedure Gtk_New_With_Mnemonic (Label : out Gtk_Label; Str : UTF8_String);
   --  Create a new label containing the text in Str.
   --  If characters in Str are preceded by an underscore, they are underlined
   --  indicating that they represent a keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically or explicitely using Set_Mnemonic_Widget.

   procedure Initialize
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_With_Mnemonic
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String);
   --  Internal initialization function.

   function Get_Justify
     (Label : access Gtk_Label_Record) return Enums.Gtk_Justification;
   --  Returns the justification of the label. See Set_Justify.

   function Get_Line_Wrap (Label : access Gtk_Label_Record) return Boolean;
   --  Returns True if the lines of the label are automatically wrapped.

   function Get_Selectable (Label : access Gtk_Label_Record) return Boolean;
   --  Returns True if the user can copy text from the label.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Label.

   function Get_Use_Markup (Label : access Gtk_Label_Record) return Boolean;
   --  Return True if the label's text will be parsed for markup.

   function Get_Use_Underline (Label : access Gtk_Label_Record) return Boolean;
   --  Return True whether an embedded underline in the label indicates
   --  the mnemonic accelerator keys.

   function Get_Mnemonic_Keyval
     (Label : access Gtk_Label_Record) return Gdk.Types.Gdk_Key_Type;
   --  Return the key value of the mnemonic accelerator key indicated by an
   --  embedded underline in the label. If there is no mnemonic set up it
   --  returns Gdk.Types.Keysyms.GDK_VoidSymbol.

   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String);
   --  Change the text of the label.
   --  The new text is visible on the screen at once. Note that the underline
   --  pattern is not modified.

   procedure Set_Text_With_Mnemonic
     (Label : access Gtk_Label_Record;
      Str   : UTF8_String);
   --  Change the text and mnemonic key of the label.
   --  The new text and mnemonic are visible on the screen at once.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically or explicitely using Set_Mnemonic_Widget.

   function Get_Text (Label : access Gtk_Label_Record) return UTF8_String;
   --  Get the current value of the text displayed in the label.

   procedure Set_Markup (Label : access Gtk_Label_Record; Str : UTF8_String);
   --  Parses Str which is marked up with the Pango text markup language,
   --  setting the label's text and attribute list based on the parse results.

   procedure Set_Markup_With_Mnemonic
     (Label : access Gtk_Label_Record;
      Str   : UTF8_String);
   --  Parse Str which is marked up with the Pango text markup language,
   --  setting the label's text and attribute list based on the parse results.
   --  If characters in Str are preceded by an underscore, they are underlined
   --  indicating that they represent a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically or explicitely using Set_Mnemonic_Widget.

   procedure Set_Mnemonic_Widget
     (Label  : access Gtk_Label_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  If the label has been set so that it has an mnemonic key, the label can
   --  be associated with a widget that is the target of the mnemonic.
   --  When the label is inside a widget (like a Gtk_Button or a Gtk_Notebook
   --  tab), it is automatically associated with the correct widget, but
   --  sometimes (i.e. when the target is a Gtk_Entry next to the label),
   --  you need to set it explicitly using this procedure.
   --  The target widget will be accelerated by emitting "mnemonic_activate"
   --  on it. The default handler for this signal will activate the widget if
   --  there are no mnemonic collisions and toggle focus between the colliding
   --  widgets otherwise.

   --  <doc_ignore>
   function Get
     (Label : access Gtk_Label_Record) return UTF8_String renames Get_Text;
   --  Same as Get_Text.
   --  pragma Deprecated (Get);
   --  </doc_ignore>

   procedure Select_Region
     (Label        : access Gtk_Label_Record;
      Start_Offset : Integer := -1;
      End_Offset   : Integer := -1);
   --  Selects a range of characters in the label, if the label is
   --  selectable.  If Start or End are -1, then the end of the label
   --  will be substituted.

   procedure Set_Justify
     (Label : access Gtk_Label_Record;
      Jtype : Enums.Gtk_Justification);
   --  Set the justification for the label.
   --  The default value is Justify_Center, which means that the text will be
   --  centered in the label. Note that this setting has an impact only when
   --  the Gtk_Label is larger than the text (its default width is the same
   --  as the text) and contains multiple lines.
   --  To justify a single line label, you should instead call Set_Alignment
   --  and make sure that the label or any surrounding container fills its
   --  horizontal allocated space.

   procedure Set_Pattern
     (Label   : access Gtk_Label_Record;
      Pattern : String);
   --  Change the underlines pattern.
   --  Pattern is a simple string made of underscore and space characters,
   --  matching the ones in the string. GtkAda will underline every letter
   --  that matches an underscore.
   --  An empty string disables the underlines.
   --  example: If the text is FooBarBaz and the Pattern is "___   ___"
   --  then both "Foo" and "Baz" will be underlined, but not "Bar".

   procedure Set_Selectable
     (Label : access Gtk_Label_Record; Selectable : Boolean);
   --  Selectable labels allow the user to select text from the label,
   --  for copy-and-paste.

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean);
   --  Toggle line wrapping within Label.
   --  if Wrap is True, then Label will break lines if the text is larger
   --  then the widget's size. If Wrap is False, then the text is simply
   --  cut off.

   procedure Set_Use_Markup
     (Label  : access Gtk_Label_Record;
      Markup : Boolean);
   --  Sets whether the text of the label contains markup in Pango's
   --  text markup language.  If Markup is True, then Label will be
   --  parsed for markup.

   procedure Set_Use_Underline
     (Label     : access Gtk_Label_Record;
      Underline : Boolean);
   --  Sets whether the text of the label should be underlined.
   --  If Underline is True, then Label will be underlined.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Label_Property
   --    Type:  UTF8_String
   --    Flags: read-write
   --    Descr: The text of the label.
   --    See also: Set_Text and Get_Text
   --
   --  - Name:  Attributes_Property
   --    Type:  Pango_Type_Attr_List
   --    Flags: read-write
   --    Descr: A list of style attributes to apply to the text of the label.
   --    See also: ??? Unsupported yet
   --
   --  - Name:  Use_Markup_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: The text of the label includes XML markup.
   --           See pango_parse_markup().
   --    See also: <none>
   --
   --  - Name:  Use_Underline_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If set, an underline in the text indicates the next character
   --           should be used for the mnemonic accelerator key
   --    See also: <none>
   --
   --  - Name:  Justify_Property
   --    Type:  Gtk_Justification
   --    Flags: read-write
   --    Descr: The alignment of the lines in the text of the label relative to
   --           each other. This does NOT affect the alignment of the label
   --           within its allocation.
   --    See also: Set_Justify
   --
   --  - Name:  Pattern_Property
   --    Type:  String
   --    Flags: read-write
   --    Descr: A string with _ characters in positions correspond to
   --           characters in the text to underline.
   --    See also: Set_Pattern
   --
   --  - Name:  Wrap_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If set, wrap lines if the text becomes too wide.
   --    See also: Set_Line_Wrap
   --
   --  - Name:  Selectable_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: Whether the label text can be selected with the mouse.
   --    See also: <none>
   --
   --  - Name:  Mnemonic_Keyval_Property
   --    Type:  Guint
   --    Flags: readable
   --    Descr: The mnemonic accelerator key for this label.
   --    See also: Gtk_New_With_Mnemonic
   --
   --  - Name:  Mnemonic_Widget_Property
   --    Type:  Gtk_Widget_Record'Class
   --    Flags: read-write
   --    Descr: The widget to be activated when the label's mnemonic key is
   --           pressed
   --    See also:
   --
   --  </properties>

   Label_Property           : constant Glib.Properties.Property_String;
   --  Attributes_Property : constant ???
   Use_Markup_Property      : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property   : constant Glib.Properties.Property_Boolean;
   Justify_Property         : constant Gtk.Enums.Property_Gtk_Justification;
   Pattern_Property         : constant Glib.Properties.Property_String;
   Wrap_Property            : constant Glib.Properties.Property_Boolean;
   Selectable_Property      : constant Glib.Properties.Property_Boolean;
   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint_RO;
   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with null record;

   Label_Property           : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Use_Markup_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use_markup");
   Use_Underline_Property   : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use_underline");
   Justify_Property         : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justify");
   Pattern_Property         : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("pattern");
   Wrap_Property            : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap");
   Selectable_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint_RO :=
     Glib.Properties.Build ("mnemonic_keyval");
   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("mnemonic_widget");

   pragma Import (C, Get_Type, "gtk_label_get_type");
end Gtk.Label;

--  missing:
--  Set_Attributes
--  Get_Attributes
--  Get_Mnemonic_Widget
--  Get_Selection_Bounds
--  Get_Layout
--  Get_Layout_Offsets
