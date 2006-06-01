-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  to display long texts (see the Gtk_Text widget instead).
--
--  Mnemonics
--  =========
--  Labels may contain mnemonics. Mnemonics are underlined characters in the
--  label, used for keyboard navigation. Mnemonics are created by providing
--  string with an underscore before the mnemonic character, such as "_File",
--  to the functions gtk_new_with_mnemonic or set_text_with_mnemonic().
--
--  Mnemonics automatically activate any activatable widget the label is
--  inside, such as a Gtk_Button; if the label is not inside the mnemonic's
--  target widget, you have to tell the label about the target using
--  set_mnemonic_widget(). For instance:
--      declare
--         Button : Gtk_Button;
--         Label  : Gtk_Label;
--      begin
--         Gtk_New (Button);
--         Gtk_New_With_Mnemonic (Label, "_File");
--         Add (Button, Label);
--      end;
--  However, there exists a convenience function in Gtk.Button to create such
--  a button already.
--
--  Markup
--  ======
--  To make it easy to format text in a label (changing colors, fonts, etc.),
--  label text can be provided in a simple markup format. Here's how to create
--  a label with a small font:
--       Gtk_New (Label, "<small>hello</small>");
--
--  The markup must be valid, and <>& characters must be escaped with
--  &lt; &gt; and &amp;
--
--  Markup strings are just a convenient way to set the Pango_Attr_List on
--  label; Set_Attributes() may be a simpler way to set attributes in some
--  cases. Be careful though; Pango_Attr_List tends to cause
--  internationalization problems, unless you're applying attributes to the
--  entire string (i.e. unless you set the range of each attribute to [0,
--  G_MAXINT)). The reason is that specifying the start_index and end_index for
--  a Pango_Attribute requires knowledge of the exact string being displayed,
--  so translations will cause problems.
--
--  Selectable labels
--  =================
--  Labels can be made selectable with Set_Selectable. Selectable
--  labels allow the user to copy the label contents to the clipboard. Only

--  should be made selectable.
--  </description>
--  <c_version>2.8.17</c_version>

with Gdk.Types;
with Glib.Properties;
with Gtk.Enums;
with Gtk.Misc;
with Gtk.Widget;
with Pango.Attributes;
with Pango.Layout;

package Gtk.Label is

   type Gtk_Label_Record is new Misc.Gtk_Misc_Record with private;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "");
   procedure Initialize
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String);
   --  Creates or initializes a new label.
   --  Str is the string to be displayed.

   procedure Gtk_New_With_Mnemonic (Label : out Gtk_Label; Str : UTF8_String);
   procedure Initialize_With_Mnemonic
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String);
   --  Creates or initializes a new label containing the text in Str.
   --  If characters in Str are preceded by an underscore, they are underlined
   --  indicating that they represent a keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically or explicitely using Set_Mnemonic_Widget.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Label.

   procedure Set_Justify
     (Label : access Gtk_Label_Record;
      Jtype : Enums.Gtk_Justification);
   function Get_Justify
     (Label : access Gtk_Label_Record) return Enums.Gtk_Justification;
   --  Set the justification for the label.
   --  The default value is Justify_Center, which means that the text will be
   --  centered in the label. Note that this setting has an impact only when
   --  the Gtk_Label is larger than the text (its default width is the same
   --  as the text) and contains multiple lines.
   --  To justify a single line label, you should instead call Set_Alignment
   --  and make sure that the label or any surrounding container fills its
   --  horizontal allocated space.

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean);
   function  Get_Line_Wrap (Label : access Gtk_Label_Record) return Boolean;
   --  Toggle line wrapping within Label.
   --  if Wrap is True, then Label will break lines if the text is larger
   --  then the widget's size. If Wrap is False, then the text is simply
   --  cut off.

   procedure Set_Selectable
     (Label : access Gtk_Label_Record; Selectable : Boolean);
   function  Get_Selectable (Label : access Gtk_Label_Record) return Boolean;
   --  Selectable labels allow the user to select text from the label,
   --  for copy-and-paste.

   procedure Set_Use_Markup
     (Label : access Gtk_Label_Record; Markup : Boolean);
   function Get_Use_Markup (Label : access Gtk_Label_Record) return Boolean;
   --  Sets whether the text of the label contains markup in Pango's
   --  text markup language.  If Markup is True, then Label will be
   --  parsed for markup.

   procedure Set_Use_Underline
     (Label : access Gtk_Label_Record; Underline : Boolean);
   function Get_Use_Underline (Label : access Gtk_Label_Record) return Boolean;
   --  Indicates wether an embedded underline in the label indicates the
   --  mnemonic accelerator key.

   procedure Set_Angle (Label : access Gtk_Label_Record;  Angle : Gdouble);
   function Get_Angle  (Label : access Gtk_Label_Record) return Gdouble;
   --  Sets the angle of rotation for the label. An angle of 90 reads from
   --  from bottom to top, an angle of 270, from top to bottom. The angle
   --  setting for the label is ignored if the label is selectable,
   --  wrapped, or ellipsized.

   procedure Set_Ellipsize
     (Label : access Gtk_Label_Record;
      Mode  : Pango.Layout.Pango_Ellipsize_Mode);
   function Get_Ellipsize
     (Label : access Gtk_Label_Record)
      return Pango.Layout.Pango_Ellipsize_Mode;
   --  Sets the mode used to ellipsize (add an ellipsis: "...") to the text if
   --  there is not enough space to render the entire string.

   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String);
   function  Get_Text (Label : access Gtk_Label_Record) return UTF8_String;
   --  Change the text of the label.
   --  The new text is visible on the screen at once. Note that the underline
   --  pattern is not modified.

   procedure Set_Label (Label : access Gtk_Label_Record; Str : String);
   function Get_Label  (Label : access Gtk_Label_Record) return String;
   --  Sets the text of the label. The label is interpreted as
   --  including embedded underlines and/or Pango markup depending
   --  on the values of label->use_underline and label->use_markup.

   function Get_Layout
     (Label : access Gtk_Label_Record) return Pango.Layout.Pango_Layout;
   --  Gets the layout used to display the label.
   --  The layout is useful to e.g. convert text positions to pixel positions,
   --  in combination with Get_Layout_Offsets(). The returned layout is owned
   --  by the label so need not be freed by the caller.

   procedure Get_Layout_Offsets
     (Label : access Gtk_Label_Record;
      X, Y  : out Gint);
   --  Obtains the coordinates where the label will draw the layout
   --  representing the text in the label; useful to convert mouse events into
   --  coordinates inside the layout, e.g. to take some action if some part of
   --  the label is clicked. Of course you will need to create a Gtk_Event_Box
   --  to receive the events, and pack the label inside it, since labels are a
   --  #GTK_NO_WINDOW widget. Remember when using the layout functions you need
   --  to convert to and from pixels using PANGO_PIXELS() or #PANGO_SCALE.

   procedure Set_Max_Width_Chars
     (Label : access Gtk_Label_Record; N_Chars : Gint);
   function Get_Max_Width_Chars (Label : access Gtk_Label_Record) return Gint;
   --  Sets the desired maximum width in characters of Label

   procedure Set_Width_Chars (Label : access Gtk_Label_Record; N_Chars : Gint);
   function Get_Width_Chars  (Label : access Gtk_Label_Record) return Gint;
   --  Sets the desired width in characters of Label.

   procedure Set_Single_Line_Mode
     (Label            : access Gtk_Label_Record;
      Single_Line_Mode : Boolean);
   function Get_Single_Line_Mode
     (Label : access Gtk_Label_Record) return Boolean;
   --  Sets whether the label is in single line mode.

   function Get_Mnemonic_Keyval
     (Label : access Gtk_Label_Record) return Gdk.Types.Gdk_Key_Type;
   --  Return the key value of the mnemonic accelerator key indicated by an
   --  embedded underline in the label. If there is no mnemonic set up it
   --  returns Gdk.Types.Keysyms.GDK_VoidSymbol.

   procedure Set_Attributes
     (Label : access Gtk_Label_Record;
      Attrs : Pango.Attributes.Pango_Attr_List);
   function Get_Attributes
     (Label : access Gtk_Label_Record) return Pango.Attributes.Pango_Attr_List;
   --  Sets a list of attributes to be applied to the label text. These
   --  attributes will be ignored if the use_underline or use_markup
   --  properties are set.
   --  Get_Attributes does not reflect attributes that come from the label's
   --  markup (see Set_Markup). If you want to get the effective attributes for
   --  the label, use Pango.Layout.Get_Attribute (Get_Layout (Label)).

   procedure Set_Text_With_Mnemonic
     (Label : access Gtk_Label_Record;
      Str   : UTF8_String);
   --  Change the text and mnemonic key of the label.
   --  The new text and mnemonic are visible on the screen at once.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically or explicitely using Set_Mnemonic_Widget.

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
   function Get_Mnemonic_Widget
     (Label : access Gtk_Label_Record) return Gtk.Widget.Gtk_Widget;
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

   procedure Select_Region
     (Label        : access Gtk_Label_Record;
      Start_Offset : Integer := -1;
      End_Offset   : Integer := -1);
   --  Selects a range of characters in the label, if the label is
   --  selectable.  If Start or End are -1, then the end of the label
   --  will be substituted.

   procedure Get_Selection_Bounds
     (Label         : access Gtk_Label_Record;
      First, Last   : out Gint;
      Has_Selection : out Boolean);
   --  Gets the selected range of characters in the label, returning True
   --  if there's a selection.

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

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   function Get
     (Label : access Gtk_Label_Record) return UTF8_String renames Get_Text;
   --  pragma Obsolescent;
   --  Same as Get_Text.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Label_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: The text of the label.
   --  See also: Set_Text and Get_Text
   --
   --  Name:  Attributes_Property
   --  Type:  Pango_Type_Attr_List
   --  Flags: read-write
   --  Descr: A list of style attributes to apply to the text of the label.
   --  See also: ??? Unsupported yet
   --
   --  Name:  Use_Markup_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: The text of the label includes XML markup.
   --         See pango_parse_markup().
   --  See also: <none>
   --
   --  Name:  Use_Underline_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: If set, an underline in the text indicates the next character
   --         should be used for the mnemonic accelerator key
   --  See also: <none>
   --
   --  Name:  Justify_Property
   --  Type:  Gtk_Justification
   --  Flags: read-write
   --  Descr: The alignment of the lines in the text of the label relative to
   --         each other. This does NOT affect the alignment of the label
   --         within its allocation.
   --  See also: Set_Justify
   --
   --  Name:  Pattern_Property
   --  Type:  String
   --  Flags: read-write
   --  Descr: A string with _ characters in positions correspond to
   --         characters in the text to underline.
   --  See also: Set_Pattern
   --
   --  Name:  Wrap_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: If set, wrap lines if the text becomes too wide.
   --  See also: Set_Line_Wrap
   --
   --  Name:  Selectable_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  Descr: Whether the label text can be selected with the mouse.
   --  See also: <none>
   --
   --  Name:  Mnemonic_Keyval_Property
   --  Type:  Guint
   --  Flags: readable
   --  Descr: The mnemonic accelerator key for this label.
   --  See also: Gtk_New_With_Mnemonic
   --
   --  Name:  Mnemonic_Widget_Property
   --  Type:  Gtk_Widget_Record'Class
   --  Flags: read-write
   --  Descr: The widget to be activated when the label's mnemonic key is
   --         pressed
   --
   --  Name:  Angle_Property
   --  Type:  Double
   --  Descr: Angle at which the label is rotated
   --
   --  Name:  Cursor_Position_Property
   --  Type:  Int
   --  Descr: The current position of the insertion cursor in chars
   --
   --  Name:  Ellipsize_Property
   --  Type:  Enum
   --  Descr: The preferred place to ellipsize the string, if the label does
   --         not have enough room to display the entire string, if at all
   --
   --  Name:  Max_Width_Chars_Property
   --  Type:  Int
   --  Descr: The desired maximum width of the label, in characters
   --
   --  Name:  Selection_Bound_Property
   --  Type:  Int
   --  Descr: The position of the opposite end of the selection from the cursor
   --         in chars
   --
   --  Name:  Single_Line_Mode_Property
   --  Type:  Boolean
   --  Descr: Whether the label is in single line mode
   --
   --  Name:  Width_Chars_Property
   --  Type:  Int
   --  Descr: The desired width of the label, in characters
   --
   --  </properties>

   Label_Property            : constant Glib.Properties.Property_String;
   --  Attributes_Property   : constant ???
   Use_Markup_Property       : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property    : constant Glib.Properties.Property_Boolean;
   Justify_Property          : constant Gtk.Enums.Property_Gtk_Justification;
   Pattern_Property          :  constant Glib.Properties.Property_String;
   Wrap_Property             : constant Glib.Properties.Property_Boolean;
   Selectable_Property       : constant Glib.Properties.Property_Boolean;
   Mnemonic_Keyval_Property  : constant Glib.Properties.Property_Uint_RO;
   Mnemonic_Widget_Property  : constant Glib.Properties.Property_Object;
   Angle_Property            : constant Glib.Properties.Property_Double;
   Cursor_Position_Property  : constant Glib.Properties.Property_Int;
   --  Ellipsize_Property        : constant Glib.Properties.Property_Enum;
   Max_Width_Chars_Property  : constant Glib.Properties.Property_Int;
   Selection_Bound_Property  : constant Glib.Properties.Property_Int;
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean;
   Width_Chars_Property      : constant Glib.Properties.Property_Int;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "copy_clipboard"
   --    procedure Handler (Label : access Gtk_Label_Record'Class);
   --    Request a copy the label's text into the clipboard. This should be
   --    bound to a key.
   --
   --  - "move_cursor"
   --    procedure Handler
   --       (Label  : access Gtk_Label_Record'Class;
   --        Step   : Gtk_Movement_Step;
   --        Amount : Gint;
   --        Extend_Selection : Boolean);
   --    You should emit this signal to request that the cursor be moved inside
   --    the label. This is mostly useful from a keybinding. The cursor is
   --    also used as the insertion point when modifying the label
   --
   --  - "populate_popup"
   --    procedure Handler
   --       (Label : access Gtk_Label_Record'Class;
   --        Menu  : access Gtk_Menu_Record'Class);
   --    ???
   --
   --  </signals>

   Signal_Copy_Clipboard : constant String := "copy_clipboard";
   Signal_Move_Cursor    : constant String := "move_cursor";
   Signal_Populate_Popup : constant String := "populate_popup";

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
   Angle_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("angle");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   --     Ellipsize_Property : constant Glib.Properties.Property_Enum :=
   --       Glib.Properties.Build ("ellipsize");
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width-chars");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-line-mode");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");

   pragma Import (C, Get_Type, "gtk_label_get_type");
end Gtk.Label;

--  These subprograms never had a binding, but are now obsolescent:
--  No binding: gtk_label_get
--  No binding: gtk_label_parse_uline
