------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  The Gtk.Label.Gtk_Label widget displays a small amount of text. As the
--  name implies, most labels are used to label another widget such as a
--  Gtk.Button.Gtk_Button, a Gtk.Menu_Item.Gtk_Menu_Item, or a
--  Gtk.Combo_Box.Gtk_Combo_Box.
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> label ├── [selection] ├── [link] ┊ ╰── [link]
--  ]|
--
--  GtkLabel has a single CSS node with the name label. A wide variety of
--  style classes may be applied to labels, such as .title, .subtitle,
--  .dim-label, etc. In the Gtk.Shortcuts_Window.Gtk_Shortcuts_Window, labels
--  are used wth the .keycap style class.
--
--  If the label has a selection, it gets a subnode with name selection.
--
--  If the label has links, there is one subnode per link. These subnodes
--  carry the link or visited state depending on whether they have been
--  visited.
--
--  # GtkLabel as GtkBuildable
--
--  The GtkLabel implementation of the GtkBuildable interface supports a
--  custom <attributes> element, which supports any number of <attribute>
--  elements. The <attribute> element has attributes named "name", "value",
--  "start" and "end" and allows you to specify
--  Pango.Attributes.Pango_Attribute values for this label.
--
--  An example of a UI definition fragment specifying Pango attributes: |[
--  <object class="GtkLabel"> <attributes> <attribute name="weight"
--  value="PANGO_WEIGHT_BOLD"/> <attribute name="background" value="red"
--  start="5" end="10"/> </attributes> </object> ]|
--
--  The start and end attributes specify the range of characters to which the
--  Pango attribute applies. If start and end are not specified, the attribute
--  is applied to the whole text. Note that specifying ranges does not make
--  much sense with translatable attributes. Use markup embedded in the
--  translatable content instead.
--
--  # Mnemonics
--
--  Labels may contain "mnemonics". Mnemonics are underlined characters in the
--  label, used for keyboard navigation. Mnemonics are created by providing a
--  string with an underscore before the mnemonic character, such as `"_File"`,
--  to the functions Gtk.Label.Gtk_New_With_Mnemonic or
--  Gtk.Label.Set_Text_With_Mnemonic.
--
--  Mnemonics automatically activate any activatable widget the label is
--  inside, such as a Gtk.Button.Gtk_Button; if the label is not inside the
--  mnemonic's target widget, you have to tell the label about the target using
--  Gtk.Label.Set_Mnemonic_Widget. Here's a simple example where the label is
--  inside a button:
--
--  |[<!-- language="C" --> // Pressing Alt+H will activate this button
--  GtkWidget *button = gtk_button_new (); GtkWidget *label =
--  gtk_label_new_with_mnemonic ("_Hello"); gtk_container_add (GTK_CONTAINER
--  (button), label); ]|
--
--  There's a convenience function to create buttons with a mnemonic label
--  already inside:
--
--  |[<!-- language="C" --> // Pressing Alt+H will activate this button
--  GtkWidget *button = gtk_button_new_with_mnemonic ("_Hello"); ]|
--
--  To create a mnemonic for a widget alongside the label, such as a
--  Gtk.GEntry.Gtk_Entry, you have to point the label at the entry with
--  Gtk.Label.Set_Mnemonic_Widget:
--
--  |[<!-- language="C" --> // Pressing Alt+H will focus the entry GtkWidget
--  *entry = gtk_entry_new (); GtkWidget *label = gtk_label_new_with_mnemonic
--  ("_Hello"); gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry); ]|
--
--  # Markup (styled text)
--
--  To make it easy to format text in a label (changing colors, fonts, etc.),
--  label text can be provided in a simple [markup format][PangoMarkupFormat].
--
--  Here's how to create a label with a small font: |[<!-- language="C" -->
--  GtkWidget *label = gtk_label_new (NULL); gtk_label_set_markup (GTK_LABEL
--  (label), "<small>Small text</small>"); ]|
--
--  (See [complete documentation][PangoMarkupFormat] of available tags in the
--  Pango manual.)
--
--  The markup passed to Gtk.Label.Set_Markup must be valid; for example,
--  literal <, > and & characters must be escaped as <, >, and &amp;. If you
--  pass text obtained from the user, file, or a network to
--  Gtk.Label.Set_Markup, you'll want to escape it with g_markup_escape_text or
--  g_markup_printf_escaped.
--
--  Markup strings are just a convenient way to set the
--  Pango.Attributes.Pango_Attr_List on a label; Gtk.Label.Set_Attributes may
--  be a simpler way to set attributes in some cases. Be careful though;
--  Pango.Attributes.Pango_Attr_List tends to cause internationalization
--  problems, unless you're applying attributes to the entire string (i.e.
--  unless you set the range of each attribute to [0, G_MAXINT)). The reason is
--  that specifying the start_index and end_index for a
--  Pango.Attributes.Pango_Attribute requires knowledge of the exact string
--  being displayed, so translations will cause problems.
--
--  # Selectable labels
--
--  Labels can be made selectable with Gtk.Label.Set_Selectable. Selectable
--  labels allow the user to copy the label contents to the clipboard. Only
--  labels that contain useful-to-copy information — such as error messages —
--  should be made selectable.
--
--  # Text layout # {label-text-layout}
--
--  A label can contain any number of paragraphs, but will have performance
--  problems if it contains more than a small number. Paragraphs are separated
--  by newlines or other paragraph separators understood by Pango.
--
--  Labels can automatically wrap text if you call Gtk.Label.Set_Line_Wrap.
--
--  Gtk.Label.Set_Justify sets how the lines in a label align with one
--  another. If you want to set how the label as a whole aligns in its
--  available space, see the Gtk.Widget.Gtk_Widget:halign and
--  Gtk.Widget.Gtk_Widget:valign properties.
--
--  The Gtk.Label.Gtk_Label:width-chars and
--  Gtk.Label.Gtk_Label:max-width-chars properties can be used to control the
--  size allocation of ellipsized or wrapped labels. For ellipsizing labels, if
--  either is specified (and less than the actual text size), it is used as the
--  minimum width, and the actual text size is used as the natural width of the
--  label. For wrapping labels, width-chars is used as the minimum width, if
--  specified, and max-width-chars is used as the natural width. Even if
--  max-width-chars specified, wrapping labels will be rewrapped to use all of
--  the available width.
--
--  Note that the interpretation of Gtk.Label.Gtk_Label:width-chars and
--  Gtk.Label.Gtk_Label:max-width-chars has changed a bit with the introduction
--  of [width-for-height geometry management.][geometry-management]
--
--  # Links
--
--  Since 2.18, GTK+ supports markup for clickable hyperlinks in addition to
--  regular Pango markup. The markup for links is borrowed from HTML, using the
--  `<a>` with "href" and "title" attributes. GTK+ renders links similar to the
--  way they appear in web browsers, with colored, underlined text. The "title"
--  attribute is displayed as a tooltip on the link.
--
--  An example looks like this:
--
--  |[<!-- language="C" --> const gchar *text = "Go to the" "<a
--  href=\"http://www.gtk.org title=\"<i>Our</i> website\">" "GTK+ website</a>
--  for more..."; GtkWidget *label = gtk_label_new (NULL); gtk_label_set_markup
--  (GTK_LABEL (label), text); ]|
--
--  It is possible to implement custom handling for links and their tooltips
--  with the Gtk.Label.Gtk_Label::activate-link signal and the
--  Gtk.Label.Get_Current_Uri function.
--
--  </description>
--  <screenshot>gtk-label</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_label.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Misc;         use Gtk.Misc;
with Gtk.Widget;       use Gtk.Widget;
with Pango.Attributes; use Pango.Attributes;
with Pango.Enums;      use Pango.Enums;
with Pango.Layout;     use Pango.Layout;

package Gtk.Label is

   type Gtk_Label_Record is new Gtk_Misc_Record with null record;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "");
   procedure Initialize
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "");
   --  Creates a new label with the given text inside it. You can pass null to
   --  get an empty label widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "str": The text of the label

   function Gtk_Label_New (Str : UTF8_String := "") return Gtk_Label;
   --  Creates a new label with the given text inside it. You can pass null to
   --  get an empty label widget.
   --  "str": The text of the label

   procedure Gtk_New_With_Mnemonic
      (Label : out Gtk_Label;
       Str   : UTF8_String := "");
   procedure Initialize_With_Mnemonic
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "");
   --  Creates a new Gtk.Label.Gtk_Label, containing the text in Str.
   --  If characters in Str are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  '__' (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. The mnemonic key can be used to
   --  activate another widget, chosen automatically, or explicitly using
   --  Gtk.Label.Set_Mnemonic_Widget.
   --  If Gtk.Label.Set_Mnemonic_Widget is not called, then the first
   --  activatable ancestor of the Gtk.Label.Gtk_Label will be chosen as the
   --  mnemonic widget. For instance, if the label is inside a button or menu
   --  item, the button or menu item will automatically become the mnemonic
   --  widget and be activated by the mnemonic.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "str": The text of the label, with an underscore in front of the
   --  mnemonic character

   function Gtk_Label_New_With_Mnemonic
      (Str : UTF8_String := "") return Gtk_Label;
   --  Creates a new Gtk.Label.Gtk_Label, containing the text in Str.
   --  If characters in Str are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  '__' (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. The mnemonic key can be used to
   --  activate another widget, chosen automatically, or explicitly using
   --  Gtk.Label.Set_Mnemonic_Widget.
   --  If Gtk.Label.Set_Mnemonic_Widget is not called, then the first
   --  activatable ancestor of the Gtk.Label.Gtk_Label will be chosen as the
   --  mnemonic widget. For instance, if the label is inside a button or menu
   --  item, the button or menu item will automatically become the mnemonic
   --  widget and be activated by the mnemonic.
   --  "str": The text of the label, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_label_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Angle
      (Label : not null access Gtk_Label_Record) return Gdouble;
   --  Gets the angle of rotation for the label. See Gtk.Label.Set_Angle.
   --  Since: gtk+ 2.6

   procedure Set_Angle
      (Label : not null access Gtk_Label_Record;
       Angle : Gdouble);
   --  Sets the angle of rotation for the label. An angle of 90 reads from
   --  from bottom to top, an angle of 270, from top to bottom. The angle
   --  setting for the label is ignored if the label is selectable, wrapped, or
   --  ellipsized.
   --  Since: gtk+ 2.6
   --  "angle": the angle that the baseline of the label makes with the
   --  horizontal, in degrees, measured counterclockwise

   function Get_Attributes
      (Label : not null access Gtk_Label_Record)
       return Pango.Attributes.Pango_Attr_List;
   --  Gets the attribute list that was set on the label using
   --  Gtk.Label.Set_Attributes, if any. This function does not reflect
   --  attributes that come from the labels markup (see Gtk.Label.Set_Markup).
   --  If you want to get the effective attributes for the label, use
   --  pango_layout_get_attribute (gtk_label_get_layout (label)).

   procedure Set_Attributes
      (Label : not null access Gtk_Label_Record;
       Attrs : Pango.Attributes.Pango_Attr_List);
   --  Sets a Pango.Attributes.Pango_Attr_List; the attributes in the list are
   --  applied to the label text.
   --  The attributes set with this function will be applied and merged with
   --  any other attributes previously effected by way of the
   --  Gtk.Label.Gtk_Label:use-underline or Gtk.Label.Gtk_Label:use-markup
   --  properties. While it is not recommended to mix markup strings with
   --  manually set attributes, if you must; know that the attributes will be
   --  applied to the label after the markup string is parsed.
   --  "attrs": a Pango.Attributes.Pango_Attr_List, or null

   function Get_Current_Uri
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Returns the URI for the currently active link in the label. The active
   --  link is the one under the mouse pointer or, in a selectable label, the
   --  link in which the text cursor is currently positioned.
   --  This function is intended for use in a
   --  Gtk.Label.Gtk_Label::activate-link handler or for use in a
   --  Gtk.Widget.Gtk_Widget::query-tooltip handler.
   --  Since: gtk+ 2.18

   function Get_Ellipsize
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   --  Returns the ellipsizing position of the label. See
   --  Gtk.Label.Set_Ellipsize.
   --  Since: gtk+ 2.6

   procedure Set_Ellipsize
      (Label : not null access Gtk_Label_Record;
       Mode  : Pango.Layout.Pango_Ellipsize_Mode);
   --  Sets the mode used to ellipsize (add an ellipsis: "...") to the text if
   --  there is not enough space to render the entire string.
   --  Since: gtk+ 2.6
   --  "mode": a Pango.Layout.Pango_Ellipsize_Mode

   function Get_Justify
      (Label : not null access Gtk_Label_Record)
       return Gtk.Enums.Gtk_Justification;
   --  Returns the justification of the label. See Gtk.Label.Set_Justify.

   procedure Set_Justify
      (Label : not null access Gtk_Label_Record;
       Jtype : Gtk.Enums.Gtk_Justification);
   --  Sets the alignment of the lines in the text of the label relative to
   --  each other. Gtk.Enums.Justify_Left is the default value when the widget
   --  is first created with Gtk.Label.Gtk_New. If you instead want to set the
   --  alignment of the label as a whole, use Gtk.Widget.Set_Halign instead.
   --  Gtk.Label.Set_Justify has no effect on labels containing only a single
   --  line.
   --  "jtype": a Gtk.Enums.Gtk_Justification

   function Get_Label
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Fetches the text from a label widget including any embedded underlines
   --  indicating mnemonics and Pango markup. (See Gtk.Label.Get_Text).

   procedure Set_Label
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the text of the label. The label is interpreted as including
   --  embedded underlines and/or Pango markup depending on the values of the
   --  Gtk.Label.Gtk_Label:use-underline and Gtk.Label.Gtk_Label:use-markup
   --  properties.
   --  "str": the new text to set for the label

   function Get_Layout
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Layout;
   --  Gets the Pango.Layout.Pango_Layout used to display the label. The
   --  layout is useful to e.g. convert text positions to pixel positions, in
   --  combination with Gtk.Label.Get_Layout_Offsets. The returned layout is
   --  owned by the Label so need not be freed by the caller. The Label is free
   --  to recreate its layout at any time, so it should be considered
   --  read-only.

   procedure Get_Layout_Offsets
      (Label : not null access Gtk_Label_Record;
       X     : out Glib.Gint;
       Y     : out Glib.Gint);
   --  Obtains the coordinates where the label will draw the
   --  Pango.Layout.Pango_Layout representing the text in the label; useful to
   --  convert mouse events into coordinates inside the
   --  Pango.Layout.Pango_Layout, e.g. to take some action if some part of the
   --  label is clicked. Of course you will need to create a
   --  Gtk.Event_Box.Gtk_Event_Box to receive the events, and pack the label
   --  inside it, since labels are windowless (they return False from
   --  Gtk.Widget.Get_Has_Window). Remember when using the
   --  Pango.Layout.Pango_Layout functions you need to convert to and from
   --  pixels using PANGO_PIXELS or PANGO_SCALE.
   --  "x": location to store X offset of layout, or null
   --  "y": location to store Y offset of layout, or null

   function Get_Line_Wrap
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether lines in the label are automatically wrapped. See
   --  Gtk.Label.Set_Line_Wrap.

   procedure Set_Line_Wrap
      (Label : not null access Gtk_Label_Record;
       Wrap  : Boolean);
   --  Toggles line wrapping within the Gtk.Label.Gtk_Label widget. True makes
   --  it break lines if text exceeds the widget's size. False lets the text
   --  get cut off by the edge of the widget if it exceeds the widget size.
   --  Note that setting line wrapping to True does not make the label wrap at
   --  its parent container's width, because GTK+ widgets conceptually can't
   --  make their requisition depend on the parent container's size. For a
   --  label that wraps at a specific position, set the label's width using
   --  Gtk.Widget.Set_Size_Request.
   --  "wrap": the setting

   function Get_Line_Wrap_Mode
      (Label : not null access Gtk_Label_Record)
       return Pango.Enums.Wrap_Mode;
   --  Returns line wrap mode used by the label. See
   --  Gtk.Label.Set_Line_Wrap_Mode.
   --  Since: gtk+ 2.10

   procedure Set_Line_Wrap_Mode
      (Label     : not null access Gtk_Label_Record;
       Wrap_Mode : Pango.Enums.Wrap_Mode);
   --  If line wrapping is on (see Gtk.Label.Set_Line_Wrap) this controls how
   --  the line wrapping is done. The default is Pango.Enums.Pango_Wrap_Word
   --  which means wrap on word boundaries.
   --  Since: gtk+ 2.10
   --  "wrap_mode": the line wrapping mode

   function Get_Lines
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Gets the number of lines to which an ellipsized, wrapping label should
   --  be limited. See Gtk.Label.Set_Lines.
   --  Since: gtk+ 3.10

   procedure Set_Lines
      (Label : not null access Gtk_Label_Record;
       Lines : Glib.Gint);
   --  Sets the number of lines to which an ellipsized, wrapping label should
   --  be limited. This has no effect if the label is not wrapping or
   --  ellipsized. Set this to -1 if you don't want to limit the number of
   --  lines.
   --  Since: gtk+ 3.10
   --  "lines": the desired number of lines, or -1

   function Get_Max_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Retrieves the desired maximum width of Label, in characters. See
   --  Gtk.Label.Set_Width_Chars.
   --  Since: gtk+ 2.6

   procedure Set_Max_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint);
   --  Sets the desired maximum width in characters of Label to N_Chars.
   --  Since: gtk+ 2.6
   --  "n_chars": the new desired maximum width, in characters.

   function Get_Mnemonic_Keyval
      (Label : not null access Gtk_Label_Record) return Guint;
   --  If the label has been set so that it has an mnemonic key this function
   --  returns the keyval used for the mnemonic accelerator. If there is no
   --  mnemonic set up it returns GDK_KEY_Void_Symbol.

   function Get_Mnemonic_Widget
      (Label : not null access Gtk_Label_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the target of the mnemonic (keyboard shortcut) of this label.
   --  See Gtk.Label.Set_Mnemonic_Widget.

   procedure Set_Mnemonic_Widget
      (Label  : not null access Gtk_Label_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  If the label has been set so that it has an mnemonic key (using i.e.
   --  Gtk.Label.Set_Markup_With_Mnemonic, Gtk.Label.Set_Text_With_Mnemonic,
   --  Gtk.Label.Gtk_New_With_Mnemonic or the "use_underline" property) the
   --  label can be associated with a widget that is the target of the
   --  mnemonic. When the label is inside a widget (like a
   --  Gtk.Button.Gtk_Button or a Gtk.Notebook.Gtk_Notebook tab) it is
   --  automatically associated with the correct widget, but sometimes (i.e.
   --  when the target is a Gtk.GEntry.Gtk_Entry next to the label) you need to
   --  set it explicitly using this function.
   --  The target widget will be accelerated by emitting the
   --  GtkWidget::mnemonic-activate signal on it. The default handler for this
   --  signal will activate the widget if there are no mnemonic collisions and
   --  toggle focus between the colliding widgets otherwise.
   --  "widget": the target Gtk.Widget.Gtk_Widget, or null to unset

   function Get_Selectable
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Gets the value set by Gtk.Label.Set_Selectable.

   procedure Set_Selectable
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  Selectable labels allow the user to select text from the label, for
   --  copy-and-paste.
   --  "setting": True to allow selecting text in the label

   procedure Get_Selection_Bounds
      (Label         : not null access Gtk_Label_Record;
       Start         : out Glib.Gint;
       The_End       : out Glib.Gint;
       Has_Selection : out Boolean);
   --  Gets the selected range of characters in the label, returning True if
   --  there's a selection.
   --  "start": return location for start of selection, as a character offset
   --  "end": return location for end of selection, as a character offset

   function Get_Single_Line_Mode
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label is in single line mode.
   --  Since: gtk+ 2.6

   procedure Set_Single_Line_Mode
      (Label            : not null access Gtk_Label_Record;
       Single_Line_Mode : Boolean);
   --  Sets whether the label is in single line mode.
   --  Since: gtk+ 2.6
   --  "single_line_mode": True if the label should be in single line mode

   function Get_Text
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Fetches the text from a label widget, as displayed on the screen. This
   --  does not include any embedded underlines indicating mnemonics or Pango
   --  markup. (See Gtk.Label.Get_Label)

   procedure Set_Text
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the text within the Gtk.Label.Gtk_Label widget. It overwrites any
   --  text that was there before.
   --  This function will clear any previously set mnemonic accelerators, and
   --  set the Gtk.Label.Gtk_Label:use-underline property to False as a side
   --  effect.
   --  This function will set the Gtk.Label.Gtk_Label:use-markup property to
   --  False as a side effect.
   --  See also: Gtk.Label.Set_Markup
   --  "str": The text you want to set

   function Get_Track_Visited_Links
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label is currently keeping track of clicked links.
   --  Since: gtk+ 2.18

   procedure Set_Track_Visited_Links
      (Label       : not null access Gtk_Label_Record;
       Track_Links : Boolean);
   --  Sets whether the label should keep track of clicked links (and use a
   --  different color for them).
   --  Since: gtk+ 2.18
   --  "track_links": True to track visited links

   function Get_Use_Markup
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label's text is interpreted as marked up with the
   --  [Pango text markup language][PangoMarkupFormat]. See
   --  gtk_label_set_use_markup ().

   procedure Set_Use_Markup
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  Sets whether the text of the label contains markup in [Pango's text
   --  markup language][PangoMarkupFormat]. See Gtk.Label.Set_Markup.
   --  "setting": True if the label's text should be parsed for markup.

   function Get_Use_Underline
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether an embedded underline in the label indicates a
   --  mnemonic. See Gtk.Label.Set_Use_Underline.

   procedure Set_Use_Underline
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  If true, an underline in the text indicates the next character should
   --  be used for the mnemonic accelerator key.
   --  "setting": True if underlines in the text indicate mnemonics

   function Get_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Retrieves the desired width of Label, in characters. See
   --  Gtk.Label.Set_Width_Chars.
   --  Since: gtk+ 2.6

   procedure Set_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint);
   --  Sets the desired width in characters of Label to N_Chars.
   --  Since: gtk+ 2.6
   --  "n_chars": the new desired width, in characters.

   function Get_Xalign
      (Label : not null access Gtk_Label_Record) return Gfloat;
   --  Gets the Gtk.Label.Gtk_Label:xalign property for Label.
   --  Since: gtk+ 3.16

   procedure Set_Xalign
      (Label  : not null access Gtk_Label_Record;
       Xalign : Gfloat);
   --  Sets the Gtk.Label.Gtk_Label:xalign property for Label.
   --  Since: gtk+ 3.16
   --  "xalign": the new xalign value, between 0 and 1

   function Get_Yalign
      (Label : not null access Gtk_Label_Record) return Gfloat;
   --  Gets the Gtk.Label.Gtk_Label:yalign property for Label.
   --  Since: gtk+ 3.16

   procedure Set_Yalign
      (Label  : not null access Gtk_Label_Record;
       Yalign : Gfloat);
   --  Sets the Gtk.Label.Gtk_Label:yalign property for Label.
   --  Since: gtk+ 3.16
   --  "yalign": the new yalign value, between 0 and 1

   procedure Select_Region
      (Label        : not null access Gtk_Label_Record;
       Start_Offset : Glib.Gint := -1;
       End_Offset   : Glib.Gint := -1);
   --  Selects a range of characters in the label, if the label is selectable.
   --  See Gtk.Label.Set_Selectable. If the label is not selectable, this
   --  function has no effect. If Start_Offset or End_Offset are -1, then the
   --  end of the label will be substituted.
   --  "start_offset": start offset (in characters not bytes)
   --  "end_offset": end offset (in characters not bytes)

   procedure Set_Markup
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Parses Str which is marked up with the [Pango text markup
   --  language][PangoMarkupFormat], setting the label's text and attribute
   --  list based on the parse results.
   --  If the Str is external data, you may need to escape it with
   --  g_markup_escape_text or g_markup_printf_escaped:
   --  |[<!-- language="C" --> GtkWidget *label = gtk_label_new (NULL); const
   --  char *str = "some text"; const char *format = "<span
   --  style=\"italic\">\%s</span>"; char *markup;
   --  markup = g_markup_printf_escaped (format, str); gtk_label_set_markup
   --  (GTK_LABEL (label), markup); g_free (markup); ]|
   --  This function will set the Gtk.Label.Gtk_Label:use-markup property to
   --  True as a side effect.
   --  If you set the label contents using the Gtk.Label.Gtk_Label:label
   --  property you should also ensure that you set the
   --  Gtk.Label.Gtk_Label:use-markup property accordingly.
   --  See also: Gtk.Label.Set_Text
   --  "str": a markup string (see [Pango markup format][PangoMarkupFormat])

   procedure Set_Markup_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Parses Str which is marked up with the [Pango text markup
   --  language][PangoMarkupFormat], setting the label's text and attribute
   --  list based on the parse results. If characters in Str are preceded by an
   --  underscore, they are underlined indicating that they represent a
   --  keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically, or explicitly using Gtk.Label.Set_Mnemonic_Widget.
   --  "str": a markup string (see [Pango markup format][PangoMarkupFormat])

   procedure Set_Pattern
      (Label   : not null access Gtk_Label_Record;
       Pattern : UTF8_String);
   --  Change the underlines pattern.
   --  Pattern is a simple string made of underscore and space characters,
   --  matching the ones in the string. GtkAda will underline every letter that
   --  matches an underscore.
   --  An empty string disables the underlines.
   --  example: If the text is FooBarBaz and the Pattern is "___ ___" then
   --  both "Foo" and "Baz" will be underlined, but not "Bar".
   --  "pattern": The pattern as described above.

   procedure Set_Text_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the label's text from the string Str. If characters in Str are
   --  preceded by an underscore, they are underlined indicating that they
   --  represent a keyboard accelerator called a mnemonic. The mnemonic key can
   --  be used to activate another widget, chosen automatically, or explicitly
   --  using Gtk.Label.Set_Mnemonic_Widget.
   --  "str": a string

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Angle_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The angle that the baseline of the label makes with the horizontal, in
   --  degrees, measured counterclockwise. An angle of 90 reads from from
   --  bottom to top, an angle of 270, from top to bottom. Ignored if the label
   --  is selectable.

   Attributes_Property : constant Glib.Properties.Property_Object;
   --  Type: Pango.Attributes.Pango_Attr_List

   Cursor_Position_Property : constant Glib.Properties.Property_Int;

   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode;
   --  Type: Pango.Layout.Pango_Ellipsize_Mode
   --  The preferred place to ellipsize the string, if the label does not have
   --  enough room to display the entire string, specified as a
   --  Pango.Layout.Pango_Ellipsize_Mode.
   --
   --  Note that setting this property to a value other than
   --  Pango.Layout.Ellipsize_None has the side-effect that the label requests
   --  only enough space to display the ellipsis "...". In particular, this
   --  means that ellipsizing labels do not work well in notebook tabs, unless
   --  the Gtk.Notebook.Gtk_Notebook tab-expand child property is set to True.
   --  Other ways to set a label's width are Gtk.Widget.Set_Size_Request and
   --  Gtk.Label.Set_Width_Chars.

   Justify_Property : constant Gtk.Enums.Property_Gtk_Justification;

   Label_Property : constant Glib.Properties.Property_String;
   --  The contents of the label.
   --
   --  If the string contains [Pango XML markup][PangoMarkupFormat], you will
   --  have to set the Gtk.Label.Gtk_Label:use-markup property to True in order
   --  for the label to display the markup attributes. See also
   --  Gtk.Label.Set_Markup for a convenience function that sets both this
   --  property and the Gtk.Label.Gtk_Label:use-markup property at the same
   --  time.
   --
   --  If the string contains underlines acting as mnemonics, you will have to
   --  set the Gtk.Label.Gtk_Label:use-underline property to True in order for
   --  the label to display them.

   Lines_Property : constant Glib.Properties.Property_Int;
   --  The number of lines to which an ellipsized, wrapping label should be
   --  limited. This property has no effect if the label is not wrapping or
   --  ellipsized. Set this property to -1 if you don't want to limit the
   --  number of lines.

   Max_Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired maximum width of the label, in characters. If this property
   --  is set to -1, the width will be calculated automatically.
   --
   --  See the section on [text layout][label-text-layout] for details of how
   --  Gtk.Label.Gtk_Label:width-chars and Gtk.Label.Gtk_Label:max-width-chars
   --  determine the width of ellipsized and wrapped labels.

   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint;

   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Pattern_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Selectable_Property : constant Glib.Properties.Property_Boolean;

   Selection_Bound_Property : constant Glib.Properties.Property_Int;

   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the label is in single line mode. In single line mode, the
   --  height of the label does not depend on the actual text, it is always set
   --  to ascent + descent of the font. This can be an advantage in situations
   --  where resizing the label because of text changes would be distracting,
   --  e.g. in a statusbar.

   Track_Visited_Links_Property : constant Glib.Properties.Property_Boolean;
   --  Set this property to True to make the label track which links have been
   --  visited. It will then apply the GTK_STATE_FLAG_VISITED when rendering
   --  this link, in addition to GTK_STATE_FLAG_LINK.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

   Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired width of the label, in characters. If this property is set
   --  to -1, the width will be calculated automatically.
   --
   --  See the section on [text layout][label-text-layout] for details of how
   --  Gtk.Label.Gtk_Label:width-chars and Gtk.Label.Gtk_Label:max-width-chars
   --  determine the width of ellipsized and wrapped labels.

   Wrap_Property : constant Glib.Properties.Property_Boolean;

   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode;
   --  Type: Pango.Enums.Wrap_Mode
   --  If line wrapping is on (see the Gtk.Label.Gtk_Label:wrap property) this
   --  controls how the line wrapping is done. The default is
   --  Pango.Enums.Pango_Wrap_Word, which means wrap on word boundaries.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The xalign property determines the horizontal aligment of the label
   --  text inside the labels size allocation. Compare this to
   --  Gtk.Widget.Gtk_Widget:halign, which determines how the labels size
   --  allocation is positioned in the space available for the label.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  The yalign property determines the vertical aligment of the label text
   --  inside the labels size allocation. Compare this to
   --  Gtk.Widget.Gtk_Widget:valign, which determines how the labels size
   --  allocation is positioned in the space available for the label.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Label_Void is not null access procedure (Self : access Gtk_Label_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate_Current_Link : constant Glib.Signal_Name := "activate-current-link";
   procedure On_Activate_Current_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Void;
       After : Boolean := False);
   procedure On_Activate_Current_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  A [keybinding signal][GtkBindingSignal] which gets emitted when the
   --  user activates a link in the label.
   --
   --  Applications may also emit the signal with g_signal_emit_by_name if
   --  they need to control activation of URIs programmatically.
   --
   --  The default bindings for this signal are all forms of the Enter key.

   type Cb_Gtk_Label_UTF8_String_Boolean is not null access function
     (Self : access Gtk_Label_Record'Class;
      URI  : UTF8_String) return Boolean;

   type Cb_GObject_UTF8_String_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      URI  : UTF8_String) return Boolean;

   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";
   procedure On_Activate_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_UTF8_String_Boolean;
       After : Boolean := False);
   procedure On_Activate_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The signal which gets emitted to activate a URI. Applications may
   --  connect to it to override the default behaviour, which is to call
   --  gtk_show_uri_on_window.
   -- 
   --  Callback parameters:
   --    --  "uri": the URI that is activated
   --    --  Returns True if the link has been activated

   Signal_Copy_Clipboard : constant Glib.Signal_Name := "copy-clipboard";
   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Void;
       After : Boolean := False);
   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::copy-clipboard signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted to copy the selection to the clipboard.
   --
   --  The default binding for this signal is Ctrl-c.

   type Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void is not null access procedure
     (Self             : access Gtk_Label_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Glib.Gint;
      Extend_Selection : Boolean);

   type Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void is not null access procedure
     (Self             : access Glib.Object.GObject_Record'Class;
      Step             : Gtk.Enums.Gtk_Movement_Step;
      Count            : Glib.Gint;
      Extend_Selection : Boolean);

   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void;
       After : Boolean := False);
   procedure On_Move_Cursor
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::move-cursor signal is a [keybinding signal][GtkBindingSignal]
   --  which gets emitted when the user initiates a cursor movement. If the
   --  cursor is not visible in Entry, this signal causes the viewport to be
   --  moved instead.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the Shift modifier extends the selection, the variant without the
   --  Shift modifer does not. There are too many key combinations to list them
   --  all here. - Arrow keys move by individual characters/lines - Ctrl-arrow
   --  key combinations move by words/paragraphs - Home/End keys move to the
   --  ends of the buffer
   -- 
   --  Callback parameters:
   --    --  "step": the granularity of the move, as a Gtk.Enums.Gtk_Movement_Step
   --    --  "count": the number of Step units to move
   --    --  "extend_selection": True if the move should extend the selection

   type Cb_Gtk_Label_Gtk_Menu_Void is not null access procedure
     (Self : access Gtk_Label_Record'Class;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   type Cb_GObject_Gtk_Menu_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   Signal_Populate_Popup : constant Glib.Signal_Name := "populate-popup";
   procedure On_Populate_Popup
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Gtk_Menu_Void;
       After : Boolean := False);
   procedure On_Populate_Popup
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Gtk_Menu_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::populate-popup signal gets emitted before showing the context
   --  menu of the label. Note that only selectable labels have context menus.
   --
   --  If you need to add items to the context menu, connect to this signal
   --  and append your menuitems to the Menu.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Label
   renames Implements_Gtk_Buildable.To_Object;

private
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode :=
     Pango.Enums.Build ("wrap-mode");
   Wrap_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Track_Visited_Links_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("track-visited-links");
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-line-mode");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Pattern_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("pattern");
   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("mnemonic-widget");
   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("mnemonic-keyval");
   Max_Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-width-chars");
   Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("lines");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Justify_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justify");
   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode :=
     Pango.Layout.Build ("ellipsize");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Attributes_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attributes");
   Angle_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("angle");
end Gtk.Label;
