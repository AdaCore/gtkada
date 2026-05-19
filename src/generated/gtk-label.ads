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

--  Displays a small amount of text.
--
--  Most labels are used to label another widget (such as an [classEntry]).
--
--  <picture> <source srcset="label-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkLabel" src="label.png"> </picture>
--  ## Shortcuts and Gestures
--
--  `GtkLabel` supports the following keyboard shortcuts, when the cursor is
--  visible:
--
--  - <kbd>Shift</kbd>+<kbd>F10</kbd> or <kbd>Menu</kbd> opens the context
--  menu. - <kbd>Ctrl</kbd>+<kbd>A</kbd> or <kbd>Ctrl</kbd>+<kbd>&sol;</kbd>
--  selects all. - <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>A</kbd> or
--  <kbd>Ctrl</kbd>+<kbd>&bsol;</kbd> unselects all.
--
--  Additionally, the following signals have default keybindings:
--
--  - [signalGtk.Label::activate-current-link] -
--  [signalGtk.Label::copy-clipboard] - [signalGtk.Label::move-cursor]
--
--  ## Actions
--
--  `GtkLabel` defines a set of built-in actions:
--
--  - `clipboard.copy` copies the text to the clipboard. - `clipboard.cut`
--  doesn't do anything, since text in labels can't be deleted. -
--  `clipboard.paste` doesn't do anything, since text in labels can't be
--  edited. - `link.open` opens the link, when activated on a link inside the
--  label. - `link.copy` copies the link to the clipboard, when activated on a
--  link inside the label. - `menu.popup` opens the context menu. -
--  `selection.delete` doesn't do anything, since text in labels can't be
--  deleted. - `selection.select-all` selects all of the text, if the label
--  allows selection.
--
--  ## CSS nodes
--
--  ``` label ├── [selection] ├── [link] ┊ ╰── [link] ```
--
--  `GtkLabel` has a single CSS node with the name label. A wide variety of
--  style classes may be applied to labels, such as .title, .subtitle,
--  .dim-label, etc. In the `GtkShortcutsWindow`, labels are used with the
--  .keycap style class.
--
--  If the label has a selection, it gets a subnode with name selection.
--
--  If the label has links, there is one subnode per link. These subnodes
--  carry the link or visited state depending on whether they have been
--  visited. In this case, label node also gets a .link style class.
--
--  ## GtkLabel as GtkBuildable
--
--  The GtkLabel implementation of the GtkBuildable interface supports a
--  custom `<attributes>` element, which supports any number of `<attribute>`
--  elements. The `<attribute>` element has attributes named "name", "value",
--  "start" and "end" and allows you to specify [structPango.Attribute] values
--  for this label.
--
--  An example of a UI definition fragment specifying Pango attributes:
--
--  ```xml <object class="GtkLabel"> <attributes> <attribute name="weight"
--  value="PANGO_WEIGHT_BOLD"/> <attribute name="background" value="red"
--  start="5" end="10"/> </attributes> </object> ```
--
--  The start and end attributes specify the range of characters to which the
--  Pango attribute applies. If start and end are not specified, the attribute
--  is applied to the whole text. Note that specifying ranges does not make
--  much sense with translatable attributes. Use markup embedded in the
--  translatable content instead.
--
--  ## Accessibility
--
--  `GtkLabel` uses the [enumGtk.AccessibleRole.label] role.
--
--  ## Mnemonics
--
--  Labels may contain "mnemonics". Mnemonics are underlined characters in the
--  label, used for keyboard navigation. Mnemonics are created by providing a
--  string with an underscore before the mnemonic character, such as `"_File"`,
--  to the functions [ctorGtk.Label.new_with_mnemonic] or
--  [methodGtk.Label.set_text_with_mnemonic].
--
--  Mnemonics automatically activate any activatable widget the label is
--  inside, such as a [classGtk.Button]; if the label is not inside the
--  mnemonic's target widget, you have to tell the label about the target using
--  [methodGtk.Label.set_mnemonic_widget].
--
--  Here's a simple example where the label is inside a button:
--
--  ```c // Pressing Alt+H will activate this button GtkWidget *button =
--  gtk_button_new (); GtkWidget *label = gtk_label_new_with_mnemonic
--  ("_Hello"); gtk_button_set_child (GTK_BUTTON (button), label); ```
--
--  There's a convenience function to create buttons with a mnemonic label
--  already inside:
--
--  ```c // Pressing Alt+H will activate this button GtkWidget *button =
--  gtk_button_new_with_mnemonic ("_Hello"); ```
--
--  To create a mnemonic for a widget alongside the label, such as a
--  [classGtk.Entry], you have to point the label at the entry with
--  [methodGtk.Label.set_mnemonic_widget]:
--
--  ```c // Pressing Alt+H will focus the entry GtkWidget *entry =
--  gtk_entry_new (); GtkWidget *label = gtk_label_new_with_mnemonic
--  ("_Hello"); gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry); ```
--
--  ## Markup (styled text)
--
--  To make it easy to format text in a label (changing colors, fonts, etc.),
--  label text can be provided in a simple markup format:
--
--  Here's how to create a label with a small font: ```c GtkWidget *label =
--  gtk_label_new (NULL); gtk_label_set_markup (GTK_LABEL (label),
--  "<small>Small text</small>"); ```
--
--  (See the Pango manual for complete documentation] of available tags,
--  [funcPango.parse_markup])
--
--  The markup passed to [methodGtk.Label.set_markup] must be valid XML; for
--  example, literal `<`, `>` and `&` characters must be escaped as `<`, `>`,
--  and `&amp;`. If you pass text obtained from the user, file, or a network to
--  [methodGtk.Label.set_markup], you'll want to escape it with
--  [funcGlib.markup_escape_text] or [funcGlib.markup_printf_escaped].
--
--  Markup strings are just a convenient way to set the [structPango.AttrList]
--  on a label; [methodGtk.Label.set_attributes] may be a simpler way to set
--  attributes in some cases. Be careful though; [structPango.AttrList] tends
--  to cause internationalization problems, unless you're applying attributes
--  to the entire string (i.e. unless you set the range of each attribute to
--  [0, `G_MAXINT`)). The reason is that specifying the `start_index` and
--  `end_index` for a [structPango.Attribute] requires knowledge of the exact
--  string being displayed, so translations will cause problems.
--
--  ## Selectable labels
--
--  Labels can be made selectable with [methodGtk.Label.set_selectable].
--  Selectable labels allow the user to copy the label contents to the
--  clipboard. Only labels that contain useful-to-copy information — such as
--  error messages — should be made selectable.
--
--  ## Text layout
--
--  A label can contain any number of paragraphs, but will have performance
--  problems if it contains more than a small number. Paragraphs are separated
--  by newlines or other paragraph separators understood by Pango.
--
--  Labels can automatically wrap text if you call [methodGtk.Label.set_wrap].
--
--  [methodGtk.Label.set_justify] sets how the lines in a label align with one
--  another. If you want to set how the label as a whole aligns in its
--  available space, see the [propertyGtk.Widget:halign] and
--  [propertyGtk.Widget:valign] properties.
--
--  The [propertyGtk.Label:width-chars] and
--  [propertyGtk.Label:max-width-chars] properties can be used to control the
--  size allocation of ellipsized or wrapped labels. For ellipsizing labels, if
--  either is specified (and less than the actual text size), it is used as the
--  minimum width, and the actual text size is used as the natural width of the
--  label. For wrapping labels, width-chars is used as the minimum width, if
--  specified, and max-width-chars is used as the natural width. Even if
--  max-width-chars specified, wrapping labels will be rewrapped to use all of
--  the available width.
--
--  ## Links
--
--  GTK supports markup for clickable hyperlinks in addition to regular Pango
--  markup. The markup for links is borrowed from HTML, using the `<a>` tag
--  with "href", "title" and "class" attributes. GTK renders links similar to
--  the way they appear in web browsers, with colored, underlined text. The
--  "title" attribute is displayed as a tooltip on the link. The "class"
--  attribute is used as style class on the CSS node for the link.
--
--  An example of inline links looks like this:
--
--  ```c const char *text = "Go to the " "<a href=\"https://www.gtk.org\"
--  title=\"<i>Our</i> website\">" "GTK website</a> for more..."; GtkWidget
--  *label = gtk_label_new (NULL); gtk_label_set_markup (GTK_LABEL (label),
--  text); ```
--
--  It is possible to implement custom handling for links and their tooltips
--  with the [signalGtk.Label::activate-link] signal and the
--  [methodGtk.Label.get_current_uri] function.
--
--  <screenshot>gtk-label</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_label.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                     use Glib;
with Glib.Menu_Model;          use Glib.Menu_Model;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Types;               use Glib.Types;
with Gtk.Accessible;           use Gtk.Accessible;
with Gtk.Accessible_Hypertext; use Gtk.Accessible_Hypertext;
with Gtk.Accessible_Text;      use Gtk.Accessible_Text;
with Gtk.Atcontext;            use Gtk.Atcontext;
with Gtk.Constraint_Target;    use Gtk.Constraint_Target;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Attributes;         use Pango.Attributes;
with Pango.Enums;              use Pango.Enums;
with Pango.Layout;             use Pango.Layout;
with Pango.Tabs;               use Pango.Tabs;

package Gtk.Label is

   type Gtk_Label_Record is new Gtk_Widget_Record with null record;
   type Gtk_Label is access all Gtk_Label_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "");
   procedure Initialize
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "");
   --  Creates a new label with the given text inside it.
   --  You can pass `NULL` to get an empty label widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Str the text of the label

   function Gtk_Label_New (Str : UTF8_String := "") return Gtk_Label;
   --  Creates a new label with the given text inside it.
   --  You can pass `NULL` to get an empty label widget.
   --  @param Str the text of the label

   procedure Gtk_New_With_Mnemonic
      (Label : out Gtk_Label;
       Str   : UTF8_String := "");
   procedure Initialize_With_Mnemonic
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "");
   --  Creates a new label with the given text inside it, and a mnemonic.
   --  If characters in Str are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  '__' (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. The mnemonic key can be used to
   --  activate another widget, chosen automatically, or explicitly using
   --  [methodGtk.Label.set_mnemonic_widget].
   --  If [methodGtk.Label.set_mnemonic_widget] is not called, then the first
   --  activatable ancestor of the label will be chosen as the mnemonic widget.
   --  For instance, if the label is inside a button or menu item, the button
   --  or menu item will automatically become the mnemonic widget and be
   --  activated by the mnemonic.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Str the text of the label, with an underscore in front of the
   --  mnemonic character

   function Gtk_Label_New_With_Mnemonic
      (Str : UTF8_String := "") return Gtk_Label;
   --  Creates a new label with the given text inside it, and a mnemonic.
   --  If characters in Str are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  '__' (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic. The mnemonic key can be used to
   --  activate another widget, chosen automatically, or explicitly using
   --  [methodGtk.Label.set_mnemonic_widget].
   --  If [methodGtk.Label.set_mnemonic_widget] is not called, then the first
   --  activatable ancestor of the label will be chosen as the mnemonic widget.
   --  For instance, if the label is inside a button or menu item, the button
   --  or menu item will automatically become the mnemonic widget and be
   --  activated by the mnemonic.
   --  @param Str the text of the label, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_label_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Attributes
      (Label : not null access Gtk_Label_Record)
       return Pango.Attributes.Pango_Attr_List;
   --  Gets the label's attribute list.
   --  This is the [structPango.AttrList] that was set on the label using
   --  [methodGtk.Label.set_attributes], if any. This function does not reflect
   --  attributes that come from the label's markup (see
   --  [methodGtk.Label.set_markup]). If you want to get the effective
   --  attributes for the label, use `pango_layout_get_attributes
   --  (gtk_label_get_layout (self))`.
   --  @return the attribute list

   procedure Set_Attributes
      (Label : not null access Gtk_Label_Record;
       Attrs : Pango.Attributes.Pango_Attr_List);
   --  Apply attributes to the label text.
   --  The attributes set with this function will be applied and merged with
   --  any other attributes previously effected by way of the
   --  [propertyGtk.Label:use-underline] or [propertyGtk.Label:use-markup]
   --  properties
   --  While it is not recommended to mix markup strings with manually set
   --  attributes, if you must; know that the attributes will be applied to the
   --  label after the markup string is parsed.
   --  @param Attrs a list of style attributes

   function Get_Current_Uri
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Returns the URI for the active link in the label.
   --  The active link is the one under the mouse pointer or, in a selectable
   --  label, the link in which the text cursor is currently positioned.
   --  This function is intended for use in a [signalGtk.Label::activate-link]
   --  handler or for use in a [signalGtk.Widget::query-tooltip] handler.
   --  @return the active URI

   function Get_Ellipsize
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   --  Returns the ellipsization mode of the label.
   --  See [methodGtk.Label.set_ellipsize].
   --  @return the ellipsization mode

   procedure Set_Ellipsize
      (Label : not null access Gtk_Label_Record;
       Mode  : Pango.Layout.Pango_Ellipsize_Mode);
   --  Sets the mode used to ellipsize the text.
   --  The text will be ellipsized if there is not enough space to render the
   --  entire string.
   --  @param Mode the ellipsization mode

   function Get_Extra_Menu
      (Label : not null access Gtk_Label_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Gets the extra menu model of the label.
   --  See [methodGtk.Label.set_extra_menu].
   --  @return the menu model

   procedure Set_Extra_Menu
      (Label : not null access Gtk_Label_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a menu model to add to the context menu of the label.
   --  @param Model a menu model

   function Get_Justify
      (Label : not null access Gtk_Label_Record)
       return Gtk.Enums.Gtk_Justification;
   --  Returns the justification of the label.
   --  See [methodGtk.Label.set_justify].
   --  @return the justification value

   procedure Set_Justify
      (Label : not null access Gtk_Label_Record;
       Jtype : Gtk.Enums.Gtk_Justification);
   --  Sets the alignment of lines in the label relative to each other.
   --  This function has no effect on labels containing only a single line.
   --  [enumGtk.Justification.left] is the default value when the widget is
   --  first created with [ctorGtk.Label.new].
   --  If you instead want to set the alignment of the label as a whole, use
   --  [methodGtk.Widget.set_halign] instead.
   --  @param Jtype the new justification

   function Get_Label
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Fetches the text from a label.
   --  The returned text includes any embedded underlines indicating mnemonics
   --  and Pango markup. (See [methodGtk.Label.get_text]).
   --  @return the text of the label widget

   procedure Set_Label
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the text of the label.
   --  The label is interpreted as including embedded underlines and/or Pango
   --  markup depending on the values of the [propertyGtk.Label:use-underline]
   --  and [propertyGtk.Label:use-markup] properties.
   --  @param Str the new text to set for the label

   function Get_Layout
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Layout;
   --  Gets the Pango layout used to display the label.
   --  The layout is useful to e.g. convert text positions to pixel positions,
   --  in combination with [methodGtk.Label.get_layout_offsets]. The returned
   --  layout is owned by the Label so need not be freed by the caller. The
   --  Label is free to recreate its layout at any time, so it should be
   --  considered read-only.
   --  @return the [classPango.Layout] for this label

   procedure Get_Layout_Offsets
      (Label : not null access Gtk_Label_Record;
       X     : out Glib.Gint;
       Y     : out Glib.Gint);
   --  Obtains the coordinates where the label will draw its Pango layout.
   --  The coordinates are useful to convert mouse events into coordinates
   --  inside the [classPango.Layout], e.g. to take some action if some part of
   --  the label is clicked. Remember when using the [classPango.Layout]
   --  functions you need to convert to and from pixels using `PANGO_PIXELS` or
   --  [constPango.SCALE].
   --  @param X location to store X offset of layout
   --  @param Y location to store Y offset of layout

   function Get_Lines
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Gets the number of lines to which an ellipsized, wrapping label should
   --  be limited.
   --  See [methodGtk.Label.set_lines].
   --  @return the number of lines

   procedure Set_Lines
      (Label : not null access Gtk_Label_Record;
       Lines : Glib.Gint);
   --  Sets the number of lines to which an ellipsized, wrapping label should
   --  be limited.
   --  This has no effect if the label is not wrapping or ellipsized. Set this
   --  to -1 if you don't want to limit the number of lines.
   --  @param Lines the desired number of lines, or -1

   function Get_Max_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Retrieves the maximum width of the label in characters.
   --  See [methodGtk.Label.set_width_chars].
   --  @return the maximum width of the label, in characters

   procedure Set_Max_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint);
   --  Sets the maximum width of the label in characters.
   --  @param N_Chars the new maximum width, in characters.

   function Get_Mnemonic_Keyval
      (Label : not null access Gtk_Label_Record) return Guint;
   --  Return the mnemonic accelerator.
   --  If the label has been set so that it has a mnemonic key this function
   --  returns the keyval used for the mnemonic accelerator. If there is no
   --  mnemonic set up it returns `GDK_KEY_VoidSymbol`.
   --  @return GDK keyval usable for accelerators, or `GDK_KEY_VoidSymbol`

   function Get_Mnemonic_Widget
      (Label : not null access Gtk_Label_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the mnemonic target of this label.
   --  See [methodGtk.Label.set_mnemonic_widget].
   --  @return the target of the label's mnemonic, or `NULL` if none has been
   --  set and the default algorithm will be used.

   procedure Set_Mnemonic_Widget
      (Label  : not null access Gtk_Label_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Associate the label with its mnemonic target.
   --  If the label has been set so that it has a mnemonic key (using i.e.
   --  [methodGtk.Label.set_markup_with_mnemonic],
   --  [methodGtk.Label.set_text_with_mnemonic],
   --  [ctorGtk.Label.new_with_mnemonic] or the
   --  [propertyGtk.Label:use_underline] property) the label can be associated
   --  with a widget that is the target of the mnemonic. When the label is
   --  inside a widget (like a [classGtk.Button] or a [classGtk.Notebook] tab)
   --  it is automatically associated with the correct widget, but sometimes
   --  (i.e. when the target is a [classGtk.Entry] next to the label) you need
   --  to set it explicitly using this function.
   --  The target widget will be accelerated by emitting the
   --  [signalGtk.Widget::mnemonic-activate] signal on it. The default handler
   --  for this signal will activate the widget if there are no mnemonic
   --  collisions and toggle focus between the colliding widgets otherwise.
   --  @param Widget the target widget

   function Get_Natural_Wrap_Mode
      (Label : not null access Gtk_Label_Record)
       return Gtk.Enums.Gtk_Natural_Wrap_Mode;
   --  Returns natural line wrap mode used by the label.
   --  See [methodGtk.Label.set_natural_wrap_mode].
   --  Since: gtk+ 4.6
   --  @return the natural line wrap mode

   procedure Set_Natural_Wrap_Mode
      (Label     : not null access Gtk_Label_Record;
       Wrap_Mode : Gtk.Enums.Gtk_Natural_Wrap_Mode);
   --  Selects the line wrapping for the natural size request.
   --  This only affects the natural size requested, for the actual wrapping
   --  used, see the [propertyGtk.Label:wrap-mode] property.
   --  Since: gtk+ 4.6
   --  @param Wrap_Mode the line wrapping mode

   function Get_Selectable
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label is selectable.
   --  @return true if the user can copy text from the label

   procedure Set_Selectable
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  Makes text in the label selectable.
   --  Selectable labels allow the user to select text from the label, for
   --  copy-and-paste.
   --  @param Setting true to allow selecting text in the label

   procedure Get_Selection_Bounds
      (Label         : not null access Gtk_Label_Record;
       Start         : out Glib.Gint;
       The_End       : out Glib.Gint;
       Has_Selection : out Boolean);
   --  Gets the selected range of characters in the label.
   --  The returned Start and End positions are in characters.
   --  @param Start return location for start of selection
   --  @param The_End return location for end of selection
   --  @return true if selection is non-empty

   function Get_Single_Line_Mode
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label is in single line mode.
   --  @return true if the label is in single line mode

   procedure Set_Single_Line_Mode
      (Label            : not null access Gtk_Label_Record;
       Single_Line_Mode : Boolean);
   --  Sets whether the label is in single line mode.
   --  @param Single_Line_Mode true to enable single line mode

   function Get_Tabs
      (Label : not null access Gtk_Label_Record)
       return Pango.Tabs.Pango_Tab_Array;
   --  Gets the tab stops for the label.
   --  The returned array will be `NULL` if "standard" (8-space) tabs are
   --  used.
   --  Since: gtk+ 4.8
   --  @return copy of default tab array, or `NULL` if standard tabs are used

   procedure Set_Tabs
      (Label : not null access Gtk_Label_Record;
       Tabs  : Pango.Tabs.Pango_Tab_Array);
   --  Sets tab stops for the label.
   --  Since: gtk+ 4.8
   --  @param Tabs tab stops

   function Get_Text
      (Label : not null access Gtk_Label_Record) return UTF8_String;
   --  Gets the text of the label.
   --  The returned text is as it appears on screen. This does not include any
   --  embedded underlines indicating mnemonics or Pango markup. (See
   --  [methodGtk.Label.get_label])
   --  @return the text in the label widget

   procedure Set_Text
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the text for the label.
   --  It overwrites any text that was there before and clears any previously
   --  set mnemonic accelerators, and sets the
   --  [propertyGtk.Label:use-underline] and [propertyGtk.Label:use-markup]
   --  properties to false.
   --  Also see [methodGtk.Label.set_markup].
   --  @param Str the text to show in Self

   function Get_Use_Markup
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether the label's text is interpreted as Pango markup.
   --  See [methodGtk.Label.set_use_markup].
   --  @return true if the label's text will be parsed for markup

   procedure Set_Use_Markup
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  Sets whether the text of the label contains markup.
   --  See [methodGtk.Label.set_markup].
   --  @param Setting true if the label's text should be parsed for markup.

   function Get_Use_Underline
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether underlines in the label indicate mnemonics.
   --  See [methodGtk.Label.set_use_underline].
   --  @return true if underlines in the label indicate mnemonics

   procedure Set_Use_Underline
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean);
   --  Sets whether underlines in the text indicate mnemonics.
   --  @param Setting true if underlines in the text indicate mnemonics

   function Get_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint;
   --  Retrieves the desired width of the label in characters.
   --  See [methodGtk.Label.set_width_chars].
   --  @return the desired width of the label, in characters

   procedure Set_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint);
   --  Sets the desired width in characters of the label.
   --  @param N_Chars the new desired width, in characters.

   function Get_Wrap
      (Label : not null access Gtk_Label_Record) return Boolean;
   --  Returns whether lines in the label are automatically wrapped.
   --  See [methodGtk.Label.set_wrap].
   --  @return true if the lines of the label are automatically wrapped

   procedure Set_Wrap
      (Label : not null access Gtk_Label_Record;
       Wrap  : Boolean);
   --  Toggles line wrapping within the label.
   --  True makes it break lines if text exceeds the widget's size. false lets
   --  the text get cut off by the edge of the widget if it exceeds the widget
   --  size.
   --  Note that setting line wrapping to true does not make the label wrap at
   --  its parent widget's width, because GTK widgets conceptually can't make
   --  their requisition depend on the parent widget's size. For a label that
   --  wraps at a specific position, set the label's width using
   --  [methodGtk.Widget.set_size_request].
   --  @param Wrap whether to wrap lines

   function Get_Wrap_Mode
      (Label : not null access Gtk_Label_Record)
       return Pango.Enums.Wrap_Mode;
   --  Returns line wrap mode used by the label.
   --  See [methodGtk.Label.set_wrap_mode].
   --  @return the line wrap mode

   procedure Set_Wrap_Mode
      (Label     : not null access Gtk_Label_Record;
       Wrap_Mode : Pango.Enums.Wrap_Mode);
   --  Controls how line wrapping is done.
   --  This only affects the label if line wrapping is on. (See
   --  [methodGtk.Label.set_wrap])
   --  The default is [enumPango.WrapMode.word], which means wrap on word
   --  boundaries.
   --  For sizing behavior, also consider the
   --  [propertyGtk.Label:natural-wrap-mode] property.
   --  @param Wrap_Mode the line wrapping mode

   function Get_Xalign
      (Label : not null access Gtk_Label_Record) return float;
   --  Gets the `xalign` of the label.
   --  See the [propertyGtk.Label:xalign] property.
   --  @return the xalign value

   procedure Set_Xalign
      (Label  : not null access Gtk_Label_Record;
       Xalign : float);
   --  Sets the `xalign` of the label.
   --  See the [propertyGtk.Label:xalign] property.
   --  @param Xalign the new xalign value, between 0 and 1

   function Get_Yalign
      (Label : not null access Gtk_Label_Record) return float;
   --  Gets the `yalign` of the label.
   --  See the [propertyGtk.Label:yalign] property.
   --  @return the yalign value

   procedure Set_Yalign
      (Label  : not null access Gtk_Label_Record;
       Yalign : float);
   --  Sets the `yalign` of the label.
   --  See the [propertyGtk.Label:yalign] property.
   --  @param Yalign the new yalign value, between 0 and 1

   procedure Select_Region
      (Label        : not null access Gtk_Label_Record;
       Start_Offset : Glib.Gint := -1;
       End_Offset   : Glib.Gint := -1);
   --  Selects a range of characters in the label, if the label is selectable.
   --  See [methodGtk.Label.set_selectable]. If the label is not selectable,
   --  this function has no effect. If Start_Offset or End_Offset are -1, then
   --  the end of the label will be substituted.
   --  @param Start_Offset start offset, in characters
   --  @param End_Offset end offset, in characters

   procedure Set_Markup
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the labels text and attributes from markup.
   --  The string must be marked up with Pango markup (see
   --  [funcPango.parse_markup]).
   --  If Str is external data, you may need to escape it with
   --  [funcGlib.markup_escape_text] or [funcGlib.markup_printf_escaped]:
   --  ```c GtkWidget *self = gtk_label_new (NULL); const char *str = "...";
   --  const char *format = "<span style=\"italic\">\%s</span>"; char *markup;
   --  markup = g_markup_printf_escaped (format, str); gtk_label_set_markup
   --  (GTK_LABEL (self), markup); g_free (markup); ```
   --  This function sets the [propertyGtk.Label:use-markup] property to true.
   --  Also see [methodGtk.Label.set_text].
   --  @param Str the markup string

   procedure Set_Markup_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the labels text, attributes and mnemonic from markup.
   --  Parses Str which is marked up with Pango markup (see
   --  [funcPango.parse_markup]), setting the label's text and attribute list
   --  based on the parse results. If characters in Str are preceded by an
   --  underscore, they are underlined indicating that they represent a
   --  keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically, or explicitly using
   --  [methodGtk.Label.set_mnemonic_widget].
   --  @param Str the markup string

   procedure Set_Text_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String);
   --  Sets the text for the label, with mnemonics.
   --  If characters in Str are preceded by an underscore, they are underlined
   --  indicating that they represent a keyboard accelerator called a mnemonic.
   --  The mnemonic key can be used to activate another widget, chosen
   --  automatically, or explicitly using
   --  [methodGtk.Label.set_mnemonic_widget].
   --  @param Str the text

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Label_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Label_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Label_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Label_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Label_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Label_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Label_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Label_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Label_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Label_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Label_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Update_Caret_Position (Self : not null access Gtk_Label_Record);

   procedure Update_Contents
      (Self    : not null access Gtk_Label_Record;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint);

   procedure Update_Selection_Bound
      (Self : not null access Gtk_Label_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Attributes_Property : constant Glib.Properties.Property_Object;
   --  Type: Pango.Attributes.Pango_Attr_List
   --  A list of style attributes to apply to the text of the label.

   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode;
   --  Type: Pango.Layout.Pango_Ellipsize_Mode
   --  The preferred place to ellipsize the string, if the label does not have
   --  enough room to display the entire string.
   --
   --  Note that setting this property to a value other than
   --  [enum.Pango.EllipsizeMode.none] has the side-effect that the label
   --  requests only enough space to display the ellipsis "...". In particular,
   --  this means that ellipsizing labels do not work well in notebook tabs,
   --  unless the [propertyGtk.NotebookPage:tab-expand] child property is set
   --  to true.
   --
   --  Other ways to set a label's width are
   --  [methodGtk.Widget.set_size_request] and
   --  [methodGtk.Label.set_width_chars].

   Extra_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  A menu model whose contents will be appended to the context menu.

   Justify_Property : constant Gtk.Enums.Property_Gtk_Justification;
   --  The alignment of the lines in the text of the label, relative to each
   --  other.
   --
   --  This does *not* affect the alignment of the label within its
   --  allocation. See [propertyGtk.Label:xalign] for that.

   Label_Property : constant Glib.Properties.Property_String;
   --  The contents of the label.
   --
   --  If the string contains Pango markup (see [funcPango.parse_markup]), you
   --  will have to set the [propertyGtk.Label:use-markup] property to true in
   --  order for the label to display the markup attributes. See also
   --  [methodGtk.Label.set_markup] for a convenience function that sets both
   --  this property and the [propertyGtk.Label:use-markup] property at the
   --  same time.
   --
   --  If the string contains underlines acting as mnemonics, you will have to
   --  set the [propertyGtk.Label:use-underline] property to true in order for
   --  the label to display them.

   Lines_Property : constant Glib.Properties.Property_Int;
   --  The number of lines to which an ellipsized, wrapping label should
   --  display before it gets ellipsized. This both prevents the label from
   --  ellipsizing before this many lines are displayed, and limits the height
   --  request of the label to this many lines.
   --
   --  ::: warning Setting this property has unintuitive and unfortunate
   --  consequences for the minimum _width_ of the label. Specifically, if the
   --  height of the label is such that it fits a smaller number of lines than
   --  the value of this property, the label can not be ellipsized at all,
   --  which means it must be wide enough to fit all the text fully.
   --
   --  This property has no effect if the label is not wrapping or ellipsized.
   --
   --  Set this property to -1 if you don't want to limit the number of lines.

   Max_Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired maximum width of the label, in characters.
   --
   --  If this property is set to -1, the width will be calculated
   --  automatically.
   --
   --  See the section on [text layout](class.Label.htmltext-layout) for
   --  details of how [propertyGtk.Label:width-chars] and
   --  [propertyGtk.Label:max-width-chars] determine the width of ellipsized
   --  and wrapped labels.

   Mnemonic_Keyval_Property : constant Glib.Properties.Property_Uint;
   --  The mnemonic accelerator key for the label.

   Mnemonic_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The widget to be activated when the labels mnemonic key is pressed.

   Natural_Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Natural_Wrap_Mode;
   --  Select the line wrapping for the natural size request.
   --
   --  This only affects the natural size requested. For the actual wrapping
   --  used, see the [propertyGtk.Label:wrap-mode] property.
   --
   --  The default is [enumGtk.NaturalWrapMode.inherit], which inherits the
   --  behavior of the [propertyGtk.Label:wrap-mode] property.

   Selectable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the label text can be selected with the mouse.

   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the label is in single line mode.
   --
   --  In single line mode, the height of the label does not depend on the
   --  actual text, it is always set to ascent + descent of the font. This can
   --  be an advantage in situations where resizing the label because of text
   --  changes would be distracting, e.g. in a statusbar.

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array
   --  Custom tabs for this label.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  True if the text of the label includes Pango markup.
   --
   --  See [funcPango.parse_markup].

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  True if the text of the label indicates a mnemonic with an `_` before
   --  the mnemonic character.

   Width_Chars_Property : constant Glib.Properties.Property_Int;
   --  The desired width of the label, in characters.
   --
   --  If this property is set to -1, the width will be calculated
   --  automatically.
   --
   --  See the section on [text layout](class.Label.htmltext-layout) for
   --  details of how [propertyGtk.Label:width-chars] and
   --  [propertyGtk.Label:max-width-chars] determine the width of ellipsized
   --  and wrapped labels.

   Wrap_Property : constant Glib.Properties.Property_Boolean;
   --  True if the label text will wrap if it gets too wide.

   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode;
   --  Type: Pango.Enums.Wrap_Mode
   --  Controls how the line wrapping is done.
   --
   --  This only affects the formatting if line wrapping is on (see the
   --  [propertyGtk.Label:wrap] property). The default is
   --  [enumPango.WrapMode.word], which means wrap on word boundaries.
   --
   --  For sizing behavior, also consider the
   --  [propertyGtk.Label:natural-wrap-mode] property.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment of the label text inside its size allocation.
   --
   --  Compare this to [propertyGtk.Widget:halign], which determines how the
   --  labels size allocation is positioned in the space available for the
   --  label.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  The vertical alignment of the label text inside its size allocation.
   --
   --  Compare this to [propertyGtk.Widget:valign], which determines how the
   --  labels size allocation is positioned in the space available for the
   --  label.

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
   --  Gets emitted when the user activates a link in the label.
   --
   --  The `::activate-current-link` is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  Applications may also emit the signal with g_signal_emit_by_name if
   --  they need to control activation of URIs programmatically.
   --
   --  The default bindings for this signal are all forms of the
   --  <kbd>Enter</kbd> key.

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
   --  Gets emitted to activate a URI.
   --
   --  Applications may connect to it to override the default behaviour, which
   --  is to call [methodGtk.FileLauncher.launch].
   -- 
   --  Callback parameters:
   --    --  @param URI the URI that is activated

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
   --  Gets emitted to copy the selection to the clipboard.
   --
   --  The `::copy-clipboard` signal is a [keybinding
   --  signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Ctrl</kbd>+<kbd>c</kbd>.

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
   --  Gets emitted when the user initiates a cursor movement.
   --
   --  The `::move-cursor` signal is a [keybinding
   --  signal](class.SignalAction.html). If the cursor is not visible in Entry,
   --  this signal causes the viewport to be moved instead.
   --
   --  Applications should not connect to it, but may emit it with
   --  [funcGobject.signal_emit_by_name] if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the <kbd>Shift</kbd> modifier extends the selection, the variant
   --  without the <kbd>Shift</kbd> modifier does not. There are too many key
   --  combinations to list them all here.
   --
   --  - <kbd>←</kbd>, <kbd>→</kbd>, <kbd>↑</kbd>, <kbd>↓</kbd> move by
   --  individual characters/lines - <kbd>Ctrl</kbd>+<kbd>←</kbd>, etc. move by
   --  words/paragraphs - <kbd>Home</kbd> and <kbd>End</kbd> move to the ends
   --  of the buffer
   -- 
   --  Callback parameters:
   --    --  @param Step the granularity of the move, as a `GtkMovementStep`
   --    --  @param Count the number of Step units to move
   --    --  @param Extend_Selection true if the move should extend the selection

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "AccessibleHypertext"
   --
   --  - "AccessibleText"
   --
   --  - "ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Label
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Accessible_Hypertext is new Glib.Types.Implements
     (Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext
   renames Implements_Gtk_Accessible_Hypertext.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext)
   return Gtk_Label
   renames Implements_Gtk_Accessible_Hypertext.To_Object;

   package Implements_Gtk_Accessible_Text is new Glib.Types.Implements
     (Gtk.Accessible_Text.Gtk_Accessible_Text, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Accessible_Text.Gtk_Accessible_Text
   renames Implements_Gtk_Accessible_Text.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Text.Gtk_Accessible_Text)
   return Gtk_Label
   renames Implements_Gtk_Accessible_Text.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Label_Record, Gtk_Label);
   function "+"
     (Widget : access Gtk_Label_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Label
   renames Implements_Gtk_Constraint_Target.To_Object;

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
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Single_Line_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("single-line-mode");
   Selectable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("selectable");
   Natural_Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Natural_Wrap_Mode :=
     Gtk.Enums.Build ("natural-wrap-mode");
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
   Extra_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("extra-menu");
   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode :=
     Pango.Layout.Build ("ellipsize");
   Attributes_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attributes");
end Gtk.Label;
