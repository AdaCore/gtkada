------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
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

--  Can be applied to text contained in a `GtkTextBuffer`.
--
--  You may wish to begin by reading the [text widget conceptual
--  overview](section-text-widget.html), which gives an overview of all the
--  objects and data types related to the text widget and how they work
--  together.
--
--  Tags should be in the [classGtk.TextTagTable] for a given `GtkTextBuffer`
--  before using them with that buffer.
--
--  [methodGtk.TextBuffer.create_tag] is the best way to create tags. See
--  "gtk4-demo" for numerous examples.
--
--  For each property of `GtkTextTag`, there is a "set" property, e.g.
--  "font-set" corresponds to "font". These "set" properties reflect whether a
--  property has been set or not.
--
--  They are maintained by GTK and you should not set them independently.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Glib.GSlist;     use Glib.GSlist;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Enums;       use Gtk.Enums;
with Pango.Enums;     use Pango.Enums;
with Pango.Font;      use Pango.Font;

package Gtk.Text_Tag is

   type Gtk_Text_Tag_Record is new GObject_Record with null record;
   type Gtk_Text_Tag is access all Gtk_Text_Tag_Record'Class;

   function Convert (R : Gtk.Text_Tag.Gtk_Text_Tag) return System.Address;
   function Convert (R : System.Address) return Gtk.Text_Tag.Gtk_Text_Tag;
   package Text_Tag_List is new Generic_SList (Gtk.Text_Tag.Gtk_Text_Tag);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Tag : out Gtk_Text_Tag; Name : UTF8_String := "");
   procedure Initialize
      (Tag  : not null access Gtk_Text_Tag_Record'Class;
       Name : UTF8_String := "");
   --  Creates a `GtkTextTag`.
   --  Newly created tags must be added to the tags table for the buffer you
   --  intend to use them in, as in: "Gtk.Text_Tag_Table.Add (Get_Tag_Table
   --  (Buffer), Tag)". See also Gtk.Text_Buffer.Create_Tag which is a more
   --  convenient way of creating a tag.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Name tag name

   function Gtk_Text_Tag_New (Name : UTF8_String := "") return Gtk_Text_Tag;
   --  Creates a `GtkTextTag`.
   --  Newly created tags must be added to the tags table for the buffer you
   --  intend to use them in, as in: "Gtk.Text_Tag_Table.Add (Get_Tag_Table
   --  (Buffer), Tag)". See also Gtk.Text_Buffer.Create_Tag which is a more
   --  convenient way of creating a tag.
   --  @param Name tag name

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_text_tag_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed
      (Tag          : not null access Gtk_Text_Tag_Record;
       Size_Changed : Boolean);
   --  Emits the [signalGtk.TextTagTable::tag-changed] signal on the
   --  `GtkTextTagTable` where the tag is included.
   --  The signal is already emitted when setting a `GtkTextTag` property.
   --  This function is useful for a `GtkTextTag` subclass.
   --  @param Size_Changed whether the change affects the `GtkTextView` layout

   function Get_Priority
      (Tag : not null access Gtk_Text_Tag_Record) return Glib.Gint;
   --  Get the tag priority.
   --  @return The tag's priority.

   procedure Set_Priority
      (Tag      : not null access Gtk_Text_Tag_Record;
       Priority : Glib.Gint);
   --  Sets the priority of a `GtkTextTag`.
   --  Valid priorities start at 0 and go to one less than
   --  [methodGtk.TextTagTable.get_size]. Each tag in a table has a unique
   --  priority; setting the priority of one tag shifts the priorities of all
   --  the other tags in the table to maintain a unique priority for each tag.
   --  Higher priority tags "win" if two tags both set the same text
   --  attribute. When adding a tag to a tag table, it will be assigned the
   --  highest priority in the table by default; so normally the precedence of
   --  a set of tags is the order in which they were added to the table, or
   --  created with [methodGtk.TextBuffer.create_tag], which adds the tag to
   --  the buffer's table automatically.
   --  @param Priority the new priority

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the margins accumulate or override each other.
   --
   --  When set to True the margins of this tag are added to the margins of
   --  any other non-accumulative margins present. When set to False the
   --  margins override one another (the default).

   Allow_Breaks_Property : constant Glib.Properties.Property_Boolean;
   --  Whether breaks are allowed.

   Allow_Breaks_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `allow-breaks` property is set.

   Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  Background color as a string.

   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the background color fills the entire line height or only the
   --  height of the tagged characters.

   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `background-full-height` property is set.

   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Background color as a `GdkRGBA`.

   Background_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `background` property is set.

   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction;
   --  Text direction, e.g. right-to-left or left-to-right.

   Editable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the text can be modified by the user.

   Editable_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `editable` property is set.

   Fallback_Property : constant Glib.Properties.Property_Boolean;
   --  Whether font fallback is enabled.
   --
   --  When set to True, other fonts will be substituted where the current
   --  font is missing glyphs.

   Fallback_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `fallback` property is set.

   Family_Property : constant Glib.Properties.Property_String;
   --  Name of the font family, e.g. Sans, Helvetica, Times, Monospace.

   Family_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `family` property is set.

   Font_Property : constant Glib.Properties.Property_String;
   --  Font description as string, e.g. \"Sans Italic 12\".
   --
   --  Note that the initial value of this property depends on the internals
   --  of `PangoFontDescription`.

   Font_Desc_Property : constant Pango.Font.Property_Font_Description;
   --  Type: Pango.Font.Pango_Font_Description
   --  Font description as a `PangoFontDescription`.

   Font_Features_Property : constant Glib.Properties.Property_String;
   --  OpenType font features, as a string.

   Font_Features_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `font-features` property is set.

   Foreground_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  Foreground color as a string.

   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Foreground color as a `GdkRGBA`.

   Foreground_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `foreground` property is set.

   Indent_Property : constant Glib.Properties.Property_Int;
   --  Amount to indent the paragraph, in pixels.
   --
   --  A negative value of indent will produce a hanging indentation. That is,
   --  the first line will have the full width, and subsequent lines will be
   --  indented by the absolute value of indent.

   Indent_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `indent` property is set.

   Insert_Hyphens_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to insert hyphens at breaks.

   Insert_Hyphens_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `insert-hyphens` property is set.

   Invisible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this text is hidden.
   --
   --  Note that there may still be problems with the support for invisible
   --  text, in particular when navigating programmatically inside a buffer
   --  containing invisible segments.

   Invisible_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `invisible` property is set.

   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification;
   --  Left, right, or center justification.

   Justification_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `justification` property is set.

   Language_Property : constant Glib.Properties.Property_String;
   --  The language this text is in, as an ISO code.
   --
   --  Pango can use this as a hint when rendering the text. If not set, an
   --  appropriate default will be used.
   --
   --  Note that the initial value of this property depends on the current
   --  locale, see also [funcGtk.get_default_language].

   Language_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `language` property is set.

   Left_Margin_Property : constant Glib.Properties.Property_Int;
   --  Width of the left margin in pixels.

   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `left-margin` property is set.

   Letter_Spacing_Property : constant Glib.Properties.Property_Int;
   --  Extra spacing between graphemes, in Pango units.

   Letter_Spacing_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `letter-spacing` property is set.

   Line_Height_Property : constant Glib.Properties.Property_Float;
   --  Factor to scale line height by.

   Line_Height_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `line-height` property is set.

   Name_Property : constant Glib.Properties.Property_String;
   --  The name used to refer to the tag.
   --
   --  null for anonymous tags.

   Overline_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Overline
   --  Style of overline for this text.

   Overline_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  This property modifies the color of overlines.
   --
   --  If not set, overlines will use the foreground color.

   Overline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `overline-rgba` property is set.

   Overline_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `overline` property is set.

   Paragraph_Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  The paragraph background color as a string.

   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The paragraph background color as a `GdkRGBA`.

   Paragraph_Background_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `paragraph-background` property is set.

   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space above paragraphs.

   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `pixels-above-lines` property is set.

   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space below paragraphs.

   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `pixels-below-lines` property is set.

   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int;
   --  Pixels of blank space between wrapped lines in a paragraph.

   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `pixels-inside-wrap` property is set.

   Right_Margin_Property : constant Glib.Properties.Property_Int;
   --  Width of the right margin, in pixels.

   Right_Margin_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `right-margin` property is set.

   Rise_Property : constant Glib.Properties.Property_Int;
   --  Offset of text above the baseline, in Pango units.
   --
   --  Negative values go below the baseline.

   Rise_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `rise` property is set.

   Scale_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  Font size as a scale factor relative to the default font size.
   --
   --  This properly adapts to theme changes, etc. so is recommended. Pango
   --  predefines some scales such as PANGO_SCALE_X_LARGE.

   Scale_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `scale` property is set.

   Sentence_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this tag represents a single sentence.
   --
   --  This affects cursor movement.

   Sentence_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `sentence` property is set.

   Show_Spaces_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Show_Flags
   --  How to render invisible characters.

   Show_Spaces_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `show-spaces` property is set.

   Size_Property : constant Glib.Properties.Property_Int;
   --  Font size in Pango units.

   Size_Points_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  Font size in points.

   Size_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `size` property is set.

   Stretch_Property : constant Pango.Enums.Property_Stretch;
   --  Type: Pango.Enums.Stretch
   --  Font stretch as a `PangoStretch`, e.g.
   --  Pango.Enums.Pango_Stretch_Condensed.

   Stretch_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `stretch` property is set.

   Strikethrough_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to strike through the text.

   Strikethrough_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  This property modifies the color of strikeouts.
   --
   --  If not set, strikeouts will use the foreground color.

   Strikethrough_Rgba_Set_Property : constant Glib.Properties.Property_Boolean;
   --  If the `strikethrough-rgba` property has been set.

   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `strikethrough` property is set.

   Style_Property : constant Pango.Enums.Property_Style;
   --  Type: Pango.Enums.Style
   --  Font style as a `PangoStyle`, e.g. Pango.Enums.Pango_Style_Italic.

   Style_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `style` property is set.

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array
   --  Custom tabs for this text.

   Tabs_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `tabs` property is set.

   Text_Transform_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Text_Transform
   --  How to transform the text for display.

   Text_Transform_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `text-transform` property is set.

   Underline_Property : constant Pango.Enums.Property_Underline;
   --  Type: Pango.Enums.Underline
   --  Style of underline for this text.

   Underline_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  This property modifies the color of underlines.
   --
   --  If not set, underlines will use the foreground color.
   --
   --  If [propertyGtk.TextTag:underline] is set to
   --  Pango.Enums.Pango_Underline_Error, an alternate color may be applied
   --  instead of the foreground. Setting this property will always override
   --  those defaults.

   Underline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean;
   --  If the `underline-rgba` property has been set.

   Underline_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `underline` property is set.

   Variant_Property : constant Pango.Enums.Property_Variant;
   --  Type: Pango.Enums.Variant
   --  Font variant as a `PangoVariant`, e.g.
   --  Pango.Enums.Pango_Variant_Small_Caps.

   Variant_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `variant` property is set.

   Weight_Property : constant Glib.Properties.Property_Int;
   --  Font weight as an integer.

   Weight_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `weight` property is set.

   Word_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this tag represents a single word.
   --
   --  This affects line breaks and cursor movement.

   Word_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `word` property is set.

   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode;
   --  Whether to wrap lines never, at word boundaries, or at character
   --  boundaries.

   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the `wrap-mode` property is set.

private
   Wrap_Mode_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-mode-set");
   Wrap_Mode_Property : constant Gtk.Enums.Property_Gtk_Wrap_Mode :=
     Gtk.Enums.Build ("wrap-mode");
   Word_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("word-set");
   Word_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("word");
   Weight_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("weight-set");
   Weight_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("weight");
   Variant_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("variant-set");
   Variant_Property : constant Pango.Enums.Property_Variant :=
     Pango.Enums.Build ("variant");
   Underline_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline-set");
   Underline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("underline-rgba-set");
   Underline_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("underline-rgba");
   Underline_Property : constant Pango.Enums.Property_Underline :=
     Pango.Enums.Build ("underline");
   Text_Transform_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("text-transform-set");
   Text_Transform_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("text-transform");
   Tabs_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tabs-set");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Style_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("style-set");
   Style_Property : constant Pango.Enums.Property_Style :=
     Pango.Enums.Build ("style");
   Strikethrough_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-set");
   Strikethrough_Rgba_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough-rgba-set");
   Strikethrough_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("strikethrough-rgba");
   Strikethrough_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("strikethrough");
   Stretch_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("stretch-set");
   Stretch_Property : constant Pango.Enums.Property_Stretch :=
     Pango.Enums.Build ("stretch");
   Size_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("size-set");
   Size_Points_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("size-points");
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Show_Spaces_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-spaces-set");
   Show_Spaces_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("show-spaces");
   Sentence_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sentence-set");
   Sentence_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sentence");
   Scale_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("scale-set");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Rise_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("rise-set");
   Rise_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("rise");
   Right_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("right-margin-set");
   Right_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("right-margin");
   Pixels_Inside_Wrap_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-inside-wrap-set");
   Pixels_Inside_Wrap_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-inside-wrap");
   Pixels_Below_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-below-lines-set");
   Pixels_Below_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-below-lines");
   Pixels_Above_Lines_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("pixels-above-lines-set");
   Pixels_Above_Lines_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixels-above-lines");
   Paragraph_Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("paragraph-background-set");
   Paragraph_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("paragraph-background-rgba");
   Paragraph_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("paragraph-background");
   Overline_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overline-set");
   Overline_Rgba_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overline-rgba-set");
   Overline_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("overline-rgba");
   Overline_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("overline");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Line_Height_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("line-height-set");
   Line_Height_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("line-height");
   Letter_Spacing_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("letter-spacing-set");
   Letter_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("letter-spacing");
   Left_Margin_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("left-margin-set");
   Left_Margin_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("left-margin");
   Language_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("language-set");
   Language_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("language");
   Justification_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("justification-set");
   Justification_Property : constant Gtk.Enums.Property_Gtk_Justification :=
     Gtk.Enums.Build ("justification");
   Invisible_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-set");
   Invisible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible");
   Insert_Hyphens_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("insert-hyphens-set");
   Insert_Hyphens_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("insert-hyphens");
   Indent_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("indent-set");
   Indent_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indent");
   Foreground_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("foreground-set");
   Foreground_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("foreground-rgba");
   Foreground_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("foreground");
   Font_Features_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("font-features-set");
   Font_Features_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font-features");
   Font_Desc_Property : constant Pango.Font.Property_Font_Description :=
     Pango.Font.Build ("font-desc");
   Font_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("font");
   Family_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("family-set");
   Family_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("family");
   Fallback_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fallback-set");
   Fallback_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("fallback");
   Editable_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable-set");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Direction_Property : constant Gtk.Enums.Property_Gtk_Text_Direction :=
     Gtk.Enums.Build ("direction");
   Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-set");
   Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("background-rgba");
   Background_Full_Height_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height-set");
   Background_Full_Height_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("background-full-height");
   Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("background");
   Allow_Breaks_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-breaks-set");
   Allow_Breaks_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("allow-breaks");
   Accumulative_Margin_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("accumulative-margin");
end Gtk.Text_Tag;
