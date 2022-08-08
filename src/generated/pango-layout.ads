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
--  The Pango.Layout.Pango_Layout structure represents an entire paragraph of
--  text. It is initialized with a Pango.Context.Pango_Context, UTF-8 string
--  and set of attributes for that string. Once that is done, the set of
--  formatted lines can be extracted from the object, the layout can be
--  rendered, and conversion between logical character positions within the
--  layout's text, and the physical position of the resulting glyphs can be
--  made.
--
--  There are also a number of parameters to adjust the formatting of a
--  Pango.Layout.Pango_Layout, which are illustrated in <xref
--  linkend="parameters"/>. It is possible, as well, to ignore the 2-D setup,
--  and simply treat the results of a Pango.Layout.Pango_Layout as a list of
--  lines.
--
--  <figure id="parameters">
--  == Adjustable parameters (on the left) and font metrics (on the right) for
--  a PangoLayout ==
--
--  <graphic fileref="layout.png" format="PNG"></graphic> </figure>
--  The Pango.Layout.Pango_Layout structure is opaque, and has no user-visible
--  fields.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Gtkada.Types;            use Gtkada.Types;
with Pango.Attributes;        use Pango.Attributes;
with Pango.Context;           use Pango.Context;
with Pango.Enums;             use Pango.Enums;
with Pango.Font;              use Pango.Font;
with Pango.Tabs;              use Pango.Tabs;

package Pango.Layout is

   type Pango_Layout_Record is new GObject_Record with null record;
   type Pango_Layout is access all Pango_Layout_Record'Class;

   type Pango_Ellipsize_Mode is (
      Ellipsize_None,
      Ellipsize_Start,
      Ellipsize_Middle,
      Ellipsize_End);
   pragma Convention (C, Pango_Ellipsize_Mode);
   --  The Pango.Layout.Pango_Ellipsize_Mode type describes what sort of (if
   --  any) ellipsization should be applied to a line of text. In the
   --  ellipsization process characters are removed from the text in order to
   --  make it fit to a given width and replaced with an ellipsis.

   type Pango_Layout_Iter is new Glib.C_Boxed with null record;
   Null_Pango_Layout_Iter : constant Pango_Layout_Iter;

   function From_Object (Object : System.Address) return Pango_Layout_Iter;
   function From_Object_Free (B : access Pango_Layout_Iter'Class) return Pango_Layout_Iter;
   pragma Inline (From_Object_Free, From_Object);

   type Pango_Layout_Line is private;
   function From_Object_Free (B : access Pango_Layout_Line) return Pango_Layout_Line;
   pragma Inline (From_Object_Free);
   --  The Pango.Layout.Pango_Layout_Line structure represents one of the
   --  lines resulting from laying out a paragraph via
   --  Pango.Layout.Pango_Layout. Pango.Layout.Pango_Layout_Line structures are
   --  obtained by calling Pango.Layout.Get_Line and are only valid until the
   --  text, attributes, or settings of the parent Pango.Layout.Pango_Layout
   --  are modified.
   --
   --  Routines for rendering PangoLayout objects are provided in code
   --  specific to each rendering system.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Pango_Ellipsize_Mode_Properties is
      new Generic_Internal_Discrete_Property (Pango_Ellipsize_Mode);
   type Property_Pango_Ellipsize_Mode is new Pango_Ellipsize_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Layout  : out Pango_Layout;
       Context : not null access Pango.Context.Pango_Context_Record'Class);
   --  Create a new Pango.Layout.Pango_Layout object with attributes
   --  initialized to default values for a particular
   --  Pango.Context.Pango_Context.
   --  "context": a Pango.Context.Pango_Context

   procedure Initialize
      (Layout  : not null access Pango_Layout_Record'Class;
       Context : not null access Pango.Context.Pango_Context_Record'Class);
   --  Create a new Pango.Layout.Pango_Layout object with attributes
   --  initialized to default values for a particular
   --  Pango.Context.Pango_Context.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "context": a Pango.Context.Pango_Context

   function Pango_Layout_New
      (Context : not null access Pango.Context.Pango_Context_Record'Class)
       return Pango_Layout;
   --  Create a new Pango.Layout.Pango_Layout object with attributes
   --  initialized to default values for a particular
   --  Pango.Context.Pango_Context.
   --  "context": a Pango.Context.Pango_Context

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_layout_get_type");

   function Get_Type_Layout_Iter return Glib.GType;
   pragma Import (C, Get_Type_Layout_Iter, "pango_layout_iter_get_type");

   function Get_Type_Layout_Line return Glib.GType;
   pragma Import (C, Get_Type_Layout_Line, "pango_layout_line_get_type");

   -------------
   -- Methods --
   -------------

   procedure Context_Changed (Layout : not null access Pango_Layout_Record);
   --  Forces recomputation of any state in the Pango.Layout.Pango_Layout that
   --  might depend on the layout's context. This function should be called if
   --  you make changes to the context subsequent to creating the layout.

   function Copy
      (Layout : not null access Pango_Layout_Record) return Pango_Layout;
   --  Does a deep copy-by-value of the Src layout. The attribute list, tab
   --  array, and text from the original layout are all copied by value.

   function Copy (Self : Pango_Layout_Iter) return Pango_Layout_Iter;
   --  Copies a Pango.Layout.Pango_Layout_Iter.
   --  Since: gtk+ 1.20

   function Get_Alignment
      (Layout : not null access Pango_Layout_Record)
       return Pango.Enums.Alignment;
   --  Gets the alignment for the layout: how partial lines are positioned
   --  within the horizontal space available.

   procedure Set_Alignment
      (Layout    : not null access Pango_Layout_Record;
       Alignment : Pango.Enums.Alignment);
   --  Sets the alignment for the layout: how partial lines are positioned
   --  within the horizontal space available.
   --  "alignment": the alignment

   function Get_Attributes
      (Layout : not null access Pango_Layout_Record)
       return Pango.Attributes.Pango_Attr_List;
   --  Gets the attribute list for the layout, if any.

   procedure Set_Attributes
      (Layout : not null access Pango_Layout_Record;
       Attrs  : Pango.Attributes.Pango_Attr_List);
   --  Sets the text attributes for a layout object. References Attrs, so the
   --  caller can unref its reference.
   --  "attrs": a Pango.Attributes.Pango_Attr_List, can be null

   function Get_Auto_Dir
      (Layout : not null access Pango_Layout_Record) return Boolean;
   --  Gets whether to calculate the bidirectional base direction for the
   --  layout according to the contents of the layout. See
   --  Pango.Layout.Set_Auto_Dir.
   --  Since: gtk+ 1.4

   procedure Set_Auto_Dir
      (Layout   : not null access Pango_Layout_Record;
       Auto_Dir : Boolean);
   --  Sets whether to calculate the bidirectional base direction for the
   --  layout according to the contents of the layout; when this flag is on
   --  (the default), then paragraphs in Layout that begin with strong
   --  right-to-left characters (Arabic and Hebrew principally), will have
   --  right-to-left layout, paragraphs with letters from other scripts will
   --  have left-to-right layout. Paragraphs with only neutral characters get
   --  their direction from the surrounding paragraphs.
   --  When False, the choice between left-to-right and right-to-left layout
   --  is done according to the base direction of the layout's
   --  Pango.Context.Pango_Context. (See Pango.Context.Set_Base_Dir).
   --  When the auto-computed direction of a paragraph differs from the base
   --  direction of the context, the interpretation of
   --  Pango.Enums.Pango_Align_Left and Pango.Enums.Pango_Align_Right are
   --  swapped.
   --  Since: gtk+ 1.4
   --  "auto_dir": if True, compute the bidirectional base direction from the
   --  layout's contents.

   function Get_Baseline
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Gets the Y position of baseline of the first line in Layout.
   --  Since: gtk+ 1.22

   function Get_Baseline (Self : Pango_Layout_Iter) return Glib.Gint;
   --  Gets the Y position of the current line's baseline, in layout
   --  coordinates (origin at top left of the entire layout).

   function Get_Character_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Returns the number of Unicode characters in the the text of Layout.
   --  Since: gtk+ 1.30

   function Get_Context
      (Layout : not null access Pango_Layout_Record)
       return Pango.Context.Pango_Context;
   --  Retrieves the Pango.Context.Pango_Context used for this layout.

   procedure Get_Cursor_Pos
      (Layout     : not null access Pango_Layout_Record;
       Index      : Glib.Gint;
       Strong_Pos : out Pango_Rectangle;
       Weak_Pos   : out Pango_Rectangle);
   --  Given an index within a layout, determines the positions that of the
   --  strong and weak cursors if the insertion point is at that index. The
   --  position of each cursor is stored as a zero-width rectangle. The strong
   --  cursor location is the location where characters of the directionality
   --  equal to the base direction of the layout are inserted. The weak cursor
   --  location is the location where characters of the directionality opposite
   --  to the base direction of the layout are inserted.
   --  "index_": the byte index of the cursor
   --  "strong_pos": location to store the strong cursor position (may be
   --  null)
   --  "weak_pos": location to store the weak cursor position (may be null)

   function Get_Ellipsize
      (Layout : not null access Pango_Layout_Record)
       return Pango_Ellipsize_Mode;
   --  Gets the type of ellipsization being performed for Layout. See
   --  Pango.Layout.Set_Ellipsize
   --  Since: gtk+ 1.6

   procedure Set_Ellipsize
      (Layout    : not null access Pango_Layout_Record;
       Ellipsize : Pango_Ellipsize_Mode);
   --  Sets the type of ellipsization being performed for Layout. Depending on
   --  the ellipsization mode Ellipsize text is removed from the start, middle,
   --  or end of text so they fit within the width and height of layout set
   --  with Pango.Layout.Set_Width and Pango.Layout.Set_Height.
   --  If the layout contains characters such as newlines that force it to be
   --  layed out in multiple paragraphs, then whether each paragraph is
   --  ellipsized separately or the entire layout is ellipsized as a whole
   --  depends on the set height of the layout. See Pango.Layout.Set_Height for
   --  details.
   --  Since: gtk+ 1.6
   --  "ellipsize": the new ellipsization mode for Layout

   procedure Get_Extents
      (Layout       : not null access Pango_Layout_Record;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Computes the logical and ink extents of Layout. Logical extents are
   --  usually what you want for positioning things. Note that both extents may
   --  have non-zero x and y. You may want to use those to offset where you
   --  render the layout. Not doing that is a very typical bug that shows up as
   --  right-to-left layouts not being correctly positioned in a layout with a
   --  set width.
   --  The extents are given in layout coordinates and in Pango units; layout
   --  coordinates begin at the top left corner of the layout.
   --  "ink_rect": rectangle used to store the extents of the layout as drawn
   --  or null to indicate that the result is not needed.
   --  "logical_rect": rectangle used to store the logical extents of the
   --  layout or null to indicate that the result is not needed.

   function Get_Font_Description
      (Layout : not null access Pango_Layout_Record)
       return Pango.Font.Pango_Font_Description;
   --  Gets the font description for the layout, if any.
   --  Since: gtk+ 1.8

   procedure Set_Font_Description
      (Layout : not null access Pango_Layout_Record;
       Desc   : Pango.Font.Pango_Font_Description);
   --  Sets the default font description for the layout. If no font
   --  description is set on the layout, the font description from the layout's
   --  context is used.
   --  "desc": the new Pango.Font.Pango_Font_Description, or null to unset the
   --  current font description

   function Get_Height
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Gets the height of layout used for ellipsization. See
   --  Pango.Layout.Set_Height for details.
   --  Since: gtk+ 1.20

   procedure Set_Height
      (Layout : not null access Pango_Layout_Record;
       Height : Glib.Gint);
   --  Sets the height to which the Pango.Layout.Pango_Layout should be
   --  ellipsized at. There are two different behaviors, based on whether
   --  Height is positive or negative.
   --  If Height is positive, it will be the maximum height of the layout.
   --  Only lines would be shown that would fit, and if there is any text
   --  omitted, an ellipsis added. At least one line is included in each
   --  paragraph regardless of how small the height value is. A value of zero
   --  will render exactly one line for the entire layout.
   --  If Height is negative, it will be the (negative of) maximum number of
   --  lines per paragraph. That is, the total number of lines shown may well
   --  be more than this value if the layout contains multiple paragraphs of
   --  text. The default value of -1 means that first line of each paragraph is
   --  ellipsized. This behvaior may be changed in the future to act per layout
   --  instead of per paragraph. File a bug against pango at <ulink
   --  url="http://bugzilla.gnome.org/">http://bugzilla.gnome.org/</ulink> if
   --  your code relies on this behavior.
   --  Height setting only has effect if a positive width is set on Layout and
   --  ellipsization mode of Layout is not Pango.Layout.Ellipsize_None. The
   --  behavior is undefined if a height other than -1 is set and ellipsization
   --  mode is set to Pango.Layout.Ellipsize_None, and may change in the
   --  future.
   --  Since: gtk+ 1.20
   --  "height": the desired height of the layout in Pango units if positive,
   --  or desired number of lines if negative.

   function Get_Indent
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Gets the paragraph indent width in Pango units. A negative value
   --  indicates a hanging indentation.

   procedure Set_Indent
      (Layout : not null access Pango_Layout_Record;
       Indent : Glib.Gint);
   --  Sets the width in Pango units to indent each paragraph. A negative
   --  value of Indent will produce a hanging indentation. That is, the first
   --  line will have the full width, and subsequent lines will be indented by
   --  the absolute value of Indent.
   --  The indent setting is ignored if layout alignment is set to
   --  Pango.Enums.Pango_Align_Center.
   --  "indent": the amount by which to indent.

   function Get_Iter
      (Layout : not null access Pango_Layout_Record'Class)
       return Pango_Layout_Iter;
   --  Returns an iterator to iterate over the visual extents of the layout.

   function Get_Justify
      (Layout : not null access Pango_Layout_Record) return Boolean;
   --  Gets whether each complete line should be stretched to fill the entire
   --  width of the layout.

   procedure Set_Justify
      (Layout  : not null access Pango_Layout_Record;
       Justify : Boolean);
   --  Sets whether each complete line should be stretched to fill the entire
   --  width of the layout. This stretching is typically done by adding
   --  whitespace, but for some scripts (such as Arabic), the justification may
   --  be done in more complex ways, like extending the characters.
   --  Note that this setting is not implemented and so is ignored in Pango
   --  older than 1.18.
   --  "justify": whether the lines in the layout should be justified.

   function Get_Line
      (Layout : not null access Pango_Layout_Record;
       Line   : Glib.Gint) return Pango_Layout_Line;
   --  Retrieves a particular line from a Pango.Layout.Pango_Layout.
   --  Use the faster Pango.Layout.Get_Line_Readonly if you do not plan to
   --  modify the contents of the line (glyphs, glyph widths, etc.).
   --  "line": the index of a line, which must be between 0 and
   --  'pango_layout_get_line_count(layout) - 1', inclusive.

   function Get_Line (Self : Pango_Layout_Iter) return Pango_Layout_Line;
   --  Gets the current line.
   --  Use the faster Pango.Layout.Get_Line_Readonly if you do not plan to
   --  modify the contents of the line (glyphs, glyph widths, etc.).

   function Get_Line_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Retrieves the count of lines for the Layout.

   function Get_Line_Readonly
      (Layout : not null access Pango_Layout_Record;
       Line   : Glib.Gint) return Pango_Layout_Line;
   --  Retrieves a particular line from a Pango.Layout.Pango_Layout.
   --  This is a faster alternative to Pango.Layout.Get_Line, but the user is
   --  not expected to modify the contents of the line (glyphs, glyph widths,
   --  etc.).
   --  Since: gtk+ 1.16
   --  "line": the index of a line, which must be between 0 and
   --  'pango_layout_get_line_count(layout) - 1', inclusive.

   function Get_Line_Readonly
      (Self : Pango_Layout_Iter) return Pango_Layout_Line;
   --  Gets the current line for read-only access.
   --  This is a faster alternative to Pango.Layout.Get_Line, but the user is
   --  not expected to modify the contents of the line (glyphs, glyph widths,
   --  etc.).
   --  Since: gtk+ 1.16

   procedure Get_Pixel_Extents
      (Layout       : not null access Pango_Layout_Record;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Computes the logical and ink extents of Layout in device units. This
   --  function just calls Pango.Layout.Get_Extents followed by two
   --  pango_extents_to_pixels calls, rounding Ink_Rect and Logical_Rect such
   --  that the rounded rectangles fully contain the unrounded one (that is,
   --  passes them as first argument to pango_extents_to_pixels).
   --  "ink_rect": rectangle used to store the extents of the layout as drawn
   --  or null to indicate that the result is not needed.
   --  "logical_rect": rectangle used to store the logical extents of the
   --  layout or null to indicate that the result is not needed.

   procedure Get_Pixel_Size
      (Layout : not null access Pango_Layout_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Determines the logical width and height of a Pango.Layout.Pango_Layout
   --  in device units. (pango_layout_get_size returns the width and height
   --  scaled by PANGO_SCALE.) This is simply a convenience function around
   --  Pango.Layout.Get_Pixel_Extents.
   --  "width": location to store the logical width, or null
   --  "height": location to store the logical height, or null

   function Get_Serial
      (Layout : not null access Pango_Layout_Record) return Guint;
   --  Returns the current serial number of Layout. The serial number is
   --  initialized to an small number larger than zero when a new layout is
   --  created and is increased whenever the layout is changed using any of the
   --  setter functions, or the Pango.Context.Pango_Context it uses has
   --  changed. The serial may wrap, but will never have the value 0. Since it
   --  can wrap, never compare it with "less than", always use "not equals".
   --  This can be used to automatically detect changes to a
   --  Pango.Layout.Pango_Layout, and is useful for example to decide whether a
   --  layout needs redrawing. To force the serial to be increased, use
   --  Pango.Layout.Context_Changed.
   --  Since: gtk+ 1.32.4

   function Get_Single_Paragraph_Mode
      (Layout : not null access Pango_Layout_Record) return Boolean;
   --  Obtains the value set by Pango.Layout.Set_Single_Paragraph_Mode.

   procedure Set_Single_Paragraph_Mode
      (Layout  : not null access Pango_Layout_Record;
       Setting : Boolean);
   --  If Setting is True, do not treat newlines and similar characters as
   --  paragraph separators; instead, keep all text in a single paragraph, and
   --  display a glyph for paragraph separator characters. Used when you want
   --  to allow editing of newlines on a single text line.
   --  "setting": new setting

   procedure Get_Size
      (Layout : not null access Pango_Layout_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Determines the logical width and height of a Pango.Layout.Pango_Layout
   --  in Pango units (device units scaled by PANGO_SCALE). This is simply a
   --  convenience function around Pango.Layout.Get_Extents.
   --  "width": location to store the logical width, or null
   --  "height": location to store the logical height, or null

   function Get_Spacing
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Gets the amount of spacing between the lines of the layout.

   procedure Set_Spacing
      (Layout  : not null access Pango_Layout_Record;
       Spacing : Glib.Gint);
   --  Sets the amount of spacing in Pango unit between the lines of the
   --  layout. When placing lines with spacing, Pango arranges things so that
   --  line2.top = line1.bottom + spacing
   --  Note: Since 1.44, Pango defaults to using the line height (as
   --  determined by the font) for placing lines. The Spacing set with this
   --  function is only taken into account when the line-height factor is set
   --  to zero with pango_layout_set_line_spacing.
   --  "spacing": the amount of spacing

   function Get_Tabs
      (Layout : not null access Pango_Layout_Record)
       return Pango.Tabs.Pango_Tab_Array;
   --  Gets the current Pango.Tabs.Pango_Tab_Array used by this layout. If no
   --  Pango.Tabs.Pango_Tab_Array has been set, then the default tabs are in
   --  use and null is returned. Default tabs are every 8 spaces. The return
   --  value should be freed with Pango.Tabs.Free.

   procedure Set_Tabs
      (Layout : not null access Pango_Layout_Record;
       Tabs   : Pango.Tabs.Pango_Tab_Array);
   --  Sets the tabs to use for Layout, overriding the default tabs (by
   --  default, tabs are every 8 spaces). If Tabs is null, the default tabs are
   --  reinstated. Tabs is copied into the layout; you must free your copy of
   --  Tabs yourself.
   --  "tabs": a Pango.Tabs.Pango_Tab_Array, or null

   function Get_Text
      (Layout : not null access Pango_Layout_Record) return UTF8_String;
   --  Gets the text in the layout. The returned text should not be freed or
   --  modified.

   procedure Set_Text
      (Layout : not null access Pango_Layout_Record;
       Text   : UTF8_String);
   --  Sets the text of the layout.
   --  This function validates Text and renders invalid UTF-8 with a
   --  placeholder glyph.
   --  Note that if you have used Pango.Layout.Set_Markup or
   --  Pango.Layout.Set_Markup_With_Accel on Layout before, you may want to
   --  call Pango.Layout.Set_Attributes to clear the attributes set on the
   --  layout from the markup as this function does not clear attributes.
   --  "text": the text

   function Get_Unknown_Glyphs_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Counts the number unknown glyphs in Layout. That is, zero if glyphs for
   --  all characters in the layout text were found, or more than zero
   --  otherwise.
   --  This function can be used to determine if there are any fonts available
   --  to render all characters in a certain string, or when used in
   --  combination with Pango.Enums.Pango_Attr_Fallback, to check if a certain
   --  font supports all the characters in the string.
   --  Since: gtk+ 1.16

   function Get_Width
      (Layout : not null access Pango_Layout_Record) return Glib.Gint;
   --  Gets the width to which the lines of the Pango.Layout.Pango_Layout
   --  should wrap.

   procedure Set_Width
      (Layout : not null access Pango_Layout_Record;
       Width  : Glib.Gint);
   --  Sets the width to which the lines of the Pango.Layout.Pango_Layout
   --  should wrap or ellipsized. The default value is -1: no width set.
   --  "width": the desired width in Pango units, or -1 to indicate that no
   --  wrapping or ellipsization should be performed.

   function Get_Wrap
      (Layout : not null access Pango_Layout_Record)
       return Pango.Enums.Wrap_Mode;
   --  Gets the wrap mode for the layout.
   --  Use Pango.Layout.Is_Wrapped to query whether any paragraphs were
   --  actually wrapped.

   procedure Set_Wrap
      (Layout : not null access Pango_Layout_Record;
       Wrap   : Pango.Enums.Wrap_Mode);
   --  Sets the wrap mode; the wrap mode only has effect if a width is set on
   --  the layout with Pango.Layout.Set_Width. To turn off wrapping, set the
   --  width to -1.
   --  "wrap": the wrap mode

   procedure Index_To_Line_X
      (Layout   : not null access Pango_Layout_Record;
       Index    : Glib.Gint;
       Trailing : Boolean;
       Line     : out Glib.Gint;
       X_Pos    : out Glib.Gint);
   --  Converts from byte Index_ within the Layout to line and X position. (X
   --  position is measured from the left edge of the line)
   --  "index_": the byte index of a grapheme within the layout.
   --  "trailing": an integer indicating the edge of the grapheme to retrieve
   --  the position of. If > 0, the trailing edge of the grapheme, if 0, the
   --  leading of the grapheme.
   --  "line": location to store resulting line index. (which will between 0
   --  and pango_layout_get_line_count(layout) - 1), or null
   --  "x_pos": location to store resulting position within line (PANGO_SCALE
   --  units per device unit), or null

   procedure Index_To_Pos
      (Layout : not null access Pango_Layout_Record;
       Index  : Glib.Gint;
       Pos    : out Pango_Rectangle);
   --  Converts from an index within a Pango.Layout.Pango_Layout to the
   --  onscreen position corresponding to the grapheme at that index, which is
   --  represented as rectangle. Note that 'pos->x' is always the leading edge
   --  of the grapheme and 'pos->x + pos->width' the trailing edge of the
   --  grapheme. If the directionality of the grapheme is right-to-left, then
   --  'pos->width' will be negative.
   --  "index_": byte index within Layout
   --  "pos": rectangle in which to store the position of the grapheme

   function Is_Ellipsized
      (Layout : not null access Pango_Layout_Record) return Boolean;
   --  Queries whether the layout had to ellipsize any paragraphs.
   --  This returns True if the ellipsization mode for Layout is not
   --  Pango.Layout.Ellipsize_None, a positive width is set on Layout, and
   --  there are paragraphs exceeding that width that have to be ellipsized.
   --  Since: gtk+ 1.16

   function Is_Wrapped
      (Layout : not null access Pango_Layout_Record) return Boolean;
   --  Queries whether the layout had to wrap any paragraphs.
   --  This returns True if a positive width is set on Layout, ellipsization
   --  mode of Layout is set to Pango.Layout.Ellipsize_None, and there are
   --  paragraphs exceeding the layout width that have to be wrapped.
   --  Since: gtk+ 1.16

   procedure Move_Cursor_Visually
      (Layout       : not null access Pango_Layout_Record;
       Strong       : Boolean;
       Old_Index    : Glib.Gint;
       Old_Trailing : Glib.Gint;
       Direction    : Glib.Gint;
       New_Index    : out Glib.Gint;
       New_Trailing : out Glib.Gint);
   --  Computes a new cursor position from an old position and a count of
   --  positions to move visually. If Direction is positive, then the new
   --  strong cursor position will be one position to the right of the old
   --  cursor position. If Direction is negative, then the new strong cursor
   --  position will be one position to the left of the old cursor position.
   --  In the presence of bidirectional text, the correspondence between
   --  logical and visual order will depend on the direction of the current
   --  run, and there may be jumps when the cursor is moved off of the end of a
   --  run.
   --  Motion here is in cursor positions, not in characters, so a single call
   --  to Pango.Layout.Move_Cursor_Visually may move the cursor over multiple
   --  characters when multiple characters combine to form a single grapheme.
   --  "strong": whether the moving cursor is the strong cursor or the weak
   --  cursor. The strong cursor is the cursor corresponding to text insertion
   --  in the base direction for the layout.
   --  "old_index": the byte index of the grapheme for the old index
   --  "old_trailing": if 0, the cursor was at the leading edge of the
   --  grapheme indicated by Old_Index, if > 0, the cursor was at the trailing
   --  edge.
   --  "direction": direction to move cursor. A negative value indicates
   --  motion to the left.
   --  "new_index": location to store the new cursor byte index. A value of -1
   --  indicates that the cursor has been moved off the beginning of the
   --  layout. A value of G_MAXINT indicates that the cursor has been moved off
   --  the end of the layout.
   --  "new_trailing": number of characters to move forward from the location
   --  returned for New_Index to get the position where the cursor should be
   --  displayed. This allows distinguishing the position at the beginning of
   --  one line from the position at the end of the preceding line. New_Index
   --  is always on the line where the cursor should be displayed.

   procedure Set_Markup
      (Layout : not null access Pango_Layout_Record;
       Markup : UTF8_String);
   --  Same as Pango.Layout.Set_Markup_With_Accel, but the markup text isn't
   --  scanned for accelerators.
   --  "markup": marked-up text

   procedure Set_Markup_With_Accel
      (Layout       : not null access Pango_Layout_Record;
       Markup       : UTF8_String;
       Length       : Glib.Gint;
       Accel_Marker : Gunichar;
       Accel_Char   : out Gunichar);
   --  Sets the layout text and attribute list from marked-up text (see <link
   --  linkend="PangoMarkupFormat">markup format</link>). Replaces the current
   --  text and attribute list.
   --  If Accel_Marker is nonzero, the given character will mark the character
   --  following it as an accelerator. For example, Accel_Marker might be an
   --  ampersand or underscore. All characters marked as an accelerator will
   --  receive a Pango.Enums.Pango_Underline_Low attribute, and the first
   --  character so marked will be returned in Accel_Char. Two Accel_Marker
   --  characters following each other produce a single literal Accel_Marker
   --  character.
   --  "markup": marked-up text (see <link linkend="PangoMarkupFormat">markup
   --  format</link>)
   --  "length": length of marked-up text in bytes, or -1 if Markup is
   --  null-terminated
   --  "accel_marker": marker for accelerators in the text
   --  "accel_char": return location for first located accelerator, or null

   procedure Xy_To_Index
      (Layout   : not null access Pango_Layout_Record;
       X        : Glib.Gint;
       Y        : Glib.Gint;
       Index    : out Glib.Gint;
       Trailing : out Glib.Gint;
       Exact    : out Boolean);
   --  Converts from X and Y position within a layout to the byte index to the
   --  character at that logical position. If the Y position is not inside the
   --  layout, the closest position is chosen (the position will be clamped
   --  inside the layout). If the X position is not within the layout, then the
   --  start or the end of the line is chosen as described for
   --  pango_layout_line_x_to_index. If either the X or Y positions were not
   --  inside the layout, then the function returns False; on an exact hit, it
   --  returns True.
   --  "x": the X offset (in Pango units) from the left edge of the layout.
   --  "y": the Y offset (in Pango units) from the top edge of the layout
   --  "index_": location to store calculated byte index
   --  "trailing": location to store a integer indicating where in the
   --  grapheme the user clicked. It will either be zero, or the number of
   --  characters in the grapheme. 0 represents the leading edge of the
   --  grapheme.

   function At_Last_Line (Self : Pango_Layout_Iter) return Boolean;
   --  Determines whether Iter is on the last line of the layout.

   procedure Free (Self : Pango_Layout_Iter);
   --  Frees an iterator that's no longer in use.

   procedure Get_Char_Extents
      (Self         : Pango_Layout_Iter;
       Logical_Rect : out Pango_Rectangle);
   --  Gets the extents of the current character, in layout coordinates
   --  (origin is the top left of the entire layout). Only logical extents can
   --  sensibly be obtained for characters; ink extents make sense only down to
   --  the level of clusters.
   --  "logical_rect": rectangle to fill with logical extents

   procedure Get_Cluster_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Gets the extents of the current cluster, in layout coordinates (origin
   --  is the top left of the entire layout).
   --  "ink_rect": rectangle to fill with ink extents, or null
   --  "logical_rect": rectangle to fill with logical extents, or null

   function Get_Index (Self : Pango_Layout_Iter) return Glib.Gint;
   --  Gets the current byte index. Note that iterating forward by char moves
   --  in visual order, not logical order, so indexes may not be sequential.
   --  Also, the index may be equal to the length of the text in the layout, if
   --  on the null run (see pango_layout_iter_get_run).

   function Get_Layout (Self : Pango_Layout_Iter) return Pango_Layout;
   --  Gets the layout associated with a Pango.Layout.Pango_Layout_Iter.
   --  Since: gtk+ 1.20

   procedure Get_Layout_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Obtains the extents of the Pango.Layout.Pango_Layout being iterated
   --  over. Ink_Rect or Logical_Rect can be null if you aren't interested in
   --  them.
   --  "ink_rect": rectangle to fill with ink extents, or null
   --  "logical_rect": rectangle to fill with logical extents, or null

   procedure Get_Line_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Obtains the extents of the current line. Ink_Rect or Logical_Rect can
   --  be null if you aren't interested in them. Extents are in layout
   --  coordinates (origin is the top-left corner of the entire
   --  Pango.Layout.Pango_Layout). Thus the extents returned by this function
   --  will be the same width/height but not at the same x/y as the extents
   --  returned from pango_layout_line_get_extents.
   --  "ink_rect": rectangle to fill with ink extents, or null
   --  "logical_rect": rectangle to fill with logical extents, or null

   procedure Get_Line_Yrange
      (Self : Pango_Layout_Iter;
       Y0   : out Glib.Gint;
       Y1   : out Glib.Gint);
   --  Divides the vertical space in the Pango.Layout.Pango_Layout being
   --  iterated over between the lines in the layout, and returns the space
   --  belonging to the current line. A line's range includes the line's
   --  logical extents, plus half of the spacing above and below the line, if
   --  Pango.Layout.Set_Spacing has been called to set layout spacing. The Y
   --  positions are in layout coordinates (origin at top left of the entire
   --  layout).
   --  Note: Since 1.44, Pango uses line heights for placing lines, and there
   --  may be gaps between the ranges returned by this function.
   --  "y0_": start of line, or null
   --  "y1_": end of line, or null

   procedure Get_Run_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle);
   --  Gets the extents of the current run in layout coordinates (origin is
   --  the top left of the entire layout).
   --  "ink_rect": rectangle to fill with ink extents, or null
   --  "logical_rect": rectangle to fill with logical extents, or null

   function Next_Char (Self : Pango_Layout_Iter) return Boolean;
   --  Moves Iter forward to the next character in visual order. If Iter was
   --  already at the end of the layout, returns False.

   function Next_Cluster (Self : Pango_Layout_Iter) return Boolean;
   --  Moves Iter forward to the next cluster in visual order. If Iter was
   --  already at the end of the layout, returns False.

   function Next_Line (Self : Pango_Layout_Iter) return Boolean;
   --  Moves Iter forward to the start of the next line. If Iter is already on
   --  the last line, returns False.

   function Next_Run (Self : Pango_Layout_Iter) return Boolean;
   --  Moves Iter forward to the next run in visual order. If Iter was already
   --  at the end of the layout, returns False.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Text (Layout : access Pango_Layout_Record)
   return Gtkada.Types.Chars_Ptr;
   --  Same a Get_Text, but return directly the C string, which is more
   --  efficient. The returned value should not be freed or modified.

private
type Pango_Layout_Line is record
   Layout : System.Address;
   Start_Index : Glib.Gint := 0;
   Length : Glib.Gint := 0;
   Runs : System.Address := System.Null_Address;
   Is_Paragraph_Start : Guint;
   Resolved_Dir : Guint;
end record;
pragma Convention (C, Pango_Layout_Line);


   Null_Pango_Layout_Iter : constant Pango_Layout_Iter := (Glib.C_Boxed with null record);

end Pango.Layout;
