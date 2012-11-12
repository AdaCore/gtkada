------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  The Gtk.GEntry.Gtk_Entry widget is a single line text entry widget. A
--  fairly large set of key bindings are supported by default. If the entered
--  text is longer than the allocation of the widget, the widget will scroll so
--  that the cursor position is visible.
--
--  When using an entry for passwords and other sensitive information, it can
--  be put into "password mode" using Gtk.GEntry.Set_Visibility. In this mode,
--  entered text is displayed using a 'invisible' character. By default, GTK+
--  picks the best invisible character that is available in the current font,
--  but it can be changed with Gtk.GEntry.Set_Invisible_Char. Since 2.16, GTK+
--  displays a warning when Caps Lock or input methods might interfere with
--  entering text in a password entry. The warning can be turned off with the
--  Gtk.GEntry.Gtk_Entry:caps-lock-warning property.
--
--  Since 2.16, GtkEntry has the ability to display progress or activity
--  information behind the text. To make an entry display such information, use
--  Gtk.GEntry.Set_Progress_Fraction or Gtk.GEntry.Set_Progress_Pulse_Step.
--
--  Additionally, GtkEntry can show icons at either side of the entry. These
--  icons can be activatable by clicking, can be set up as drag source and can
--  have tooltips. To add an icon, use Gtk.GEntry.Set_Icon_From_Gicon or one of
--  the various other functions that set an icon from a stock id, an icon name
--  or a pixbuf. To trigger an action when the user clicks an icon, connect to
--  the Gtk.GEntry.Gtk_Entry::icon-press signal. To allow DND operations from
--  an icon, use Gtk.GEntry.Set_Icon_Drag_Source. To set a tooltip on an icon,
--  use Gtk.GEntry.Set_Icon_Tooltip_Text or the corresponding function for
--  markup.
--
--  Note that functionality or information that is only available by clicking
--  on an icon in an entry may not be accessible at all to users which are not
--  able to use a mouse or other pointing device. It is therefore recommended
--  that any such functionality should also be available by other means, e.g.
--  via the context menu of the entry.
--
--  </description>
--  <description>
--  A Gtk_Entry is a single line text editing widget. The text is
--  automatically scrolled if it is longer than can be displayed on the screen,
--  so that the cursor position is visible at all times.
--
--  See Gtk_Text_View for a multiple-line text editing widget.
--
--  </description>
--  <screenshot>gtk-gentry</screenshot>
--  <group>Numeric/Text Data Entry</group>
--  <testgtk>create_entry.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Drag_Contexts;       use Gdk.Drag_Contexts;
with Gdk.Event;               use Gdk.Event;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Cell_Editable;       use Gtk.Cell_Editable;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Entry_Buffer;        use Gtk.Entry_Buffer;
with Gtk.Entry_Completion;    use Gtk.Entry_Completion;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Image;               use Gtk.Image;
with Gtk.Style;               use Gtk.Style;
with Gtk.Target_List;         use Gtk.Target_List;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Layout;            use Pango.Layout;

package Gtk.GEntry is

   type Gtk_Entry_Record is new Gtk_Widget_Record with null record;
   type Gtk_Entry is access all Gtk_Entry_Record'Class;

   type Gtk_Entry_Icon_Position is (
      Gtk_Entry_Icon_Primary,
      Gtk_Entry_Icon_Secondary);
   pragma Convention (C, Gtk_Entry_Icon_Position);
   --  Specifies the side of the entry at which an icon is placed.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Entry_Icon_Position_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Entry_Icon_Position);
   type Property_Gtk_Entry_Icon_Position is new Gtk_Entry_Icon_Position_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (The_Entry : out Gtk_Entry);
   procedure Initialize (The_Entry : not null access Gtk_Entry_Record'Class);
   --  Creates a new entry.

   procedure Gtk_New_With_Buffer
      (The_Entry : out Gtk_Entry;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
      ;
   procedure Initialize_With_Buffer
      (The_Entry : not null access Gtk_Entry_Record'Class;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
      ;
   --  Creates a new entry with the specified text buffer.
   --  Since: gtk+ 2.18
   --  "buffer": The buffer to use for the new Gtk.GEntry.Gtk_Entry.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_entry_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Activates_Default
      (The_Entry : not null access Gtk_Entry_Record) return Boolean;
   --  Retrieves the value set by Gtk.GEntry.Set_Activates_Default.

   procedure Set_Activates_Default
      (The_Entry : not null access Gtk_Entry_Record;
       Setting   : Boolean);
   --  If Setting is True, pressing Enter in the Entry will activate the
   --  default widget for the window containing the entry. This usually means
   --  that the dialog box containing the entry will be closed, since the
   --  default widget is usually one of the dialog buttons.
   --  (For experts: if Setting is True, the entry calls
   --  Gtk.Window.Activate_Default on the window containing the entry, in the
   --  default handler for the Gtk.Widget.Gtk_Widget::activate signal.)
   --  "setting": True to activate window's default widget on Enter keypress

   function Get_Alignment
      (The_Entry : not null access Gtk_Entry_Record) return Gfloat;
   --  Gets the value set by Gtk.GEntry.Set_Alignment.
   --  Since: gtk+ 2.4

   procedure Set_Alignment
      (The_Entry : not null access Gtk_Entry_Record;
       Xalign    : Gfloat);
   --  Sets the alignment for the contents of the entry. This controls the
   --  horizontal positioning of the contents when the displayed text is
   --  shorter than the width of the entry.
   --  Since: gtk+ 2.4
   --  "xalign": The horizontal alignment, from 0 (left) to 1 (right).
   --  Reversed for RTL layouts

   function Get_Buffer
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Entry_Buffer.Gtk_Entry_Buffer;
   --  Get the Gtk.Entry_Buffer.Gtk_Entry_Buffer object which holds the text
   --  for this widget.
   --  Since: gtk+ 2.18

   procedure Set_Buffer
      (The_Entry : not null access Gtk_Entry_Record;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
      ;
   --  Set the Gtk.Entry_Buffer.Gtk_Entry_Buffer object which holds the text
   --  for this widget.
   --  Since: gtk+ 2.18
   --  "buffer": a Gtk.Entry_Buffer.Gtk_Entry_Buffer

   function Get_Completion
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Entry_Completion.Gtk_Entry_Completion;
   --  Returns the auxiliary completion object currently in use by Entry.
   --  in use by Entry.
   --  Since: gtk+ 2.4

   procedure Set_Completion
      (The_Entry  : not null access Gtk_Entry_Record;
       Completion : access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class)
      ;
   --  Sets Completion to be the auxiliary completion object to use with
   --  Entry. All further configuration of the completion mechanism is done on
   --  Completion using the Gtk.Entry_Completion.Gtk_Entry_Completion API.
   --  Completion is disabled if Completion is set to null.
   --  Since: gtk+ 2.4
   --  "completion": The Gtk.Entry_Completion.Gtk_Entry_Completion or null

   function Get_Current_Icon_Drag_Source
      (The_Entry : not null access Gtk_Entry_Record) return Gint;
   --  Returns the index of the icon which is the source of the current DND
   --  operation, or -1.
   --  This function is meant to be used in a
   --  Gtk.Widget.Gtk_Widget::drag-data-get callback.
   --  DND operation, or -1.
   --  Since: gtk+ 2.16

   function Get_Cursor_Hadjustment
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Adjustment.Gtk_Adjustment;
   --  Retrieves the horizontal cursor adjustment for the entry. See
   --  Gtk.GEntry.Set_Cursor_Hadjustment.
   --  if none has been set.
   --  Since: gtk+ 2.12

   procedure Set_Cursor_Hadjustment
      (The_Entry  : not null access Gtk_Entry_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
      ;
   --  Hooks up an adjustment to the cursor position in an entry, so that when
   --  the cursor is moved, the adjustment is scrolled to show that position.
   --  See Gtk.Scrolled_Window.Get_Hadjustment for a typical way of obtaining
   --  the adjustment.
   --  The adjustment has to be in pixel units and in the same coordinate
   --  system as the entry.
   --  Since: gtk+ 2.12
   --  "adjustment": an adjustment which should be adjusted when the cursor is
   --  moved, or null

   function Get_Has_Frame
      (The_Entry : not null access Gtk_Entry_Record) return Boolean;
   --  Gets the value set by Gtk.GEntry.Set_Has_Frame.

   procedure Set_Has_Frame
      (The_Entry : not null access Gtk_Entry_Record;
       Setting   : Boolean := True);
   --  Sets whether the entry has a beveled frame around it.
   --  "setting": new value

   function Get_Icon_Activatable
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Boolean;
   --  Returns whether the icon is activatable.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   procedure Set_Icon_Activatable
      (The_Entry   : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk_Entry_Icon_Position;
       Activatable : Boolean);
   --  Sets whether the icon is activatable.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position
   --  "activatable": True if the icon should be activatable

   procedure Get_Icon_Area
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets the area where entry's icon at Icon_Pos is drawn. This function is
   --  useful when drawing something to the entry in a draw callback.
   --  If the entry is not realized or has no icon at the given position,
   --  Icon_Area is filled with zeros.
   --  See also Gtk.GEntry.Get_Text_Area
   --  Since: gtk+ 3.0
   --  "icon_pos": Icon position
   --  "icon_area": Return location for the icon's area

   function Get_Icon_At_Pos
      (The_Entry : not null access Gtk_Entry_Record;
       X         : Gint;
       Y         : Gint) return Gint;
   --  Finds the icon at the given position and return its index. The
   --  position's coordinates are relative to the Entry's top left corner. If
   --  X, Y doesn't lie inside an icon, -1 is returned. This function is
   --  intended for use in a Gtk.Widget.Gtk_Widget::query-tooltip signal
   --  handler.
   --  Since: gtk+ 2.16
   --  "x": the x coordinate of the position to find
   --  "y": the y coordinate of the position to find

   function Get_Icon_Gicon
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Glib.G_Icon.G_Icon;
   --  Retrieves the GIcon used for the icon, or null if there is no icon or
   --  if the icon was set by some other method (e.g., by stock, pixbuf, or
   --  icon name).
   --  or if the icon is not a GIcon
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   function Get_Icon_Name
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String;
   --  Retrieves the icon name used for the icon, or null if there is no icon
   --  or if the icon was set by some other method (e.g., by pixbuf, stock or
   --  gicon).
   --  wasn't set from an icon name
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   function Get_Icon_Pixbuf
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Retrieves the image used for the icon.
   --  Unlike the other methods of setting and getting icon data, this method
   --  will work regardless of whether the icon was set using a
   --  Gdk.Pixbuf.Gdk_Pixbuf, a GIcon, a stock item, or an icon name.
   --  set for this position.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   function Get_Icon_Sensitive
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Boolean;
   --  Returns whether the icon appears sensitive or insensitive.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   procedure Set_Icon_Sensitive
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Sensitive : Boolean);
   --  Sets the sensitivity for the specified icon.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position
   --  "sensitive": Specifies whether the icon should appear sensitive or
   --  insensitive

   function Get_Icon_Stock
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String;
   --  Retrieves the stock id used for the icon, or null if there is no icon
   --  or if the icon was set by some other method (e.g., by pixbuf, icon name
   --  or gicon).
   --  wasn't set from a stock id
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   function Get_Icon_Storage_Type
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Gtk.Image.Gtk_Image_Type;
   --  Gets the type of representation being used by the icon to store image
   --  data. If the icon has no image data, the return value will be
   --  Gtk.Image.Image_Empty.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position

   function Get_Icon_Tooltip_Markup
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String;
   --  Gets the contents of the tooltip on the icon at the specified position
   --  in Entry.
   --  with g_free when done.
   --  Since: gtk+ 2.16
   --  "icon_pos": the icon position

   procedure Set_Icon_Tooltip_Markup
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Tooltip   : UTF8_String := "");
   --  Sets Tooltip as the contents of the tooltip for the icon at the
   --  specified position. Tooltip is assumed to be marked up with the <link
   --  linkend="PangoMarkupFormat">Pango text markup language</link>.
   --  Use null for Tooltip to remove an existing tooltip.
   --  See also Gtk.Widget.Set_Tooltip_Markup and
   --  gtk_enty_set_icon_tooltip_text.
   --  Since: gtk+ 2.16
   --  "icon_pos": the icon position
   --  "tooltip": the contents of the tooltip for the icon, or null

   function Get_Icon_Tooltip_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String;
   --  Gets the contents of the tooltip on the icon at the specified position
   --  in Entry.
   --  with g_free when done.
   --  Since: gtk+ 2.16
   --  "icon_pos": the icon position

   procedure Set_Icon_Tooltip_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Tooltip   : UTF8_String := "");
   --  Sets Tooltip as the contents of the tooltip for the icon at the
   --  specified position.
   --  Use null for Tooltip to remove an existing tooltip.
   --  See also Gtk.Widget.Set_Tooltip_Text and
   --  Gtk.GEntry.Set_Icon_Tooltip_Markup.
   --  Since: gtk+ 2.16
   --  "icon_pos": the icon position
   --  "tooltip": the contents of the tooltip for the icon, or null

   function Get_Inner_Border
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Style.Gtk_Border;
   --  This function returns the entry's Gtk.GEntry.Gtk_Entry:inner-border
   --  property. See Gtk.GEntry.Set_Inner_Border for more information.
   --  Since: gtk+ 2.10

   procedure Set_Inner_Border
      (The_Entry : not null access Gtk_Entry_Record;
       Border    : Gtk.Style.Gtk_Border);
   --  Sets %entry's inner-border property to %border, or clears it if null is
   --  passed. The inner-border is the area around the entry's text, but inside
   --  its frame.
   --  If set, this property overrides the inner-border style property.
   --  Overriding the style-provided border is useful when you want to do
   --  in-place editing of some text in a canvas or list widget, where
   --  pixel-exact positioning of the entry is important.
   --  Since: gtk+ 2.10
   --  "border": a Gtk.Style.Gtk_Border, or null

   function Get_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record) return gunichar;
   --  Retrieves the character displayed in place of the real characters for
   --  entries with visibility set to false. See Gtk.GEntry.Set_Invisible_Char.
   --  show invisible text at all.

   procedure Set_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record;
       Char      : gunichar);
   --  Sets the character to use in place of the actual text when
   --  Gtk.GEntry.Set_Visibility has been called to set text visibility to
   --  False. i.e. this is the character used in "password mode" to show the
   --  user how many characters have been typed. By default, GTK+ picks the
   --  best invisible char available in the current font. If you set the
   --  invisible char to 0, then the user will get no feedback at all; there
   --  will be no text on the screen as they type.
   --  "Char": a Unicode character

   function Get_Layout
      (The_Entry : not null access Gtk_Entry_Record)
       return Pango.Layout.Pango_Layout;
   --  Gets the Pango.Layout.Pango_Layout used to display the entry. The
   --  layout is useful to e.g. convert text positions to pixel positions, in
   --  combination with Gtk.GEntry.Get_Layout_Offsets. The returned layout is
   --  owned by the entry and must not be modified or freed by the caller.
   --  Keep in mind that the layout text may contain a preedit string, so
   --  Gtk.GEntry.Layout_Index_To_Text_Index and
   --  Gtk.GEntry.Text_Index_To_Layout_Index are needed to convert byte indices
   --  in the layout to byte indices in the entry contents.

   procedure Get_Layout_Offsets
      (The_Entry : not null access Gtk_Entry_Record;
       X         : out Gint;
       Y         : out Gint);
   --  Obtains the position of the Pango.Layout.Pango_Layout used to render
   --  text in the entry, in widget coordinates. Useful if you want to line up
   --  the text in an entry with some other text, e.g. when using the entry to
   --  implement editable cells in a sheet widget.
   --  Also useful to convert mouse events into coordinates inside the
   --  Pango.Layout.Pango_Layout, e.g. to take some action if some part of the
   --  entry text is clicked.
   --  Note that as the user scrolls around in the entry the offsets will
   --  change; you'll need to connect to the "notify::scroll-offset" signal to
   --  track this. Remember when using the Pango.Layout.Pango_Layout functions
   --  you need to convert to and from pixels using PANGO_PIXELS or
   --  PANGO_SCALE.
   --  Keep in mind that the layout text may contain a preedit string, so
   --  Gtk.GEntry.Layout_Index_To_Text_Index and
   --  Gtk.GEntry.Text_Index_To_Layout_Index are needed to convert byte indices
   --  in the layout to byte indices in the entry contents.
   --  "x": location to store X offset of layout, or null
   --  "y": location to store Y offset of layout, or null

   function Get_Max_Length
      (The_Entry : not null access Gtk_Entry_Record) return Gint;
   --  Retrieves the maximum allowed length of the text in Entry. See
   --  Gtk.GEntry.Set_Max_Length.
   --  This is equivalent to:
   --    gtk_entry_buffer_get_max_length (gtk_entry_get_buffer (entry));
   --  in Gtk.GEntry.Gtk_Entry, or 0 if there is no maximum.

   procedure Set_Max_Length
      (The_Entry : not null access Gtk_Entry_Record;
       Max       : Gint);
   --  Sets the maximum allowed length of the contents of the widget. If the
   --  current contents are longer than the given length, then they will be
   --  truncated to fit.
   --  This is equivalent to:
   --    gtk_entry_buffer_set_max_length (gtk_entry_get_buffer (entry), max);
   --  "max": the maximum length of the entry, or 0 for no maximum. (other
   --  than the maximum length of entries.) The value passed in will be clamped
   --  to the range 0-65536.

   function Get_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record) return Boolean;
   --  Gets the value set by Gtk.GEntry.Set_Overwrite_Mode.
   --  Since: gtk+ 2.14

   procedure Set_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record;
       Overwrite : Boolean);
   --  Sets whether the text is overwritten when typing in the
   --  Gtk.GEntry.Gtk_Entry.
   --  Since: gtk+ 2.14
   --  "overwrite": new value

   function Get_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String;
   --  Retrieves the text that will be displayed when Entry is empty and
   --  unfocused
   --  storage in the widget and must not be freed, modified or stored.
   --  Since: gtk+ 3.2

   procedure Set_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String);
   --  Sets text to be displayed in Entry when it is empty and unfocused. This
   --  can be used to give a visual hint of the expected contents of the
   --  Gtk.GEntry.Gtk_Entry.
   --  Note that since the placeholder text gets removed when the entry
   --  received focus, using this feature is a bit problematic if the entry is
   --  given the initial focus in a window. Sometimes this can be worked around
   --  by delaying the initial focus setting until the first key event arrives.
   --  Since: gtk+ 3.2
   --  "text": a string to be displayed when Entry is empty an unfocused, or
   --  null

   function Get_Progress_Fraction
      (The_Entry : not null access Gtk_Entry_Record) return Gdouble;
   --  Returns the current fraction of the task that's been completed. See
   --  Gtk.GEntry.Set_Progress_Fraction.
   --  Since: gtk+ 2.16

   procedure Set_Progress_Fraction
      (The_Entry : not null access Gtk_Entry_Record;
       Fraction  : Gdouble);
   --  Causes the entry's progress indicator to "fill in" the given fraction
   --  of the bar. The fraction should be between 0.0 and 1.0, inclusive.
   --  Since: gtk+ 2.16
   --  "fraction": fraction of the task that's been completed

   function Get_Progress_Pulse_Step
      (The_Entry : not null access Gtk_Entry_Record) return Gdouble;
   --  Retrieves the pulse step set with Gtk.GEntry.Set_Progress_Pulse_Step.
   --  Since: gtk+ 2.16

   procedure Set_Progress_Pulse_Step
      (The_Entry : not null access Gtk_Entry_Record;
       Fraction  : Gdouble);
   --  Sets the fraction of total entry width to move the progress bouncing
   --  block for each call to Gtk.GEntry.Progress_Pulse.
   --  Since: gtk+ 2.16
   --  "fraction": fraction between 0.0 and 1.0

   function Get_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String;
   --  Retrieves the contents of the entry widget. See also
   --  Gtk.Editable.Get_Chars.
   --  This is equivalent to:
   --    gtk_entry_buffer_get_text (gtk_entry_get_buffer (entry));
   --  string. This string points to internally allocated storage in the
   --  widget and must not be freed, modified or stored.

   procedure Set_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String);
   --  Sets the text in the widget to the given value, replacing the current
   --  contents.
   --  See Gtk.Entry_Buffer.Set_Text.
   --  "text": the new text

   procedure Get_Text_Area
      (The_Entry : not null access Gtk_Entry_Record;
       Text_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets the area where the entry's text is drawn. This function is useful
   --  when drawing something to the entry in a draw callback.
   --  If the entry is not realized, Text_Area is filled with zeros.
   --  See also Gtk.GEntry.Get_Icon_Area.
   --  Since: gtk+ 3.0
   --  "text_area": Return location for the text area.

   function Get_Text_Length
      (The_Entry : not null access Gtk_Entry_Record) return Guint16;
   --  Retrieves the current length of the text in Entry.
   --  This is equivalent to:
   --    gtk_entry_buffer_get_length (gtk_entry_get_buffer (entry));
   --  in Gtk.GEntry.Gtk_Entry, or 0 if there are none.
   --  Since: gtk+ 2.14

   function Get_Visibility
      (The_Entry : not null access Gtk_Entry_Record) return Boolean;
   --  Retrieves whether the text in Entry is visible. See
   --  Gtk.GEntry.Set_Visibility.

   procedure Set_Visibility
      (The_Entry : not null access Gtk_Entry_Record;
       Visible   : Boolean);
   --  Sets whether the contents of the entry are visible or not. When
   --  visibility is set to False, characters are displayed as the invisible
   --  char, and will also appear that way when the text in the entry widget is
   --  copied elsewhere.
   --  By default, GTK+ picks the best invisible character available in the
   --  current font, but it can be changed with Gtk.GEntry.Set_Invisible_Char.
   --  "visible": True if the contents of the entry are displayed as plaintext

   function Get_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record) return Gint;
   --  Gets the value set by Gtk.GEntry.Set_Width_Chars.

   procedure Set_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record;
       Width     : Gint);
   --  Changes the size request of the entry to be about the right size for
   --  N_Chars characters. Note that it changes the size *request*, the size
   --  can still be affected by how you pack the widget into containers. If
   --  N_Chars is -1, the size reverts to the default entry size.
   --  "Width": width in chars

   function Im_Context_Filter_Keypress
      (The_Entry : not null access Gtk_Entry_Record;
       Event     : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Allow the Gtk.GEntry.Gtk_Entry input method to internally handle key
   --  press and release events. If this function returns True, then no further
   --  processing should be done for this key event. See
   --  gtk_im_context_filter_keypress.
   --  Note that you are expected to call this function from your handler when
   --  overriding key event handling. This is needed in the case when you need
   --  to insert your own key handling between the input method and the default
   --  key event handling of the Gtk.GEntry.Gtk_Entry. See
   --  gtk_text_view_reset_im_context for an example of use.
   --  Since: gtk+ 2.22
   --  "event": the key event

   function Layout_Index_To_Text_Index
      (The_Entry    : not null access Gtk_Entry_Record;
       Layout_Index : Gint) return Gint;
   --  Converts from a position in the entry contents (returned by
   --  Gtk.GEntry.Get_Text) to a position in the entry's
   --  Pango.Layout.Pango_Layout (returned by Gtk.GEntry.Get_Layout, with text
   --  retrieved via Pango.Layout.Get_Text).
   --  "layout_index": byte index into the entry layout text

   procedure Progress_Pulse (The_Entry : not null access Gtk_Entry_Record);
   --  Indicates that some progress is made, but you don't know how much.
   --  Causes the entry's progress indicator to enter "activity mode," where a
   --  block bounces back and forth. Each call to Gtk.GEntry.Progress_Pulse
   --  causes the block to move by a little bit (the amount of movement per
   --  pulse is determined by Gtk.GEntry.Set_Progress_Pulse_Step).
   --  Since: gtk+ 2.16

   procedure Reset_Im_Context (The_Entry : not null access Gtk_Entry_Record);
   --  Reset the input method context of the entry if needed.
   --  This can be necessary in the case where modifying the buffer would
   --  confuse on-going input method behavior.
   --  Since: gtk+ 2.22

   procedure Set_Icon_Drag_Source
      (The_Entry   : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk_Entry_Icon_Position;
       Target_List : Gtk.Target_List.Gtk_Target_List;
       Actions     : Gdk.Drag_Contexts.Gdk_Drag_Action);
   --  Sets up the icon at the given position so that GTK+ will start a drag
   --  operation when the user clicks and drags the icon.
   --  To handle the drag operation, you need to connect to the usual
   --  Gtk.Widget.Gtk_Widget::drag-data-get (or possibly
   --  Gtk.Widget.Gtk_Widget::drag-data-delete) signal, and use
   --  Gtk.GEntry.Get_Current_Icon_Drag_Source in your signal handler to find
   --  out if the drag was started from an icon.
   --  By default, GTK+ uses the icon as the drag icon. You can use the
   --  Gtk.Widget.Gtk_Widget::drag-begin signal to set a different icon. Note
   --  that you have to use g_signal_connect_after to ensure that your signal
   --  handler gets executed after the default handler.
   --  Since: gtk+ 2.16
   --  "icon_pos": icon position
   --  "target_list": the targets (data formats) in which the data can be
   --  provided
   --  "actions": a bitmask of the allowed drag actions

   procedure Set_Icon_From_Gicon
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon      : Glib.G_Icon.G_Icon);
   --  Sets the icon shown in the entry at the specified position from the
   --  current icon theme. If the icon isn't known, a "broken image" icon will
   --  be displayed instead.
   --  If Icon is null, no icon will be shown in the specified position.
   --  Since: gtk+ 2.16
   --  "icon_pos": The position at which to set the icon
   --  "icon": The icon to set, or null

   procedure Set_Icon_From_Icon_Name
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon_Name : UTF8_String := "");
   --  Sets the icon shown in the entry at the specified position from the
   --  current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead.
   --  If Icon_Name is null, no icon will be shown in the specified position.
   --  Since: gtk+ 2.16
   --  "icon_pos": The position at which to set the icon
   --  "icon_name": An icon name, or null

   procedure Set_Icon_From_Pixbuf
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the icon shown in the specified position using a pixbuf.
   --  If Pixbuf is null, no icon will be shown in the specified position.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position
   --  "pixbuf": A Gdk.Pixbuf.Gdk_Pixbuf, or null

   procedure Set_Icon_From_Stock
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Stock_Id  : UTF8_String := "");
   --  Sets the icon shown in the entry at the specified position from a stock
   --  image.
   --  If Stock_Id is null, no icon will be shown in the specified position.
   --  Since: gtk+ 2.16
   --  "icon_pos": Icon position
   --  "stock_id": The name of the stock item, or null

   function Text_Index_To_Layout_Index
      (The_Entry  : not null access Gtk_Entry_Record;
       Text_Index : Gint) return Gint;
   --  Converts from a position in the entry's Pango.Layout.Pango_Layout
   --  (returned by Gtk.GEntry.Get_Layout) to a position in the entry contents
   --  (returned by Gtk.GEntry.Get_Text).
   --  "text_index": byte index into the entry contents

   procedure Unset_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record);
   --  Unsets the invisible char previously set with
   --  Gtk.GEntry.Set_Invisible_Char. So that the default invisible char is
   --  used again.
   --  Since: gtk+ 2.16

   ----------------------
   -- GtkAda additions --
   ----------------------

   subtype Gtk_GEntry is Gtk_Entry;

   procedure Insert_Text
     (Editable : access Gtk_Entry_Record;
      New_Text : UTF8_String;
      Position : in out Gint);
   --  Convenience subprogram, identical to Insert_Text above without
   --  the requirement to supply the New_Text_Length argument.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Editing_Done (Cell_Editable : not null access Gtk_Entry_Record);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Entry_Record);

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Entry_Record;
       Event         : Gdk.Event.Gdk_Event);

   procedure Copy_Clipboard (Editable : not null access Gtk_Entry_Record);

   procedure Cut_Clipboard (Editable : not null access Gtk_Entry_Record);

   procedure Delete_Selection (Editable : not null access Gtk_Entry_Record);

   procedure Delete_Text
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1);

   function Get_Chars
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1) return UTF8_String;

   function Get_Editable
      (Editable : not null access Gtk_Entry_Record) return Boolean;

   procedure Set_Editable
      (Editable    : not null access Gtk_Entry_Record;
       Is_Editable : Boolean);

   function Get_Position
      (Editable : not null access Gtk_Entry_Record) return Gint;

   procedure Set_Position
      (Editable : not null access Gtk_Entry_Record;
       Position : Gint);

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Entry_Record;
       Start_Pos     : out Gint;
       End_Pos       : out Gint;
       Has_Selection : out Boolean);

   procedure Insert_Text
      (Editable        : not null access Gtk_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Gint;
       Position        : in out Gint);

   procedure Paste_Clipboard (Editable : not null access Gtk_Entry_Record);

   procedure Select_Region
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellEditable"
   --
   --  - "Editable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Entry_Record, Gtk_Entry);
   function "+"
     (Widget : access Gtk_Entry_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Entry
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Editable is new Glib.Types.Implements
     (Gtk.Cell_Editable.Gtk_Cell_Editable, Gtk_Entry_Record, Gtk_Entry);
   function "+"
     (Widget : access Gtk_Entry_Record'Class)
   return Gtk.Cell_Editable.Gtk_Cell_Editable
   renames Implements_Gtk_Cell_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Editable.Gtk_Cell_Editable)
   return Gtk_Entry
   renames Implements_Gtk_Cell_Editable.To_Object;

   package Implements_Gtk_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, Gtk_Entry_Record, Gtk_Entry);
   function "+"
     (Widget : access Gtk_Entry_Record'Class)
   return Gtk.Editable.Gtk_Editable
   renames Implements_Gtk_Editable.To_Interface;
   function "-"
     (Interf : Gtk.Editable.Gtk_Editable)
   return Gtk_Entry
   renames Implements_Gtk_Editable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Activates_Default_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Buffer_Property
   --  Type: Gtk.Entry_Buffer.Gtk_Entry_Buffer
   --  Flags: read-write
   --
   --  Name: Caps_Lock_Warning_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether password entries will show a warning when Caps Lock is on.
   --
   --  Note that the warning is shown using a secondary icon, and thus does
   --  not work if you are using the secondary icon position for some other
   --  purpose.
   --
   --  Name: Completion_Property
   --  Type: Gtk.Entry_Completion.Gtk_Entry_Completion
   --  Flags: read-write
   --  The auxiliary completion object to use with the entry.
   --
   --  Name: Cursor_Position_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Editable_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Has_Frame_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Im_Module_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Which IM (input method) module should be used for this entry. See
   --  Gtk.Imcontext.Gtk_Imcontext.
   --
   --  Setting this to a non-null value overrides the system-wide IM module
   --  setting. See the GtkSettings Gtk.Settings.Gtk_Settings:gtk-im-module
   --  property.
   --
   --  Name: Inner_Border_Property
   --  Type: Gtk.Style.Gtk_Border
   --  Flags: read-write
   --  Sets the text area's border between the text and the frame.
   --
   --  Name: Invisible_Char_Property
   --  Type: Guint
   --  Flags: read-write
   --  The invisible character is used when masking entry contents (in
   --  \"password mode\")"). When it is not explicitly set with the
   --  Gtk.GEntry.Gtk_Entry::invisible-char property, GTK+ determines the
   --  character to use from a list of possible candidates, depending on
   --  availability in the current font.
   --
   --  This style property allows the theme to prepend a character to the list
   --  of candidates.
   --
   --  Name: Invisible_Char_Set_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the invisible char has been set for the Gtk.GEntry.Gtk_Entry.
   --
   --  Name: Max_Length_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Overwrite_Mode_Property
   --  Type: Boolean
   --  Flags: read-write
   --  If text is overwritten when typing in the Gtk.GEntry.Gtk_Entry.
   --
   --  Name: Placeholder_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The text that will be displayed in the Gtk.GEntry.Gtk_Entry when it is
   --  empty and unfocused.
   --
   --  Name: Primary_Icon_Activatable_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the primary icon is activatable.
   --
   --  GTK+ emits the Gtk.GEntry.Gtk_Entry::icon-press and
   --  Gtk.GEntry.Gtk_Entry::icon-release signals only on sensitive,
   --  activatable icons.
   --
   --  Sensitive, but non-activatable icons can be used for purely
   --  informational purposes.
   --
   --  Name: Primary_Icon_Gicon_Property
   --  Type: Glib.G_Icon.G_Icon
   --  Flags: read-write
   --  The GIcon to use for the primary icon for the entry.
   --
   --  Name: Primary_Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The icon name to use for the primary icon for the entry.
   --
   --  Name: Primary_Icon_Pixbuf_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --  A pixbuf to use as the primary icon for the entry.
   --
   --  Name: Primary_Icon_Sensitive_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the primary icon is sensitive.
   --
   --  An insensitive icon appears grayed out. GTK+ does not emit the
   --  Gtk.GEntry.Gtk_Entry::icon-press and Gtk.GEntry.Gtk_Entry::icon-release
   --  signals and does not allow DND from insensitive icons.
   --
   --  An icon should be set insensitive if the action that would trigger when
   --  clicked is currently not available.
   --
   --  Name: Primary_Icon_Stock_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The stock id to use for the primary icon for the entry.
   --
   --  Name: Primary_Icon_Storage_Type_Property
   --  Type: Gtk.Image.Gtk_Image_Type
   --  Flags: read-write
   --  The representation which is used for the primary icon of the entry.
   --
   --  Name: Primary_Icon_Tooltip_Markup_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The contents of the tooltip on the primary icon, which is marked up
   --  with the <link linkend="PangoMarkupFormat">Pango text markup
   --  language</link>.
   --
   --  Also see Gtk.GEntry.Set_Icon_Tooltip_Markup.
   --
   --  Name: Primary_Icon_Tooltip_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The contents of the tooltip on the primary icon.
   --
   --  Also see Gtk.GEntry.Set_Icon_Tooltip_Text.
   --
   --  Name: Progress_Fraction_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The current fraction of the task that's been completed.
   --
   --  Name: Progress_Pulse_Step_Property
   --  Type: Gdouble
   --  Flags: read-write
   --  The fraction of total entry width to move the progress bouncing block
   --  for each call to Gtk.GEntry.Progress_Pulse.
   --
   --  Name: Scroll_Offset_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Secondary_Icon_Activatable_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the secondary icon is activatable.
   --
   --  GTK+ emits the Gtk.GEntry.Gtk_Entry::icon-press and
   --  Gtk.GEntry.Gtk_Entry::icon-release signals only on sensitive,
   --  activatable icons.
   --
   --  Sensitive, but non-activatable icons can be used for purely
   --  informational purposes.
   --
   --  Name: Secondary_Icon_Gicon_Property
   --  Type: Glib.G_Icon.G_Icon
   --  Flags: read-write
   --  The GIcon to use for the secondary icon for the entry.
   --
   --  Name: Secondary_Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The icon name to use for the secondary icon for the entry.
   --
   --  Name: Secondary_Icon_Pixbuf_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --  An pixbuf to use as the secondary icon for the entry.
   --
   --  Name: Secondary_Icon_Sensitive_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether the secondary icon is sensitive.
   --
   --  An insensitive icon appears grayed out. GTK+ does not emit the
   --  Gtk.GEntry.Gtk_Entry::icon-press and Gtk.GEntry.Gtk_Entry::icon-release
   --  signals and does not allow DND from insensitive icons.
   --
   --  An icon should be set insensitive if the action that would trigger when
   --  clicked is currently not available.
   --
   --  Name: Secondary_Icon_Stock_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The stock id to use for the secondary icon for the entry.
   --
   --  Name: Secondary_Icon_Storage_Type_Property
   --  Type: Gtk.Image.Gtk_Image_Type
   --  Flags: read-write
   --  The representation which is used for the secondary icon of the entry.
   --
   --  Name: Secondary_Icon_Tooltip_Markup_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The contents of the tooltip on the secondary icon, which is marked up
   --  with the <link linkend="PangoMarkupFormat">Pango text markup
   --  language</link>.
   --
   --  Also see Gtk.GEntry.Set_Icon_Tooltip_Markup.
   --
   --  Name: Secondary_Icon_Tooltip_Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The contents of the tooltip on the secondary icon.
   --
   --  Also see Gtk.GEntry.Set_Icon_Tooltip_Text.
   --
   --  Name: Selection_Bound_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Shadow_Type_Property
   --  Type: Gtk.Enums.Gtk_Shadow_Type
   --  Flags: read-write
   --  Which kind of shadow to draw around the entry when
   --  Gtk.GEntry.Gtk_Entry:has-frame is set to True.
   --
   --  Name: Text_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Text_Length_Property
   --  Type: Guint
   --  Flags: read-write
   --  The length of the text in the Gtk.GEntry.Gtk_Entry.
   --
   --  Name: Truncate_Multiline_Property
   --  Type: Boolean
   --  Flags: read-write
   --  When True, pasted multi-line text is truncated to the first line.
   --
   --  Name: Visibility_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Width_Chars_Property
   --  Type: Gint
   --  Flags: read-write
   --
   --  Name: Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --  The horizontal alignment, from 0 (left) to 1 (right). Reversed for RTL
   --  layouts.

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   Buffer_Property : constant Glib.Properties.Property_Object;
   Caps_Lock_Warning_Property : constant Glib.Properties.Property_Boolean;
   Completion_Property : constant Glib.Properties.Property_Object;
   Cursor_Position_Property : constant Glib.Properties.Property_Int;
   Editable_Property : constant Glib.Properties.Property_Boolean;
   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   Im_Module_Property : constant Glib.Properties.Property_String;
   Inner_Border_Property : constant Glib.Properties.Property_Object;
   Invisible_Char_Property : constant Glib.Properties.Property_Uint;
   Invisible_Char_Set_Property : constant Glib.Properties.Property_Boolean;
   Max_Length_Property : constant Glib.Properties.Property_Int;
   Overwrite_Mode_Property : constant Glib.Properties.Property_Boolean;
   Placeholder_Text_Property : constant Glib.Properties.Property_String;
   Primary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean;
   Primary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed;
   Primary_Icon_Name_Property : constant Glib.Properties.Property_String;
   Primary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object;
   Primary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   Primary_Icon_Stock_Property : constant Glib.Properties.Property_String;
   Primary_Icon_Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type;
   Primary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   Primary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String;
   Progress_Fraction_Property : constant Glib.Properties.Property_Double;
   Progress_Pulse_Step_Property : constant Glib.Properties.Property_Double;
   Scroll_Offset_Property : constant Glib.Properties.Property_Int;
   Secondary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean;
   Secondary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed;
   Secondary_Icon_Name_Property : constant Glib.Properties.Property_String;
   Secondary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object;
   Secondary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   Secondary_Icon_Stock_Property : constant Glib.Properties.Property_String;
   Secondary_Icon_Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type;
   Secondary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   Secondary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String;
   Selection_Bound_Property : constant Glib.Properties.Property_Int;
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;
   Text_Property : constant Glib.Properties.Property_String;
   Text_Length_Property : constant Glib.Properties.Property_Uint;
   Truncate_Multiline_Property : constant Glib.Properties.Property_Boolean;
   Visibility_Property : constant Glib.Properties.Property_Boolean;
   Width_Chars_Property : constant Glib.Properties.Property_Int;
   Xalign_Property : constant Glib.Properties.Property_Float;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  A <link linkend="keybinding-signals">keybinding signal</link> which
   --  gets emitted when the user activates the entry.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control activation
   --  programmatically.
   --
   --  The default bindings for this signal are all forms of the Enter key.
   --
   --  "backspace"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  The ::backspace signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  when the user asks for it.
   --
   --  The default bindings for this signal are Backspace and Shift-Backspace.
   --
   --  "copy-clipboard"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  The ::copy-clipboard signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to copy the selection to the clipboard.
   --
   --  The default bindings for this signal are Ctrl-c and Ctrl-Insert.
   --
   --  "cut-clipboard"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  The ::cut-clipboard signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to cut the selection to the clipboard.
   --
   --  The default bindings for this signal are Ctrl-x and Shift-Delete.
   --
   --  "delete-from-cursor"
   --     procedure Handler
   --       (Self     : access Gtk_Entry_Record'Class;
   --        The_Type : Gtk.Enums.Gtk_Delete_Type;
   --        Count    : Gint);
   --    --  "type": the granularity of the deletion, as a Gtk.Enums.Gtk_Delete_Type
   --    --  "count": the number of Type units to delete
   --  The ::delete-from-cursor signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  when the user initiates a text deletion.
   --
   --  If the Type is Gtk.Enums.Delete_Chars, GTK+ deletes the selection if
   --  there is one, otherwise it deletes the requested number of characters.
   --
   --  The default bindings for this signal are Delete for deleting a
   --  character and Ctrl-Delete for deleting a word.
   --
   --  "icon-press"
   --     procedure Handler
   --       (Self     : access Gtk_Entry_Record'Class;
   --        Icon_Pos : Gtk_Entry_Icon_Position;
   --        Event    : Gdk.Event_Button);
   --    --  "icon_pos": The position of the clicked icon
   --    --  "event": the button press event
   --  The ::icon-press signal is emitted when an activatable icon is clicked.
   --
   --  "icon-release"
   --     procedure Handler
   --       (Self     : access Gtk_Entry_Record'Class;
   --        Icon_Pos : Gtk_Entry_Icon_Position;
   --        Event    : Gdk.Event_Button);
   --    --  "icon_pos": The position of the clicked icon
   --    --  "event": the button release event
   --  The ::icon-release signal is emitted on the button release from a mouse
   --  click over an activatable icon.
   --
   --  "insert-at-cursor"
   --     procedure Handler
   --       (Self   : access Gtk_Entry_Record'Class;
   --        String : UTF8_String);
   --    --  "string": the string to insert
   --  The ::insert-at-cursor signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  when the user initiates the insertion of a fixed string at the cursor.
   --
   --  This signal has no default bindings.
   --
   --  "move-cursor"
   --     procedure Handler
   --       (Self             : access Gtk_Entry_Record'Class;
   --        Step             : Gtk.Enums.Gtk_Movement_Step;
   --        Count            : Gint;
   --        Extend_Selection : Boolean);
   --    --  "step": the granularity of the move, as a Gtk.Enums.Gtk_Movement_Step
   --    --  "count": the number of Step units to move
   --    --  "extend_selection": True if the move should extend the selection
   --  The ::move-cursor signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  when the user initiates a cursor movement. If the cursor is not visible
   --  in Entry, this signal causes the viewport to be moved instead.
   --
   --  Applications should not connect to it, but may emit it with
   --  g_signal_emit_by_name if they need to control the cursor
   --  programmatically.
   --
   --  The default bindings for this signal come in two variants, the variant
   --  with the Shift modifier extends the selection, the variant without the
   --  Shift modifer does not. There are too many key combinations to list them
   --  all here.
   --
   --     * Arrow keys move by individual characters/lines
   --
   --     * Ctrl-arrow key combinations move by words/paragraphs
   --
   --     * Home/End keys move to the ends of the buffer
   --
   --  "paste-clipboard"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  The ::paste-clipboard signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to paste the contents of the clipboard into the text view.
   --
   --  The default bindings for this signal are Ctrl-v and Shift-Insert.
   --
   --  "populate-popup"
   --     procedure Handler
   --       (Self : access Gtk_Entry_Record'Class;
   --        Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   --    --  "menu": the menu that is being populated
   --  The ::populate-popup signal gets emitted before showing the context
   --  menu of the entry.
   --
   --  If you need to add items to the context menu, connect to this signal
   --  and append your menuitems to the Menu.
   --
   --  "preedit-changed"
   --     procedure Handler
   --       (Self    : access Gtk_Entry_Record'Class;
   --        Preedit : UTF8_String);
   --    --  "preedit": the current preedit string
   --  If an input method is used, the typed text will not immediately be
   --  committed to the buffer. So if you are interested in the text, connect
   --  to this signal.
   --
   --  "toggle-overwrite"
   --     procedure Handler (Self : access Gtk_Entry_Record'Class);
   --  The ::toggle-overwrite signal is a <link
   --  linkend="keybinding-signals">keybinding signal</link> which gets emitted
   --  to toggle the overwrite mode of the entry.
   --
   --  The default bindings for this signal is Insert.

   Signal_Activate : constant Glib.Signal_Name := "activate";
   Signal_Backspace : constant Glib.Signal_Name := "backspace";
   Signal_Copy_Clipboard : constant Glib.Signal_Name := "copy-clipboard";
   Signal_Cut_Clipboard : constant Glib.Signal_Name := "cut-clipboard";
   Signal_Delete_From_Cursor : constant Glib.Signal_Name := "delete-from-cursor";
   Signal_Icon_Press : constant Glib.Signal_Name := "icon-press";
   Signal_Icon_Release : constant Glib.Signal_Name := "icon-release";
   Signal_Insert_At_Cursor : constant Glib.Signal_Name := "insert-at-cursor";
   Signal_Move_Cursor : constant Glib.Signal_Name := "move-cursor";
   Signal_Paste_Clipboard : constant Glib.Signal_Name := "paste-clipboard";
   Signal_Populate_Popup : constant Glib.Signal_Name := "populate-popup";
   Signal_Preedit_Changed : constant Glib.Signal_Name := "preedit-changed";
   Signal_Toggle_Overwrite : constant Glib.Signal_Name := "toggle-overwrite";

private
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates-default");
   Buffer_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("buffer");
   Caps_Lock_Warning_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("caps-lock-warning");
   Completion_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("completion");
   Cursor_Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cursor-position");
   Editable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editable");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("im-module");
   Inner_Border_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("inner-border");
   Invisible_Char_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("invisible-char");
   Invisible_Char_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-char-set");
   Max_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-length");
   Overwrite_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overwrite-mode");
   Placeholder_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("placeholder-text");
   Primary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-activatable");
   Primary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("primary-icon-gicon");
   Primary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-name");
   Primary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("primary-icon-pixbuf");
   Primary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-sensitive");
   Primary_Icon_Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-stock");
   Primary_Icon_Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type :=
     Gtk.Image.Build ("primary-icon-storage-type");
   Primary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-markup");
   Primary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-text");
   Progress_Fraction_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-fraction");
   Progress_Pulse_Step_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-pulse-step");
   Scroll_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll-offset");
   Secondary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-activatable");
   Secondary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("secondary-icon-gicon");
   Secondary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-name");
   Secondary_Icon_Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("secondary-icon-pixbuf");
   Secondary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-sensitive");
   Secondary_Icon_Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-stock");
   Secondary_Icon_Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type :=
     Gtk.Image.Build ("secondary-icon-storage-type");
   Secondary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-markup");
   Secondary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-text");
   Selection_Bound_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("selection-bound");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Text_Length_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("text-length");
   Truncate_Multiline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("truncate-multiline");
   Visibility_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visibility");
   Width_Chars_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-chars");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
end Gtk.GEntry;
