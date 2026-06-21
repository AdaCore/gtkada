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

--  A single-line text entry widget.
--
--  <picture> <source srcset="entry-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkEntry" src="entry.png"> </picture>
--  A fairly large set of key bindings are supported by default. If the
--  entered text is longer than the allocation of the widget, the widget will
--  scroll so that the cursor position is visible.
--
--  When using an entry for passwords and other sensitive information, it can
--  be put into "password mode" using [methodGtk.Entry.set_visibility]. In this
--  mode, entered text is displayed using a "invisible" character. By default,
--  GTK picks the best invisible character that is available in the current
--  font, but it can be changed with [methodGtk.Entry.set_invisible_char].
--
--  `GtkEntry` has the ability to display progress or activity information
--  behind the text. To make an entry display such information, use
--  [methodGtk.Entry.set_progress_fraction] or
--  [methodGtk.Entry.set_progress_pulse_step].
--
--  Additionally, `GtkEntry` can show icons at either side of the entry. These
--  icons can be activatable by clicking, can be set up as drag source and can
--  have tooltips. To add an icon, use [methodGtk.Entry.set_icon_from_gicon] or
--  one of the various other functions that set an icon from an icon name or a
--  paintable. To trigger an action when the user clicks an icon, connect to
--  the [signalGtk.Entry::icon-press] signal. To allow DND operations from an
--  icon, use [methodGtk.Entry.set_icon_drag_source]. To set a tooltip on an
--  icon, use [methodGtk.Entry.set_icon_tooltip_text] or the corresponding
--  function for markup.
--
--  Note that functionality or information that is only available by clicking
--  on an icon in an entry may not be accessible at all to users which are not
--  able to use a mouse or other pointing device. It is therefore recommended
--  that any such functionality should also be available by other means, e.g.
--  via the context menu of the entry.
--
--  # CSS nodes
--
--  ``` entry[.flat][.warning][.error] ├── text[.readonly] ├── image.left ├──
--  image.right ╰── [progress[.pulse]] ```
--
--  `GtkEntry` has a main node with the name entry. Depending on the
--  properties of the entry, the style classes .read-only and .flat may appear.
--  The style classes .warning and .error may also be used with entries.
--
--  When the entry shows icons, it adds subnodes with the name image and the
--  style class .left or .right, depending on where the icon appears.
--
--  When the entry shows progress, it adds a subnode with the name progress.
--  The node has the style class .pulse when the shown progress is pulsing.
--
--  For all the subnodes added to the text node in various situations, see
--  [classGtk.Text].
--
--  # GtkEntry as GtkBuildable
--
--  The `GtkEntry` implementation of the `GtkBuildable` interface supports a
--  custom `<attributes>` element, which supports any number of `<attribute>`
--  elements. The `<attribute>` element has attributes named "name", "value",
--  "start" and "end" and allows you to specify `PangoAttribute` values for
--  this label.
--
--  An example of a UI definition fragment specifying Pango attributes: ```xml
--  <object class="GtkEntry"> <attributes> <attribute name="weight"
--  value="PANGO_WEIGHT_BOLD"/> <attribute name="background" value="red"
--  start="5" end="10"/> </attributes> </object> ```
--
--  The start and end attributes specify the range of characters to which the
--  Pango attribute applies. If start and end are not specified, the attribute
--  is applied to the whole text. Note that specifying ranges does not make
--  much sense with translatable attributes. Use markup embedded in the
--  translatable content instead.
--
--  # Accessibility
--
--  `GtkEntry` uses the [enumGtk.AccessibleRole.text_box] role.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Content_Provider;  use Gdk.Content_Provider;
with Gdk.Drag;              use Gdk.Drag;
with Gdk.Paintable;         use Gdk.Paintable;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Cell_Editable;     use Gtk.Cell_Editable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Entry_Buffer;      use Gtk.Entry_Buffer;
with Gtk.Entry_Completion;  use Gtk.Entry_Completion;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C;          use Interfaces.C;
with Pango.Attributes;      use Pango.Attributes;
with Pango.Tabs;            use Pango.Tabs;

package Gtk.GEntry is

   type Gtk_Entry_Record is new Gtk_Widget_Record with null record;
   type Gtk_Entry is access all Gtk_Entry_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Entry);
   procedure Initialize (Self : not null access Gtk_Entry_Record'Class);
   --  Creates a new entry.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Entry_New return Gtk_Entry;
   --  Creates a new entry.

   procedure Gtk_New_With_Buffer
      (Self   : out Gtk_Entry;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class);
   procedure Initialize_With_Buffer
      (Self   : not null access Gtk_Entry_Record'Class;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class);
   --  Creates a new entry with the specified text buffer.
   --  Initialize_With_Buffer does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Buffer The buffer to use for the new `GtkEntry`.

   function Gtk_Entry_New_With_Buffer
      (Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
       return Gtk_Entry;
   --  Creates a new entry with the specified text buffer.
   --  @param Buffer The buffer to use for the new `GtkEntry`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_entry_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Activates_Default
      (Self : not null access Gtk_Entry_Record) return Boolean;
   --  Retrieves the value set by Gtk.GEntry.Set_Activates_Default.
   --  @return True if the entry will activate the default widget

   procedure Set_Activates_Default
      (Self    : not null access Gtk_Entry_Record;
       Setting : Boolean);
   --  Sets whether pressing Enter in the Entry will activate the default
   --  widget for the window containing the entry.
   --  This usually means that the dialog containing the entry will be closed,
   --  since the default widget is usually one of the dialog buttons.
   --  @param Setting True to activate window's default widget on Enter
   --  keypress

   function Get_Alignment
      (Self : not null access Gtk_Entry_Record) return Interfaces.C.C_float;
   --  Gets the value set by Gtk.GEntry.Set_Alignment.
   --  See also: [propertyGtk.Editable:xalign]
   --  @return the alignment

   procedure Set_Alignment
      (Self   : not null access Gtk_Entry_Record;
       Xalign : Interfaces.C.C_float);
   --  Sets the alignment for the contents of the entry.
   --  This controls the horizontal positioning of the contents when the
   --  displayed text is shorter than the width of the entry.
   --  See also: [propertyGtk.Editable:xalign]
   --  @param Xalign The horizontal alignment, from 0 (left) to 1 (right).
   --  Reversed for RTL layouts

   function Get_Attributes
      (Self : not null access Gtk_Entry_Record)
       return Pango.Attributes.Pango_Attr_List;
   --  Gets the attribute list of the `GtkEntry`.
   --  See [methodGtk.Entry.set_attributes].
   --  @return the attribute list

   procedure Set_Attributes
      (Self  : not null access Gtk_Entry_Record;
       Attrs : Pango.Attributes.Pango_Attr_List);
   --  Sets a `PangoAttrList`.
   --  The attributes in the list are applied to the entry text.
   --  Since the attributes will be applied to text that changes as the user
   --  types, it makes most sense to use attributes with unlimited extent.
   --  @param Attrs a `PangoAttrList`

   function Get_Buffer
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Entry_Buffer.Gtk_Entry_Buffer;
   --  Get the `GtkEntryBuffer` object which holds the text for this widget.
   --  @return A `GtkEntryBuffer` object.

   procedure Set_Buffer
      (Self   : not null access Gtk_Entry_Record;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class);
   --  Set the `GtkEntryBuffer` object which holds the text for this widget.
   --  @param Buffer a `GtkEntryBuffer`

   function Get_Completion
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Entry_Completion.Gtk_Entry_Completion;
   pragma Obsolescent (Get_Completion);
   --  Returns the auxiliary completion object currently in use by Entry.
   --  Deprecated since 4.10, 1
   --  @return The auxiliary completion object currently in use by Entry

   procedure Set_Completion
      (Self       : not null access Gtk_Entry_Record;
       Completion : access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class);
   pragma Obsolescent (Set_Completion);
   --  Sets Completion to be the auxiliary completion object to use with
   --  Entry.
   --  All further configuration of the completion mechanism is done on
   --  Completion using the `GtkEntryCompletion` API. Completion is disabled if
   --  Completion is set to null.
   --  Deprecated since 4.10, 1
   --  @param Completion The `GtkEntryCompletion`

   function Get_Current_Icon_Drag_Source
      (Self : not null access Gtk_Entry_Record) return Glib.Gint;
   --  Returns the index of the icon which is the source of the current DND
   --  operation, or -1.
   --  @return index of the icon which is the source of the current DND
   --  operation, or -1.

   function Get_Extra_Menu
      (Self : not null access Gtk_Entry_Record)
       return Glib.Menu_Model.Gmenu_Model;
   --  Gets the menu model set with Gtk.GEntry.Set_Extra_Menu.
   --  @return the menu model

   procedure Set_Extra_Menu
      (Self  : not null access Gtk_Entry_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Sets a menu model to add when constructing the context menu for Entry.
   --  @param Model a `GMenuModel`

   function Get_Has_Frame
      (Self : not null access Gtk_Entry_Record) return Boolean;
   --  Gets the value set by Gtk.GEntry.Set_Has_Frame.
   --  @return whether the entry has a beveled frame

   procedure Set_Has_Frame
      (Self    : not null access Gtk_Entry_Record;
       Setting : Boolean);
   --  Sets whether the entry has a beveled frame around it.
   --  @param Setting new value

   function Get_Icon_Activatable
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Boolean;
   --  Returns whether the icon is activatable.
   --  @param Icon_Pos Icon position
   --  @return True if the icon is activatable.

   procedure Set_Icon_Activatable
      (Self        : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk.Enums.Gtk_Entry_Icon_Position;
       Activatable : Boolean);
   --  Sets whether the icon is activatable.
   --  @param Icon_Pos Icon position
   --  @param Activatable True if the icon should be activatable

   procedure Get_Icon_Area
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets the area where entry's icon at Icon_Pos is drawn.
   --  This function is useful when drawing something to the entry in a draw
   --  callback.
   --  If the entry is not realized or has no icon at the given position,
   --  Icon_Area is filled with zeros. Otherwise, Icon_Area will be filled with
   --  the icon's allocation, relative to Entry's allocation.
   --  @param Icon_Pos Icon position
   --  @param Icon_Area Return location for the icon's area

   function Get_Icon_At_Pos
      (Self : not null access Gtk_Entry_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Glib.Gint;
   --  Finds the icon at the given position and return its index.
   --  The position's coordinates are relative to the Entry's top left corner.
   --  If X, Y doesn't lie inside an icon, -1 is returned. This function is
   --  intended for use in a [signalGtk.Widget::query-tooltip] signal handler.
   --  @param X the x coordinate of the position to find, relative to Entry
   --  @param Y the y coordinate of the position to find, relative to Entry
   --  @return the index of the icon at the given position, or -1

   function Get_Icon_Gicon
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Glib.G_Icon.G_Icon;
   --  Retrieves the `GIcon` used for the icon.
   --  null will be returned if there is no icon or if the icon was set by
   --  some other method (e.g., by `GdkPaintable` or icon name).
   --  @param Icon_Pos Icon position
   --  @return A `GIcon`

   function Get_Icon_Name
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String;
   --  Retrieves the icon name used for the icon.
   --  null is returned if there is no icon or if the icon was set by some
   --  other method (e.g., by `GdkPaintable` or gicon).
   --  @param Icon_Pos Icon position
   --  @return An icon name

   function Get_Icon_Paintable
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Gdk.Paintable.Gdk_Paintable;
   --  Retrieves the `GdkPaintable` used for the icon.
   --  If no `GdkPaintable` was used for the icon, null is returned.
   --  @param Icon_Pos Icon position
   --  @return A `GdkPaintable` if no icon is set for this position or the
   --  icon set is not a `GdkPaintable`.

   function Get_Icon_Sensitive
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Boolean;
   --  Returns whether the icon appears sensitive or insensitive.
   --  @param Icon_Pos Icon position
   --  @return True if the icon is sensitive.

   procedure Set_Icon_Sensitive
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Sensitive : Boolean);
   --  Sets the sensitivity for the specified icon.
   --  @param Icon_Pos Icon position
   --  @param Sensitive Specifies whether the icon should appear sensitive or
   --  insensitive

   function Get_Icon_Storage_Type
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Gtk.Enums.Gtk_Image_Type;
   --  Gets the type of representation being used by the icon to store image
   --  data.
   --  If the icon has no image data, the return value will be
   --  Gtk.Enums.Image_Empty.
   --  @param Icon_Pos Icon position
   --  @return image representation being used

   function Get_Icon_Tooltip_Markup
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String;
   --  Gets the contents of the tooltip on the icon at the specified position
   --  in Entry.
   --  @param Icon_Pos the icon position
   --  @return the tooltip text

   procedure Set_Icon_Tooltip_Markup
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Tooltip  : UTF8_String := "");
   --  Sets Tooltip as the contents of the tooltip for the icon at the
   --  specified position.
   --  Tooltip is assumed to be marked up with Pango Markup.
   --  Use null for Tooltip to remove an existing tooltip.
   --  See also [methodGtk.Widget.set_tooltip_markup] and
   --  [methodGtk.Entry.set_icon_tooltip_text].
   --  @param Icon_Pos the icon position
   --  @param Tooltip the contents of the tooltip for the icon

   function Get_Icon_Tooltip_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String;
   --  Gets the contents of the tooltip on the icon at the specified position
   --  in Entry.
   --  @param Icon_Pos the icon position
   --  @return the tooltip text

   procedure Set_Icon_Tooltip_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Tooltip  : UTF8_String := "");
   --  Sets Tooltip as the contents of the tooltip for the icon at the
   --  specified position.
   --  Use null for Tooltip to remove an existing tooltip.
   --  See also [methodGtk.Widget.set_tooltip_text] and
   --  [methodGtk.Entry.set_icon_tooltip_markup].
   --  If you unset the widget tooltip via [methodGtk.Widget.set_tooltip_text]
   --  or [methodGtk.Widget.set_tooltip_markup], this sets
   --  [propertyGtk.Widget:has-tooltip] to False, which suppresses icon
   --  tooltips too. You can resolve this by then calling
   --  [methodGtk.Widget.set_has_tooltip] to set
   --  [propertyGtk.Widget:has-tooltip] back to True, or setting at least one
   --  non-empty tooltip on any icon achieves the same result.
   --  @param Icon_Pos the icon position
   --  @param Tooltip the contents of the tooltip for the icon

   function Get_Input_Hints
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Hints;
   --  Gets the input hints of this `GtkEntry`.
   --  @return the input hints

   procedure Set_Input_Hints
      (Self  : not null access Gtk_Entry_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints);
   --  Set additional hints which allow input methods to fine-tune their
   --  behavior.
   --  @param Hints the hints

   function Get_Input_Purpose
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Purpose;
   --  Gets the input purpose of the `GtkEntry`.
   --  @return the input purpose

   procedure Set_Input_Purpose
      (Self    : not null access Gtk_Entry_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose);
   --  Sets the input purpose which can be used by input methods to adjust
   --  their behavior.
   --  @param Purpose the purpose

   function Get_Invisible_Char
      (Self : not null access Gtk_Entry_Record) return Gunichar;
   --  Retrieves the character displayed in place of the actual text in
   --  "password mode".
   --  @return the current invisible char, or 0, if the entry does not show
   --  invisible text at all.

   procedure Set_Invisible_Char
      (Self : not null access Gtk_Entry_Record;
       Ch   : Gunichar);
   --  Sets the character to use in place of the actual text in "password
   --  mode".
   --  See [methodGtk.Entry.set_visibility] for how to enable "password mode".
   --  By default, GTK picks the best invisible char available in the current
   --  font. If you set the invisible char to 0, then the user will get no
   --  feedback at all; there will be no text on the screen as they type.
   --  @param Ch a Unicode character

   function Get_Max_Length
      (Self : not null access Gtk_Entry_Record) return Glib.Gint;
   --  Retrieves the maximum allowed length of the text in Entry.
   --  See [methodGtk.Entry.set_max_length].
   --  @return the maximum allowed number of characters in `GtkEntry`, or 0 if
   --  there is no maximum.

   procedure Set_Max_Length
      (Self : not null access Gtk_Entry_Record;
       Max  : Glib.Gint);
   --  Sets the maximum allowed length of the contents of the widget.
   --  If the current contents are longer than the given length, then they
   --  will be truncated to fit. The length is in characters.
   --  This is equivalent to getting Entry's `GtkEntryBuffer` and calling
   --  [methodGtk.EntryBuffer.set_max_length] on it.
   --  @param Max the maximum length of the entry, or 0 for no maximum. (other
   --  than the maximum length of entries.) The value passed in will be clamped
   --  to the range 0-65536.

   function Get_Menu_Entry_Icon_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String;
   --  Gets the text that will be used in the context menu of the `GtkEntry`
   --  when the specified icon is activatable. Selecting this item in the menu
   --  results, from all aspects, the same than clicking on the specified icon.
   --  This greatly simplifies making accessible applications, because the
   --  icons aren't focusable when using keyboard navigation. This is why Gtk
   --  recommends to add the same action to the context menu.
   --  Since: gtk+ 4.20
   --  @param Icon_Pos either Gtk_Entry_Icon_Primary or
   --  Gtk_Entry_Icon_Secondary
   --  @return the text that will be used in the menu item, or NULL if no menu
   --  item is desired.

   procedure Set_Menu_Entry_Icon_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Text     : UTF8_String);
   --  Sets the text that will be used in the context menu of the `GtkEntry`
   --  when the specified icon is activatable. Selecting this item in the menu
   --  results, from all aspects, the same than clicking on the specified icon.
   --  This greatly simplifies making accessible applications, because the
   --  icons aren't focusable when using keyboard navigation. This is why Gtk
   --  recommends to add the same action to the context menu.
   --  Since: gtk+ 4.20
   --  @param Icon_Pos either Gtk_Entry_Icon_Primary or
   --  Gtk_Entry_Icon_Secondary
   --  @param Text the text used for the menu item in the context menu, or
   --  NULL to not add a menu item.

   function Get_Overwrite_Mode
      (Self : not null access Gtk_Entry_Record) return Boolean;
   --  Gets whether the `GtkEntry` is in overwrite mode.
   --  @return whether the text is overwritten when typing.

   procedure Set_Overwrite_Mode
      (Self      : not null access Gtk_Entry_Record;
       Overwrite : Boolean);
   --  Sets whether the text is overwritten when typing in the `GtkEntry`.
   --  @param Overwrite new value

   function Get_Placeholder_Text
      (Self : not null access Gtk_Entry_Record) return UTF8_String;
   --  Retrieves the text that will be displayed when Entry is empty and
   --  unfocused
   --  @return a pointer to the placeholder text as a string. This string
   --  points to internally allocated storage in the widget and must not be
   --  freed, modified or stored. If no placeholder text has been set, null
   --  will be returned.

   procedure Set_Placeholder_Text
      (Self : not null access Gtk_Entry_Record;
       Text : UTF8_String := "");
   --  Sets text to be displayed in Entry when it is empty.
   --  This can be used to give a visual hint of the expected contents of the
   --  `GtkEntry`.
   --  @param Text a string to be displayed when Entry is empty and unfocused

   function Get_Progress_Fraction
      (Self : not null access Gtk_Entry_Record) return Gdouble;
   --  Returns the current fraction of the task that's been completed.
   --  See [methodGtk.Entry.set_progress_fraction].
   --  @return a fraction from 0.0 to 1.0

   procedure Set_Progress_Fraction
      (Self     : not null access Gtk_Entry_Record;
       Fraction : Gdouble);
   --  Causes the entry's progress indicator to "fill in" the given fraction
   --  of the bar.
   --  The fraction should be between 0.0 and 1.0, inclusive.
   --  @param Fraction fraction of the task that's been completed

   function Get_Progress_Pulse_Step
      (Self : not null access Gtk_Entry_Record) return Gdouble;
   --  Retrieves the pulse step set with Gtk.GEntry.Set_Progress_Pulse_Step.
   --  @return a fraction from 0.0 to 1.0

   procedure Set_Progress_Pulse_Step
      (Self     : not null access Gtk_Entry_Record;
       Fraction : Gdouble);
   --  Sets the fraction of total entry width to move the progress bouncing
   --  block for each pulse.
   --  Use [methodGtk.Entry.progress_pulse] to pulse the progress.
   --  @param Fraction fraction between 0.0 and 1.0

   function Get_Tabs
      (Self : not null access Gtk_Entry_Record)
       return Pango.Tabs.Pango_Tab_Array;
   --  Gets the tabstops of the `GtkEntry`.
   --  See [methodGtk.Entry.set_tabs].
   --  @return the tabstops

   procedure Set_Tabs
      (Self : not null access Gtk_Entry_Record;
       Tabs : Pango.Tabs.Pango_Tab_Array);
   --  Sets a `PangoTabArray`.
   --  The tabstops in the array are applied to the entry text.
   --  @param Tabs a `PangoTabArray`

   function Get_Text_Length
      (Self : not null access Gtk_Entry_Record) return Guint16;
   --  Retrieves the current length of the text in Entry.
   --  This is equivalent to getting Entry's `GtkEntryBuffer` and calling
   --  [methodGtk.EntryBuffer.get_length] on it.
   --  @return the current number of characters in `GtkEntry`, or 0 if there
   --  are none.

   function Get_Visibility
      (Self : not null access Gtk_Entry_Record) return Boolean;
   --  Retrieves whether the text in Entry is visible.
   --  See [methodGtk.Entry.set_visibility].
   --  @return True if the text is currently visible

   procedure Set_Visibility
      (Self    : not null access Gtk_Entry_Record;
       Visible : Boolean);
   --  Sets whether the contents of the entry are visible or not.
   --  When visibility is set to False, characters are displayed as the
   --  invisible char, and will also appear that way when the text in the entry
   --  widget is copied elsewhere.
   --  By default, GTK picks the best invisible character available in the
   --  current font, but it can be changed with
   --  [methodGtk.Entry.set_invisible_char].
   --  Note that you probably want to set [propertyGtk.Entry:input-purpose] to
   --  Gtk.Enums.Input_Purpose_Password or Gtk.Enums.Input_Purpose_Pin to
   --  inform input methods about the purpose of this entry, in addition to
   --  setting visibility to False.
   --  @param Visible True if the contents of the entry are displayed as
   --  plaintext

   function Grab_Focus_Without_Selecting
      (Self : not null access Gtk_Entry_Record) return Boolean;
   --  Causes Entry to have keyboard focus.
   --  It behaves like [methodGtk.Widget.grab_focus], except that it doesn't
   --  select the contents of the entry. You only want to call this on some
   --  special entries which the user usually doesn't want to replace all text
   --  in, such as search-as-you-type entries.
   --  @return True if focus is now inside Self

   procedure Progress_Pulse (Self : not null access Gtk_Entry_Record);
   --  Indicates that some progress is made, but you don't know how much.
   --  Causes the entry's progress indicator to enter "activity mode", where a
   --  block bounces back and forth. Each call to Gtk.GEntry.Progress_Pulse
   --  causes the block to move by a little bit (the amount of movement per
   --  pulse is determined by [methodGtk.Entry.set_progress_pulse_step]).

   procedure Reset_Im_Context (Self : not null access Gtk_Entry_Record);
   --  Reset the input method context of the entry if needed.
   --  This can be necessary in the case where modifying the buffer would
   --  confuse on-going input method behavior.

   procedure Set_Icon_Drag_Source
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Provider : not null access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class;
       Actions  : Gdk.Drag.Drag_Action);
   --  Sets up the icon at the given position as drag source.
   --  This makes it so that GTK will start a drag operation when the user
   --  clicks and drags the icon.
   --  @param Icon_Pos icon position
   --  @param Provider a `GdkContentProvider`
   --  @param Actions a bitmask of the allowed drag actions

   procedure Set_Icon_From_Gicon
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon     : Glib.G_Icon.G_Icon);
   --  Sets the icon shown in the entry at the specified position from the
   --  current icon theme.
   --  If the icon isn't known, a "broken image" icon will be displayed
   --  instead.
   --  If Icon is null, no icon will be shown in the specified position.
   --  @param Icon_Pos The position at which to set the icon
   --  @param Icon The icon to set

   procedure Set_Icon_From_Icon_Name
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon_Name : UTF8_String := "");
   --  Sets the icon shown in the entry at the specified position from the
   --  current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead.
   --  If Icon_Name is null, no icon will be shown in the specified position.
   --  @param Icon_Pos The position at which to set the icon
   --  @param Icon_Name An icon name

   procedure Set_Icon_From_Paintable
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Sets the icon shown in the specified position using a `GdkPaintable`.
   --  If Paintable is null, no icon will be shown in the specified position.
   --  @param Icon_Pos Icon position
   --  @param Paintable A `GdkPaintable`

   procedure Unset_Invisible_Char (Self : not null access Gtk_Entry_Record);
   --  Unsets the invisible char, so that the default invisible char is used
   --  again. See [methodGtk.Entry.set_invisible_char].

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Entry_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Entry_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Entry_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Entry_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Entry_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Entry_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Entry_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Editing_Done (Cell_Editable : not null access Gtk_Entry_Record);
   pragma Obsolescent (Editing_Done);

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Entry_Record);
   pragma Obsolescent (Remove_Widget);

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Delete_Selection (Self : not null access Gtk_Entry_Record);

   procedure Delete_Text
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   procedure Finish_Delegate (Self : not null access Gtk_Entry_Record);

   function Get_Chars
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String;

   function Get_Delegate
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Editable.Gtk_Editable;

   function Get_Editable
      (Self : not null access Gtk_Entry_Record) return Boolean;

   procedure Set_Editable
      (Self        : not null access Gtk_Entry_Record;
       Is_Editable : Boolean);

   function Get_Enable_Undo
      (Self : not null access Gtk_Entry_Record) return Boolean;

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Entry_Record;
       Enable_Undo : Boolean);

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Entry_Record) return Glib.Gint;

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Entry_Record;
       N_Chars : Glib.Gint);

   function Get_Position
      (Self : not null access Gtk_Entry_Record) return Glib.Gint;

   procedure Set_Position
      (Self     : not null access Gtk_Entry_Record;
       Position : Glib.Gint);

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean);

   function Get_Text
      (Self : not null access Gtk_Entry_Record) return UTF8_String;

   procedure Set_Text
      (Self : not null access Gtk_Entry_Record;
       Text : UTF8_String);

   function Get_Width_Chars
      (Self : not null access Gtk_Entry_Record) return Glib.Gint;

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Entry_Record;
       N_Chars : Glib.Gint);

   procedure Init_Delegate (Self : not null access Gtk_Entry_Record);

   procedure Insert_Text
      (Self     : not null access Gtk_Entry_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint);

   procedure Select_Region
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activates_Default_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to activate the default widget when Enter is pressed.

   Attributes_Property : constant Glib.Properties.Property_Object;
   --  Type: Pango.Attributes.Pango_Attr_List
   --  A list of Pango attributes to apply to the text of the entry.
   --
   --  This is mainly useful to change the size or weight of the text.
   --
   --  The `PangoAttribute`'s Start_Index and End_Index must refer to the
   --  [classGtk.EntryBuffer] text, i.e. without the preedit string.

   Buffer_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Entry_Buffer.Gtk_Entry_Buffer
   --  The buffer object which actually stores the text.

   Completion_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Entry_Completion.Gtk_Entry_Completion
   --  The auxiliary completion object to use with the entry.

   Enable_Emoji_Completion_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to suggest Emoji replacements for :-delimited names like
   --  `:heart:`.

   Extra_Menu_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.Menu_Model
   --  A menu model whose contents will be appended to the context menu.

   Has_Frame_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the entry should draw a frame.

   Im_Module_Property : constant Glib.Properties.Property_String;
   --  Which IM (input method) module should be used for this entry.
   --
   --  See [classGtk.IMContext].
   --
   --  Setting this to a non-null value overrides the system-wide IM module
   --  setting. See the GtkSettings [propertyGtk.Settings:gtk-im-module]
   --  property.

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;
   --  Additional hints that allow input methods to fine-tune their behavior.
   --
   --  Also see [propertyGtk.Entry:input-purpose]

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;
   --  The purpose of this text field.
   --
   --  This property can be used by on-screen keyboards and other input
   --  methods to adjust their behaviour.
   --
   --  Note that setting the purpose to Gtk.Enums.Input_Purpose_Password or
   --  Gtk.Enums.Input_Purpose_Pin is independent from setting
   --  [propertyGtk.Entry:visibility].

   Invisible_Char_Property : constant Glib.Properties.Property_Uint;
   --  The character to use when masking entry contents ("password mode").

   Invisible_Char_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the invisible char has been set for the `GtkEntry`.

   Max_Length_Property : constant Glib.Properties.Property_Int;
   --  Maximum number of characters for this entry.

   Menu_Entry_Icon_Primary_Text_Property : constant Glib.Properties.Property_String;
   --  Text for an item in the context menu to activate the primary icon
   --  action.
   --
   --  When the primary icon is activatable and this property has been set, a
   --  new entry in the context menu of this GtkEntry will appear with this
   --  text. Selecting that menu entry will result in the primary icon being
   --  activated, exactly in the same way as it would be activated from a mouse
   --  click.
   --
   --  This simplifies adding accessibility support to applications using
   --  activatable icons. The activatable icons aren't focusable when
   --  navigating the interface with the keyboard This is why Gtk recommends to
   --  also add those actions in the context menu. This set of methods greatly
   --  simplifies this, by adding a menu item that, when enabled, calls the
   --  same callback than clicking on the icon.

   Menu_Entry_Icon_Secondary_Text_Property : constant Glib.Properties.Property_String;
   --  Text for an item in the context menu to activate the secondary icon
   --  action.
   --
   --  When the primary icon is activatable and this property has been set, a
   --  new entry in the context menu of this GtkEntry will appear with this
   --  text. Selecting that menu entry will result in the primary icon being
   --  activated, exactly in the same way as it would be activated from a mouse
   --  click.
   --
   --  This simplifies adding accessibility support to applications using
   --  activatable icons. The activatable icons aren't focusable when
   --  navigating the interface with the keyboard This is why Gtk recommends to
   --  also add those actions in the context menu. This set of methods greatly
   --  simplifies this, by adding a menu item that, when enabled, calls the
   --  same callback than clicking on the icon.

   Overwrite_Mode_Property : constant Glib.Properties.Property_Boolean;
   --  If text is overwritten when typing in the `GtkEntry`.

   Placeholder_Text_Property : constant Glib.Properties.Property_String;
   --  The text that will be displayed in the `GtkEntry` when it is empty and
   --  unfocused.

   Primary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the primary icon is activatable.
   --
   --  GTK emits the [signalGtk.Entry::icon-press] and
   --  [signalGtk.Entry::icon-release] signals only on sensitive, activatable
   --  icons.
   --
   --  Sensitive, but non-activatable icons can be used for purely
   --  informational purposes.

   Primary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The `GIcon` to use for the primary icon for the entry.

   Primary_Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The icon name to use for the primary icon for the entry.

   Primary_Icon_Paintable_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gdk.Paintable.Gdk_Paintable
   --  A `GdkPaintable` to use as the primary icon for the entry.

   Primary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the primary icon is sensitive.
   --
   --  An insensitive icon appears grayed out. GTK does not emit the
   --  [signalGtk.Entry::icon-press] and [signalGtk.Entry::icon-release]
   --  signals and does not allow DND from insensitive icons.
   --
   --  An icon should be set insensitive if the action that would trigger when
   --  clicked is currently not available.

   Primary_Icon_Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type;
   --  The representation which is used for the primary icon of the entry.

   Primary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   --  The contents of the tooltip on the primary icon, with markup.
   --
   --  Also see [methodGtk.Entry.set_icon_tooltip_markup].

   Primary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String;
   --  The contents of the tooltip on the primary icon.
   --
   --  Also see [methodGtk.Entry.set_icon_tooltip_text].

   Progress_Fraction_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The current fraction of the task that's been completed.

   Progress_Pulse_Step_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The fraction of total entry width to move the progress bouncing block
   --  for each pulse.
   --
   --  See [methodGtk.Entry.progress_pulse].

   Scroll_Offset_Property : constant Glib.Properties.Property_Int;
   --  Number of pixels of the entry scrolled off the screen to the left.

   Secondary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the secondary icon is activatable.
   --
   --  GTK emits the [signalGtk.Entry::icon-press] and
   --  [signalGtk.Entry::icon-release] signals only on sensitive, activatable
   --  icons.
   --
   --  Sensitive, but non-activatable icons can be used for purely
   --  informational purposes.

   Secondary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The `GIcon` to use for the secondary icon for the entry.

   Secondary_Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The icon name to use for the secondary icon for the entry.

   Secondary_Icon_Paintable_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gdk.Paintable.Gdk_Paintable
   --  A `GdkPaintable` to use as the secondary icon for the entry.

   Secondary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the secondary icon is sensitive.
   --
   --  An insensitive icon appears grayed out. GTK does not emit the
   --  [signalGtk.Entry::icon-press[ and [signalGtk.Entry::icon-release]
   --  signals and does not allow DND from insensitive icons.
   --
   --  An icon should be set insensitive if the action that would trigger when
   --  clicked is currently not available.

   Secondary_Icon_Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type;
   --  The representation which is used for the secondary icon of the entry.

   Secondary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String;
   --  The contents of the tooltip on the secondary icon, with markup.
   --
   --  Also see [methodGtk.Entry.set_icon_tooltip_markup].

   Secondary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String;
   --  The contents of the tooltip on the secondary icon.
   --
   --  Also see [methodGtk.Entry.set_icon_tooltip_text].

   Show_Emoji_Icon_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the entry will show an Emoji icon in the secondary icon
   --  position to open the Emoji chooser.

   Tabs_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Pango.Tab_Array
   --  A list of tabstops to apply to the text of the entry.

   Text_Length_Property : constant Glib.Properties.Property_Uint;
   --  The length of the text in the `GtkEntry`.

   Truncate_Multiline_Property : constant Glib.Properties.Property_Boolean;
   --  When True, pasted multi-line text is truncated to the first line.

   Visibility_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the entry should show the "invisible char" instead of the
   --  actual text ("password mode").

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Entry_Void is not null access procedure (Self : access Gtk_Entry_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the entry is activated.
   --
   --  The keybindings for this signal are all forms of the Enter key.

   type Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void is not null access procedure
     (Self     : access Gtk_Entry_Record'Class;
      Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position);

   type Cb_GObject_Gtk_Entry_Icon_Position_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position);

   Signal_Icon_Press : constant Glib.Signal_Name := "icon-press";
   procedure On_Icon_Press
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After : Boolean := False);
   procedure On_Icon_Press
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an activatable icon is clicked.

   Signal_Icon_Release : constant Glib.Signal_Name := "icon-release";
   procedure On_Icon_Release
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After : Boolean := False);
   procedure On_Icon_Release
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted on the button release from a mouse click over an activatable
   --  icon.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.CellEditable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Editable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Entry_Record, Gtk_Entry);
   function "+"
     (Widget : access Gtk_Entry_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Entry
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Entry_Record, Gtk_Entry);
   function "+"
     (Widget : access Gtk_Entry_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Entry
   renames Implements_Gtk_Constraint_Target.To_Object;

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

private
   Visibility_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visibility");
   Truncate_Multiline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("truncate-multiline");
   Text_Length_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("text-length");
   Tabs_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tabs");
   Show_Emoji_Icon_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-emoji-icon");
   Secondary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-text");
   Secondary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-tooltip-markup");
   Secondary_Icon_Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type :=
     Gtk.Enums.Build ("secondary-icon-storage-type");
   Secondary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-sensitive");
   Secondary_Icon_Paintable_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("secondary-icon-paintable");
   Secondary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-icon-name");
   Secondary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("secondary-icon-gicon");
   Secondary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-icon-activatable");
   Scroll_Offset_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scroll-offset");
   Progress_Pulse_Step_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-pulse-step");
   Progress_Fraction_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("progress-fraction");
   Primary_Icon_Tooltip_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-text");
   Primary_Icon_Tooltip_Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-tooltip-markup");
   Primary_Icon_Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type :=
     Gtk.Enums.Build ("primary-icon-storage-type");
   Primary_Icon_Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-sensitive");
   Primary_Icon_Paintable_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("primary-icon-paintable");
   Primary_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("primary-icon-name");
   Primary_Icon_Gicon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("primary-icon-gicon");
   Primary_Icon_Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("primary-icon-activatable");
   Placeholder_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("placeholder-text");
   Overwrite_Mode_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("overwrite-mode");
   Menu_Entry_Icon_Secondary_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("menu-entry-icon-secondary-text");
   Menu_Entry_Icon_Primary_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("menu-entry-icon-primary-text");
   Max_Length_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("max-length");
   Invisible_Char_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("invisible-char-set");
   Invisible_Char_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("invisible-char");
   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose :=
     Gtk.Enums.Build ("input-purpose");
   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints :=
     Gtk.Enums.Build ("input-hints");
   Im_Module_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("im-module");
   Has_Frame_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-frame");
   Extra_Menu_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("extra-menu");
   Enable_Emoji_Completion_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("enable-emoji-completion");
   Completion_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("completion");
   Buffer_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("buffer");
   Attributes_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attributes");
   Activates_Default_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activates-default");
end Gtk.GEntry;
