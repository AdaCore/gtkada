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

--  Used to create and destroy cursors.
--
--  Cursors are immutable objects, so once you created them, there is no way
--  to modify them later. You should create a new cursor when you want to
--  change something about it.
--
--  Cursors by themselves are not very interesting: they must be bound to a
--  window for users to see them. This is done with
--  [methodGdk.Surface.set_cursor] or [methodGdk.Surface.set_device_cursor].
--  Applications will typically use higher-level GTK functions such as
--  [gtk_widget_set_cursor](../gtk4/method.Widget.set_cursor.html) instead.
--
--  Cursors are not bound to a given [classGdk.Display], so they can be
--  shared. However, the appearance of cursors may vary when used on different
--  platforms.
--
--  ## Named and texture cursors
--
--  There are multiple ways to create cursors. The platform's own cursors can
--  be created with [ctorGdk.Cursor.new_from_name]. That function lists the
--  commonly available names that are shared with the CSS specification. Other
--  names may be available, depending on the platform in use. On some
--  platforms, what images are used for named cursors may be influenced by the
--  cursor theme.
--
--  Another option to create a cursor is to use
--  [ctorGdk.Cursor.new_from_texture] and provide an image to use for the
--  cursor.
--
--  To ease work with unsupported cursors, a fallback cursor can be provided.
--  If a [classGdk.Surface] cannot use a cursor because of the reasons
--  mentioned above, it will try the fallback cursor. Fallback cursors can
--  themselves have fallback cursors again, so it is possible to provide a
--  chain of progressively easier to support cursors. If none of the provided
--  cursors can be supported, the default cursor will be the ultimate fallback.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Cursor is

   type Gdk_Cursor_Record is new GObject_Record with null record;
   type Gdk_Cursor is access all Gdk_Cursor_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New_From_Name
      (Self     : out Gdk_Cursor;
       Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class);
   --  Creates a new cursor by looking up Name in the current cursor theme.
   --  A recommended set of cursor names that will work across different
   --  platforms can be found in the CSS specification:
   --  | | | | | --- | --- | --- | | | "none" | No cursor | |
   --  ![](default_cursor.png) | "default" | The default cursor | |
   --  ![](help_cursor.png) | "help" | Help is available | |
   --  ![](pointer_cursor.png) | "pointer" | Indicates a link or interactive
   --  element | | ![](context_menu_cursor.png) |"context-menu" | A context
   --  menu is available | | ![](progress_cursor.png) | "progress" | Progress
   --  indicator | | ![](wait_cursor.png) | "wait" | Busy cursor | |
   --  ![](cell_cursor.png) | "cell" | Cell(s) may be selected | |
   --  ![](crosshair_cursor.png) | "crosshair" | Simple crosshair | |
   --  ![](text_cursor.png) | "text" | Text may be selected | |
   --  ![](vertical_text_cursor.png) | "vertical-text" | Vertical text may be
   --  selected | | ![](alias_cursor.png) | "alias" | DND: Something will be
   --  linked | | ![](copy_cursor.png) | "copy" | DND: Something will be copied
   --  | | ![](move_cursor.png) | "move" | DND: Something will be moved | |
   --  ![](dnd_ask_cursor.png) | "dnd-ask" | DND: User can choose action to be
   --  carried out | | ![](no_drop_cursor.png) | "no-drop" | DND: Can't drop
   --  here | | ![](not_allowed_cursor.png) | "not-allowed" | DND: Action will
   --  not be carried out | | ![](grab_cursor.png) | "grab" | DND: Something
   --  can be grabbed | | ![](grabbing_cursor.png) | "grabbing" | DND:
   --  Something is being grabbed | | ![](n_resize_cursor.png) | "n-resize" |
   --  Resizing: Move north border | | ![](e_resize_cursor.png) | "e-resize" |
   --  Resizing: Move east border | | ![](s_resize_cursor.png) | "s-resize" |
   --  Resizing: Move south border | | ![](w_resize_cursor.png) | "w-resize" |
   --  Resizing: Move west border | | ![](ne_resize_cursor.png) | "ne-resize" |
   --  Resizing: Move north-east corner | | ![](nw_resize_cursor.png) |
   --  "nw-resize" | Resizing: Move north-west corner | |
   --  ![](sw_resize_cursor.png) | "sw-resize" | Resizing: Move south-west
   --  corner | | ![](se_resize_cursor.png) | "se-resize" | Resizing: Move
   --  south-east corner | | ![](col_resize_cursor.png) | "col-resize" |
   --  Resizing: Move an item or border horizontally | |
   --  ![](row_resize_cursor.png) | "row-resize" | Resizing: Move an item or
   --  border vertically | | ![](ew_resize_cursor.png) | "ew-resize" | Moving:
   --  Something can be moved horizontally | | ![](ns_resize_cursor.png) |
   --  "ns-resize" | Moving: Something can be moved vertically | |
   --  ![](nesw_resize_cursor.png) | "nesw-resize" | Moving: Something can be
   --  moved diagonally, north-east to south-west | |
   --  ![](nwse_resize_cursor.png) | "nwse-resize" | Moving: something can be
   --  moved diagonally, north-west to south-east | |
   --  ![](all_resize_cursor.png) | "all-resize" | Moving: Something can be
   --  moved in any direction | | ![](all_scroll_cursor.png) | "all-scroll" |
   --  Can scroll in any direction | | ![](zoom_in_cursor.png) | "zoom-in" |
   --  Zoom in | | ![](zoom_out_cursor.png) | "zoom-out" | Zoom out |
   --  @param Name the name of the cursor
   --  @param Fallback null or the `GdkCursor` to fall back to when this one
   --  cannot be supported

   procedure Initialize_From_Name
      (Self     : not null access Gdk_Cursor_Record'Class;
       Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class);
   --  Creates a new cursor by looking up Name in the current cursor theme.
   --  A recommended set of cursor names that will work across different
   --  platforms can be found in the CSS specification:
   --  | | | | | --- | --- | --- | | | "none" | No cursor | |
   --  ![](default_cursor.png) | "default" | The default cursor | |
   --  ![](help_cursor.png) | "help" | Help is available | |
   --  ![](pointer_cursor.png) | "pointer" | Indicates a link or interactive
   --  element | | ![](context_menu_cursor.png) |"context-menu" | A context
   --  menu is available | | ![](progress_cursor.png) | "progress" | Progress
   --  indicator | | ![](wait_cursor.png) | "wait" | Busy cursor | |
   --  ![](cell_cursor.png) | "cell" | Cell(s) may be selected | |
   --  ![](crosshair_cursor.png) | "crosshair" | Simple crosshair | |
   --  ![](text_cursor.png) | "text" | Text may be selected | |
   --  ![](vertical_text_cursor.png) | "vertical-text" | Vertical text may be
   --  selected | | ![](alias_cursor.png) | "alias" | DND: Something will be
   --  linked | | ![](copy_cursor.png) | "copy" | DND: Something will be copied
   --  | | ![](move_cursor.png) | "move" | DND: Something will be moved | |
   --  ![](dnd_ask_cursor.png) | "dnd-ask" | DND: User can choose action to be
   --  carried out | | ![](no_drop_cursor.png) | "no-drop" | DND: Can't drop
   --  here | | ![](not_allowed_cursor.png) | "not-allowed" | DND: Action will
   --  not be carried out | | ![](grab_cursor.png) | "grab" | DND: Something
   --  can be grabbed | | ![](grabbing_cursor.png) | "grabbing" | DND:
   --  Something is being grabbed | | ![](n_resize_cursor.png) | "n-resize" |
   --  Resizing: Move north border | | ![](e_resize_cursor.png) | "e-resize" |
   --  Resizing: Move east border | | ![](s_resize_cursor.png) | "s-resize" |
   --  Resizing: Move south border | | ![](w_resize_cursor.png) | "w-resize" |
   --  Resizing: Move west border | | ![](ne_resize_cursor.png) | "ne-resize" |
   --  Resizing: Move north-east corner | | ![](nw_resize_cursor.png) |
   --  "nw-resize" | Resizing: Move north-west corner | |
   --  ![](sw_resize_cursor.png) | "sw-resize" | Resizing: Move south-west
   --  corner | | ![](se_resize_cursor.png) | "se-resize" | Resizing: Move
   --  south-east corner | | ![](col_resize_cursor.png) | "col-resize" |
   --  Resizing: Move an item or border horizontally | |
   --  ![](row_resize_cursor.png) | "row-resize" | Resizing: Move an item or
   --  border vertically | | ![](ew_resize_cursor.png) | "ew-resize" | Moving:
   --  Something can be moved horizontally | | ![](ns_resize_cursor.png) |
   --  "ns-resize" | Moving: Something can be moved vertically | |
   --  ![](nesw_resize_cursor.png) | "nesw-resize" | Moving: Something can be
   --  moved diagonally, north-east to south-west | |
   --  ![](nwse_resize_cursor.png) | "nwse-resize" | Moving: something can be
   --  moved diagonally, north-west to south-east | |
   --  ![](all_resize_cursor.png) | "all-resize" | Moving: Something can be
   --  moved in any direction | | ![](all_scroll_cursor.png) | "all-scroll" |
   --  Can scroll in any direction | | ![](zoom_in_cursor.png) | "zoom-in" |
   --  Zoom in | | ![](zoom_out_cursor.png) | "zoom-out" | Zoom out |
   --  Initialize_From_Name does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Name the name of the cursor
   --  @param Fallback null or the `GdkCursor` to fall back to when this one
   --  cannot be supported

   function Gdk_Cursor_New_From_Name
      (Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class) return Gdk_Cursor;
   --  Creates a new cursor by looking up Name in the current cursor theme.
   --  A recommended set of cursor names that will work across different
   --  platforms can be found in the CSS specification:
   --  | | | | | --- | --- | --- | | | "none" | No cursor | |
   --  ![](default_cursor.png) | "default" | The default cursor | |
   --  ![](help_cursor.png) | "help" | Help is available | |
   --  ![](pointer_cursor.png) | "pointer" | Indicates a link or interactive
   --  element | | ![](context_menu_cursor.png) |"context-menu" | A context
   --  menu is available | | ![](progress_cursor.png) | "progress" | Progress
   --  indicator | | ![](wait_cursor.png) | "wait" | Busy cursor | |
   --  ![](cell_cursor.png) | "cell" | Cell(s) may be selected | |
   --  ![](crosshair_cursor.png) | "crosshair" | Simple crosshair | |
   --  ![](text_cursor.png) | "text" | Text may be selected | |
   --  ![](vertical_text_cursor.png) | "vertical-text" | Vertical text may be
   --  selected | | ![](alias_cursor.png) | "alias" | DND: Something will be
   --  linked | | ![](copy_cursor.png) | "copy" | DND: Something will be copied
   --  | | ![](move_cursor.png) | "move" | DND: Something will be moved | |
   --  ![](dnd_ask_cursor.png) | "dnd-ask" | DND: User can choose action to be
   --  carried out | | ![](no_drop_cursor.png) | "no-drop" | DND: Can't drop
   --  here | | ![](not_allowed_cursor.png) | "not-allowed" | DND: Action will
   --  not be carried out | | ![](grab_cursor.png) | "grab" | DND: Something
   --  can be grabbed | | ![](grabbing_cursor.png) | "grabbing" | DND:
   --  Something is being grabbed | | ![](n_resize_cursor.png) | "n-resize" |
   --  Resizing: Move north border | | ![](e_resize_cursor.png) | "e-resize" |
   --  Resizing: Move east border | | ![](s_resize_cursor.png) | "s-resize" |
   --  Resizing: Move south border | | ![](w_resize_cursor.png) | "w-resize" |
   --  Resizing: Move west border | | ![](ne_resize_cursor.png) | "ne-resize" |
   --  Resizing: Move north-east corner | | ![](nw_resize_cursor.png) |
   --  "nw-resize" | Resizing: Move north-west corner | |
   --  ![](sw_resize_cursor.png) | "sw-resize" | Resizing: Move south-west
   --  corner | | ![](se_resize_cursor.png) | "se-resize" | Resizing: Move
   --  south-east corner | | ![](col_resize_cursor.png) | "col-resize" |
   --  Resizing: Move an item or border horizontally | |
   --  ![](row_resize_cursor.png) | "row-resize" | Resizing: Move an item or
   --  border vertically | | ![](ew_resize_cursor.png) | "ew-resize" | Moving:
   --  Something can be moved horizontally | | ![](ns_resize_cursor.png) |
   --  "ns-resize" | Moving: Something can be moved vertically | |
   --  ![](nesw_resize_cursor.png) | "nesw-resize" | Moving: Something can be
   --  moved diagonally, north-east to south-west | |
   --  ![](nwse_resize_cursor.png) | "nwse-resize" | Moving: something can be
   --  moved diagonally, north-west to south-east | |
   --  ![](all_resize_cursor.png) | "all-resize" | Moving: Something can be
   --  moved in any direction | | ![](all_scroll_cursor.png) | "all-scroll" |
   --  Can scroll in any direction | | ![](zoom_in_cursor.png) | "zoom-in" |
   --  Zoom in | | ![](zoom_out_cursor.png) | "zoom-out" | Zoom out |
   --  @param Name the name of the cursor
   --  @param Fallback null or the `GdkCursor` to fall back to when this one
   --  cannot be supported

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_cursor_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Fallback
      (Self : not null access Gdk_Cursor_Record) return Gdk_Cursor;
   --  Returns the fallback for this Cursor.
   --  The fallback will be used if this cursor is not available on a given
   --  `GdkDisplay`. For named cursors, this can happen when using nonstandard
   --  names or when using an incomplete cursor theme. For textured cursors,
   --  this can happen when the texture is too large or when the `GdkDisplay`
   --  it is used on does not support textured cursors.
   --  @return the fallback of the cursor or null to use the default cursor as
   --  fallback

   function Get_Hotspot_X
      (Self : not null access Gdk_Cursor_Record) return Glib.Gint;
   --  Returns the horizontal offset of the hotspot.
   --  The hotspot indicates the pixel that will be directly above the cursor.
   --  Note that named cursors may have a nonzero hotspot, but this function
   --  will only return the hotspot position for cursors created with
   --  [ctorGdk.Cursor.new_from_texture].
   --  @return the horizontal offset of the hotspot or 0 for named cursors

   function Get_Hotspot_Y
      (Self : not null access Gdk_Cursor_Record) return Glib.Gint;
   --  Returns the vertical offset of the hotspot.
   --  The hotspot indicates the pixel that will be directly above the cursor.
   --  Note that named cursors may have a nonzero hotspot, but this function
   --  will only return the hotspot position for cursors created with
   --  [ctorGdk.Cursor.new_from_texture].
   --  @return the vertical offset of the hotspot or 0 for named cursors

   function Get_Name
      (Self : not null access Gdk_Cursor_Record) return UTF8_String;
   --  Returns the name of the cursor.
   --  If the cursor is not a named cursor, null will be returned.
   --  @return the name of the cursor or null if it is not a named cursor

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Fallback_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cursor
   --  Cursor to fall back to if this cursor cannot be displayed.

   Hotspot_X_Property : constant Glib.Properties.Property_Int;
   --  X position of the cursor hotspot in the cursor image.

   Hotspot_Y_Property : constant Glib.Properties.Property_Int;
   --  Y position of the cursor hotspot in the cursor image.

   Name_Property : constant Glib.Properties.Property_String;
   --  Name of this this cursor.
   --
   --  The name will be null if the cursor was created from a texture.

   Texture_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Texture
   --  The texture displayed by this cursor.
   --
   --  The texture will be null if the cursor was created from a name.

private
   Texture_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("texture");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Hotspot_Y_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("hotspot-y");
   Hotspot_X_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("hotspot-x");
   Fallback_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("fallback");
end Gdk.Cursor;
