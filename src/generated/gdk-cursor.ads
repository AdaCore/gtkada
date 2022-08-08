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
--  A Gdk.Gdk_Cursor represents a cursor. Its contents are private.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk.Display;             use Gdk.Display;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Cursor is

   type Gdk_Cursor_Type is (
      Blank_Cursor,
      Cursor_Is_Pixmap,
      X_Cursor,
      Arrow,
      Based_Arrow_Down,
      Based_Arrow_Up,
      Boat,
      Bogosity,
      Bottom_Left_Corner,
      Bottom_Right_Corner,
      Bottom_Side,
      Bottom_Tee,
      Box_Spiral,
      Center_Ptr,
      Circle,
      Clock,
      Coffee_Mug,
      Cross,
      Cross_Reverse,
      Crosshair,
      Diamond_Cross,
      Dot,
      Dotbox,
      Double_Arrow,
      Draft_Large,
      Draft_Small,
      Draped_Box,
      Exchange,
      Fleur,
      Gobbler,
      Gumby,
      Hand1,
      Hand2,
      Heart,
      Icon,
      Iron_Cross,
      Left_Ptr,
      Left_Side,
      Left_Tee,
      Leftbutton,
      Ll_Angle,
      Lr_Angle,
      Man,
      Middlebutton,
      Mouse,
      Pencil,
      Pirate,
      Plus,
      Question_Arrow,
      Right_Ptr,
      Right_Side,
      Right_Tee,
      Rightbutton,
      Rtl_Logo,
      Sailboat,
      Sb_Down_Arrow,
      Sb_H_Double_Arrow,
      Sb_Left_Arrow,
      Sb_Right_Arrow,
      Sb_Up_Arrow,
      Sb_V_Double_Arrow,
      Shuttle,
      Sizing,
      Spider,
      Spraycan,
      Star,
      Target,
      Tcross,
      Top_Left_Arrow,
      Top_Left_Corner,
      Top_Right_Corner,
      Top_Side,
      Top_Tee,
      Trek,
      Ul_Angle,
      Umbrella,
      Ur_Angle,
      Watch,
      Xterm,
      Last_Cursor);
   pragma Convention (C, Gdk_Cursor_Type);
   --  Predefined cursors.
   --
   --  Note that these IDs are directly taken from the X cursor font, and many
   --  of these cursors are either not useful, or are not available on other
   --  platforms.
   --
   --  The recommended way to create cursors is to use
   --  gdk_cursor_new_from_name.

   for Gdk_Cursor_Type use (
      Blank_Cursor => -2,
      Cursor_Is_Pixmap => -1,
      X_Cursor => 0,
      Arrow => 2,
      Based_Arrow_Down => 4,
      Based_Arrow_Up => 6,
      Boat => 8,
      Bogosity => 10,
      Bottom_Left_Corner => 12,
      Bottom_Right_Corner => 14,
      Bottom_Side => 16,
      Bottom_Tee => 18,
      Box_Spiral => 20,
      Center_Ptr => 22,
      Circle => 24,
      Clock => 26,
      Coffee_Mug => 28,
      Cross => 30,
      Cross_Reverse => 32,
      Crosshair => 34,
      Diamond_Cross => 36,
      Dot => 38,
      Dotbox => 40,
      Double_Arrow => 42,
      Draft_Large => 44,
      Draft_Small => 46,
      Draped_Box => 48,
      Exchange => 50,
      Fleur => 52,
      Gobbler => 54,
      Gumby => 56,
      Hand1 => 58,
      Hand2 => 60,
      Heart => 62,
      Icon => 64,
      Iron_Cross => 66,
      Left_Ptr => 68,
      Left_Side => 70,
      Left_Tee => 72,
      Leftbutton => 74,
      Ll_Angle => 76,
      Lr_Angle => 78,
      Man => 80,
      Middlebutton => 82,
      Mouse => 84,
      Pencil => 86,
      Pirate => 88,
      Plus => 90,
      Question_Arrow => 92,
      Right_Ptr => 94,
      Right_Side => 96,
      Right_Tee => 98,
      Rightbutton => 100,
      Rtl_Logo => 102,
      Sailboat => 104,
      Sb_Down_Arrow => 106,
      Sb_H_Double_Arrow => 108,
      Sb_Left_Arrow => 110,
      Sb_Right_Arrow => 112,
      Sb_Up_Arrow => 114,
      Sb_V_Double_Arrow => 116,
      Shuttle => 118,
      Sizing => 120,
      Spider => 122,
      Spraycan => 124,
      Star => 126,
      Target => 128,
      Tcross => 130,
      Top_Left_Arrow => 132,
      Top_Left_Corner => 134,
      Top_Right_Corner => 136,
      Top_Side => 138,
      Top_Tee => 140,
      Trek => 142,
      Ul_Angle => 144,
      Umbrella => 146,
      Ur_Angle => 148,
      Watch => 150,
      Xterm => 152,
      Last_Cursor => 153);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Cursor_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Cursor_Type);
   type Property_Gdk_Cursor_Type is new Gdk_Cursor_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Gdk_Cursor; Cursor_Type : Gdk_Cursor_Type);
   --  Creates a new cursor from the set of builtin cursors for the default
   --  display. See gdk_cursor_new_for_display.
   --  To make the cursor invisible, use Gdk.Blank_Cursor.
   --  "cursor_type": cursor to create

   function Gdk_Cursor_New (Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
   --  Creates a new cursor from the set of builtin cursors for the default
   --  display. See gdk_cursor_new_for_display.
   --  To make the cursor invisible, use Gdk.Blank_Cursor.
   --  "cursor_type": cursor to create

   procedure Gdk_New_For_Display
      (Self        : out Gdk_Cursor;
       Display     : not null access Gdk.Display.Gdk_Display_Record'Class;
       Cursor_Type : Gdk_Cursor_Type);
   --  Creates a new cursor from the set of builtin cursors.
   --  Since: gtk+ 2.2
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "cursor_type": cursor to create

   function Gdk_Cursor_New_For_Display
      (Display     : not null access Gdk.Display.Gdk_Display_Record'Class;
       Cursor_Type : Gdk_Cursor_Type) return Gdk_Cursor;
   --  Creates a new cursor from the set of builtin cursors.
   --  Since: gtk+ 2.2
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "cursor_type": cursor to create

   procedure Gdk_New_From_Name
      (Self    : out Gdk_Cursor;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Name    : UTF8_String);
   --  Creates a new cursor by looking up Name in the current cursor theme.
   --  A recommended set of cursor names that will work across different
   --  platforms can be found in the CSS specification: - "none" -
   --  ![](default_cursor.png) "default" - ![](help_cursor.png) "help" -
   --  ![](pointer_cursor.png) "pointer" - ![](context_menu_cursor.png)
   --  "context-menu" - ![](progress_cursor.png) "progress" -
   --  ![](wait_cursor.png) "wait" - ![](cell_cursor.png) "cell" -
   --  ![](crosshair_cursor.png) "crosshair" - ![](text_cursor.png) "text" -
   --  ![](vertical_text_cursor.png) "vertical-text" - ![](alias_cursor.png)
   --  "alias" - ![](copy_cursor.png) "copy" - ![](no_drop_cursor.png)
   --  "no-drop" - ![](move_cursor.png) "move" - ![](not_allowed_cursor.png)
   --  "not-allowed" - ![](grab_cursor.png) "grab" - ![](grabbing_cursor.png)
   --  "grabbing" - ![](all_scroll_cursor.png) "all-scroll" -
   --  ![](col_resize_cursor.png) "col-resize" - ![](row_resize_cursor.png)
   --  "row-resize" - ![](n_resize_cursor.png) "n-resize" -
   --  ![](e_resize_cursor.png) "e-resize" - ![](s_resize_cursor.png)
   --  "s-resize" - ![](w_resize_cursor.png) "w-resize" -
   --  ![](ne_resize_cursor.png) "ne-resize" - ![](nw_resize_cursor.png)
   --  "nw-resize" - ![](sw_resize_cursor.png) "sw-resize" -
   --  ![](se_resize_cursor.png) "se-resize" - ![](ew_resize_cursor.png)
   --  "ew-resize" - ![](ns_resize_cursor.png) "ns-resize" -
   --  ![](nesw_resize_cursor.png) "nesw-resize" - ![](nwse_resize_cursor.png)
   --  "nwse-resize" - ![](zoom_in_cursor.png) "zoom-in" -
   --  ![](zoom_out_cursor.png) "zoom-out"
   --  Since: gtk+ 2.8
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "name": the name of the cursor

   function Gdk_Cursor_New_From_Name
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Name    : UTF8_String) return Gdk_Cursor;
   --  Creates a new cursor by looking up Name in the current cursor theme.
   --  A recommended set of cursor names that will work across different
   --  platforms can be found in the CSS specification: - "none" -
   --  ![](default_cursor.png) "default" - ![](help_cursor.png) "help" -
   --  ![](pointer_cursor.png) "pointer" - ![](context_menu_cursor.png)
   --  "context-menu" - ![](progress_cursor.png) "progress" -
   --  ![](wait_cursor.png) "wait" - ![](cell_cursor.png) "cell" -
   --  ![](crosshair_cursor.png) "crosshair" - ![](text_cursor.png) "text" -
   --  ![](vertical_text_cursor.png) "vertical-text" - ![](alias_cursor.png)
   --  "alias" - ![](copy_cursor.png) "copy" - ![](no_drop_cursor.png)
   --  "no-drop" - ![](move_cursor.png) "move" - ![](not_allowed_cursor.png)
   --  "not-allowed" - ![](grab_cursor.png) "grab" - ![](grabbing_cursor.png)
   --  "grabbing" - ![](all_scroll_cursor.png) "all-scroll" -
   --  ![](col_resize_cursor.png) "col-resize" - ![](row_resize_cursor.png)
   --  "row-resize" - ![](n_resize_cursor.png) "n-resize" -
   --  ![](e_resize_cursor.png) "e-resize" - ![](s_resize_cursor.png)
   --  "s-resize" - ![](w_resize_cursor.png) "w-resize" -
   --  ![](ne_resize_cursor.png) "ne-resize" - ![](nw_resize_cursor.png)
   --  "nw-resize" - ![](sw_resize_cursor.png) "sw-resize" -
   --  ![](se_resize_cursor.png) "se-resize" - ![](ew_resize_cursor.png)
   --  "ew-resize" - ![](ns_resize_cursor.png) "ns-resize" -
   --  ![](nesw_resize_cursor.png) "nesw-resize" - ![](nwse_resize_cursor.png)
   --  "nwse-resize" - ![](zoom_in_cursor.png) "zoom-in" -
   --  ![](zoom_out_cursor.png) "zoom-out"
   --  Since: gtk+ 2.8
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "name": the name of the cursor

   procedure Gdk_New_From_Surface
      (Self    : out Gdk_Cursor;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble);
   --  Creates a new cursor from a cairo image surface.
   --  Not all GDK backends support RGBA cursors. If they are not supported, a
   --  monochrome approximation will be displayed. The functions
   --  Gdk.Display.Supports_Cursor_Alpha and Gdk.Display.Supports_Cursor_Color
   --  can be used to determine whether RGBA cursors are supported;
   --  Gdk.Display.Get_Default_Cursor_Size and
   --  Gdk.Display.Get_Maximal_Cursor_Size give information about cursor sizes.
   --  On the X backend, support for RGBA cursors requires a sufficently new
   --  version of the X Render extension.
   --  Since: gtk+ 3.10
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "surface": the cairo image surface containing the cursor pixel data
   --  "x": the horizontal offset of the "hotspot" of the cursor
   --  "y": the vertical offset of the "hotspot" of the cursor

   function Gdk_Cursor_New_From_Surface
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble) return Gdk_Cursor;
   --  Creates a new cursor from a cairo image surface.
   --  Not all GDK backends support RGBA cursors. If they are not supported, a
   --  monochrome approximation will be displayed. The functions
   --  Gdk.Display.Supports_Cursor_Alpha and Gdk.Display.Supports_Cursor_Color
   --  can be used to determine whether RGBA cursors are supported;
   --  Gdk.Display.Get_Default_Cursor_Size and
   --  Gdk.Display.Get_Maximal_Cursor_Size give information about cursor sizes.
   --  On the X backend, support for RGBA cursors requires a sufficently new
   --  version of the X Render extension.
   --  Since: gtk+ 3.10
   --  "display": the Gdk.Display.Gdk_Display for which the cursor will be
   --  created
   --  "surface": the cairo image surface containing the cursor pixel data
   --  "x": the horizontal offset of the "hotspot" of the cursor
   --  "y": the vertical offset of the "hotspot" of the cursor

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_cursor_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Cursor_Type (Self : Gdk.Gdk_Cursor) return Gdk_Cursor_Type;
   pragma Import (C, Get_Cursor_Type, "gdk_cursor_get_cursor_type");
   --  Returns the cursor type for this cursor.
   --  Since: gtk+ 2.22

   function Get_Display
      (Self : Gdk.Gdk_Cursor) return Gdk.Display.Gdk_Display;
   --  Returns the display on which the Gdk.Gdk_Cursor is defined.
   --  Since: gtk+ 2.2

   procedure Get_Surface
      (Self    : Gdk.Gdk_Cursor;
       X_Hot   : out Gdouble;
       Y_Hot   : out Gdouble;
       Surface : out Cairo.Cairo_Surface);
   --  Returns a cairo image surface with the image used to display the
   --  cursor.
   --  Note that depending on the capabilities of the windowing system and on
   --  the cursor, GDK may not be able to obtain the image data. In this case,
   --  null is returned.
   --  Since: gtk+ 3.10
   --  "x_hot": Location to store the hotspot x position, or null
   --  "y_hot": Location to store the hotspot y position, or null

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Ref (Self : Gdk.Gdk_Cursor);
   pragma Import (C, Ref, "g_object_ref");

   procedure Unref (Self : Gdk.Gdk_Cursor);
   pragma Import (C, Unref, "g_object_unref");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cursor_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cursor_Type

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Cursor_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("cursor-type");
end Gdk.Cursor;
