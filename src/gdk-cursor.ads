------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
--  This package provides the capability to create predefined mouse cursors
--  as well as user defined ones.
--  </description>
--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>
--  <testgtk>create_cursors.adb</testgtk>

with Glib;

package Gdk.Cursor is

   type Gdk_Cursor is new Gdk.C_Proxy;
   Null_Cursor : constant Gdk_Cursor;

   type Gdk_Cursor_Type is
     (X_Cursor,
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
      Xterm);

   procedure Gdk_New
     (Widget      : out Gdk_Cursor;
      Cursor_Type : Gdk_Cursor_Type);
   --  Create a new standard cursor.

   --  procedure Gdk_New_From_Pixbuf (...)
   --  This function is declared in Gdk.Pixbuf, for dependency circularity
   --  reasons. It can be used to create a cursor directly from a pixbuf.

   procedure Gdk_New
     (Cursor  : out Gdk_Cursor;
      Name    : String);
   --  Create a cursor from a name

   procedure Destroy (Cursor : Gdk_Cursor);
   pragma Obsolescent;  --  Destroy
   --  Destroy a cursor, freeing any resources allocated for it.
   --  Deprecated, use Unref instead.

   procedure Ref (Cursor : Gdk_Cursor);
   --  Increment the reference counting for the cursor.

   procedure Unref (Cursor : Gdk_Cursor);
   --  Decrement the reference counting for the cursor.
   --  When this reaches 0, the cursor is destroyed.

private
   Null_Cursor : constant Gdk_Cursor := null;
   pragma Import (C, Destroy, "gdk_cursor_unref");
   pragma Import (C, Ref, "gdk_cursor_ref");
   pragma Import (C, Unref, "gdk_cursor_unref");

   for Gdk_Cursor_Type'Size use Glib.Gint'Size;
   for Gdk_Cursor_Type use
     (X_Cursor => 0,
      Arrow    => 2,
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
      Xterm => 152);

end Gdk.Cursor;
