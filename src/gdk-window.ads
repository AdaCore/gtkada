-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

--  A Gdk_Window is the physical window that appears on the screen.
--  This is the low-level structure known to the X server or to Win32.
--  All the widgets are internally associated with a specific Gdk_Window,
--  that holds attributes such as the window decoration, whether the window
--  can be interactively resized,...
--
--  On some systems, the graphic server knows how to display non-rectangular
--  windows (this is part of the X extensions).
--
--  If you simply want to create a simply window, you should instead look
--  at the functions provided in Gtk.Window and Gtk.Widget, which are higher
--  level than these. See also the function Gtk.Widget.Get_Window to get the
--  Gdk_Window associated with a widget. Be aware that some of them don't have
--  such windows!
--
--  Scrolling can be implemented in several ways with GtkAda (toplevel
--  scrolling should be done with the Gtk_Scrolled_Window widget, but you
--  might want to handle scrolling yourself). See the function
--  Gdk.Event.Get_Graphics_Expose for more information.
--  <c_version>1.2.7</c_version>

with System;
with Glib; use Glib;
with Glib.Glist;
with Gdk;
with Gdk.Color;
with Gdk.Cursor;
with Gdk.Types;
with Gdk.Visual;
with Gdk.Window_Attr;
with Unchecked_Conversion;

package Gdk.Window is

   subtype Gdk_Window is Gdk.Gdk_Window;

   Null_Window : constant Gdk_Window;

   procedure Gdk_New
     (Window          : out Gdk_Window;
      Parent          : Gdk_Window;
      Attributes      : Gdk.Window_Attr.Gdk_Window_Attr;
      Attributes_Mask : Gdk.Types.Gdk_Window_Attributes_Type);

   procedure Foreign_New (Window : out Gdk_Window; An_Id : Guint32);

   procedure Destroy (Window : in out Gdk_Window);
   --  Destroy a window and all its children.

   procedure Ref (Window : Gdk_Window);
   --  Increment the reference counter associated with window.

   procedure Unref (Window : Gdk_Window);
   --  Decrement the reference counter associated with window.

   procedure Window_At_Pointer
     (Win_X  : out Gint;
      Win_Y  : out Gint;
      Window : out Gdk_Window);
   --  Return the window and the coordinates corresponding to the current
   --  position of the cursor.

   procedure Show (Window : Gdk_Window);

   procedure Hide (Window : Gdk_Window);

   procedure Withdraw (Window : Gdk_Window);

   procedure Move
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint);

   procedure Resize
     (Window : Gdk_Window;
      Width  : Gint;
      Height : Gint);

   procedure Move_Resize
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);

   procedure Reparent
     (Window     : Gdk_Window;
      New_Parent : Gdk_Window;
      X          : Gint;
      Y          : Gint);

   procedure Clear (Window : Gdk_Window);

   procedure Clear_Area
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   --  Does not generate an expose event.

   procedure Clear_Area_E
     (Window : Gdk_Window;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint);
   --  Same as Clear_Area, but generates an expose event.

   procedure Copy_Area
     (Window        : Gdk_Window;
      Gc            : Gdk.Gdk_GC;
      X             : Gint;
      Y             : Gint;
      Source_Window : Gdk_Window;
      Source_X      : Gint;
      Source_Y      : Gint;
      Width         : Gint;
      Height        : Gint);

   procedure Gdk_Raise (Window : Gdk_Window);

   procedure Lower (Window : Gdk_Window);

   procedure Set_Override_Redirect
     (Window            : Gdk_Window;
      Override_Redirect : Boolean := True);

   procedure Set_Child_Shapes (Window : Gdk_Window);

   procedure Merge_Child_Shapes (Window : Gdk_Window);

   function Is_Visible (Window : Gdk_Window) return Boolean;

   function Is_Viewable (Window : Gdk_Window) return Boolean;

   procedure Set_Hints
     (Window     : Gdk_Window;
      X          : Gint;
      Y          : Gint;
      Min_Width  : Gint;
      Min_Height : Gint;
      Max_Width  : Gint;
      Max_Height : Gint;
      Flags      : Gdk.Types.Gdk_Window_Hints);

   procedure Set_Geometry_Hints
     (Window   : Gdk_Window;
      Geometry : in out Gdk.Types.Gdk_Geometry;
      Flags    : Gdk.Types.Gdk_Window_Hints);

   procedure Set_Title (Window : Gdk_Window; Title : String);

   procedure Set_Role (Window : Gdk_Window; Role : String);

   procedure Set_Transient_For
     (Window : Gdk_Window; Leader : Gdk_Window);

   procedure Set_Background
     (Window : Gdk_Window; Color : Gdk.Color.Gdk_Color);

   procedure Set_Back_Pixmap
     (Window          : Gdk_Window;
      Pixmap          : Gdk.Gdk_Pixmap;
      Parent_Relative : Boolean);

   procedure Set_Cursor
     (Window : Gdk_Window; Cursor : Gdk.Cursor.Gdk_Cursor);
   --  Note: the window must be realized first, ie have an associated X11/Win32
   --  window.

   procedure Set_Colormap
     (Window : Gdk_Window; Colormap : Gdk.Color.Gdk_Colormap);

   procedure Get_Geometry
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint;
      Width  : out Gint;
      Height : out Gint;
      Depth  : out Gint);
   --  You can get the size of the root window (ie the size of the screen)
   --  simply by giving Null_Window as the first argument to this procedure.

   procedure Get_Position
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint);

   procedure Get_Size
     (Window : Gdk_Window;
      Width  : out Gint;
      Height : out Gint);

   function Get_Visual (Window : Gdk_Window) return Gdk.Visual.Gdk_Visual;

   function Get_Colormap (Window : Gdk_Window) return Gdk.Color.Gdk_Colormap;

   function Get_Type (Window : Gdk_Window) return Gdk.Types.Gdk_Window_Type;

   procedure Get_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean);

   procedure Get_Desk_Relative_Origin
     (Window  : Gdk_Window;
      X       : out Gint;
      Y       : out Gint;
      Success : out Boolean);

   procedure Get_Root_Origin
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint);

   procedure Get_Pointer
     (Window : Gdk_Window;
      X      : out Gint;
      Y      : out Gint;
      Mask   : out Gdk.Types.Gdk_Modifier_Type;
      Result : out Gdk_Window);

   function Get_Parent (Window : Gdk_Window) return Gdk_Window;

   function Get_Toplevel (Window : Gdk_Window) return Gdk_Window;

   function Get_Events (Window : Gdk_Window) return Gdk.Types.Gdk_Event_Mask;

   procedure Set_Events
     (Window : Gdk_Window; Event_Mask : Gdk.Types.Gdk_Event_Mask);

   procedure Set_Icon
     (Window      : Gdk_Window;
      Icon_Window : Gdk_Window;
      Pixmap      : Gdk_Pixmap;
      Mask        : Gdk_Bitmap);

   procedure Set_Icon_Name
     (Window : Gdk_Window;
      Name   : String);

   procedure Set_Group (Window : Gdk_Window; Leader : Gdk_Window);

   procedure Set_Decorations
     (Window      : Gdk_Window;
      Decorations : Gdk.Types.Gdk_Wm_Decoration);

   procedure Set_Functions
     (Window    : Gdk_Window;
      Functions : Gdk.Types.Gdk_Wm_Function);

   --  Gdk_Window_List
   --
   function Convert is new Unchecked_Conversion (Gdk_Window, System.Address);
   function Convert is new Unchecked_Conversion (System.Address, Gdk_Window);

   package Gdk_Window_List is new
     Glib.Glist.Generic_List (Gpointer => Gdk_Window);

   function Get_Children
     (Window : Gdk_Window) return Gdk_Window_List.Glist;

   function Get_Toplevels return Gdk_Window_List.Glist;

private

   Null_Window : constant Gdk_Window := null;
   pragma Import (C, Clear, "gdk_window_clear");
   pragma Import (C, Clear_Area, "gdk_window_clear_area");
   pragma Import (C, Clear_Area_E, "gdk_window_clear_area_e");
   pragma Import (C, Copy_Area, "gdk_window_copy_area");
   pragma Import (C, Get_Events, "gdk_window_get_events");
   pragma Import (C, Get_Geometry, "gdk_window_get_geometry");
   pragma Import (C, Get_Parent, "gdk_window_get_parent");
   pragma Import (C, Get_Position, "gdk_window_get_position");
   pragma Import (C, Get_Root_Origin, "gdk_window_get_root_origin");
   pragma Import (C, Get_Size, "gdk_window_get_size");
   pragma Import (C, Get_Toplevel, "gdk_window_get_toplevel");
   pragma Import (C, Get_Type, "gdk_window_get_type");
   pragma Import (C, Hide, "gdk_window_hide");
   pragma Import (C, Lower, "gdk_window_lower");
   pragma Import (C, Merge_Child_Shapes, "gdk_window_merge_child_shapes");
   pragma Import (C, Move, "gdk_window_move");
   pragma Import (C, Move_Resize, "gdk_window_move_resize");
   pragma Import (C, Gdk_Raise, "gdk_window_raise");
   pragma Import (C, Ref, "gdk_window_ref");
   pragma Import (C, Reparent, "gdk_window_reparent");
   pragma Import (C, Resize, "gdk_window_resize");
   pragma Import (C, Set_Child_Shapes, "gdk_window_set_child_shapes");
   pragma Import (C, Set_Decorations, "gdk_window_set_decorations");
   pragma Import (C, Set_Events, "gdk_window_set_events");
   pragma Import (C, Set_Functions, "gdk_window_set_functions");
   pragma Import (C, Set_Geometry_Hints, "gdk_window_set_geometry_hints");
   pragma Import (C, Set_Group, "gdk_window_set_group");
   pragma Import (C, Set_Hints, "gdk_window_set_hints");
   pragma Import (C, Set_Transient_For, "gdk_window_set_transient_for");
   pragma Import (C, Show, "gdk_window_show");
   pragma Import (C, Unref, "gdk_window_unref");
   pragma Import (C, Withdraw, "gdk_window_withdraw");
   pragma Import (C, Set_Cursor, "gdk_window_set_cursor");
   pragma Import (C, Get_Colormap, "gdk_window_get_colormap");
   pragma Import (C, Get_Visual, "gdk_window_get_visual");
   pragma Import (C, Set_Colormap, "gdk_window_set_colormap");
   pragma Import (C, Set_Icon, "gdk_window_set_icon");
end Gdk.Window;
