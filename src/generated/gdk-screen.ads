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
--  Gdk.Screen.Gdk_Screen objects are the GDK representation of the screen on
--  which windows can be displayed and on which the pointer moves. X originally
--  identified screens with physical screens, but nowadays it is more common to
--  have a single Gdk.Screen.Gdk_Screen which combines several physical
--  monitors (see Gdk.Screen.Get_N_Monitors).
--
--  GdkScreen is used throughout GDK and GTK+ to specify which screen the top
--  level windows are to be displayed on. it is also used to query the screen
--  specification and default settings such as the default visual
--  (gdk_screen_get_system_visual), the dimensions of the physical monitors
--  (gdk_screen_get_monitor_geometry), etc.
--
--  </description>
--  <group>Gdk, the low-level API</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;           use Cairo;
with Gdk.Display;     use Gdk.Display;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gdk.Types;       use Gdk.Types;
with Gdk.Visual;      use Gdk.Visual;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Screen is

   type Gdk_Screen_Record is new GObject_Record with null record;
   type Gdk_Screen is access all Gdk_Screen_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_screen_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Active_Window
      (Screen : not null access Gdk_Screen_Record) return Gdk.Gdk_Window;
   pragma Obsolescent (Get_Active_Window);
   --  Returns the screen's currently active window.
   --  On X11, this is done by inspecting the _NET_ACTIVE_WINDOW property on
   --  the root window, as described in the [Extended Window Manager
   --  Hints](http://www.freedesktop.org/Standards/wm-spec). If there is no
   --  currently currently active window, or the window manager does not
   --  support the _NET_ACTIVE_WINDOW hint, this function returns null.
   --  On other platforms, this function may return null, depending on whether
   --  it is implementable on that platform.
   --  The returned window should be unrefed using g_object_unref when no
   --  longer needed.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.22, 1

   function Get_Display
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Display.Gdk_Display;
   --  Gets the display to which the Screen belongs.
   --  Since: gtk+ 2.2

   function Get_Font_Options
      (Screen : not null access Gdk_Screen_Record)
       return Cairo.Cairo_Font_Options;
   --  Gets any options previously set with Gdk.Screen.Set_Font_Options.
   --  Since: gtk+ 2.10

   procedure Set_Font_Options
      (Screen  : not null access Gdk_Screen_Record;
       Options : in out Cairo.Cairo_Font_Options);
   --  Sets the default font options for the screen. These options will be set
   --  on any Pango.Context.Pango_Context's newly created with
   --  gdk_pango_context_get_for_screen. Changing the default set of font
   --  options does not affect contexts that have already been created.
   --  Since: gtk+ 2.10
   --  "options": a Cairo.Cairo_Font_Options, or null to unset any previously
   --  set default font options.

   function Get_Height
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Height);
   --  Gets the height of Screen in pixels. The returned size is in
   --  "application pixels", not in "device pixels" (see
   --  Gdk.Screen.Get_Monitor_Scale_Factor).
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Get_Height_Mm
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Height_Mm);
   --  Returns the height of Screen in millimeters.
   --  Note that this value is somewhat ill-defined when the screen has
   --  multiple monitors of different resolution. It is recommended to use the
   --  monitor dimensions instead.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Get_Monitor_At_Point
      (Screen : not null access Gdk_Screen_Record;
       X      : Glib.Gint;
       Y      : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Get_Monitor_At_Point);
   --  Returns the monitor number in which the point (X,Y) is located.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1
   --  "x": the x coordinate in the virtual screen.
   --  "y": the y coordinate in the virtual screen.

   function Get_Monitor_At_Window
      (Screen : not null access Gdk_Screen_Record;
       Window : Gdk.Gdk_Window) return Glib.Gint;
   pragma Obsolescent (Get_Monitor_At_Window);
   --  Returns the number of the monitor in which the largest area of the
   --  bounding rectangle of Window resides.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1
   --  "window": a Gdk.Gdk_Window

   procedure Get_Monitor_Geometry
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint;
       Dest        : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Monitor_Geometry);
   --  Retrieves the Gdk.Rectangle.Gdk_Rectangle representing the size and
   --  position of the individual monitor within the entire screen area. The
   --  returned geometry is in "application pixels", not in "device pixels"
   --  (see Gdk.Screen.Get_Monitor_Scale_Factor).
   --  Monitor numbers start at 0. To obtain the number of monitors of Screen,
   --  use Gdk.Screen.Get_N_Monitors.
   --  Note that the size of the entire screen area can be retrieved via
   --  Gdk.Screen.Get_Width and Gdk.Screen.Get_Height.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1
   --  "monitor_num": the monitor number
   --  "dest": a Gdk.Rectangle.Gdk_Rectangle to be filled with the monitor
   --  geometry

   function Get_Monitor_Height_Mm
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Get_Monitor_Height_Mm);
   --  Gets the height in millimeters of the specified monitor.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.22, 1
   --  "monitor_num": number of the monitor, between 0 and
   --  gdk_screen_get_n_monitors (screen)

   function Get_Monitor_Plug_Name
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint) return UTF8_String;
   pragma Obsolescent (Get_Monitor_Plug_Name);
   --  Returns the output name of the specified monitor. Usually something
   --  like VGA, DVI, or TV, not the actual product name of the display device.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.22, 1
   --  "monitor_num": number of the monitor, between 0 and
   --  gdk_screen_get_n_monitors (screen)

   function Get_Monitor_Scale_Factor
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Get_Monitor_Scale_Factor);
   --  Returns the internal scale factor that maps from monitor coordinates to
   --  the actual device pixels. On traditional systems this is 1, but on very
   --  high density outputs this can be a higher value (often 2).
   --  This can be used if you want to create pixel based data for a
   --  particular monitor, but most of the time you're drawing to a window
   --  where it is better to use Gdk.Window.Get_Scale_Factor instead.
   --  Since: gtk+ 3.10
   --  Deprecated since 3.22, 1
   --  "monitor_num": number of the monitor, between 0 and
   --  gdk_screen_get_n_monitors (screen)

   function Get_Monitor_Width_Mm
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint) return Glib.Gint;
   pragma Obsolescent (Get_Monitor_Width_Mm);
   --  Gets the width in millimeters of the specified monitor, if available.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.22, 1
   --  "monitor_num": number of the monitor, between 0 and
   --  gdk_screen_get_n_monitors (screen)

   procedure Get_Monitor_Workarea
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Glib.Gint;
       Dest        : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Monitor_Workarea);
   --  Retrieves the Gdk.Rectangle.Gdk_Rectangle representing the size and
   --  position of the "work area" on a monitor within the entire screen area.
   --  The returned geometry is in "application pixels", not in "device pixels"
   --  (see Gdk.Screen.Get_Monitor_Scale_Factor).
   --  The work area should be considered when positioning menus and similar
   --  popups, to avoid placing them below panels, docks or other desktop
   --  components.
   --  Note that not all backends may have a concept of workarea. This
   --  function will return the monitor geometry if a workarea is not
   --  available, or does not apply.
   --  Monitor numbers start at 0. To obtain the number of monitors of Screen,
   --  use Gdk.Screen.Get_N_Monitors.
   --  Since: gtk+ 3.4
   --  Deprecated since 3.22, 1
   --  "monitor_num": the monitor number
   --  "dest": a Gdk.Rectangle.Gdk_Rectangle to be filled with the monitor
   --  workarea

   function Get_N_Monitors
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_N_Monitors);
   --  Returns the number of monitors which Screen consists of.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Get_Number
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Number);
   --  Gets the index of Screen among the screens in the display to which it
   --  belongs. (See Gdk.Screen.Get_Display)
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Get_Primary_Monitor
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Primary_Monitor);
   --  Gets the primary monitor for Screen. The primary monitor is considered
   --  the monitor where the "main desktop" lives. While normal application
   --  windows typically allow the window manager to place the windows,
   --  specialized desktop applications such as panels should place themselves
   --  on the primary monitor.
   --  If no primary monitor is configured by the user, the return value will
   --  be 0, defaulting to the first monitor.
   --  Since: gtk+ 2.20
   --  Deprecated since 3.22, 1

   function Get_Resolution
      (Screen : not null access Gdk_Screen_Record) return Gdouble;
   --  Gets the resolution for font handling on the screen; see
   --  Gdk.Screen.Set_Resolution for full details.
   --  Since: gtk+ 2.10

   procedure Set_Resolution
      (Screen : not null access Gdk_Screen_Record;
       Dpi    : Gdouble);
   --  Sets the resolution for font handling on the screen. This is a scale
   --  factor between points specified in a Pango.Font.Pango_Font_Description
   --  and cairo units. The default value is 96, meaning that a 10 point font
   --  will be 13 units high. (10 * 96. / 72. = 13.3).
   --  Since: gtk+ 2.10
   --  "dpi": the resolution in "dots per inch". (Physical inches aren't
   --  actually involved; the terminology is conventional.)

   function Get_Rgba_Visual
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Visual.Gdk_Visual;
   --  Gets a visual to use for creating windows with an alpha channel. The
   --  windowing system on which GTK+ is running may not support this
   --  capability, in which case null will be returned. Even if a non-null
   --  value is returned, its possible that the window's alpha channel won't be
   --  honored when displaying the window on the screen: in particular, for X
   --  an appropriate windowing manager and compositing manager must be running
   --  to provide appropriate display.
   --  This functionality is not implemented in the Windows backend.
   --  For setting an overall opacity for a top-level window, see
   --  Gdk.Window.Set_Opacity.
   --  Since: gtk+ 2.8

   function Get_Root_Window
      (Screen : not null access Gdk_Screen_Record) return Gdk.Gdk_Window;
   --  Gets the root window of Screen.
   --  Since: gtk+ 2.2

   function Get_System_Visual
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Visual.Gdk_Visual;
   --  Get the system's default visual for Screen. This is the visual for the
   --  root window of the display. The return value should not be freed.
   --  Since: gtk+ 2.2

   function Get_Width
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Width);
   --  Gets the width of Screen in pixels. The returned size is in
   --  "application pixels", not in "device pixels" (see
   --  Gdk.Screen.Get_Monitor_Scale_Factor).
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Get_Width_Mm
      (Screen : not null access Gdk_Screen_Record) return Glib.Gint;
   pragma Obsolescent (Get_Width_Mm);
   --  Gets the width of Screen in millimeters.
   --  Note that this value is somewhat ill-defined when the screen has
   --  multiple monitors of different resolution. It is recommended to use the
   --  monitor dimensions instead.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   function Is_Composited
      (Screen : not null access Gdk_Screen_Record) return Boolean;
   --  Returns whether windows with an RGBA visual can reasonably be expected
   --  to have their alpha channel drawn correctly on the screen.
   --  On X11 this function returns whether a compositing manager is
   --  compositing Screen.
   --  Since: gtk+ 2.10

   function Make_Display_Name
      (Screen : not null access Gdk_Screen_Record) return UTF8_String;
   pragma Obsolescent (Make_Display_Name);
   --  Determines the name to pass to Gdk.Display.Open to get a
   --  Gdk.Display.Gdk_Display with this screen as the default screen.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.22, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   -------------
   -- Display --
   -------------
   --  These subprograms should really be in gdk-display.ads to match what is
   --  done for gtk+ itself, but that would create dependency circularities.
   --  Ada 2012 has support for these, but we want GtkAda to build with Ada95
   --  compilers.

   function Get_Screen
     (Display    : access Gdk.Display.Gdk_Display_Record'Class;
      Screen_Num : Glib.Gint)
   return Gdk_Screen;
   --  Returns a screen object for one of the screens of the display.

   function Get_Default_Screen
     (Display : access Gdk.Display.Gdk_Display_Record'Class) return Gdk_Screen;
   --  Get the default Gdk_Screen for display.

   procedure Get_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : out Gdk_Screen;
      X       : out Glib.Gint;
      Y       : out Glib.Gint;
      Mask    : out Gdk.Types.Gdk_Modifier_Type);
   --  Gets the current location of the pointer and the current modifier
   --  mask for a given display.
   --  (X, Y) are coordinates relative to the root window on the display

   procedure Warp_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : access Gdk_Screen_Record;
      X       : Glib.Gint;
      Y       : Glib.Gint);
   --  Warps the pointer of display to the point x,y on the screen screen,
   --  unless the pointer is confined to a window by a grab, in which case it
   --  will be moved as far as allowed by the grab. Warping the pointer creates
   --  events as if the user had moved the mouse instantaneously to the
   --  destination.
   --
   --  Note that the pointer should normally be under the control of the user.
   --  This function was added to cover some rare use cases like keyboard
   --  navigation support for the color picker in the GtkColorSelectionDialog.

   ---------------
   -- Functions --
   ---------------

   function Get_Default return Gdk_Screen;
   --  Gets the default screen for the default display. (See
   --  gdk_display_get_default ()).
   --  Since: gtk+ 2.2

   function Height return Glib.Gint;
   pragma Obsolescent (Height);
   --  Gets the height of the default screen in pixels. The returned size is
   --  in "application pixels", not in "device pixels" (see
   --  Gdk.Screen.Get_Monitor_Scale_Factor).
   --  Deprecated since 3.22, 1

   function Height_Mm return Glib.Gint;
   pragma Obsolescent (Height_Mm);
   --  Returns the height of the default screen in millimeters. Note that on
   --  many X servers this value will not be correct.
   --  Deprecated since 3.22, 1

   function Width return Glib.Gint;
   pragma Obsolescent (Width);
   --  Gets the width of the default screen in pixels. The returned size is in
   --  "application pixels", not in "device pixels" (see
   --  Gdk.Screen.Get_Monitor_Scale_Factor).
   --  Deprecated since 3.22, 1

   function Width_Mm return Glib.Gint;
   pragma Obsolescent (Width_Mm);
   --  Returns the width of the default screen in millimeters. Note that on
   --  many X servers this value will not be correct.
   --  Deprecated since 3.22, 1

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Font_Options_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("font-options");--  Unknown type: gpointer

   Resolution_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   -------------
   -- Signals --
   -------------

   Signal_Composited_Changed : constant Glib.Signal_Name := "composited-changed";
   --  The ::composited-changed signal is emitted when the composited status
   --  of the screen changes
   --    procedure Handler (Self : access Gdk_Screen_Record'Class)

   Signal_Monitors_Changed : constant Glib.Signal_Name := "monitors-changed";
   --  The ::monitors-changed signal is emitted when the number, size or
   --  position of the monitors attached to the screen change.
   --
   --  Only for X11 and OS X for now. A future implementation for Win32 may be
   --  a possibility.
   --    procedure Handler (Self : access Gdk_Screen_Record'Class)

   Signal_Size_Changed : constant Glib.Signal_Name := "size-changed";
   --  The ::size-changed signal is emitted when the pixel width or height of
   --  a screen changes.
   --    procedure Handler (Self : access Gdk_Screen_Record'Class)

private
   Resolution_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("resolution");
end Gdk.Screen;
