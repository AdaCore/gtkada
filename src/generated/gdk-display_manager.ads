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

--  Offers notification when displays appear or disappear.
--
--  `GdkDisplayManager` is a singleton object.
--
--  You can use [funcGdk.DisplayManager.get] to obtain the `GdkDisplayManager`
--  singleton, but that should be rarely necessary. Typically, initializing GTK
--  opens a display that you can work with without ever accessing the
--  `GdkDisplayManager`.
--
--  The GDK library can be built with support for multiple backends. The
--  `GdkDisplayManager` object determines which backend is used at runtime.
--
--  In the rare case that you need to influence which of the backends is being
--  used, you can use [funcGdk.set_allowed_backends]. Note that you need to
--  call this function before initializing GTK.
--
--  ## Backend-specific code
--
--  When writing backend-specific code that is supposed to work with multiple
--  GDK backends, you have to consider both compile time and runtime. At
--  compile time, use the `GDK_WINDOWING_X11`, `GDK_WINDOWING_WIN32` macros,
--  etc. to find out which backends are present in the GDK library you are
--  building your application against. At runtime, use type-check macros like
--  GDK_IS_X11_DISPLAY to find out which backend is in use:
--
--  ```c ifdef GDK_WINDOWING_X11 if (GDK_IS_X11_DISPLAY (display)) { // make
--  X11-specific calls here } else endif ifdef GDK_WINDOWING_MACOS if
--  (GDK_IS_MACOS_DISPLAY (display)) { // make Quartz-specific calls here }
--  else endif g_error ("Unsupported GDK backend"); ```

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Display;     use Gdk.Display;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Display_Manager is

   type Gdk_Display_Manager_Record is new GObject_Record with null record;
   type Gdk_Display_Manager is access all Gdk_Display_Manager_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_display_manager_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Default_Display
      (Self : not null access Gdk_Display_Manager_Record)
       return Gdk.Gdk_Display;
   --  Gets the default `GdkDisplay`.
   --  @return a `GdkDisplay`
   --  Return has transfer-ownership='none'

   procedure Set_Default_Display
      (Self    : not null access Gdk_Display_Manager_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   --  Sets Display as the default display.
   --  @param Display a `GdkDisplay`

   function List_Displays
      (Self : not null access Gdk_Display_Manager_Record)
       return Gdk.Display.Display_List.Glist;
   --  List all currently open displays.

   function Open_Display
      (Self : not null access Gdk_Display_Manager_Record;
       Name : UTF8_String := "") return Gdk.Gdk_Display;
   --  Opens a display.
   --  @param Name the name of the display to open
   --  @return a `GdkDisplay`, or null if the display could not be opened
   --  Return has transfer-ownership='none'

   ---------------
   -- Functions --
   ---------------

   function Get return Gdk_Display_Manager;
   --  Gets the singleton `GdkDisplayManager` object.
   --  When called for the first time, this function consults the
   --  `GDK_BACKEND` environment variable to find out which of the supported
   --  GDK backends to use (in case GDK has been compiled with multiple
   --  backends).
   --  Applications can use [funcSet_Allowed_Backends] to limit what backends
   --  will be used.
   --  @return The global `GdkDisplayManager` singleton
   --  Return has transfer-ownership='none'

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Default_Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The default display.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Display_Manager_Gdk_Display_Void is not null access procedure
     (Self    : access Gdk_Display_Manager_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class);

   type Cb_GObject_Gdk_Display_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Display : not null access Gdk.Display.Gdk_Display_Record'Class);

   Signal_Display_Opened : constant Glib.Signal_Name := "display-opened";
   procedure On_Display_Opened
      (Self  : not null access Gdk_Display_Manager_Record;
       Call  : Cb_Gdk_Display_Manager_Gdk_Display_Void;
       After : Boolean := False);
   procedure On_Display_Opened
      (Self  : not null access Gdk_Display_Manager_Record;
       Call  : Cb_GObject_Gdk_Display_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a display is opened.

private
   Default_Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("default-display");
end Gdk.Display_Manager;
