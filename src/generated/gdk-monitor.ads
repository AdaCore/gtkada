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
--  GdkMonitor objects represent the individual outputs that are associated
--  with a Gdk.Display.Gdk_Display. GdkDisplay has APIs to enumerate monitors
--  with Gdk.Display.Get_N_Monitors and Gdk.Display.Get_Monitor, and to find
--  particular monitors with Gdk.Display.Get_Primary_Monitor or
--  Gdk.Display.Get_Monitor_At_Window.
--
--  GdkMonitor was introduced in GTK+ 3.22 and supersedes earlier APIs in
--  GdkScreen to obtain monitor-related information.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

package Gdk.Monitor is

   type Gdk_Monitor_Record is new GObject_Record with null record;
   type Gdk_Monitor is access all Gdk_Monitor_Record'Class;

   type Gdk_Subpixel_Layout is (
      Gdk_Subpixel_Layout_Unknown,
      Gdk_Subpixel_Layout_None,
      Gdk_Subpixel_Layout_Horizontal_Rgb,
      Gdk_Subpixel_Layout_Horizontal_Bgr,
      Gdk_Subpixel_Layout_Vertical_Rgb,
      Gdk_Subpixel_Layout_Vertical_Bgr);
   pragma Convention (C, Gdk_Subpixel_Layout);
   --  This enumeration describes how the red, green and blue components of
   --  physical pixels on an output device are laid out.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Subpixel_Layout_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Subpixel_Layout);
   type Property_Gdk_Subpixel_Layout is new Gdk_Subpixel_Layout_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_monitor_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Display
      (Self : not null access Gdk_Monitor_Record) return Glib.Object.GObject;
   --  Gets the display that this monitor belongs to.
   --  Since: gtk+ 3.22

   procedure Get_Geometry
      (Self     : not null access Gdk_Monitor_Record;
       Geometry : out Gdk.Rectangle.Gdk_Rectangle);
   --  Retrieves the size and position of an individual monitor within the
   --  display coordinate space. The returned geometry is in "application
   --  pixels", not in "device pixels" (see Gdk.Monitor.Get_Scale_Factor).
   --  Since: gtk+ 3.22
   --  "geometry": a Gdk.Rectangle.Gdk_Rectangle to be filled with the monitor
   --  geometry

   function Get_Height_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the height in millimeters of the monitor.
   --  Since: gtk+ 3.22

   function Get_Manufacturer
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets the name or PNP ID of the monitor's manufacturer, if available.
   --  Note that this value might also vary depending on actual display
   --  backend.
   --  PNP ID registry is located at https://uefi.org/pnp_id_list

   function Get_Model
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets the a string identifying the monitor model, if available.

   function Get_Refresh_Rate
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the refresh rate of the monitor, if available.
   --  The value is in milli-Hertz, so a refresh rate of 60Hz is returned as
   --  60000.
   --  Since: gtk+ 3.22

   function Get_Scale_Factor
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the internal scale factor that maps from monitor coordinates to
   --  the actual device pixels. On traditional systems this is 1, but on very
   --  high density outputs this can be a higher value (often 2).
   --  This can be used if you want to create pixel based data for a
   --  particular monitor, but most of the time you're drawing to a window
   --  where it is better to use Gdk.Window.Get_Scale_Factor instead.
   --  Since: gtk+ 3.22

   function Get_Subpixel_Layout
      (Self : not null access Gdk_Monitor_Record) return Gdk_Subpixel_Layout;
   --  Gets information about the layout of red, green and blue primaries for
   --  each pixel in this monitor, if available.
   --  Since: gtk+ 3.22

   function Get_Width_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the width in millimeters of the monitor.
   --  Since: gtk+ 3.22

   procedure Get_Workarea
      (Self     : not null access Gdk_Monitor_Record;
       Workarea : out Gdk.Rectangle.Gdk_Rectangle);
   --  Retrieves the size and position of the "work area" on a monitor within
   --  the display coordinate space. The returned geometry is in "application
   --  pixels", not in "device pixels" (see Gdk.Monitor.Get_Scale_Factor).
   --  The work area should be considered when positioning menus and similar
   --  popups, to avoid placing them below panels, docks or other desktop
   --  components.
   --  Note that not all backends may have a concept of workarea. This
   --  function will return the monitor geometry if a workarea is not
   --  available, or does not apply.
   --  Since: gtk+ 3.22
   --  "workarea": a Gdk.Rectangle.Gdk_Rectangle to be filled with the monitor
   --  workarea

   function Is_Primary
      (Self : not null access Gdk_Monitor_Record) return Boolean;
   --  Gets whether this monitor should be considered primary (see
   --  Gdk.Display.Get_Primary_Monitor).
   --  Since: gtk+ 3.22

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display

   Geometry_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Rectangle

   Height_Mm_Property : constant Glib.Properties.Property_Int;

   Manufacturer_Property : constant Glib.Properties.Property_String;

   Model_Property : constant Glib.Properties.Property_String;

   Refresh_Rate_Property : constant Glib.Properties.Property_Int;

   Scale_Factor_Property : constant Glib.Properties.Property_Int;

   Subpixel_Layout_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Subpixel_Layout

   Width_Mm_Property : constant Glib.Properties.Property_Int;

   Workarea_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Rectangle

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Monitor_Void is not null access procedure (Self : access Gdk_Monitor_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Invalidate : constant Glib.Signal_Name := "invalidate";
   procedure On_Invalidate
      (Self  : not null access Gdk_Monitor_Record;
       Call  : Cb_Gdk_Monitor_Void;
       After : Boolean := False);
   procedure On_Invalidate
      (Self  : not null access Gdk_Monitor_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

private
   Workarea_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("workarea");
   Width_Mm_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-mm");
   Subpixel_Layout_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("subpixel-layout");
   Scale_Factor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale-factor");
   Refresh_Rate_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("refresh-rate");
   Model_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("model");
   Manufacturer_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("manufacturer");
   Height_Mm_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height-mm");
   Geometry_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("geometry");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.Monitor;
