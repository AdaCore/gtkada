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

--  Represents the individual outputs that are associated with a `GdkDisplay`.
--
--  `GdkDisplay` keeps a `GListModel` to enumerate and monitor monitors with
--  [methodGdk.Display.get_monitors]. You can use
--  [methodGdk.Display.get_monitor_at_surface] to find a particular monitor.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Display;             use Gdk.Display;
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

   function Get_Connector
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets the name of the monitor's connector, if available.
   --  These are strings such as "eDP-1", or "HDMI-2". They depend on software
   --  and hardware configuration, and should not be relied on as stable
   --  identifiers of a specific monitor.
   --  @return the name of the connector

   function Get_Description
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets a string describing the monitor, if available.
   --  This can be used to identify a monitor in the UI.
   --  Since: gtk+ 4.10
   --  @return the monitor description

   function Get_Display
      (Self : not null access Gdk_Monitor_Record)
       return Gdk.Display.Gdk_Display;
   --  Gets the display that this monitor belongs to.
   --  @return the display

   procedure Get_Geometry
      (Self     : not null access Gdk_Monitor_Record;
       Geometry : out Gdk.Rectangle.Gdk_Rectangle);
   --  Retrieves the size and position of the monitor within the display
   --  coordinate space.
   --  The returned geometry is in "application pixels", not in "device
   --  pixels" (see [methodGdk.Monitor.get_scale]).
   --  @param Geometry a `GdkRectangle` to be filled with the monitor geometry

   function Get_Height_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the height in millimeters of the monitor.
   --  @return the physical height of the monitor

   function Get_Manufacturer
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets the name or PNP ID of the monitor's manufacturer.
   --  Note that this value might also vary depending on actual display
   --  backend.
   --  The PNP ID registry is located at
   --  [https://uefi.org/pnp_id_list](https://uefi.org/pnp_id_list).
   --  @return the name of the manufacturer

   function Get_Model
      (Self : not null access Gdk_Monitor_Record) return UTF8_String;
   --  Gets the string identifying the monitor model, if available.
   --  @return the monitor model

   function Get_Refresh_Rate
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the refresh rate of the monitor, if available.
   --  The value is in milli-Hertz, so a refresh rate of 60Hz is returned as
   --  60000.
   --  @return the refresh rate in milli-Hertz, or 0

   function Get_Scale
      (Self : not null access Gdk_Monitor_Record) return Gdouble;
   --  Gets the internal scale factor that maps from monitor coordinates to
   --  device pixels.
   --  This can be used if you want to create pixel based data for a
   --  particular monitor, but most of the time you're drawing to a surface
   --  where it is better to use [methodGdk.Surface.get_scale] instead.
   --  Since: gtk+ 4.14
   --  @return the scale

   function Get_Scale_Factor
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the internal scale factor that maps from monitor coordinates to
   --  device pixels.
   --  On traditional systems this is 1, but on very high density outputs it
   --  can be a higher value (often 2).
   --  This can be used if you want to create pixel based data for a
   --  particular monitor, but most of the time you're drawing to a surface
   --  where it is better to use [methodGdk.Surface.get_scale_factor] instead.
   --  @return the scale factor

   function Get_Subpixel_Layout
      (Self : not null access Gdk_Monitor_Record) return Gdk_Subpixel_Layout;
   --  Gets information about the layout of red, green and blue primaries for
   --  pixels.
   --  @return the subpixel layout

   function Get_Width_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint;
   --  Gets the width in millimeters of the monitor.
   --  @return the physical width of the monitor

   function Is_Valid
      (Self : not null access Gdk_Monitor_Record) return Boolean;
   --  Returns True if the Monitor object corresponds to a physical monitor.
   --  The Monitor becomes invalid when the physical monitor is unplugged or
   --  removed.
   --  @return True if the object corresponds to a physical monitor

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Connector_Property : constant Glib.Properties.Property_String;
   --  The connector name.

   Description_Property : constant Glib.Properties.Property_String;
   --  A short description of the monitor, meant for display to the user.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` of the monitor.

   Geometry_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Rectangle
   --  The geometry of the monitor.

   Height_Mm_Property : constant Glib.Properties.Property_Int;
   --  The height of the monitor, in millimeters.

   Manufacturer_Property : constant Glib.Properties.Property_String;
   --  The manufacturer name.

   Model_Property : constant Glib.Properties.Property_String;
   --  The model name.

   Refresh_Rate_Property : constant Glib.Properties.Property_Int;
   --  The refresh rate, in milli-Hertz.

   Scale_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The scale of the monitor.

   Scale_Factor_Property : constant Glib.Properties.Property_Int;
   --  The scale factor.
   --
   --  The scale factor is the next larger integer, compared to
   --  [propertyGdk.Surface:scale].

   Subpixel_Layout_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Subpixel_Layout
   --  The subpixel layout.

   Valid_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the object is still valid.

   Width_Mm_Property : constant Glib.Properties.Property_Int;
   --  The width of the monitor, in millimeters.

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
   --  Emitted when the output represented by Monitor gets disconnected.

private
   Width_Mm_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width-mm");
   Valid_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("valid");
   Subpixel_Layout_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("subpixel-layout");
   Scale_Factor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale-factor");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
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
   Description_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("description");
   Connector_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("connector");
end Gdk.Monitor;
