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

--  A physical tool associated to a `GdkDevice`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with System;

package Gdk.Device_Tool is

   type Gdk_Device_Tool_Record is new GObject_Record with null record;
   type Gdk_Device_Tool is access all Gdk_Device_Tool_Record'Class;

   type Gdk_Device_Tool_Type is (
      Gdk_Device_Tool_Type_Unknown,
      Gdk_Device_Tool_Type_Pen,
      Gdk_Device_Tool_Type_Eraser,
      Gdk_Device_Tool_Type_Brush,
      Gdk_Device_Tool_Type_Pencil,
      Gdk_Device_Tool_Type_Airbrush,
      Gdk_Device_Tool_Type_Mouse,
      Gdk_Device_Tool_Type_Lens);
   pragma Convention (C, Gdk_Device_Tool_Type);
   --  Indicates the specific type of tool being used being a tablet. Such as
   --  an airbrush, pencil, etc.

   type Gdk_Axis_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Axis_Flags);
   --  Flags describing the current capabilities of a device/tool.

   Gdk_Axis_Flag_X : constant Gdk_Axis_Flags := 2;
   Gdk_Axis_Flag_Y : constant Gdk_Axis_Flags := 4;
   Gdk_Axis_Flag_Delta_X : constant Gdk_Axis_Flags := 8;
   Gdk_Axis_Flag_Delta_Y : constant Gdk_Axis_Flags := 16;
   Gdk_Axis_Flag_Pressure : constant Gdk_Axis_Flags := 32;
   Gdk_Axis_Flag_Xtilt : constant Gdk_Axis_Flags := 64;
   Gdk_Axis_Flag_Ytilt : constant Gdk_Axis_Flags := 128;
   Gdk_Axis_Flag_Wheel : constant Gdk_Axis_Flags := 256;
   Gdk_Axis_Flag_Distance : constant Gdk_Axis_Flags := 512;
   Gdk_Axis_Flag_Rotation : constant Gdk_Axis_Flags := 1024;
   Gdk_Axis_Flag_Slider : constant Gdk_Axis_Flags := 2048;

   function Convert (R : Gdk.Device_Tool.Gdk_Device_Tool) return System.Address;
   function Convert (R : System.Address) return Gdk.Device_Tool.Gdk_Device_Tool;
   package Device_Tool_List is new Generic_List (Gdk.Device_Tool.Gdk_Device_Tool);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Device_Tool_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Device_Tool_Type);
   type Property_Gdk_Device_Tool_Type is new Gdk_Device_Tool_Type_Properties.Property;

   package Gdk_Axis_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Axis_Flags);
   type Property_Gdk_Axis_Flags is new Gdk_Axis_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_device_tool_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Axes
      (Self : not null access Gdk_Device_Tool_Record) return Gdk_Axis_Flags;
   --  Gets the axes of the tool.
   --  @return the axes of Tool

   function Get_Hardware_Id
      (Self : not null access Gdk_Device_Tool_Record) return Guint64;
   --  Gets the hardware ID of this tool, or 0 if it's not known.
   --  When non-zero, the identifier is unique for the given tool model,
   --  meaning that two identical tools will share the same Hardware_Id, but
   --  will have different serial numbers (see
   --  [methodGdk.DeviceTool.get_serial]).
   --  This is a more concrete (and device specific) method to identify a
   --  `GdkDeviceTool` than [methodGdk.DeviceTool.get_tool_type], as a tablet
   --  may support multiple devices with the same `GdkDeviceToolType`, but
   --  different hardware identifiers.
   --  @return The hardware identifier of this tool.

   function Get_Serial
      (Self : not null access Gdk_Device_Tool_Record) return Guint64;
   --  Gets the serial number of this tool.
   --  This value can be used to identify a physical tool (eg. a tablet pen)
   --  across program executions.
   --  @return The serial ID for this tool

   function Get_Tool_Type
      (Self : not null access Gdk_Device_Tool_Record)
       return Gdk_Device_Tool_Type;
   --  Gets the `GdkDeviceToolType` of the tool.
   --  @return The physical type for this tool. This can be used to figure out
   --  what sort of pen is being used, such as an airbrush or a pencil.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Axes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Axis_Flags
   --  The axes of the tool.

   Hardware_Id_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64
   --  The hardware ID of the tool.

   Serial_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64
   --  The serial number of the tool.

   Tool_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Tool_Type
   --  The type of the tool.

private
   Tool_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("tool-type");
   Serial_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("serial");
   Hardware_Id_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("hardware-id");
   Axes_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("axes");
end Gdk.Device_Tool;
