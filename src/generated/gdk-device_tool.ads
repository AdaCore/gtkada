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


pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;

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

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Device_Tool_Type_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Device_Tool_Type);
   type Property_Gdk_Device_Tool_Type is new Gdk_Device_Tool_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_device_tool_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Hardware_Id
      (Self : not null access Gdk_Device_Tool_Record) return Guint64;
   --  Gets the hardware ID of this tool, or 0 if it's not known. When
   --  non-zero, the identificator is unique for the given tool model, meaning
   --  that two identical tools will share the same Hardware_Id, but will have
   --  different serial numbers (see Gdk.Device_Tool.Get_Serial).
   --  This is a more concrete (and device specific) method to identify a
   --  Gdk.Device_Tool.Gdk_Device_Tool than Gdk.Device_Tool.Get_Tool_Type, as a
   --  tablet may support multiple devices with the same
   --  Gdk.Device_Tool.Gdk_Device_Tool_Type, but having different hardware
   --  identificators.
   --  Since: gtk+ 3.22

   function Get_Serial
      (Self : not null access Gdk_Device_Tool_Record) return Guint64;
   --  Gets the serial of this tool, this value can be used to identify a
   --  physical tool (eg. a tablet pen) across program executions.
   --  Since: gtk+ 3.22

   function Get_Tool_Type
      (Self : not null access Gdk_Device_Tool_Record)
       return Gdk_Device_Tool_Type;
   --  Gets the Gdk.Device_Tool.Gdk_Device_Tool_Type of the tool.
   --  Since: gtk+ 3.22

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Axes_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Axis_Flags

   Hardware_Id_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64

   Serial_Property : constant Glib.Properties.Property_Uint;
   --  Type: Guint64

   Tool_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Device_Tool_Type

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
