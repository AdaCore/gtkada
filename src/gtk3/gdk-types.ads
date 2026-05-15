------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);

package Gdk.Types is

   Current_Time : constant Guint32 := 0;
   --  Represents the current time in timestamps stored in events

   type Gdk_Point is record
      X : Gint;
      Y : Gint;
   end record;
   pragma Convention (C, Gdk_Point);

   type Gdk_Points_Array is array (Positive range <>) of Gdk_Point;
   pragma Convention (C, Gdk_Points_Array);

   type Gdk_Segment is record
      X1 : Gint;
      Y1 : Gint;
      X2 : Gint;
      Y2 : Gint;
   end record;
   pragma Convention (C, Gdk_Segment);

   type Gdk_Segments_Array is array (Positive range <>) of Gdk_Segment;
   pragma Convention (C, Gdk_Segments_Array);

   --  See at the end of the package a list of all the types that
   --  have not been "bound".

   type Gdk_Atom is new C_Proxy;
   --  This type represents a property of the X-server, that can be
   --  manipulated through functions in Gdk.Property. They have an associated
   --  name, that can be printed, as well as a value whose type may vary.
   --  See the program xlsatoms on any X-Window machine to list all the atoms
   --  known by the Xserver.

   Gdk_None : constant Gdk_Atom := null;
   --  No atom constant.

   type Gdk_Atom_Array is array (Natural range <>) of Gdk_Atom;

   type Gdk_Axis_Use is
     (Axis_Ignore,
      Axis_X,
      Axis_Y,
      Axis_Pressure,
      Axis_X_Tilt,
      Axis_Y_Tilt,
      Axis_Last);
   pragma Convention (C, Gdk_Axis_Use);

   type Gdk_Byte_Order is (Lsb_First, Msb_First);
   pragma Convention (C, Gdk_Byte_Order);

   type Gdk_Extension_Mode is
     (Extension_Events_None, Extension_Events_All, Extension_Events_Cursor);
   pragma Convention (C, Gdk_Extension_Mode);

   type Gdk_IC_Attributes_Type is new Guint;
   Ic_Style                : constant Gdk_IC_Attributes_Type;
   Ic_Client_Window        : constant Gdk_IC_Attributes_Type;
   Ic_Focus_Window         : constant Gdk_IC_Attributes_Type;
   Ic_Filter_Events        : constant Gdk_IC_Attributes_Type;
   Ic_Spot_Location        : constant Gdk_IC_Attributes_Type;
   Ic_Line_Spacing         : constant Gdk_IC_Attributes_Type;
   Ic_Cursor               : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Fontset      : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area         : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area_Needed  : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Foreground   : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Background   : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Pixmap       : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Colormap     : constant Gdk_IC_Attributes_Type;
   Ic_Status_Fontset       : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area          : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area_Needed   : constant Gdk_IC_Attributes_Type;
   Ic_Status_Foreground    : constant Gdk_IC_Attributes_Type;
   Ic_Status_Background    : constant Gdk_IC_Attributes_Type;
   Ic_Status_Pixmap        : constant Gdk_IC_Attributes_Type;
   Ic_Status_Colormap      : constant Gdk_IC_Attributes_Type;
   Ic_All_Req              : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Area_Req     : constant Gdk_IC_Attributes_Type;
   Ic_Preedit_Position_Req : constant Gdk_IC_Attributes_Type;
   Ic_Status_Area_Req      : constant Gdk_IC_Attributes_Type;

   type Gdk_IM_Style is new Guint;
   Im_Preedit_Area      : constant Gdk_IM_Style;
   Im_Preedit_Callbacks : constant Gdk_IM_Style;
   Im_Preedit_Position  : constant Gdk_IM_Style;
   Im_Preedit_Nothing   : constant Gdk_IM_Style;
   Im_Preedit_None      : constant Gdk_IM_Style;
   Im_Preedit_Mask      : constant Gdk_IM_Style;
   Im_Status_Area       : constant Gdk_IM_Style;
   Im_Status_Callbacks  : constant Gdk_IM_Style;
   Im_Status_Nothing    : constant Gdk_IM_Style;
   Im_Status_None       : constant Gdk_IM_Style;
   Im_Status_Mask       : constant Gdk_IM_Style;

   type Gdk_Input_Condition is (Input_Read, Input_Write, Input_Exception);
   pragma Convention (C, Gdk_Input_Condition);

   type Gdk_Input_Mode is (Mode_Disabled, Mode_Screen, Mode_Window);
   pragma Convention (C, Gdk_Input_Mode);

   type Gdk_Input_Source is
     (Source_Mouse,
      Source_Pen,
      Source_Eraser,
      Source_Cursor,
      Source_Keyboard,
      Source_Touchscreen,
      Source_Touchpad);
   pragma Convention (C, Gdk_Input_Source);

   type Gdk_Grab_Status is
     (Grab_Success,   --  successfully grabbed
      Grab_Already_Grabbed,   --  resource actively grabbed by another client
      Grab_Invalid_Time,      --  resource grabbed more recently than the
                              --  specified time
      Grab_Not_Viewable,      --  grab window or confine_to_window are not
                              --  viewable
      Grab_Frozen);           --  resource is frozen by an active grab of
                              --  another client
   --  Indicates the success or reason of failure for a grab attempt

   type Gdk_Grab_Ownership is
     (Ownership_None,
      Ownership_Window,
      Ownership_Application);
   --  Defines how device grabs interact with other devices:
   --     None: all other devices' events are allowed
   --     Window: other devices' events are blocked for the grab window
   --     Application: other devices' events are blocked for the whole app.

   type Gdk_Key_Type is new Guint;
   --  see Gdk.Types.Keysyms for key type constants

   type Gdk_Modifier_Type is new Guint;
   Shift_Mask    : constant Gdk_Modifier_Type;
   Lock_Mask     : constant Gdk_Modifier_Type;
   Control_Mask  : constant Gdk_Modifier_Type;
   Mod1_Mask     : constant Gdk_Modifier_Type;
   Mod2_Mask     : constant Gdk_Modifier_Type;
   Mod3_Mask     : constant Gdk_Modifier_Type;
   Mod4_Mask     : constant Gdk_Modifier_Type;
   Mod5_Mask     : constant Gdk_Modifier_Type;
   Button1_Mask  : constant Gdk_Modifier_Type;
   Button2_Mask  : constant Gdk_Modifier_Type;
   Button3_Mask  : constant Gdk_Modifier_Type;
   Button4_Mask  : constant Gdk_Modifier_Type;
   Button5_Mask  : constant Gdk_Modifier_Type;
   Super_Mask    : constant Gdk_Modifier_Type;
   Hyper_Mask    : constant Gdk_Modifier_Type;
   Meta_Mask     : constant Gdk_Modifier_Type;
   Release_Mask  : constant Gdk_Modifier_Type;
   Modifier_Mask : constant Gdk_Modifier_Type;

   function Primary_Mod_Mask return Gdk_Modifier_Type;

   type Gdk_Modifier_Intent is
      (Primary_Accelerator,
       Context_Menu,
       Extend_Selection,
       Modify_Selection,
       No_Text_Input,
       Shift_Group);
   --  This enum is used with gdk_keymap_get_modifier_mask() and
   --  gdk_get_modifier_mask() in order to determine what modifiers the
   --  currently used windowing system backend uses for particular
   --  purposes. For example, on X11/Windows, the Control key is used for
   --  invoking menu shortcuts (accelerators), whereas on Apple computers
   --  it's the Command key (which correspond to %GDK_CONTROL_MASK and
   --  GDK_MOD2_MASK, respectively).

   subtype Gdk_WChar is Standard.Wide_Character;
   subtype Gdk_WString is Standard.Wide_String;
   --  Gdk does not define a Gdk_WString type, but uses pointers
   --  to Gdk_WChar instead.

   ----------------
   -- Properties --
   ----------------
   --  The following packages and types are used to represent properties of
   --  the given type. They are used in the packages that use these properties

   package Extension_Mode_Properties is new Generic_Internal_Discrete_Property
     (Gdk_Extension_Mode);

   type Property_Gdk_Extension_Mode is new Extension_Mode_Properties.Property;

private

   -------------------------
   --  Private constants  --
   -------------------------

   Ic_Style                : constant Gdk_IC_Attributes_Type := 2 ** 0;
   Ic_Client_Window        : constant Gdk_IC_Attributes_Type := 2 ** 1;
   Ic_Focus_Window         : constant Gdk_IC_Attributes_Type := 2 ** 2;
   Ic_Filter_Events        : constant Gdk_IC_Attributes_Type := 2 ** 3;
   Ic_Spot_Location        : constant Gdk_IC_Attributes_Type := 2 ** 4;
   Ic_Line_Spacing         : constant Gdk_IC_Attributes_Type := 2 ** 5;
   Ic_Cursor               : constant Gdk_IC_Attributes_Type := 2 ** 6;
   Ic_Preedit_Fontset      : constant Gdk_IC_Attributes_Type := 2 ** 10;
   Ic_Preedit_Area         : constant Gdk_IC_Attributes_Type := 2 ** 11;
   Ic_Preedit_Area_Needed  : constant Gdk_IC_Attributes_Type := 2 ** 12;
   Ic_Preedit_Foreground   : constant Gdk_IC_Attributes_Type := 2 ** 13;
   Ic_Preedit_Background   : constant Gdk_IC_Attributes_Type := 2 ** 14;
   Ic_Preedit_Pixmap       : constant Gdk_IC_Attributes_Type := 2 ** 15;
   Ic_Preedit_Colormap     : constant Gdk_IC_Attributes_Type := 2 ** 16;
   Ic_Status_Fontset       : constant Gdk_IC_Attributes_Type := 2 ** 21;
   Ic_Status_Area          : constant Gdk_IC_Attributes_Type := 2 ** 22;
   Ic_Status_Area_Needed   : constant Gdk_IC_Attributes_Type := 2 ** 23;
   Ic_Status_Foreground    : constant Gdk_IC_Attributes_Type := 2 ** 24;
   Ic_Status_Background    : constant Gdk_IC_Attributes_Type := 2 ** 25;
   Ic_Status_Pixmap        : constant Gdk_IC_Attributes_Type := 2 ** 26;
   Ic_Status_Colormap      : constant Gdk_IC_Attributes_Type := 2 ** 27;
   Ic_All_Req              : constant Gdk_IC_Attributes_Type :=
     Ic_Style or Ic_Client_Window;
   Ic_Preedit_Area_Req     : constant Gdk_IC_Attributes_Type :=
     Ic_Preedit_Area or Ic_Preedit_Fontset;
   Ic_Preedit_Position_Req : constant Gdk_IC_Attributes_Type :=
     Ic_Preedit_Area or Ic_Spot_Location or Ic_Preedit_Fontset;
   Ic_Status_Area_Req      : constant Gdk_IC_Attributes_Type :=
     Ic_Status_Area or Ic_Status_Fontset;

   Im_Preedit_Area      : constant Gdk_IM_Style := 16#0001#;
   Im_Preedit_Callbacks : constant Gdk_IM_Style := 16#0002#;
   Im_Preedit_Position  : constant Gdk_IM_Style := 16#0004#;
   Im_Preedit_Nothing   : constant Gdk_IM_Style := 16#0008#;
   Im_Preedit_None      : constant Gdk_IM_Style := 16#0010#;
   Im_Preedit_Mask      : constant Gdk_IM_Style := 16#001F#;
   Im_Status_Area       : constant Gdk_IM_Style := 16#0100#;
   Im_Status_Callbacks  : constant Gdk_IM_Style := 16#0200#;
   Im_Status_Nothing    : constant Gdk_IM_Style := 16#0400#;
   Im_Status_None       : constant Gdk_IM_Style := 16#0800#;
   Im_Status_Mask       : constant Gdk_IM_Style := 16#0F00#;

   Shift_Mask   : constant Gdk_Modifier_Type := 2 ** 0;
   Lock_Mask    : constant Gdk_Modifier_Type := 2 ** 1;
   Control_Mask : constant Gdk_Modifier_Type := 2 ** 2;
   Mod1_Mask    : constant Gdk_Modifier_Type := 2 ** 3;
   Mod2_Mask    : constant Gdk_Modifier_Type := 2 ** 4;
   Mod3_Mask    : constant Gdk_Modifier_Type := 2 ** 5;
   Mod4_Mask    : constant Gdk_Modifier_Type := 2 ** 6;
   Mod5_Mask    : constant Gdk_Modifier_Type := 2 ** 7;
   Button1_Mask : constant Gdk_Modifier_Type := 2 ** 8;
   Button2_Mask : constant Gdk_Modifier_Type := 2 ** 9;
   Button3_Mask : constant Gdk_Modifier_Type := 2 ** 10;
   Button4_Mask : constant Gdk_Modifier_Type := 2 ** 11;
   Button5_Mask : constant Gdk_Modifier_Type := 2 ** 12;
   Super_Mask    : constant Gdk_Modifier_Type := 2 ** 26;
   Hyper_Mask    : constant Gdk_Modifier_Type := 2 ** 27;
   Meta_Mask     : constant Gdk_Modifier_Type := 2 ** 28;
   Release_Mask  : constant Gdk_Modifier_Type := 2 ** 30;
   Modifier_Mask : constant Gdk_Modifier_Type := 16#5C001FFF#;

   pragma Import (C, Primary_Mod_Mask, "ada_gdk_get_default_modifier");

   ------------------------------
   --  Representation clauses  --
   ------------------------------

   for Gdk_Input_Condition use
     (Input_Read      => 2 ** 0,
      Input_Write     => 2 ** 1,
      Input_Exception => 2 ** 2);
end Gdk.Types;

--  The following types were not bound because it did not seem
--  to be necessary (yet).
--
--  + GdkColorContextMode
--  + GdkCrossingMode
--  + GdkFilterReturn
--  + GdkFontType
--  + GdkNotifyType
--  + GdkPropertyState
--  + GdkStatus
