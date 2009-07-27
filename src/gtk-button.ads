-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2009, AdaCore                   --
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

--  <description>
--  This package implements a general button widget. This button can
--  be clicked on by the user to start any action.
--  This button does not have multiple states, it can just be temporarily
--  pressed while the mouse is on it, but does not keep its pressed state.
--
--  The gtk+ sources provide the following drawing that explains the role of
--  the various spacings that can be set for a button:
--  </description>
--
--  <example>
--  +------------------------------------------------+
--  |                   BORDER                       |
--  |  +------------------------------------------+  |
--  |  |\\\\\\\\\\\\\\\\DEFAULT\\\\\\\\\\\\\\\\\  |  |
--  |  |\\+------------------------------------+  |  |
--  |  |\\| |           SPACING       3      | |  |  |
--  |  |\\| +--------------------------------+ |  |  |
--  |  |\\| |########## FOCUS ###############| |  |  |
--  |  |\\| |#+----------------------------+#| |  |  |
--  |  |\\| |#|         RELIEF            \|#| |  |  |
--  |  |\\| |#|  +-----------------------+\|#| |  |  |
--  |  |\\|1|#|  +     THE TEXT          +\|#|2|  |  |
--  |  |\\| |#|  +-----------------------+\|#| |  |  |
--  |  |\\| |#| \\\\\ ythickness \\\\\\\\\\|#| |  |  |
--  |  |\\| |#+----------------------------+#| |  |  |
--  |  |\\| |########### 1 ##################| |  |  |
--  |  |\\| +--------------------------------+ |  |  |
--  |  |\\| |        default spacing   4     | |  |  |
--  |  |\\+------------------------------------+  |  |
--  |  |\            ythickness                   |  |
--  |  +------------------------------------------+  |
--  |                border_width                    |
--  +------------------------------------------------+
--  </example>
--  <c_version>2.14</c_version>
--  <screenshot>gtk-button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_buttons.adb</testgtk>

with Glib.Properties;
with Gtk.Bin;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Button is

   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   procedure Gtk_New (Button : out Gtk_Button; Label : UTF8_String := "");
   --  Create a new button.
   --  if Label is not the empty string, then the text appears in the
   --  button (and the child of the button is a Gtk_Label). On the other
   --  hand, if Label is the empty string, then no child is created for
   --  the button and it is your responsibility to add one. This is the
   --  recommended way to put a pixmap inside the button.

   procedure Gtk_New_From_Stock
     (Button : out Gtk_Button; Stock_Id : String);
   --  Create a new button containing the image and text from a stock item.
   --  Some stock ids have predefined contants like Gtk.Stock.Stock_OK or
   --  Gtk.Stock.Stock_Apply. See Gtk.Stock for a complete list of predefined
   --  stock items.
   --  Stock_Id: the name of the stock item.

   procedure Gtk_New_With_Mnemonic
     (Button : out Gtk_Button; Label : UTF8_String);
   --  Create a new button containing a label.
   --  Label: The text of the button, with an underscore in front of the
   --         mnemonic character
   --  If characters in Label are preceded by an underscore, they are
   --  underlined indicating that they represent a keyboard accelerator called
   --  a mnemonic. Pressing Alt and that key activates the button.

   procedure Initialize
     (Button : access Gtk_Button_Record'Class;
      Label  : UTF8_String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_From_Stock
     (Button   : access Gtk_Button_Record'Class;
      Stock_Id : String);
   --  Internal initialization function.

   procedure Initialize_With_Mnemonic
     (Button : access Gtk_Button_Record'Class;
      Label  : UTF8_String);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Button.

   procedure Set_Relief
     (Button    : access Gtk_Button_Record;
      New_Style : Gtk.Enums.Gtk_Relief_Style);
   function Get_Relief
     (Button    : access Gtk_Button_Record) return Gtk.Enums.Gtk_Relief_Style;
   --  Modify the relief style for the button.
   --  This modifies only its visual aspect, not its behavior.

   procedure Set_Label
     (Button : access Gtk_Button_Record;
      Label  : UTF8_String);
   function Get_Label
     (Button : access Gtk_Button_Record) return UTF8_String;
   --  Set or gets the label of the button.
   --  This text is also used to select an icon if Set_Use_Stock was called
   --  with a parameter set to True.

   procedure Set_Use_Underline
     (Button        : access Gtk_Button_Record;
      Use_Underline : Boolean);
   function Get_Use_Underline
     (Button : access Gtk_Button_Record) return Boolean;
   --  Sets whether an underscore used in the button's label designates an
   --  accelerator.
   --  If True, then if the user presses alt and the character following the
   --  underscore, then the button will act as if it had been pressed.

   procedure Set_Use_Stock
     (Button    : access Gtk_Button_Record;
      Use_Stock : Boolean);
   function Get_Use_Stock
     (Button : access Gtk_Button_Record) return Boolean;
   --  Sets or Gets whether a stock item is used by the button.

   procedure Set_Alignment
     (Button : access Gtk_Button_Record;
      Xalign : Gfloat := 0.5;
      Yalign : Gfloat := 0.5);
   procedure Get_Alignment
     (Button : access Gtk_Button_Record;
      Xalign : out Gfloat;
      Yalign : out Gfloat);
   --  Specify the alignment of the label inside the button.
   --  Passing (0.0, 0.0) indicates the label should be at the top-left corner
   --  of the button. (0.5, 0.5) indicates that the label should be centered.
   --  This property has no effect unless the button's child is a child of
   --  Gtk_Alignment or Gtk_Misc

   procedure Set_Focus_On_Click
     (Button         : access Gtk_Button_Record;
      Focus_On_Click : Boolean := True);
   function Get_Focus_On_Click
     (Button : access Gtk_Button_Record)
      return Boolean;
   --  Sets whether the button will grab focus when it is clicked with the
   --  mouse.
   --  Setting Focus_On_Click to False is useful in contexts like toolbars
   --  where the focus should not be removed from the main area of the
   --  application.

   procedure Set_Image
     (Button : access Gtk_Button_Record;
      Image  : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Image
     (Button : access Gtk_Button_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Set the image of the button.
   --  You do not need to call Show on the image yourself.
   --  This settings might have no effect, depending on the theme configuration
   --  that the application's user is using (in particular, the setting
   --  "gtk-button-images" indicates whether or not images should be displayed
   --  in buttons).

   function Get_Image_Position
     (Button : access Gtk_Button_Record) return Gtk.Enums.Gtk_Position_Type;
   procedure Set_Image_Position
     (Button   : access Gtk_Button_Record;
      Position : Gtk.Enums.Gtk_Position_Type);
   --  Gets the position of the image relative to the text
   --  inside the button.
   --  Since: 2.10

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Pressed  (Button : access Gtk_Button_Record);
   --  Send the "pressed" signal to the button

   procedure Released (Button : access Gtk_Button_Record);
   --  Send the "release" signal to the button

   procedure Clicked  (Button : access Gtk_Button_Record);
   --  Send the "clicked" signal to the button

   procedure Enter    (Button : access Gtk_Button_Record);
   --  Send the "enter" signal to the button

   procedure Leave    (Button : access Gtk_Button_Record);
   --  Send the "leave" signal to the button

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Label_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: Changes the text contained in the button.
   --  See also: Same as calling Set_Label directly.
   --
   --  Name:  Relief_Property
   --  Type:  Gtk_Relief_Style
   --  Flags: read-write
   --  Descr: Changes the relief used to draw the border of the button.
   --  See also: Same as calling Set_Relief directly.
   --
   --  Name:  Use_Underline_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  See also: Same as calling Set_Use_Underline directly
   --
   --  Name:  Use_Stock_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  See also: Same as calling Set_Use_Stock directly
   --
   --  Name:  Focus_On_Click_Property
   --  Type:  Boolean
   --  Flags: read-write
   --  See also: Same as calling Set_Focus_On_Click directly
   --
   --  Name:  Image_Property
   --  Type:  Object
   --  Descr: Child widget to appear next to the button text
   --
   --  Name:  Image_Position_Property
   --  Type:  Enum
   --  Descr: The position of the image relative to the text
   --
   --  Name:  Xalign_Property
   --  Type:  Float
   --  Descr: Horizontal position of child in available space. 0.0 is left
   --         aligned, 1.0 is right aligned
   --
   --  Name:  Yalign_Property
   --  Type:  Float
   --  Descr: Vertical position of child in available space. 0.0 is top
   --         aligned, 1.0 is bottom aligned
   --  </properties>

   Label_Property          : constant Glib.Properties.Property_String;
   Relief_Property         : constant Gtk.Enums.Property_Gtk_Relief_Style;
   Use_Underline_Property  : constant Glib.Properties.Property_Boolean;
   Use_Stock_Property      : constant Glib.Properties.Property_Boolean;
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean;
   Image_Property          : constant Glib.Properties.Property_Object;
   --  Image_Position_Property : constant Glib.Properties.Property_Enum;
   Xalign_Property         : constant Glib.Properties.Property_Float;
   Yalign_Property         : constant Glib.Properties.Property_Float;

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Child_Displacement_X_Property
   --  Type:  Int
   --  Descr: How far in the x direction to move the child when the button is
   --         depressed
   --
   --  Name:  Child_Displacement_Y_Property
   --  Type:  Int
   --  Descr: How far in the y direction to move the child when the button is
   --         depressed
   --
   --  Name:  Default_Border_Property
   --  Type:  Boxed
   --  Descr: Extra space to add for CAN_DEFAULT buttons
   --
   --  Name:  Default_Outside_Border_Property
   --  Type:  Boxed
   --  Descr: Extra space to add for CAN_DEFAULT buttons that is always drawn
   --         outside the border
   --
   --  Name:  Displace_Focus_Property
   --  Type:  Boolean
   --  Descr: Whether the child_displacement_x/_y properties should also affect
   --         the focus rectangle
   --
   --  Name:  Image_Spacing_Property
   --  Type:  Int
   --  Descr: Spacing in pixels between the image and label
   --
   --  Name:  Inner_Border_Property
   --  Type:  Boxed
   --  Descr: Border between button edges and child
   --  </style_properties>

   Child_Displacement_X_Property   : constant Glib.Properties.Property_Int;
   Child_Displacement_Y_Property   : constant Glib.Properties.Property_Int;
   --  Default_Border_Property      : constant Glib.Properties.Property_Boxed;
   --  Default_Outside_Border_Property :
   --     constant Glib.Properties.Property_Boxed;
   Displace_Focus_Property         : constant Glib.Properties.Property_Boolean;
   Image_Spacing_Property : constant Glib.Properties.Property_Int;
   --  Inner_Border_Property : constant Glib.Properties.Property_Boxed;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    Emitted when the button has been clicked on by the user. This is the
   --    signal you should use to start your own actions.
   --
   --  - "pressed"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    Emitted when the user presses the mouse button on
   --    the widget. The default implementation modifies the widget state
   --    and its visual aspect.
   --
   --  - "released"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    Emitted when the user releases the mouse button and
   --    is inside of the widget. The default implementation modifies the
   --    widget state and its visual aspect. If the mouse is still inside
   --    the widget, then the "clicked" signal is emitted.
   --
   --  - "enter"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    Emitted when the mouse enters the button. The clicked
   --    signal can only be emitted when the mouse is inside the button.
   --
   --  - "leave"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    Emitted when the mouse leaves the button.
   --
   --  - "activate"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --    You should emit this signal to force the activation of this widget, as
   --    if the user has clicked on it. This is generally only useful when
   --    bound to a key binding.
   --
   --  </signals>

   Signal_Clicked  : constant Glib.Signal_Name := "clicked";
   Signal_Pressed  : constant Glib.Signal_Name := "pressed";
   Signal_Released : constant Glib.Signal_Name := "released";
   Signal_Enter    : constant Glib.Signal_Name := "enter";
   Signal_Leave    : constant Glib.Signal_Name := "leave";
   Signal_Activate : constant Glib.Signal_Name := "activate";

private
   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with null record;

   --  Image_Position_Property : constant Glib.Properties.Property_Enum :=
   --    Glib.Properties.Build ("image-position");
   Label_Property  : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style :=
     Gtk.Enums.Build ("relief");
   Use_Underline_Property  : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Use_Stock_Property      : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-stock");
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");

   Child_Displacement_X_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("child-displacement-x");
   Child_Displacement_Y_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("child-displacement-y");
--     Default_Border_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("default-border");
   --  Default_Outside_Border_Property : constant
   --    Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("default-outside-border");
   Displace_Focus_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("displace-focus");
   Image_Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("image-spacing");
   --  Inner_Border_Property : constant Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("inner-border");

   pragma Import (C, Get_Type, "gtk_button_get_type");
end Gtk.Button;
