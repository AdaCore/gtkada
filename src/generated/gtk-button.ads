------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  The Gtk.Button.Gtk_Button widget is generally used to attach a function to
--  that is called when the button is pressed. The various signals and how to
--  use them are outlined below.
--
--  The Gtk.Button.Gtk_Button widget can hold any valid child widget. That is
--  it can hold most any other standard Gtk.Widget.Gtk_Widget. The most
--  commonly used child is the Gtk.Label.Gtk_Label.
--
--  </description>
--  <screenshot>gtk-button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_buttons.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Button is

   type Gtk_Button_Record is new Gtk_Bin_Record with null record;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Stock
      (Button   : out Gtk_Button;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Button   : not null access Gtk_Button_Record'Class;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Button.Gtk_Button containing the image and text from
   --  a stock item. Some stock ids have preprocessor macros like GTK_STOCK_OK
   --  and GTK_STOCK_APPLY.
   --  If Stock_Id is unknown, then it will be treated as a mnemonic label (as
   --  for Gtk.Button.Gtk_New_With_Mnemonic).
   --  "stock_id": the name of the stock item

   procedure Gtk_New (Button : out Gtk_Button; Label : UTF8_String := "");
   procedure Initialize
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String := "");
   --  Creates a Gtk.Button.Gtk_Button widget with a Gtk.Label.Gtk_Label child
   --  containing the given text.
   --  "label": The text you want the Gtk.Label.Gtk_Label to hold.

   procedure Gtk_New_With_Mnemonic
      (Button : out Gtk_Button;
       Label  : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Button : not null access Gtk_Button_Record'Class;
       Label  : UTF8_String);
   --  Creates a new Gtk.Button.Gtk_Button containing a label. If characters
   --  in Label are preceded by an underscore, they are underlined. If you need
   --  a literal underscore character in a label, use '__' (two underscores).
   --  The first underlined character represents a keyboard accelerator called
   --  a mnemonic. Pressing Alt and that key activates the button.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_button_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clicked (Button : not null access Gtk_Button_Record);
   --  Emits a Gtk.Button.Gtk_Button::clicked signal to the given
   --  Gtk.Button.Gtk_Button.

   procedure Enter (Button : not null access Gtk_Button_Record);
   pragma Obsolescent (Enter);
   --  Emits a Gtk.Button.Gtk_Button::enter signal to the given
   --  Gtk.Button.Gtk_Button.
   --  Deprecated since 2.20, Use the
   --  Gtk.Widget.Gtk_Widget::enter-notify-event signal.

   procedure Get_Alignment
      (Button : not null access Gtk_Button_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   --  Gets the alignment of the child in the button.
   --  Since: gtk+ 2.4
   --  "xalign": return location for horizontal alignment
   --  "yalign": return location for vertical alignment

   procedure Set_Alignment
      (Button : not null access Gtk_Button_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   --  Sets the alignment of the child. This property has no effect unless the
   --  child is a Gtk.Misc.Gtk_Misc or a Gtk.Alignment.Gtk_Alignment.
   --  Since: gtk+ 2.4
   --  "xalign": the horizontal position of the child, 0.0 is left aligned,
   --  1.0 is right aligned
   --  "yalign": the vertical position of the child, 0.0 is top aligned, 1.0
   --  is bottom aligned

   function Get_Event_Window
      (Button : not null access Gtk_Button_Record)
       return Gdk.Window.Gdk_Window;
   --  Returns the button's event window if it is realized, null otherwise.
   --  This function should be rarely needed.
   --  Since: gtk+ 2.22

   function Get_Focus_On_Click
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  Returns whether the button grabs focus when it is clicked with the
   --  mouse. See Gtk.Button.Set_Focus_On_Click.
   --  the mouse.
   --  Since: gtk+ 2.4

   procedure Set_Focus_On_Click
      (Button         : not null access Gtk_Button_Record;
       Focus_On_Click : Boolean);
   --  Sets whether the button will grab focus when it is clicked with the
   --  mouse. Making mouse clicks not grab focus is useful in places like
   --  toolbars where you don't want the keyboard focus removed from the main
   --  area of the application.
   --  Since: gtk+ 2.4
   --  "focus_on_click": whether the button grabs focus when clicked with the
   --  mouse

   function Get_Image
      (Button : not null access Gtk_Button_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the widget that is currenty set as the image of Button. This may
   --  have been explicitly set by Gtk.Button.Set_Image or constructed by
   --  Gtk.Button.Gtk_New_From_Stock.
   --  Since: gtk+ 2.6

   procedure Set_Image
      (Button : not null access Gtk_Button_Record;
       Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the image of Button to the given widget. Note that it depends on
   --  the Gtk.Settings.Gtk_Settings:gtk-button-images setting whether the
   --  image will be displayed or not, you don't have to call Gtk.Widget.Show
   --  on Image yourself.
   --  Since: gtk+ 2.6
   --  "image": a widget to set as the image for the button

   function Get_Image_Position
      (Button : not null access Gtk_Button_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Gets the position of the image relative to the text inside the button.
   --  Since: gtk+ 2.10

   procedure Set_Image_Position
      (Button   : not null access Gtk_Button_Record;
       Position : Gtk.Enums.Gtk_Position_Type);
   --  Sets the position of the image relative to the text inside the button.
   --  Since: gtk+ 2.10
   --  "position": the position

   function Get_Label
      (Button : not null access Gtk_Button_Record) return UTF8_String;
   --  Fetches the text from the label of the button, as set by
   --  Gtk.Button.Set_Label. If the label text has not been set the return
   --  value will be null. This will be the case if you create an empty button
   --  with gtk_button_new to use as a container.
   --  by the widget and must not be modified or freed.

   procedure Set_Label
      (Button : not null access Gtk_Button_Record;
       Label  : UTF8_String);
   --  Sets the text of the label of the button to Str. This text is also used
   --  to select the stock item if Gtk.Button.Set_Use_Stock is used.
   --  This will also clear any previously set labels.
   --  "label": a string

   function Get_Relief
      (Button : not null access Gtk_Button_Record)
       return Gtk.Enums.Gtk_Relief_Style;
   --  Returns the current relief style of the given Gtk.Button.Gtk_Button.

   procedure Set_Relief
      (Button   : not null access Gtk_Button_Record;
       Newstyle : Gtk.Enums.Gtk_Relief_Style);
   --  Sets the relief style of the edges of the given Gtk.Button.Gtk_Button
   --  widget. Three styles exist, GTK_RELIEF_NORMAL, GTK_RELIEF_HALF,
   --  GTK_RELIEF_NONE. The default style is, as one can guess,
   --  GTK_RELIEF_NORMAL.
   --  <!-- FIXME: put pictures of each style -->
   --  "newstyle": The GtkReliefStyle as described above.

   function Get_Use_Stock
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  Returns whether the button label is a stock item.
   --  select a stock item instead of being used directly as the label text.

   procedure Set_Use_Stock
      (Button    : not null access Gtk_Button_Record;
       Use_Stock : Boolean);
   --  If True, the label set on the button is used as a stock id to select
   --  the stock item for the button.
   --  "use_stock": True if the button should use a stock item

   function Get_Use_Underline
      (Button : not null access Gtk_Button_Record) return Boolean;
   --  Returns whether an embedded underline in the button label indicates a
   --  mnemonic. See gtk_button_set_use_underline ().
   --  indicates the mnemonic accelerator keys.

   procedure Set_Use_Underline
      (Button        : not null access Gtk_Button_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text of the button label indicates the
   --  next character should be used for the mnemonic accelerator key.
   --  "use_underline": True if underlines in the text indicate mnemonics

   procedure Leave (Button : not null access Gtk_Button_Record);
   pragma Obsolescent (Leave);
   --  Emits a Gtk.Button.Gtk_Button::leave signal to the given
   --  Gtk.Button.Gtk_Button.
   --  Deprecated since 2.20, Use the
   --  Gtk.Widget.Gtk_Widget::leave-notify-event signal.

   procedure Pressed (Button : not null access Gtk_Button_Record);
   pragma Obsolescent (Pressed);
   --  Emits a Gtk.Button.Gtk_Button::pressed signal to the given
   --  Gtk.Button.Gtk_Button.
   --  Deprecated since 2.20, Use the
   --  Gtk.Widget.Gtk_Widget::button-press-event signal.

   procedure Released (Button : not null access Gtk_Button_Record);
   pragma Obsolescent (Released);
   --  Emits a Gtk.Button.Gtk_Button::released signal to the given
   --  Gtk.Button.Gtk_Button.
   --  Deprecated since 2.20, Use the
   --  Gtk.Widget.Gtk_Widget::button-release-event signal.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Button_Record)
       return Gtk.Action.Gtk_Action;

   procedure Set_Related_Action
      (Self   : not null access Gtk_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Button_Record) return Boolean;

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Button_Record, Gtk_Button);
   function "+"
     (Widget : access Gtk_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Button_Record, Gtk_Button);
   function "+"
     (Widget : access Gtk_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Button
   renames Implements_Gtk_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Focus_On_Click_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Image_Property
   --  Type: Gtk.Widget.Gtk_Widget
   --  Flags: read-write
   --  The child widget to appear next to the button text.
   --
   --  Name: Image_Position_Property
   --  Type: Gtk.Enums.Gtk_Position_Type
   --  Flags: read-write
   --  The position of the image relative to the text inside the button.
   --
   --  Name: Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --
   --  Name: Relief_Property
   --  Type: Gtk.Enums.Gtk_Relief_Style
   --  Flags: read-write
   --
   --  Name: Use_Stock_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Use_Underline_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Xalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --  If the child of the button is a Gtk.Misc.Gtk_Misc or
   --  Gtk.Alignment.Gtk_Alignment, this property can be used to control its
   --  horizontal alignment. 0.0 is left aligned, 1.0 is right aligned.
   --
   --  Name: Yalign_Property
   --  Type: Gfloat
   --  Flags: read-write
   --  If the child of the button is a Gtk.Misc.Gtk_Misc or
   --  Gtk.Alignment.Gtk_Alignment, this property can be used to control its
   --  vertical alignment. 0.0 is top aligned, 1.0 is bottom aligned.

   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean;
   Image_Property : constant Glib.Properties.Property_Object;
   Image_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   Label_Property : constant Glib.Properties.Property_String;
   Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style;
   Use_Stock_Property : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   Xalign_Property : constant Glib.Properties.Property_Float;
   Yalign_Property : constant Glib.Properties.Property_Float;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  The ::activate signal on GtkButton is an action signal and emitting it
   --  causes the button to animate press then release. Applications should
   --  never connect to this signal, but use the Gtk.Button.Gtk_Button::clicked
   --  signal.
   --
   --  "clicked"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  Emitted when the button has been activated (pressed and released).
   --
   --  "enter"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  Emitted when the pointer enters the button.
   --
   --  "leave"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  Emitted when the pointer leaves the button.
   --
   --  "pressed"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  Emitted when the button is pressed.
   --
   --  "released"
   --     procedure Handler (Self : access Gtk_Button_Record'Class);
   --  Emitted when the button is released.

   Signal_Activate : constant Glib.Signal_Name := "activate";
   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   Signal_Enter : constant Glib.Signal_Name := "enter";
   Signal_Leave : constant Glib.Signal_Name := "leave";
   Signal_Pressed : constant Glib.Signal_Name := "pressed";
   Signal_Released : constant Glib.Signal_Name := "released";

private
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("focus-on-click");
   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Image_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("image-position");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style :=
     Gtk.Enums.Build ("relief");
   Use_Stock_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-stock");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
end Gtk.Button;
