-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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
--  This package implements a general button widget. This button can be clicked
--  on by the user to start any action. This button does not have multiple
--  states, it can just be temporarily pressed while the mouse is on it, but
--  does not keep its pressed state.
-- 
--  The gtk+ sources provide the following drawing that explains the role of
--  the various spacings that can be set for a button:
-- 
--  </description>
--  <screenshot>gtk-button</screenshot>
--  <group>Buttons and Toggles</group>
--  <testgtk>create_buttons.adb</testgtk>

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Button is

   type Gtk_Button_Record is new Gtk_Bin_Record with null record;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_From_Stock
      (Self     : out Gtk_Button;
       Stock_Id : UTF8_String);
   procedure Initialize_From_Stock
      (Self     : access Gtk_Button_Record'Class;
       Stock_Id : UTF8_String);
   --  Creates a new Gtk.Button.Gtk_Button containing the image and text from a
   --  stock item. Some stock ids have preprocessor macros like GTK_STOCK_OK and
   --  GTK_STOCK_APPLY. If @stock_id is unknown, then it will be treated as a
   --  mnemonic label (as for Gtk.Button.Gtk_New_With_Mnemonic).
   --  "stock_id": the name of the stock item

   procedure Gtk_New (Self : out Gtk_Button; Label : UTF8_String := "");
   procedure Initialize
      (Self  : access Gtk_Button_Record'Class;
       Label : UTF8_String := "");

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Button;
       Label : UTF8_String);
   procedure Initialize_With_Mnemonic
      (Self  : access Gtk_Button_Record'Class;
       Label : UTF8_String);
   --  Creates a new Gtk.Button.Gtk_Button containing a label. If characters in
   --  @label are preceded by an underscore, they are underlined. If you need a
   --  literal underscore character in a label, use '__' (two underscores). The
   --  first underlined character represents a keyboard accelerator called a
   --  mnemonic. Pressing Alt and that key activates the button.
   --  "label": The text of the button, with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_button_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clicked (Self : access Gtk_Button_Record);

   procedure Enter (Self : access Gtk_Button_Record);

   procedure Get_Alignment
      (Self   : access Gtk_Button_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   procedure Set_Alignment
      (Self   : access Gtk_Button_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   --  Sets the alignment of the child. This property has no effect unless the
   --  child is a Gtk.Misc.Gtk_Misc or a Gtk.Aligment.Gtk_Aligment.
   --  Since: gtk+ 2.4
   --  "xalign": the horizontal position of the child, 0.0 is left aligned, 1.0
   --  is right aligned
   --  "yalign": the vertical position of the child, 0.0 is top aligned, 1.0 is
   --  bottom aligned

   function Get_Event_Window
      (Self : access Gtk_Button_Record) return Gdk.Window.Gdk_Window;
   --  Returns the button's event window if it is realized, null otherwise.
   --  This function should be rarely needed.
   --  Since: gtk+ 2.22

   function Get_Focus_On_Click
      (Self : access Gtk_Button_Record) return Boolean;
   procedure Set_Focus_On_Click
      (Self           : access Gtk_Button_Record;
       Focus_On_Click : Boolean);
   --  Sets whether the button will grab focus when it is clicked with the
   --  mouse. Making mouse clicks not grab focus is useful in places like
   --  toolbars where you don't want the keyboard focus removed from the main
   --  area of the application.
   --  Since: gtk+ 2.4
   --  "focus_on_click": whether the button grabs focus when clicked with the
   --  mouse

   function Get_Image (Self : access Gtk_Button_Record) return Gtk_Widget;
   procedure Set_Image
      (Self  : access Gtk_Button_Record;
       Image : access Gtk_Widget_Record'Class);
   --  Set the image of @button to the given widget. Note that it depends on
   --  the Gtk.Settings.Gtk_Settings:gtk-button-images setting whether the image
   --  will be displayed or not, you don't have to call Gtk.Widget.Show on
   --  @image yourself.
   --  Since: gtk+ 2.6
   --  "image": a widget to set as the image for the button

   function Get_Image_Position
      (Self : access Gtk_Button_Record) return Gtk.Enums.Gtk_Position_Type;
   procedure Set_Image_Position
      (Self     : access Gtk_Button_Record;
       Position : Gtk.Enums.Gtk_Position_Type);
   --  Sets the position of the image relative to the text inside the button.
   --  Since: gtk+ 2.10
   --  "position": the position

   function Get_Label (Self : access Gtk_Button_Record) return UTF8_String;
   procedure Set_Label
      (Self  : access Gtk_Button_Record;
       Label : UTF8_String);
   --  Sets the text of the label of the button to @str. This text is also used
   --  to select the stock item if Gtk.Button.Set_Use_Stock is used. This will
   --  also clear any previously set labels.
   --  "label": a string

   function Get_Relief
      (Self : access Gtk_Button_Record) return Gtk.Enums.Gtk_Relief_Style;
   procedure Set_Relief
      (Self     : access Gtk_Button_Record;
       Newstyle : Gtk.Enums.Gtk_Relief_Style);

   function Get_Use_Stock (Self : access Gtk_Button_Record) return Boolean;
   procedure Set_Use_Stock
      (Self      : access Gtk_Button_Record;
       Use_Stock : Boolean);
   --  If True, the label set on the button is used as a stock id to select the
   --  stock item for the button.
   --  "use_stock": True if the button should use a stock item

   function Get_Use_Underline
      (Self : access Gtk_Button_Record) return Boolean;
   procedure Set_Use_Underline
      (Self          : access Gtk_Button_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text of the button label indicates the next
   --  character should be used for the mnemonic accelerator key.
   --  "use_underline": True if underlines in the text indicate mnemonics

   procedure Leave (Self : access Gtk_Button_Record);

   procedure Pressed (Self : access Gtk_Button_Record);

   procedure Released (Self : access Gtk_Button_Record);

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
   --  Type: Object
   --  Flags: read-write
   -- 
   --  Name: Image_Position_Property
   --  Type: Gtk_Position_Type
   --  Flags: read-write
   --  The position of the image relative to the text inside the button.
   -- 
   --  Name: Label_Property
   --  Type: String
   --  Flags: read-write
   -- 
   --  Name: Relief_Property
   --  Type: Gtk_Relief_Style
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
   --  Type: Float
   --  Flags: read-write
   --  If the child of the button is a #GtkMisc or #GtkAlignment, this property
   --  can be used to control it's horizontal alignment. 0.0 is left aligned,
   --  1.0 is right aligned.
   -- 
   --  Name: Yalign_Property
   --  Type: Float
   --  Flags: read-write
   --  If the child of the button is a #GtkMisc or #GtkAlignment, this property
   --  can be used to control it's vertical alignment. 0.0 is top aligned, 1.0
   --  is bottom aligned.

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
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  The ::activate signal on GtkButton is an action signal and emitting it
   --  causes the button to animate press then release. Applications should
   --  never connect to this signal, but use the #GtkButton::clicked signal.
   -- 
   --  "clicked"
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  Emitted when the button has been activated (pressed and released).
   -- 
   --  "enter"
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  Emitted when the pointer enters the button.
   -- 
   --  "leave"
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  Emitted when the pointer leaves the button.
   -- 
   --  "pressed"
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  Emitted when the button is pressed.
   -- 
   --  "released"
   --  function Handler (Self : access Gtk_Button_Record'Class) return none;
   --  Emitted when the button is released.

   Signal_Activate : constant Glib.Signal_Name := "activate";
   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   Signal_Enter : constant Glib.Signal_Name := "enter";
   Signal_Leave : constant Glib.Signal_Name := "leave";
   Signal_Pressed : constant Glib.Signal_Name := "pressed";
   Signal_Released : constant Glib.Signal_Name := "released";

private
   Focus_On_Click_Property : constant Glib.Properties.Property_Boolean:=
     Glib.Properties.Build ("Focus_On_Click_Property");
   Image_Property : constant Glib.Properties.Property_Object:=
     Glib.Properties.Build ("Image_Property");
   Image_Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type:=
     Gtk.Enums.Build ("Image_Position_Property");
   Label_Property : constant Glib.Properties.Property_String:=
     Glib.Properties.Build ("Label_Property");
   Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style:=
     Gtk.Enums.Build ("Relief_Property");
   Use_Stock_Property : constant Glib.Properties.Property_Boolean:=
     Glib.Properties.Build ("Use_Stock_Property");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean:=
     Glib.Properties.Build ("Use_Underline_Property");
   Xalign_Property : constant Glib.Properties.Property_Float:=
     Glib.Properties.Build ("Xalign_Property");
   Yalign_Property : constant Glib.Properties.Property_Float:=
     Glib.Properties.Build ("Yalign_Property");
end Gtk.Button;
