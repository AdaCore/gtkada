-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Bin;
with Gtk.Enums;

package Gtk.Button is

   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   procedure Gtk_New (Button : out Gtk_Button; Label : String := "");
   --  Create a new button.
   --  if Label is not the empty string, then the text appears in the
   --  button (and the child of the button is a Gtk_Label). On the other
   --  hand, if Label is the empty string, then no child is created for
   --  the button and it is your responsibility to add one. This is the
   --  recommended way to put a pixmap inside the button.

   procedure Gtk_New_From_Stock
     (Button : out Gtk_Button; Stock_Id : String);
   --  Create a new button containing the image and text from a stock item.
   --  Some stock ids have predefined contants like Gtk_Stock_Button_OK and
   --  Gtk_Stock_Button_Apply.
   --  Stock_Id: the name of the stock item

   procedure Gtk_New_With_Mnemonic
     (Button : out Gtk_Button; Label : String);
   --  Create a new button containing a label.
   --  Label: The text of the button, with an underscore in front of the
   --         mnemonic character
   --  If characters in Label are preceded by an underscore, they are
   --  underlined indicating that they represent a keyboard accelerator called
   --  a mnemonic. Pressing Alt and that key activates the button.

   procedure Initialize
     (Button : access Gtk_Button_Record'Class;
      Label  : String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Initialize_From_Stock
     (Button   : access Gtk_Button_Record'Class;
      Stock_Id : String);
   --  Internal initialization function.

   procedure Initialize_With_Mnemonic
     (Button : access Gtk_Button_Record'Class;
      Label  : String);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Button.

   procedure Set_Relief
     (Button    : access Gtk_Button_Record;
      New_Style : Gtk.Enums.Gtk_Relief_Style);
   --  Modify the relief style for the button.
   --  This modifies only its visual aspect, not its behavior.

   function Get_Relief
     (Button : access Gtk_Button_Record) return Gtk.Enums.Gtk_Relief_Style;
   --  Get the current relief style for the button

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

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Emitted when the button has been clicked on by the user. This is the
   --    signal you should use to start your own actions.
   --
   --  - "pressed"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Emitted when the user presses the mouse button on
   --    the widget. The default implementation modifies the widget state
   --    and its visual aspect.
   --
   --  - "released"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Emitted when the user releases the mouse button and
   --    is inside of the widget. The default implementation modifies the
   --    widget state and its visual aspect. If the mouse is still inside
   --    the widget, then the "clicked" signal is emitted.
   --
   --  - "enter"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Emitted when the mouse enters the button. The clicked
   --    signal can only be emitted when the mouse is inside the button.
   --
   --  - "leave"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Emitted when the mouse leaves the button.
   --
   --  </signals>

private
   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with null record;
   pragma Import (C, Get_Type, "gtk_button_get_type");
end Gtk.Button;
