-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--  <c_version>1.2.6</c_version>

with Gtk.Object;
with Gtk.Bin;
with Gtk.Enums;

package Gtk.Button is

   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with private;
   type Gtk_Button is access all Gtk_Button_Record'Class;

   procedure Gtk_New (Button : out Gtk_Button;
                      Label : in String := "");
   --  Creates a new button.
   --  if LABEL is not the empty string, then the text appears in the
   --  button (and the child of the button is a Gtk_Label). On the other
   --  hand, if LABEL is the empty string, then no child is created for
   --  the button and it is your responsability to add one. This is the
   --  recommended way to put a pixmap inside the button.

   procedure Initialize (Button : access Gtk_Button_Record'Class;
                         Label : in String);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Relief (Button   : access Gtk_Button_Record;
                         NewStyle : in     Gtk.Enums.Gtk_Relief_Style);
   --  Modifies the relief style for the button.
   --  This modifies only its visual aspect, not its behavior.

   function Get_Relief (Button : access Gtk_Button_Record)
                       return Gtk.Enums.Gtk_Relief_Style;
   --  Get the current relief style for the button

   ----------------------
   -- Signals emission --
   ----------------------

   procedure Pressed  (Button : access Gtk_Button_Record);
   --  Sends the "pressed" signal to the button

   procedure Released (Button : access Gtk_Button_Record);
   --  Sends the "release" signal to the button

   procedure Clicked  (Button : access Gtk_Button_Record);
   --  Sends the "clicked" signal to the button

   procedure Enter    (Button : access Gtk_Button_Record);
   --  Sends the "enter" signal to the button

   procedure Leave    (Button : access Gtk_Button_Record);
   --  Sends the "leave" signal to the button

   ----------------------------
   -- Support for GATE/DGATE --
   ----------------------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
   --  Gate internal function

   procedure Generate (Button : in out Gtk.Object.Gtk_Object;
                       N      : in Node_Ptr);
   --  Dgate internal function

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    Calls when the button has been clicked on by the user. This is the
   --    signal you should use to start your own actions.
   --
   --  - "pressed"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    This signal is emitted when the user presses the mouse button on
   --    the widget. The default implementation modifies the widget state
   --    and its visual aspect.
   --
   --  - "released"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    This signal is emitted when the user releases the mouse button and
   --    is inside of the widget. The default implementation modifies the
   --    widget state and its visual aspect. If the mouse is still inside
   --    the widget, then the "clicked" signal is emitted.
   --
   --  - "enter"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    This signal is emitted when the mouse enters the button. The clicked
   --    signal can only be emitted when the mouse is inside the button.
   --
   --  - "leave"
   --    procedure Handler (Button : access Gtk_Button_Record'Class);
   --
   --    This signal is emitted when the mouse leaves the button.
   --
   --  </signals>

private
   type Gtk_Button_Record is new Bin.Gtk_Bin_Record with null record;

end Gtk.Button;
