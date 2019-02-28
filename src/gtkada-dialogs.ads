------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2019, AdaCore                     --
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
--
--  This package provides a ready to use high level dialog capability.
--
--  </description>
--  <group>Windows</group>
--  <see>Gtk.Message_Dialog</see>

with Glib;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Window; use Gtk.Window;

package Gtkada.Dialogs is
   pragma Elaborate_Body;

   type Message_Dialog_Buttons is mod 2 ** 32;
   --  Define the set of values a button in a message dialog box can have.

   type Button_Range is range 0 .. 8;
   --  The range of valid buttons.

   Button_None   : constant Message_Dialog_Buttons := 0;
   Button_Yes    : constant Message_Dialog_Buttons := 2 ** 0;
   Button_No     : constant Message_Dialog_Buttons := 2 ** 1;
   Button_All    : constant Message_Dialog_Buttons := 2 ** 2;
   Button_OK     : constant Message_Dialog_Buttons := 2 ** 3;
   Button_Cancel : constant Message_Dialog_Buttons := 2 ** 4;
   Button_Abort  : constant Message_Dialog_Buttons := 2 ** 5;
   Button_Retry  : constant Message_Dialog_Buttons := 2 ** 6;
   Button_Ignore : constant Message_Dialog_Buttons := 2 ** 7;
   Button_Help   : constant Message_Dialog_Buttons := 2 ** 8;

   type Message_Dialog_Type is
     (Warning,
      --  Message box with a yellow exclamation point.

      Error,
      --  Message box with a red stop sign.

      Information,
      --  Message box with a blue "i".

      Confirmation,
      --  Message box with a blue question mark.

      Custom
      --  Message box with no pixmap. The caption of the box should be set by
      --  the user.
     );
   --  Define the values describing the type of message box.
   --  Used by the Message_Dialog function.

   function Message_Dialog
     (Msg            : Glib.UTF8_String;
      Dialog_Type    : Message_Dialog_Type := Information;
      Buttons        : Message_Dialog_Buttons := Button_OK or Button_Help;
      Default_Button : Message_Dialog_Buttons := Button_OK;
      Help_Msg       : Glib.UTF8_String := "";
      Title          : Glib.UTF8_String := "";
      Justification  : Gtk_Justification := Justify_Center;
      Parent         : Gtk.Window.Gtk_Window := null;
      Icon_Name      : String := "")
      return Message_Dialog_Buttons;
   --  Display a message dialog box centered on the mouse.
   --  This will create a dialog box containing the specified message.
   --  Dialog_Type indicates the purpose of the dialog.
   --  Buttons indicates which buttons should appear in the dialog.
   --  Help_Msg is the message displayed in a separate dialog box when the help
   --  button is pressed while the dialog is displayed.
   --  If Help_Msg is null, a dialog containing the message
   --  "No help available" will be displayed. In both cases, the dialog
   --  displayed will only have a OK button.
   --  If Title is null, a default title will be chosen depending on the value
   --  of Dialog_Type.
   --  If Icon_Name is specified, this icon will be displayed instead of the
   --  default one.
   --  The dialog will be centered with regards to Parent
   --
   --  This function will return only after the user pressed one of the buttons
   --  or deleted the dialog, by running an additional level of main loop.
   --  One of the following values will be returned:
   --    - Button_None
   --    - Button_Abort
   --    - Button_Yes
   --    - Button_Ok
   --    - Button_Retry
   --    - Button_No
   --    - Button_Cancel
   --    - Button_Ignore
   --    - Button_All

   function Create_Gtk_Dialog
     (Msg           : Glib.UTF8_String;
      Dialog_Type   : Message_Dialog_Type := Information;
      Title         : Glib.UTF8_String := "";
      Justification : Gtk_Justification := Justify_Center;
      Parent        : Gtk.Window.Gtk_Window := null;
      Icon_Name     : String := "")
      return Gtk.Dialog.Gtk_Dialog;
   --  Convenience function to create a new dialog.
   --  This function was introduced in GtkAda 2.0 to provide a compatibility
   --  with Message_Dialog, while using the standard Gtk.Dialog. You should add
   --  the buttons yourself, through Gtk.Dialog.Gtk_Dialog, and then display
   --  the dialog on the screen through Gtk.Dialog.Run.
   --  If Icon_Name is specified, this icon will be displayed instead of the
   --  default one.
   --  As opposed to Message_Dialog, you can provide your own custom buttons if
   --  needed.

end Gtkada.Dialogs;
