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

--  Collects the arguments that are needed to present a message to the user.
--
--  The message is shown with the [methodGtk.AlertDialog.choose] function.
--
--  If you don't need to wait for a button to be clicked, you can use
--  [methodGtk.AlertDialog.show].
--
--  <group>Dialogs</group>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;     use GNAT.Strings;
with Glib;             use Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Gtk.Window;       use Gtk.Window;

package Gtk.Alert_Dialog is

   type Gtk_Alert_Dialog_Record is new GObject_Record with null record;
   type Gtk_Alert_Dialog is access all Gtk_Alert_Dialog_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the [thread-default main
   --  context][g-main-context-push-thread-default] where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_alert_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Choose
      (Self        : not null access Gtk_Alert_Dialog_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Shows the alert to the user.
   --  It is ok to pass `NULL` for the callback if the alert does not have
   --  more than one button. A simpler API for this case is
   --  [methodGtk.AlertDialog.show].
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Choose_Finish
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Result : Glib.G_Async_Result) return Glib.Gint;
   --  Finishes the [methodGtk.AlertDialog.choose] call.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @return the index of the button that was clicked, or -1 if the dialog
   --  was cancelled and [propertyGtk.AlertDialog:cancel-button] is not set

   function Get_Buttons
      (Self : not null access Gtk_Alert_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the button labels for the alert.
   --  Since: gtk+ 4.10
   --  @return the button labels

   procedure Set_Buttons
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Labels : GNAT.Strings.String_List);
   --  Sets the button labels for the alert.
   --  Since: gtk+ 4.10
   --  @param Labels the new button labels

   function Get_Cancel_Button
      (Self : not null access Gtk_Alert_Dialog_Record) return Glib.Gint;
   --  Returns the index of the cancel button.
   --  Since: gtk+ 4.10
   --  @return the index of the cancel button, or -1

   procedure Set_Cancel_Button
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Button : Glib.Gint);
   --  Sets the index of the cancel button.
   --  See [propertyGtk.AlertDialog:cancel-button] for details of how this
   --  value is used.
   --  Since: gtk+ 4.10
   --  @param Button the new cancel button

   function Get_Default_Button
      (Self : not null access Gtk_Alert_Dialog_Record) return Glib.Gint;
   --  Returns the index of the default button.
   --  Since: gtk+ 4.10
   --  @return the index of the default button, or -1

   procedure Set_Default_Button
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Button : Glib.Gint);
   --  Sets the index of the default button.
   --  See [propertyGtk.AlertDialog:default-button] for details of how this
   --  value is used.
   --  Since: gtk+ 4.10
   --  @param Button the new default button

   function Get_Detail
      (Self : not null access Gtk_Alert_Dialog_Record) return UTF8_String;
   --  Returns the detail text that will be shown in the alert.
   --  Since: gtk+ 4.10
   --  @return the detail text

   procedure Set_Detail
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Detail : UTF8_String);
   --  Sets the detail text that will be shown in the alert.
   --  Since: gtk+ 4.10
   --  @param Detail the new detail text

   function Get_Message
      (Self : not null access Gtk_Alert_Dialog_Record) return UTF8_String;
   --  Returns the message that will be shown in the alert.
   --  Since: gtk+ 4.10
   --  @return the message

   procedure Set_Message
      (Self    : not null access Gtk_Alert_Dialog_Record;
       Message : UTF8_String);
   --  Sets the message that will be shown in the alert.
   --  Since: gtk+ 4.10
   --  @param Message the new message

   function Get_Modal
      (Self : not null access Gtk_Alert_Dialog_Record) return Boolean;
   --  Returns whether the alert blocks interaction with the parent window
   --  while it is presented.
   --  Since: gtk+ 4.10
   --  @return true if the alert is modal

   procedure Set_Modal
      (Self  : not null access Gtk_Alert_Dialog_Record;
       Modal : Boolean);
   --  Sets whether the alert blocks interaction with the parent window while
   --  it is presented.
   --  Since: gtk+ 4.10
   --  @param Modal the new value

   procedure Show
      (Self   : not null access Gtk_Alert_Dialog_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Shows the alert to the user.
   --  This function is a simpler version of [methodGtk.AlertDialog.choose]
   --  intended for dialogs with a single button.
   --  If you want to cancel the dialog or if the alert has more than one
   --  button, you should use that function instead and provide it with a
   --  [classGio.Cancellable] and callback respectively.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Self    : out Gtk_Alert_Dialog;
      Message : UTF8_String := "");
   procedure Initialize
     (Self    : not null access Gtk_Alert_Dialog_Record'Class;
      Message : UTF8_String := "");
   --  Creates a new alert dialog. The Message text can be left empty
   --  and later set with Set_Message.

   function Gtk_Alert_Dialog_New
     (Message : UTF8_String := "") return Gtk_Alert_Dialog;
   --  Creates a new alert dialog with the given message.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Buttons_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("buttons");--  Unknown type: unspecified

   Cancel_Button_Property : constant Glib.Properties.Property_Int;
   --  Determines what happens when the <kbd>Escape</kbd> key is pressed while
   --  the alert is shown.
   --
   --  If this property holds the index of a button in
   --  [propertyGtk.AlertDialog:buttons], then pressing Escape is treated as if
   --  that button was pressed. If it is -1 or not a valid index for the
   --  `buttons` array, then an error is returned.
   --
   --  If `buttons` is `NULL`, then the automatically created 'Close' button
   --  is treated as both cancel and default button, so 0 is returned.

   Default_Button_Property : constant Glib.Properties.Property_Int;
   --  Determines what happens when the <kbd>Return</kbd> key is pressed while
   --  the alert is shown.
   --
   --  If this property holds the index of a button in
   --  [propertyGtk.AlertDialog:buttons], then pressing Return is treated as if
   --  that button was pressed. If it is -1 or not a valid index for the
   --  `buttons` array, then nothing happens.
   --
   --  If `buttons` is `NULL`, then the automatically created 'Close' button
   --  is treated as both cancel and default button, so 0 is returned.

   Detail_Property : constant Glib.Properties.Property_String;
   --  The detail text for the alert.

   Message_Property : constant Glib.Properties.Property_String;
   --  The message for the alert.

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the alert is modal.

private
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Message_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("message");
   Detail_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("detail");
   Default_Button_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("default-button");
   Cancel_Button_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("cancel-button");
end Gtk.Alert_Dialog;
