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

--  <description>
--  Dialog boxes are a convenient way to prompt the user for a small amount of
--  input, e.g. to display a message, ask a question, or anything else that
--  does not require extensive effort on the user's part.
--
--  GTK+ treats a dialog as a window split vertically. The top section is a
--  Gtk.Box.Gtk_Vbox, and is where widgets such as a Gtk.Label.Gtk_Label or a
--  Gtk.GEntry.Gtk_Entry should be packed. The bottom area is known as the
--  "action area". This is generally used for packing buttons into the dialog
--  which may perform functions such as cancel, ok, or apply.
--
--  Gtk.Dialog.Gtk_Dialog boxes are created with a call to Gtk.Dialog.Gtk_New
--  or gtk_dialog_new_with_buttons. gtk_dialog_new_with_buttons is recommended;
--  it allows you to set the dialog title, some convenient flags, and add
--  simple buttons.
--
--  If "dialog" is a newly created dialog, the two primary areas of the window
--  can be accessed through Gtk.Dialog.Get_Content_Area and
--  Gtk.Dialog.Get_Action_Area, as can be seen from the example below.
--
--  A "modal" dialog (that is, one which freezes the rest of the application
--  from user input), can be created by calling Gtk.Window.Set_Modal on the
--  dialog. Use the GTK_WINDOW macro to cast the widget returned from
--  Gtk.Dialog.Gtk_New into a Gtk.Window.Gtk_Window. When using
--  gtk_dialog_new_with_buttons you can also pass the GTK_DIALOG_MODAL flag to
--  make a dialog modal.
--
--  If you add buttons to Gtk.Dialog.Gtk_Dialog using
--  gtk_dialog_new_with_buttons, Gtk.Dialog.Add_Button, gtk_dialog_add_buttons,
--  or Gtk.Dialog.Add_Action_Widget, clicking the button will emit a signal
--  called Gtk.Dialog.Gtk_Dialog::response with a response ID that you
--  specified. GTK+ will never assign a meaning to positive response IDs; these
--  are entirely user-defined. But for convenience, you can use the response
--  IDs in the Gtk_Response_Type enumeration (these all have values less than
--  zero). If a dialog receives a delete event, the
--  Gtk.Dialog.Gtk_Dialog::response signal will be emitted with a response ID
--  of GTK_RESPONSE_DELETE_EVENT.
--
--  If you want to block waiting for a dialog to return before returning
--  control flow to your code, you can call Gtk.Dialog.Run. This function
--  enters a recursive main loop and waits for the user to respond to the
--  dialog, returning the response ID corresponding to the button the user
--  clicked.
--
--  For the simple dialog in the following example, in reality you'd probably
--  use Gtk.Message_Dialog.Gtk_Message_Dialog to save yourself some effort. But
--  you'd need to create the dialog contents manually if you had more than a
--  simple message in the dialog.
--
--  An example for simple GtkDialog usage: |[<!-- language="C" --> // Function
--  to open a dialog box with a message void quick_message (GtkWindow *parent,
--  gchar *message) { GtkWidget *dialog, *label, *content_area; GtkDialogFlags
--  flags;
--
--  // Create the widgets flags = GTK_DIALOG_DESTROY_WITH_PARENT; dialog =
--  gtk_dialog_new_with_buttons ("Message", parent, flags, _("_OK"),
--  GTK_RESPONSE_NONE, NULL); content_area = gtk_dialog_get_content_area
--  (GTK_DIALOG (dialog)); label = gtk_label_new (message);
--
--  // Ensure that the dialog box is destroyed when the user responds
--
--  g_signal_connect_swapped (dialog, "response", G_CALLBACK
--  (gtk_widget_destroy), dialog);
--
--  // Add the label, and show everything we've added
--
--  gtk_container_add (GTK_CONTAINER (content_area), label);
--  gtk_widget_show_all (dialog); } ]|
--
--  # GtkDialog as GtkBuildable
--
--  The GtkDialog implementation of the Gtk.Buildable.Gtk_Buildable interface
--  exposes the Vbox and Action_Area as internal children with the names "vbox"
--  and "action_area".
--
--  GtkDialog supports a custom <action-widgets> element, which can contain
--  multiple <action-widget> elements. The "response" attribute specifies a
--  numeric response, and the content of the element is the id of widget (which
--  should be a child of the dialogs Action_Area). To mark a response as
--  default, set the "default" attribute of the <action-widget> element to
--  true.
--
--  GtkDialog supports adding action widgets by specifying "action" as the
--  "type" attribute of a <child> element. The widget will be added either to
--  the action area or the headerbar of the dialog, depending on the
--  "use-header-bar" property. The response id has to be associated with the
--  action widget using the <action-widgets> element.
--
--  An example of a Gtk.Dialog.Gtk_Dialog UI definition fragment: |[ <object
--  class="GtkDialog" id="dialog1"> <child type="action"> <object
--  class="GtkButton" id="button_cancel"/> </child> <child type="action">
--  <object class="GtkButton" id="button_ok"> <property
--  name="can-default">True</property> </object> </child> <action-widgets>
--  <action-widget response="cancel">button_cancel</action-widget>
--  <action-widget response="ok" default="true">button_ok</action-widget>
--  </action-widgets> </object> ]|
--
--  </description>
--  <description>
--  See Gtkada.Dialogs for a higher level dialog interface.
--
--  </description>
--  <screenshot>gtk-dialog</screenshot>
--  <group>Windows</group>
--  <testgtk>create_dialog.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Screen;      use Gdk.Screen;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;

package Gtk.Dialog is

   type Gtk_Dialog_Record is new Gtk_Window_Record with null record;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   type Gtk_Dialog_Flags is mod 8;
   for Gtk_Dialog_Flags'Size use Gint'Size;
   pragma Convention (C, Gtk_Dialog_Flags);
   Modal               : constant Gtk_Dialog_Flags := 2 ** 0;
   Destroy_With_Parent : constant Gtk_Dialog_Flags := 2 ** 1;
   Use_Header_Bar      : constant Gtk_Dialog_Flags := 2 ** 2;
   --  Various flags that can be set for the dialog, with the following
   --  implications:
   --     - Modal : the dialog is modal, see Gtk.Window.Set_Modal
   --     - Destroy_With_Parent: The dialog is destroyed if its parent is
   --       destroyed. See Gtk.Window.Set_Destroy_With_Parent
   --     - Use_Header_Bar: create dialogs with actions in the header bar
   --       instead of action area (since 3.12)

   type Gtk_Response_Type is new Gint;
   --  Type used for Response_Id's.
   --  Positive values are totally user-interpreted.
   --  GtkAda will sometimes return Gtk_Response_None if no Response_Id is
   --  available.
   --
   --  Typical usage is:
   --    if Gtk.Dialog.Run (Dialog) = Gtk_Response_Accept then
   --       blah;
   --    end if;

   Gtk_Response_None : constant Gtk_Response_Type := -1;
   --  GtkAda returns this if a response widget has no Response_Id,
   --  or if the dialog gets programmatically hidden or destroyed.

   Gtk_Response_Reject : constant Gtk_Response_Type := -2;
   Gtk_Response_Accept : constant Gtk_Response_Type := -3;
   --  GtkAda won't return these unless you pass them in
   --  as the response for an action widget. They are
   --  for your convenience.

   Gtk_Response_Delete_Event : constant Gtk_Response_Type := -4;
   --  If the dialog is deleted through the button in the titlebar

   Gtk_Response_OK     : constant Gtk_Response_Type := -5;
   Gtk_Response_Cancel : constant Gtk_Response_Type := -6;
   Gtk_Response_Close  : constant Gtk_Response_Type := -7;
   Gtk_Response_Yes    : constant Gtk_Response_Type := -8;
   Gtk_Response_No     : constant Gtk_Response_Type := -9;
   Gtk_Response_Apply  : constant Gtk_Response_Type := -10;
   Gtk_Response_Help   : constant Gtk_Response_Type := -11;
   --  These are returned from dialogs, and you can also use them
   --  yourself if you like.

   type Response_Type_Array is array (Natural range <>) of Gtk_Response_Type;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Dialog : out Gtk_Dialog);
   procedure Initialize (Dialog : not null access Gtk_Dialog_Record'Class);
   --  Creates a new dialog box.
   --  Widgets should not be packed into this Gtk.Window.Gtk_Window directly,
   --  but into the Vbox and Action_Area, as described above.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Dialog_New return Gtk_Dialog;
   --  Creates a new dialog box.
   --  Widgets should not be packed into this Gtk.Window.Gtk_Window directly,
   --  but into the Vbox and Action_Area, as described above.

   function Gtk_Dialog_New
      (Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags) return Gtk_Dialog;
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is used
   --  for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog).
   --  Since: gtk+ GtkAda 1.0

   procedure Gtk_New
      (Dialog : out Gtk_Dialog;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   procedure Initialize
      (Dialog : not null access Gtk_Dialog_Record'Class;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is used
   --  for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog).
   --  Since: gtk+ GtkAda 1.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Dialog      : not null access Gtk_Dialog_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Gtk_Response_Type);
   --  Adds an activatable widget to the action area of a
   --  Gtk.Dialog.Gtk_Dialog, connecting a signal handler that will emit the
   --  Gtk.Dialog.Gtk_Dialog::response signal on the dialog when the widget is
   --  activated. The widget is appended to the end of the dialog's action
   --  area. If you want to add a non-activatable widget, simply pack it into
   --  the Action_Area field of the Gtk.Dialog.Gtk_Dialog struct.
   --  "child": an activatable widget
   --  "response_id": response ID for Child

   function Add_Button
      (Dialog      : not null access Gtk_Dialog_Record;
       Text        : UTF8_String;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   --  Adds a button with the given text and sets things up so that clicking
   --  the button will emit the Gtk.Dialog.Gtk_Dialog::response signal with the
   --  given Response_Id. The button is appended to the end of the dialog's
   --  action area. The button widget is returned, but usually you don't need
   --  it.
   --  "text": text of button
   --  "response_id": response ID for the button

   function Get_Action_Area
      (Dialog : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   pragma Obsolescent (Get_Action_Area);
   --  Returns the action area of Dialog.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.12, 1

   function Get_Content_Area
      (Dialog : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   --  Returns the content area of Dialog.
   --  Since: gtk+ 2.14

   function Get_Header_Bar
      (Dialog : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the header bar of Dialog. Note that the headerbar is only used
   --  by the dialog if the Gtk.Dialog.Gtk_Dialog:use-header-bar property is
   --  True.
   --  Since: gtk+ 3.12

   function Get_Response_For_Widget
      (Dialog : not null access Gtk_Dialog_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Response_Type;
   --  Gets the response id of a widget in the action area of a dialog.
   --  Since: gtk+ 2.8
   --  "widget": a widget in the action area of Dialog

   function Get_Widget_For_Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   --  Gets the widget button that uses the given response ID in the action
   --  area of a dialog.
   --  Since: gtk+ 2.20
   --  "response_id": the response ID used by the Dialog widget

   procedure Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   --  Emits the Gtk.Dialog.Gtk_Dialog::response signal with the given
   --  response ID. Used to indicate that the user has responded to the dialog
   --  in some way; typically either you or Gtk.Dialog.Run will be monitoring
   --  the ::response signal and take appropriate action.
   --  "response_id": response ID

   function Run
      (Dialog : not null access Gtk_Dialog_Record) return Gtk_Response_Type;
   --  Blocks in a recursive main loop until the Dialog either emits the
   --  Gtk.Dialog.Gtk_Dialog::response signal, or is destroyed. If the dialog
   --  is destroyed during the call to Gtk.Dialog.Run, Gtk.Dialog.Run returns
   --  GTK_RESPONSE_NONE. Otherwise, it returns the response ID from the
   --  ::response signal emission.
   --  Before entering the recursive main loop, Gtk.Dialog.Run calls
   --  Gtk.Widget.Show on the dialog for you. Note that you still need to show
   --  any children of the dialog yourself.
   --  During Gtk.Dialog.Run, the default behavior of
   --  Gtk.Widget.Gtk_Widget::delete-event is disabled; if the dialog receives
   --  ::delete_event, it will not be destroyed as windows usually are, and
   --  Gtk.Dialog.Run will return GTK_RESPONSE_DELETE_EVENT. Also, during
   --  Gtk.Dialog.Run the dialog will be modal. You can force Gtk.Dialog.Run to
   --  return at any time by calling Gtk.Dialog.Response to emit the ::response
   --  signal. Destroying the dialog during Gtk.Dialog.Run is a very bad idea,
   --  because your post-run code won't know whether the dialog was destroyed
   --  or not.
   --  After Gtk.Dialog.Run returns, you are responsible for hiding or
   --  destroying the dialog if you wish to do so.
   --  Typical usage of this function might be: |[<!-- language="C" -->
   --  GtkWidget *dialog = gtk_dialog_new (); // Set up dialog...
   --  int result = gtk_dialog_run (GTK_DIALOG (dialog)); switch (result) {
   --  case GTK_RESPONSE_ACCEPT: // do_application_specific_something ();
   --  break; default: // do_nothing_since_dialog_was_cancelled (); break; }
   --  gtk_widget_destroy (dialog); ]|
   --  Note that even though the recursive main loop gives the effect of a
   --  modal dialog (it prevents the user from interacting with other windows
   --  in the same window group while the dialog is run), callbacks such as
   --  timeouts, IO channel watches, DND drops, etc, will be triggered during a
   --  Gtk.Dialog.Run call.

   procedure Set_Default_Response
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   --  Sets the last widget in the dialog's action area with the given
   --  Response_Id as the default widget for the dialog. Pressing "Enter"
   --  normally activates the default widget.
   --  "response_id": a response ID

   procedure Set_Response_Sensitive
      (Dialog      : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type;
       Setting     : Boolean);
   --  Calls `gtk_widget_set_sensitive (widget, Setting)` for each widget in
   --  the dialog's action area with the given Response_Id. A convenient way to
   --  sensitize/desensitize dialog buttons.
   --  "response_id": a response ID
   --  "setting": True for sensitive

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Set_Alternative_Button_Order_From_Array
     (Dialog    : access Gtk_Dialog_Record;
      New_Order : Response_Type_Array);
   --  Sets an alternative button order. If the gtk-alternative-button-order
   --  setting is set to %TRUE, the dialog buttons are reordered according to
   --  the order of the response ids passed to this function.
   --
   --  By default, GTK+ dialogs use the button order advocated by the Gnome
   --  Human Interface Guidelines with the affirmative button at the far right,
   --  and the cancel button left of it. But the builtin GTK+ dialogs and
   --  message dialogs' do provide an alternative button order, which is more
   --  suitable on some platforms, e.g. Windows.
   --
   --  Use this function after adding all the buttons to your dialog.

   function Gtk_Alternative_Dialog_Button_Order
     (Screen : Gdk.Screen.Gdk_Screen := null)  return Boolean;
   --  Returns True if dialogs are expected to use an alternative button order
   --  on the given screen (or current screen if null) . See
   --  Set_Alternative_Button_Order_From_Array for more details about
   --  alternative button order.
   --
   --  If you need to use this function, you should probably connect to the
   --  ::notify:gtk-alternative-button-order signal on the Gtk_Settings object
   --  associated to Screen, in order to be notified if the button order
   --  setting changes.
   --
   --  Returns: Whether the alternative button order should be used

   function Use_Header_Bar_From_Settings
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class := null)
   return Gtk_Dialog_Flags;
   --  Check in the gtk settings whether dialogs should display their action
   --  buttons in the header bar rather than in the action area at the bottom.
   --  Widget is used to retrieve the settings. If unspecified, the default
   --  settings are used.
   --  The value of the setting can be set in the file
   --    $HOME/.config/gtk-3.0/settings.ini
   --  with the following line:
   --    gtk-dialogs-use-header=0

   procedure G_New_Dialog
     (Self  : not null access Gtk_Dialog_Record'Class;
      Flags : Gtk_Dialog_Flags;
      Typ   : Glib.GType := Gtk.Dialog.Get_Type);
   --  Equivalent of Glib.Object.G_New for a dialog. This function should be
   --  used when you are subclassing the dialog class (for instance to add new
   --  signals). The Use_Header_Bar flag can only have an impact before the
   --  dialog is created, so this function will take that into account as
   --  appropriate. Other flags (Modal and Destroy_With_Parent) are ignored.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Use_Header_Bar_Property : constant Glib.Properties.Property_Int;
   --  True if the dialog uses a Gtk.Header_Bar.Gtk_Header_Bar for action
   --  buttons instead of the action-area.
   --
   --  For technical reasons, this property is declared as an integer
   --  property, but you should only set it to True or False.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Dialog_Void is not null access procedure (Self : access Gtk_Dialog_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Close : constant Glib.Signal_Name := "close";
   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Void;
       After : Boolean := False);
   procedure On_Close
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::close signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user uses a keybinding to close the dialog.
   --
   --  The default binding for this signal is the Escape key.

   type Cb_Gtk_Dialog_Gtk_Response_Type_Void is not null access procedure
     (Self        : access Gtk_Dialog_Record'Class;
      Response_Id : Gtk_Response_Type);

   type Cb_GObject_Gtk_Response_Type_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Response_Id : Gtk_Response_Type);

   Signal_Response : constant Glib.Signal_Name := "response";
   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_Gtk_Dialog_Gtk_Response_Type_Void;
       After : Boolean := False);
   procedure On_Response
      (Self  : not null access Gtk_Dialog_Record;
       Call  : Cb_GObject_Gtk_Response_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an action widget is clicked, the dialog receives a delete
   --  event, or the application programmer calls Gtk.Dialog.Response. On a
   --  delete event, the response ID is GTK_RESPONSE_DELETE_EVENT. Otherwise,
   --  it depends on which action widget was clicked.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Dialog
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Header_Bar_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("use-header-bar");
end Gtk.Dialog;
