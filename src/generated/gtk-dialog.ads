------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Dialogs are a convenient way to prompt the user for a small amount of
--  input.
--
--  <picture> <source srcset="dialog-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkDialog" src="dialog.png"> </picture>
--  Typical uses are to display a message, ask a question, or anything else
--  that does not require extensive effort on the user's part.
--
--  The main area of a `GtkDialog` is called the "content area", and is yours
--  to populate with widgets such a `GtkLabel` or `GtkEntry`, to present your
--  information, questions, or tasks to the user.
--
--  In addition, dialogs allow you to add "action widgets". Most commonly,
--  action widgets are buttons. Depending on the platform, action widgets may
--  be presented in the header bar at the top of the window, or at the bottom
--  of the window. To add action widgets, create your `GtkDialog` using
--  [ctorGtk.Dialog.new_with_buttons], or use [methodGtk.Dialog.add_button],
--  [methodGtk.Dialog.add_buttons], or [methodGtk.Dialog.add_action_widget].
--
--  `GtkDialogs` uses some heuristics to decide whether to add a close button
--  to the window decorations. If any of the action buttons use the response ID
--  GTK_RESPONSE_CLOSE or GTK_RESPONSE_CANCEL, the close button is omitted.
--
--  Clicking a button that was added as an action widget will emit the
--  [signalGtk.Dialog::response] signal with a response ID that you specified.
--  GTK will never assign a meaning to positive response IDs; these are
--  entirely user-defined. But for convenience, you can use the response IDs in
--  the [enumGtk.ResponseType] enumeration (these all have values less than
--  zero). If a dialog receives a delete event, the
--  [signalGtk.Dialog::response] signal will be emitted with the
--  GTK_RESPONSE_DELETE_EVENT response ID.
--
--  Dialogs are created with a call to [ctorGtk.Dialog.new] or
--  [ctorGtk.Dialog.new_with_buttons]. The latter is recommended; it allows you
--  to set the dialog title, some convenient flags, and add buttons.
--
--  A "modal" dialog (that is, one which freezes the rest of the application
--  from user input), can be created by calling [methodGtk.Window.set_modal] on
--  the dialog. When using [ctorGtk.Dialog.new_with_buttons], you can also pass
--  the Gtk.Dialog.Dialog_Modal flag to make a dialog modal.
--
--  For the simple dialog in the following example, a [classGtk.MessageDialog]
--  would save some effort. But you'd need to create the dialog contents
--  manually if you had more than a simple message in the dialog.
--
--  An example for simple `GtkDialog` usage:
--
--  ```c // Function to open a dialog box with a message void quick_message
--  (GtkWindow *parent, char *message) { GtkWidget *dialog, *label,
--  *content_area; GtkDialogFlags flags;
--
--  // Create the widgets flags = GTK_DIALOG_DESTROY_WITH_PARENT; dialog =
--  gtk_dialog_new_with_buttons ("Message", parent, flags, _("_OK"),
--  GTK_RESPONSE_NONE, NULL); content_area = gtk_dialog_get_content_area
--  (GTK_DIALOG (dialog)); label = gtk_label_new (message);
--
--  // Ensure that the dialog box is destroyed when the user responds
--
--  g_signal_connect_swapped (dialog, "response", G_CALLBACK
--  (gtk_window_destroy), dialog);
--
--  // Add the label, and show everything we've added
--
--  gtk_box_append (GTK_BOX (content_area), label); gtk_widget_show (dialog);
--  } ```
--
--  # GtkDialog as GtkBuildable
--
--  The `GtkDialog` implementation of the `GtkBuildable` interface exposes the
--  Content_Area as an internal child with the name "content_area".
--
--  `GtkDialog` supports a custom `<action-widgets>` element, which can
--  contain multiple `<action-widget>` elements. The "response" attribute
--  specifies a numeric response, and the content of the element is the id of
--  widget (which should be a child of the dialogs Action_Area). To mark a
--  response as default, set the "default" attribute of the `<action-widget>`
--  element to true.
--
--  `GtkDialog` supports adding action widgets by specifying "action" as the
--  "type" attribute of a `<child>` element. The widget will be added either to
--  the action area or the headerbar of the dialog, depending on the
--  "use-header-bar" property. The response id has to be associated with the
--  action widget using the `<action-widgets>` element.
--
--  An example of a `GtkDialog` UI definition fragment:
--
--  ```xml <object class="GtkDialog" id="dialog1"> <child type="action">
--  <object class="GtkButton" id="button_cancel"/> </child> <child
--  type="action"> <object class="GtkButton" id="button_ok"> </object> </child>
--  <action-widgets> <action-widget
--  response="cancel">button_cancel</action-widget> <action-widget
--  response="ok" default="true">button_ok</action-widget> </action-widgets>
--  </object> ```
--
--  # Accessibility
--
--  `GtkDialog` uses the Gtk.Accessible.Accessible_Role_Dialog role.
--
--  <screenshot>gtk-dialog</screenshot>
--  <group>Dialogs</group>
--  <gtkada_demo>create_dialog.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                     use Gdk;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Native;              use Gtk.Native;
with Gtk.Root;                use Gtk.Root;
with Gtk.Shortcut_Manager;    use Gtk.Shortcut_Manager;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.Dialog is

   pragma Obsolescent;
   --  Use [class@Gtk.Window] instead

   type Gtk_Dialog_Record is new Gtk_Window_Record with null record;
   type Gtk_Dialog is access all Gtk_Dialog_Record'Class;

   type Gtk_Dialog_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Dialog_Flags);
   --  Flags used to influence dialog construction.

   Dialog_Modal : constant Gtk_Dialog_Flags := 1;
   Dialog_Destroy_With_Parent : constant Gtk_Dialog_Flags := 2;
   Dialog_Use_Header_Bar : constant Gtk_Dialog_Flags := 4;

   type Gtk_Response_Type is new Gint;
   --  Type used for Response_Id's.
   --  Positive values are totally user-interpreted.
   --  GtkAda will sometimes return Gtk_Response_None if no Response_Id is
   --  available.
   --
   --  Typical usage is:
   --    procedure On_Response
   --      (Self : access Gtk_Dialog_Record'Class;
   --       Response_Id : Gtk_Response_Type) is
   --    begin
   --       if Response_Id = Gtk_Response_Accept then
   --          blah;
   --       end if;
   --    end On_Response;

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

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Dialog_Flags_Properties is
      new Generic_Internal_Flags_Property (Gtk_Dialog_Flags);
   type Property_Gtk_Dialog_Flags is new Gtk_Dialog_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Dialog);
   procedure Initialize (Self : not null access Gtk_Dialog_Record'Class);
   --  Creates a new dialog box.
   --  Widgets should not be packed into the `GtkWindow` directly, but into
   --  the Content_Area and Action_Area, as described above.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Dialog_New return Gtk_Dialog;
   --  Creates a new dialog box.
   --  Widgets should not be packed into the `GtkWindow` directly, but into
   --  the Content_Area and Action_Area, as described above.

   function Gtk_Dialog_New
      (Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags) return Gtk_Dialog;
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is used
   --  for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog). Buttons are added afterwards with
   --  Add_Button.
   --  Since: gtk+ GtkAda 1.0

   procedure Gtk_New
      (Self   : out Gtk_Dialog;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   procedure Initialize
      (Self   : not null access Gtk_Dialog_Record'Class;
       Title  : UTF8_String;
       Parent : Gtk.Window.Gtk_Window := null;
       Flags  : Gtk_Dialog_Flags);
   --  Create a new dialog with a specific title, and specific attributes.
   --  Parent is the transient parent for the dialog (ie the one that is used
   --  for reference for the flag Destroy_With_Parent, or to compute the
   --  initial position of the dialog). Buttons are added afterwards with
   --  Add_Button.
   --  Since: gtk+ GtkAda 1.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Self        : not null access Gtk_Dialog_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Gtk_Response_Type);
   pragma Obsolescent (Add_Action_Widget);
   --  Adds an activatable widget to the action area of a `GtkDialog`.
   --  GTK connects a signal handler that will emit the
   --  [signalGtk.Dialog::response] signal on the dialog when the widget is
   --  activated. The widget is appended to the end of the dialog's action
   --  area.
   --  If you want to add a non-activatable widget, simply pack it into the
   --  Action_Area field of the `GtkDialog` struct.
   --  Deprecated since 4.10, 1
   --  @param Child an activatable widget
   --  @param Response_Id response ID for Child

   function Add_Button
      (Self        : not null access Gtk_Dialog_Record;
       Button_Text : UTF8_String;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Add_Button);
   --  Adds a button with the given text.
   --  GTK arranges things so that clicking the button will emit the
   --  [signalGtk.Dialog::response] signal with the given Response_Id. The
   --  button is appended to the end of the dialog's action area. The button
   --  widget is returned, but usually you don't need it.
   --  Deprecated since 4.10, 1
   --  @param Button_Text text of button
   --  @param Response_Id response ID for the button
   --  @return the `GtkButton` widget that was added. Has
   --  transfer-ownership='none'.

   function Get_Content_Area
      (Self : not null access Gtk_Dialog_Record) return Gtk.Box.Gtk_Box;
   pragma Obsolescent (Get_Content_Area);
   --  Returns the content area of Dialog.
   --  Deprecated since 4.10, 1
   --  @return the content area `GtkBox`. Has transfer-ownership='none'.

   function Get_Header_Bar
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Header_Bar);
   --  Returns the header bar of Dialog.
   --  Note that the headerbar is only used by the dialog if the
   --  [propertyGtk.Dialog:use-header-bar] property is True.
   --  Deprecated since 4.10, 1
   --  @return the header bar. Has transfer-ownership='none'.

   function Get_Response_For_Widget
      (Self   : not null access Gtk_Dialog_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Response_Type;
   pragma Obsolescent (Get_Response_For_Widget);
   --  Gets the response id of a widget in the action area of a dialog.
   --  Deprecated since 4.10, 1
   --  @param Widget a widget in the action area of Dialog
   --  @return the response id of Widget, or GTK_RESPONSE_NONE if Widget
   --  doesn't have a response id set.

   function Get_Widget_For_Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Widget_For_Response);
   --  Gets the widget button that uses the given response ID in the action
   --  area of a dialog.
   --  Deprecated since 4.10, 1
   --  @param Response_Id the response ID used by the Dialog widget
   --  @return the Widget button that uses the given Response_Id. Has
   --  transfer-ownership='none'.

   procedure Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   pragma Obsolescent (Response);
   --  Emits the ::response signal with the given response ID.
   --  Used to indicate that the user has responded to the dialog in some way.
   --  Deprecated since 4.10, 1
   --  @param Response_Id response ID

   procedure Set_Default_Response
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type);
   pragma Obsolescent (Set_Default_Response);
   --  Sets the default widget for the dialog based on the response ID.
   --  Pressing "Enter" normally activates the default widget.
   --  Deprecated since 4.10, 1
   --  @param Response_Id a response ID

   procedure Set_Response_Sensitive
      (Self        : not null access Gtk_Dialog_Record;
       Response_Id : Gtk_Response_Type;
       Setting     : Boolean);
   pragma Obsolescent (Set_Response_Sensitive);
   --  A convenient way to sensitize/desensitize dialog buttons.
   --  Calls `gtk_widget_set_sensitive (widget, Setting)` for each widget in
   --  the dialog's action area with the given Response_Id.
   --  Deprecated since 4.10, 1
   --  @param Response_Id a response ID
   --  @param Setting True for sensitive

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure G_New_Dialog
     (Self  : not null access Gtk_Dialog_Record'Class;
      Flags : Gtk_Dialog_Flags;
      Typ   : Glib.GType := Gtk.Dialog.Get_Type);
   --  Equivalent of Glib.Object.G_New for a dialog. This function should be
   --  used when you are subclassing the dialog class (for instance to add new
   --  signals). The Use_Header_Bar flag can only have an impact before the
   --  dialog is created, so this function will take that into account as
   --  appropriate. Other flags (Modal and Destroy_With_Parent) are ignored.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Dialog_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Dialog_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Dialog_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Dialog_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Dialog_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Dialog_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Dialog_Record) return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Dialog_Record);

   procedure Unrealize (Self : not null access Gtk_Dialog_Record);

   function Get_Display
      (Self : not null access Gtk_Dialog_Record) return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Use_Header_Bar_Property : constant Glib.Properties.Property_Int;
   --  True if the dialog uses a headerbar for action buttons instead of the
   --  action-area.
   --
   --  For technical reasons, this property is declared as an integer
   --  property, but you should only set it to True or False.
   --
   --  ## Creating a dialog with headerbar
   --
   --  Builtin `GtkDialog` subclasses such as [classGtk.ColorChooserDialog]
   --  set this property according to platform conventions (using the
   --  [propertyGtk.Settings:gtk-dialogs-use-header] setting).
   --
   --  Here is how you can achieve the same:
   --
   --  ```c g_object_get (settings, "gtk-dialogs-use-header", &header, NULL);
   --  dialog = g_object_new (GTK_TYPE_DIALOG, header, TRUE, NULL); ```

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
   --  Emitted when the user uses a keybinding to close the dialog.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
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
   --  Emitted when an action widget is clicked.
   --
   --  The signal is also emitted when the dialog receives a delete event, and
   --  when [methodGtk.Dialog.response] is called. On a delete event, the
   --  response ID is GTK_RESPONSE_DELETE_EVENT. Otherwise, it depends on which
   --  action widget was clicked.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Native"
   --
   --  - "Gtk.Root"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Dialog
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Dialog
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Dialog
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Dialog
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Dialog_Record, Gtk_Dialog);
   function "+"
     (Widget : access Gtk_Dialog_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Dialog
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Use_Header_Bar_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("use-header-bar");
end Gtk.Dialog;
