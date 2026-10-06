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

--  `GtkInfoBar` can be used to show messages to the user without a dialog.
--
--  <picture> <source srcset="info-bar-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkInfoBar" src="info-bar.png"> </picture>
--  It is often temporarily shown at the top or bottom of a document. In
--  contrast to [classGtk.Dialog], which has an action area at the bottom,
--  `GtkInfoBar` has an action area at the side.
--
--  The API of `GtkInfoBar` is very similar to `GtkDialog`, allowing you to
--  add buttons to the action area with [methodGtk.InfoBar.add_button] or
--  [ctorGtk.InfoBar.new_with_buttons]. The sensitivity of action widgets can
--  be controlled with [methodGtk.InfoBar.set_response_sensitive].
--
--  To add widgets to the main content area of a `GtkInfoBar`, use
--  [methodGtk.InfoBar.add_child].
--
--  Similar to [classGtk.MessageDialog], the contents of a `GtkInfoBar` can by
--  classified as error message, warning, informational message, etc, by using
--  [methodGtk.InfoBar.set_message_type]. GTK may use the message type to
--  determine how the message is displayed.
--
--  A simple example for using a `GtkInfoBar`: ```c GtkWidget *message_label;
--  GtkWidget *widget; GtkWidget *grid; GtkInfoBar *bar;
--
--  // set up info bar widget = gtk_info_bar_new (); bar = GTK_INFO_BAR
--  (widget); grid = gtk_grid_new ();
--
--  message_label = gtk_label_new (""); gtk_info_bar_add_child (bar,
--  message_label); gtk_info_bar_add_button (bar, _("_OK"), GTK_RESPONSE_OK);
--  g_signal_connect (bar, "response", G_CALLBACK (gtk_widget_hide), NULL);
--  gtk_grid_attach (GTK_GRID (grid), widget, 0, 2, 1, 1);
--
--  // ...
--
--  // show an error message gtk_label_set_text (GTK_LABEL (message_label),
--  "An error occurred!"); gtk_info_bar_set_message_type (bar,
--  GTK_MESSAGE_ERROR); gtk_widget_show (bar); ```
--
--  # GtkInfoBar as GtkBuildable
--
--  `GtkInfoBar` supports a custom `<action-widgets>` element, which can
--  contain multiple `<action-widget>` elements. The "response" attribute
--  specifies a numeric response, and the content of the element is the id of
--  widget (which should be a child of the dialogs Action_Area).
--
--  `GtkInfoBar` supports adding action widgets by specifying "action" as the
--  "type" attribute of a `<child>` element. The widget will be added either to
--  the action area. The response id has to be associated with the action
--  widget using the `<action-widgets>` element.
--
--  # CSS nodes
--
--  `GtkInfoBar` has a single CSS node with name infobar. The node may get one
--  of the style classes .info, .warning, .error or .question, depending on the
--  message type. If the info bar shows a close button, that button will have
--  the .close style class applied.
--
--  <group>Dialogs</group>
--  <gtkada_demo>create_info_bar.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Message_Dialog;    use Gtk.Message_Dialog;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Info_Bar is

   pragma Obsolescent;
   --  There is no replacement in GTK for an "info bar" widget; you can use
   --  [class@Gtk.Revealer] with a [class@Gtk.Box] containing a
   --  [class@Gtk.Label] and an optional [class@Gtk.Button], according to your
   --  application's design.

   type Gtk_Info_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Info_Bar is access all Gtk_Info_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Info_Bar);
   procedure Initialize (Self : not null access Gtk_Info_Bar_Record'Class);
   --  Creates a new `GtkInfoBar` object.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Info_Bar_New return Gtk_Info_Bar;
   --  Creates a new `GtkInfoBar` object.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_info_bar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Self        : not null access Gtk_Info_Bar_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Glib.Gint);
   pragma Obsolescent (Add_Action_Widget);
   --  Add an activatable widget to the action area of a `GtkInfoBar`.
   --  This also connects a signal handler that will emit the
   --  [signalGtk.InfoBar::response] signal on the message area when the widget
   --  is activated. The widget is appended to the end of the message areas
   --  action area.
   --  Deprecated since 4.10, 1
   --  @param Child an activatable widget
   --  @param Response_Id response ID for Child

   function Add_Button
      (Self        : not null access Gtk_Info_Bar_Record;
       Button_Text : UTF8_String;
       Response_Id : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Add_Button);
   --  Adds a button with the given text.
   --  Clicking the button will emit the [signalGtk.InfoBar::response] signal
   --  with the given response_id. The button is appended to the end of the
   --  info bar's action area. The button widget is returned, but usually you
   --  don't need it.
   --  Deprecated since 4.10, 1
   --  @param Button_Text text of button
   --  @param Response_Id response ID for the button
   --  @return the `GtkButton` widget that was added. Has
   --  transfer-ownership='none'.

   procedure Add_Child
      (Self   : not null access Gtk_Info_Bar_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Add_Child);
   --  Adds a widget to the content area of the info bar.
   --  Deprecated since 4.10, 1
   --  @param Widget the child to be added

   function Get_Message_Type
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Message_Dialog.Gtk_Message_Type;
   pragma Obsolescent (Get_Message_Type);
   --  Returns the message type of the message area.
   --  Deprecated since 4.10, 1
   --  @return the message type of the message area.

   procedure Set_Message_Type
      (Self         : not null access Gtk_Info_Bar_Record;
       Message_Type : Gtk.Message_Dialog.Gtk_Message_Type);
   pragma Obsolescent (Set_Message_Type);
   --  Sets the message type of the message area.
   --  GTK uses this type to determine how the message is displayed.
   --  Deprecated since 4.10, 1
   --  @param Message_Type a `GtkMessageType`

   function Get_Revealed
      (Self : not null access Gtk_Info_Bar_Record) return Boolean;
   pragma Obsolescent (Get_Revealed);
   --  Returns whether the info bar is currently revealed.
   --  Deprecated since 4.10, 1
   --  @return the current value of the [propertyGtk.InfoBar:revealed]
   --  property

   procedure Set_Revealed
      (Self     : not null access Gtk_Info_Bar_Record;
       Revealed : Boolean);
   pragma Obsolescent (Set_Revealed);
   --  Sets whether the `GtkInfoBar` is revealed.
   --  Changing this will make Info_Bar reveal or conceal itself via a sliding
   --  transition.
   --  Note: this does not show or hide Info_Bar in the
   --  [propertyGtk.Widget:visible] sense, so revealing has no effect if
   --  [propertyGtk.Widget:visible] is False.
   --  Deprecated since 4.10, 1
   --  @param Revealed The new value of the property

   function Get_Show_Close_Button
      (Self : not null access Gtk_Info_Bar_Record) return Boolean;
   pragma Obsolescent (Get_Show_Close_Button);
   --  Returns whether the widget will display a standard close button.
   --  Deprecated since 4.10, 1
   --  @return True if the widget displays standard close button

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Info_Bar_Record;
       Setting : Boolean);
   pragma Obsolescent (Set_Show_Close_Button);
   --  If true, a standard close button is shown.
   --  When clicked it emits the response GTK_RESPONSE_CLOSE.
   --  Deprecated since 4.10, 1
   --  @param Setting True to include a close button

   procedure Remove_Action_Widget
      (Self   : not null access Gtk_Info_Bar_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Remove_Action_Widget);
   --  Removes a widget from the action area of Info_Bar.
   --  The widget must have been put there by a call to
   --  [methodGtk.InfoBar.add_action_widget] or [methodGtk.InfoBar.add_button].
   --  Deprecated since 4.10, 1
   --  @param Widget an action widget to remove

   procedure Remove_Child
      (Self   : not null access Gtk_Info_Bar_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Remove_Child);
   --  Removes a widget from the content area of the info bar.
   --  Deprecated since 4.10, 1
   --  @param Widget a child that has been added to the content area

   procedure Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint);
   pragma Obsolescent (Response);
   --  Emits the "response" signal with the given Response_Id.
   --  Deprecated since 4.10, 1
   --  @param Response_Id a response ID

   procedure Set_Default_Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint);
   pragma Obsolescent (Set_Default_Response);
   --  Sets the last widget in the info bar's action area with the given
   --  response_id as the default widget for the dialog.
   --  Pressing "Enter" normally activates the default widget.
   --  Note that this function currently requires Info_Bar to be added to a
   --  widget hierarchy.
   --  Deprecated since 4.10, 1
   --  @param Response_Id a response ID

   procedure Set_Response_Sensitive
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint;
       Setting     : Boolean);
   pragma Obsolescent (Set_Response_Sensitive);
   --  Sets the sensitivity of action widgets for Response_Id.
   --  Calls `gtk_widget_set_sensitive (widget, setting)` for each widget in
   --  the info bars's action area with the given Response_Id. A convenient way
   --  to sensitize/desensitize buttons.
   --  Deprecated since 4.10, 1
   --  @param Response_Id a response ID
   --  @param Setting TRUE for sensitive

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Info_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Info_Bar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Info_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Info_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Info_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Info_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Info_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Info_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Info_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Info_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type;
   --  Type: Gtk.Message_Dialog.Gtk_Message_Type
   --  The type of the message.
   --
   --  The type may be used to determine the appearance of the info bar.

   Revealed_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the info bar shows its contents.

   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to include a standard close button.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Info_Bar_Void is not null access procedure (Self : access Gtk_Info_Bar_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Close : constant Glib.Signal_Name := "close";
   procedure On_Close
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_Gtk_Info_Bar_Void;
       After : Boolean := False);
   procedure On_Close
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user uses a keybinding to dismiss the info bar.
   --
   --  The ::close signal is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is the Escape key.

   type Cb_Gtk_Info_Bar_Gint_Void is not null access procedure
     (Self        : access Gtk_Info_Bar_Record'Class;
      Response_Id : Glib.Gint);

   type Cb_GObject_Gint_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Response_Id : Glib.Gint);

   Signal_Response : constant Glib.Signal_Name := "response";
   procedure On_Response
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_Gtk_Info_Bar_Gint_Void;
       After : Boolean := False);
   procedure On_Response
      (Self  : not null access Gtk_Info_Bar_Record;
       Call  : Cb_GObject_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an action widget is clicked.
   --
   --  The signal is also emitted when the application programmer calls
   --  [methodGtk.InfoBar.response]. The Response_Id depends on which action
   --  widget was clicked.

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

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Info_Bar_Record, Gtk_Info_Bar);
   function "+"
     (Widget : access Gtk_Info_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Info_Bar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Info_Bar_Record, Gtk_Info_Bar);
   function "+"
     (Widget : access Gtk_Info_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Info_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Info_Bar_Record, Gtk_Info_Bar);
   function "+"
     (Widget : access Gtk_Info_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Info_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-close-button");
   Revealed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("revealed");
   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type :=
     Gtk.Message_Dialog.Build ("message-type");
end Gtk.Info_Bar;
