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
--  Gtk.Info_Bar.Gtk_Info_Bar is a widget that can be used to show messages to
--  the user without showing a dialog. It is often temporarily shown at the top
--  or bottom of a document. In contrast to Gtk.Dialog.Gtk_Dialog, which has a
--  action area at the bottom, Gtk.Info_Bar.Gtk_Info_Bar has an action area at
--  the side.
--
--  The API of Gtk.Info_Bar.Gtk_Info_Bar is very similar to
--  Gtk.Dialog.Gtk_Dialog, allowing you to add buttons to the action area with
--  Gtk.Info_Bar.Add_Button or gtk_info_bar_new_with_buttons. The sensitivity
--  of action widgets can be controlled with
--  Gtk.Info_Bar.Set_Response_Sensitive. To add widgets to the main content
--  area of a Gtk.Info_Bar.Gtk_Info_Bar, use Gtk.Info_Bar.Get_Content_Area and
--  add your widgets to the container.
--
--  Similar to Gtk.Message_Dialog.Gtk_Message_Dialog, the contents of a
--  Gtk.Info_Bar.Gtk_Info_Bar can by classified as error message, warning,
--  informational message, etc, by using Gtk.Info_Bar.Set_Message_Type. GTK+
--  may use the message type to determine how the message is displayed.
--
--  A simple example for using a Gtk.Info_Bar.Gtk_Info_Bar: |[<!--
--  language="C" --> GtkWidget *widget, *message_label, *content_area;
--  GtkWidget *grid; GtkInfoBar *bar;
--
--  // set up info bar widget = gtk_info_bar_new (); bar = GTK_INFO_BAR
--  (widget); grid = gtk_grid_new ();
--
--  gtk_widget_set_no_show_all (widget, TRUE); message_label = gtk_label_new
--  (""); content_area = gtk_info_bar_get_content_area (bar); gtk_container_add
--  (GTK_CONTAINER (content_area), message_label); gtk_info_bar_add_button
--  (bar, _("_OK"), GTK_RESPONSE_OK); g_signal_connect (bar, "response",
--  G_CALLBACK (gtk_widget_hide), NULL); gtk_grid_attach (GTK_GRID (grid),
--  widget, 0, 2, 1, 1);
--
--  // ...
--
--  // show an error message gtk_label_set_text (GTK_LABEL (message_label),
--  "An error occurred!"); gtk_info_bar_set_message_type (bar,
--  GTK_MESSAGE_ERROR); gtk_widget_show (bar); ]|
--
--  # GtkInfoBar as GtkBuildable
--
--  The GtkInfoBar implementation of the GtkBuildable interface exposes the
--  content area and action area as internal children with the names
--  "content_area" and "action_area".
--
--  GtkInfoBar supports a custom <action-widgets> element, which can contain
--  multiple <action-widget> elements. The "response" attribute specifies a
--  numeric response, and the content of the element is the id of widget (which
--  should be a child of the dialogs Action_Area).
--
--  # CSS nodes
--
--  GtkInfoBar has a single CSS node with name infobar. The node may get one
--  of the style classes .info, .warning, .error or .question, depending on the
--  message type.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Types;         use Glib.Types;
with Gtk.Box;            use Gtk.Box;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Orientable;     use Gtk.Orientable;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Info_Bar is

   type Gtk_Info_Bar_Record is new Gtk_Box_Record with null record;
   type Gtk_Info_Bar is access all Gtk_Info_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Info_Bar);
   procedure Initialize (Self : not null access Gtk_Info_Bar_Record'Class);
   --  Creates a new Gtk.Info_Bar.Gtk_Info_Bar object.
   --  Since: gtk+ 2.18
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Info_Bar_New return Gtk_Info_Bar;
   --  Creates a new Gtk.Info_Bar.Gtk_Info_Bar object.
   --  Since: gtk+ 2.18

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_info_bar_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Action_Widget
      (Self        : not null access Gtk_Info_Bar_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Response_Id : Glib.Gint);
   --  Add an activatable widget to the action area of a
   --  Gtk.Info_Bar.Gtk_Info_Bar, connecting a signal handler that will emit
   --  the Gtk.Info_Bar.Gtk_Info_Bar::response signal on the message area when
   --  the widget is activated. The widget is appended to the end of the
   --  message areas action area.
   --  Since: gtk+ 2.18
   --  "child": an activatable widget
   --  "response_id": response ID for Child

   function Add_Button
      (Self        : not null access Gtk_Info_Bar_Record;
       Button_Text : UTF8_String;
       Response_Id : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Adds a button with the given text and sets things up so that clicking
   --  the button will emit the "response" signal with the given response_id.
   --  The button is appended to the end of the info bars's action area. The
   --  button widget is returned, but usually you don't need it.
   --  Since: gtk+ 2.18
   --  "button_text": text of button
   --  "response_id": response ID for the button

   function Get_Action_Area
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the action area of Info_Bar.
   --  Since: gtk+ 2.18

   function Get_Content_Area
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the content area of Info_Bar.
   --  Since: gtk+ 2.18

   function Get_Message_Type
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Message_Dialog.Gtk_Message_Type;
   --  Returns the message type of the message area.
   --  Since: gtk+ 2.18

   procedure Set_Message_Type
      (Self         : not null access Gtk_Info_Bar_Record;
       Message_Type : Gtk.Message_Dialog.Gtk_Message_Type);
   --  Sets the message type of the message area.
   --  GTK+ uses this type to determine how the message is displayed.
   --  Since: gtk+ 2.18
   --  "message_type": a Gtk.Message_Dialog.Gtk_Message_Type

   function Get_Revealed
      (Self : not null access Gtk_Info_Bar_Record) return Boolean;
   --  Since: gtk+ 3.22.29

   procedure Set_Revealed
      (Self     : not null access Gtk_Info_Bar_Record;
       Revealed : Boolean);
   --  Sets the GtkInfoBar:revealed property to Revealed. This will cause
   --  Info_Bar to show up with a slide-in transition.
   --  Note that this property does not automatically show Info_Bar and thus
   --  won't have any effect if it is invisible.
   --  Since: gtk+ 3.22.29
   --  "revealed": The new value of the property

   function Get_Show_Close_Button
      (Self : not null access Gtk_Info_Bar_Record) return Boolean;
   --  Returns whether the widget will display a standard close button.
   --  Since: gtk+ 3.10

   procedure Set_Show_Close_Button
      (Self    : not null access Gtk_Info_Bar_Record;
       Setting : Boolean);
   --  If true, a standard close button is shown. When clicked it emits the
   --  response GTK_RESPONSE_CLOSE.
   --  Since: gtk+ 3.10
   --  "setting": True to include a close button

   procedure Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint);
   --  Emits the "response" signal with the given Response_Id.
   --  Since: gtk+ 2.18
   --  "response_id": a response ID

   procedure Set_Default_Response
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint);
   --  Sets the last widget in the info bar's action area with the given
   --  response_id as the default widget for the dialog. Pressing "Enter"
   --  normally activates the default widget.
   --  Note that this function currently requires Info_Bar to be added to a
   --  widget hierarchy.
   --  Since: gtk+ 2.18
   --  "response_id": a response ID

   procedure Set_Response_Sensitive
      (Self        : not null access Gtk_Info_Bar_Record;
       Response_Id : Glib.Gint;
       Setting     : Boolean);
   --  Calls gtk_widget_set_sensitive (widget, setting) for each widget in the
   --  info bars's action area with the given response_id. A convenient way to
   --  sensitize/desensitize dialog buttons.
   --  Since: gtk+ 2.18
   --  "response_id": a response ID
   --  "setting": TRUE for sensitive

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Info_Bar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Info_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

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
   --  The ::close signal is a [keybinding signal][GtkBindingSignal] which
   --  gets emitted when the user uses a keybinding to dismiss the info bar.
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
   --  Emitted when an action widget is clicked or the application programmer
   --  calls Gtk.Dialog.Response. The Response_Id depends on which action
   --  widget was clicked.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

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

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Info_Bar_Record, Gtk_Info_Bar);
   function "+"
     (Widget : access Gtk_Info_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Info_Bar
   renames Implements_Gtk_Orientable.To_Object;

private
   Show_Close_Button_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-close-button");
   Revealed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("revealed");
   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type :=
     Gtk.Message_Dialog.Build ("message-type");
end Gtk.Info_Bar;
