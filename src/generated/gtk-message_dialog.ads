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
--  Gtk.Message_Dialog.Gtk_Message_Dialog presents a dialog with some message
--  text. It's simply a convenience widget; you could construct the equivalent
--  of Gtk.Message_Dialog.Gtk_Message_Dialog from Gtk.Dialog.Gtk_Dialog without
--  too much effort, but Gtk.Message_Dialog.Gtk_Message_Dialog saves typing.
--
--  One difference from Gtk.Dialog.Gtk_Dialog is that
--  Gtk.Message_Dialog.Gtk_Message_Dialog sets the
--  Gtk.Window.Gtk_Window:skip-taskbar-hint property to True, so that the
--  dialog is hidden from the taskbar by default.
--
--  The easiest way to do a modal message dialog is to use Gtk.Dialog.Run,
--  though you can also pass in the GTK_DIALOG_MODAL flag, Gtk.Dialog.Run
--  automatically makes the dialog modal and waits for the user to respond to
--  it. Gtk.Dialog.Run returns when any dialog button is clicked.
--
--  An example for using a modal dialog: |[<!-- language="C" -->
--  GtkDialogFlags flags = GTK_DIALOG_DESTROY_WITH_PARENT; dialog =
--  gtk_message_dialog_new (parent_window, flags, GTK_MESSAGE_ERROR,
--  GTK_BUTTONS_CLOSE, "Error reading "%s": %s", filename, g_strerror (errno));
--  gtk_dialog_run (GTK_DIALOG (dialog)); gtk_widget_destroy (dialog); ]|
--
--  You might do a non-modal Gtk.Message_Dialog.Gtk_Message_Dialog as follows:
--
--  An example for a non-modal dialog: |[<!-- language="C" --> GtkDialogFlags
--  flags = GTK_DIALOG_DESTROY_WITH_PARENT; dialog = gtk_message_dialog_new
--  (parent_window, flags, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, "Error reading
--  "%s": %s", filename, g_strerror (errno));
--
--  // Destroy the dialog when the user responds to it // (e.g. clicks a
--  button)
--
--  g_signal_connect_swapped (dialog, "response", G_CALLBACK
--  (gtk_widget_destroy), dialog); ]|
--
--  # GtkMessageDialog as GtkBuildable
--
--  The GtkMessageDialog implementation of the GtkBuildable interface exposes
--  the message area as an internal child with the name "message_area".
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.Message_Dialog is

   type Gtk_Message_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Message_Dialog is access all Gtk_Message_Dialog_Record'Class;

   type Gtk_Message_Type is (
      Message_Info,
      Message_Warning,
      Message_Question,
      Message_Error,
      Message_Other);
   pragma Convention (C, Gtk_Message_Type);
   --  The type of message being displayed in the dialog.

   type Gtk_Buttons_Type is (
      Buttons_None,
      Buttons_Ok,
      Buttons_Close,
      Buttons_Cancel,
      Buttons_Yes_No,
      Buttons_Ok_Cancel);
   pragma Convention (C, Gtk_Buttons_Type);
   --  Prebuilt sets of buttons for the dialog. If none of these choices are
   --  appropriate, simply use Gtk.Message_Dialog.Buttons_None then call
   --  gtk_dialog_add_buttons.
   --
   --  > Please note that Gtk.Message_Dialog.Buttons_Ok,
   --  Gtk.Message_Dialog.Buttons_Yes_No > and
   --  Gtk.Message_Dialog.Buttons_Ok_Cancel are discouraged by the > [GNOME
   --  Human Interface
   --  Guidelines](http://library.gnome.org/devel/hig-book/stable/).

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Message_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Message_Type);
   type Property_Gtk_Message_Type is new Gtk_Message_Type_Properties.Property;

   package Gtk_Buttons_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Buttons_Type);
   type Property_Gtk_Buttons_Type is new Gtk_Buttons_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Dialog   : out Gtk_Message_Dialog;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   procedure Initialize
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   --  Creates a new message dialog, which is a simple dialog with some text
   --  the user may want to see. When the user clicks a button a "response"
   --  signal is emitted with response IDs from Gtk_Response_Type. See
   --  Gtk.Dialog.Gtk_Dialog for more details.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "parent": transient parent, or null for none
   --  "flags": flags
   --  "type": type of message
   --  "buttons": set of buttons to use
   --  "message": printf-style format string, or null

   function Gtk_Message_Dialog_New
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog;
   --  Creates a new message dialog, which is a simple dialog with some text
   --  the user may want to see. When the user clicks a button a "response"
   --  signal is emitted with response IDs from Gtk_Response_Type. See
   --  Gtk.Dialog.Gtk_Dialog for more details.
   --  "parent": transient parent, or null for none
   --  "flags": flags
   --  "type": type of message
   --  "buttons": set of buttons to use
   --  "message": printf-style format string, or null

   procedure Gtk_New_With_Markup
      (Dialog   : out Gtk_Message_Dialog;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   procedure Initialize_With_Markup
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   --  Creates a new message dialog, which is a simple dialog with some text
   --  that is marked up with the [Pango text markup
   --  language][PangoMarkupFormat]. When the user clicks a button a "response"
   --  signal is emitted with response IDs from Gtk_Response_Type. See
   --  Gtk.Dialog.Gtk_Dialog for more details.
   --  Special XML characters in the printf arguments passed to this function
   --  will automatically be escaped as necessary. (See g_markup_printf_escaped
   --  for how this is implemented.) Usually this is what you want, but if you
   --  have an existing Pango markup string that you want to use literally as
   --  the label, then you need to use Gtk.Message_Dialog.Set_Markup instead,
   --  since you can't pass the markup string either as the format (it might
   --  contain "%" characters) or as a string argument. |[<!-- language="C" -->
   --  GtkWidget *dialog; GtkDialogFlags flags =
   --  GTK_DIALOG_DESTROY_WITH_PARENT; dialog = gtk_message_dialog_new
   --  (parent_window, flags, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, NULL);
   --  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup); ]|
   --  Since: gtk+ 2.4
   --  Initialize_With_Markup does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "parent": transient parent, or null for none
   --  "flags": flags
   --  "type": type of message
   --  "buttons": set of buttons to use
   --  "message": printf-style format string, or null

   function Gtk_Message_Dialog_New_With_Markup
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog;
   --  Creates a new message dialog, which is a simple dialog with some text
   --  that is marked up with the [Pango text markup
   --  language][PangoMarkupFormat]. When the user clicks a button a "response"
   --  signal is emitted with response IDs from Gtk_Response_Type. See
   --  Gtk.Dialog.Gtk_Dialog for more details.
   --  Special XML characters in the printf arguments passed to this function
   --  will automatically be escaped as necessary. (See g_markup_printf_escaped
   --  for how this is implemented.) Usually this is what you want, but if you
   --  have an existing Pango markup string that you want to use literally as
   --  the label, then you need to use Gtk.Message_Dialog.Set_Markup instead,
   --  since you can't pass the markup string either as the format (it might
   --  contain "%" characters) or as a string argument. |[<!-- language="C" -->
   --  GtkWidget *dialog; GtkDialogFlags flags =
   --  GTK_DIALOG_DESTROY_WITH_PARENT; dialog = gtk_message_dialog_new
   --  (parent_window, flags, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, NULL);
   --  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup); ]|
   --  Since: gtk+ 2.4
   --  "parent": transient parent, or null for none
   --  "flags": flags
   --  "type": type of message
   --  "buttons": set of buttons to use
   --  "message": printf-style format string, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_message_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Format_Secondary_Markup
      (Dialog  : not null access Gtk_Message_Dialog_Record;
       Message : UTF8_String := "");
   --  Sets the secondary text of the message dialog to be Message_Format
   --  (with printf-style), which is marked up with the [Pango text markup
   --  language][PangoMarkupFormat].
   --  Due to an oversight, this function does not escape special XML
   --  characters like Gtk.Message_Dialog.Gtk_New_With_Markup does. Thus, if
   --  the arguments may contain special XML characters, you should use
   --  g_markup_printf_escaped to escape it.
   --  |[<!-- language="C" --> gchar *msg;
   --  msg = g_markup_printf_escaped (message_format, ...);
   --  gtk_message_dialog_format_secondary_markup (message_dialog, "%s", msg);
   --  g_free (msg); ]|
   --  Since: gtk+ 2.6
   --  "message": printf-style markup string (see [Pango markup
   --  format][PangoMarkupFormat]), or null

   function Get_Image
      (Dialog : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Image);
   --  Gets the dialog's image.
   --  Since: gtk+ 2.14
   --  Deprecated since 3.12, 1

   procedure Set_Image
      (Dialog : not null access Gtk_Message_Dialog_Record;
       Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   pragma Obsolescent (Set_Image);
   --  Sets the dialog's image to Image.
   --  Since: gtk+ 2.10
   --  Deprecated since 3.12, 1
   --  "image": the image

   function Get_Message_Area
      (Dialog : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the message area of the dialog. This is the box where the
   --  dialog's primary and secondary labels are packed. You can add your own
   --  extra content to that box and it will appear below those labels. See
   --  Gtk.Dialog.Get_Content_Area for the corresponding function in the parent
   --  Gtk.Dialog.Gtk_Dialog.
   --  Since: gtk+ 2.22

   procedure Set_Markup
      (Dialog : not null access Gtk_Message_Dialog_Record;
       Str    : UTF8_String);
   --  Sets the text of the message dialog to be Str, which is marked up with
   --  the [Pango text markup language][PangoMarkupFormat].
   --  Since: gtk+ 2.4
   --  "str": markup string (see [Pango markup format][PangoMarkupFormat])

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Buttons_Property : constant Gtk.Message_Dialog.Property_Gtk_Buttons_Type;
   --  Type: Gtk_Buttons_Type
   --  Flags: write

   Image_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The image for this dialog.

   Message_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The Gtk.Box.Gtk_Box that corresponds to the message area of this
   --  dialog. See Gtk.Message_Dialog.Get_Message_Area for a detailed
   --  description of this area.

   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type;
   --  Type: Gtk_Message_Type
   --  The type of the message.

   Secondary_Text_Property : constant Glib.Properties.Property_String;
   --  The secondary text of the message dialog.

   Secondary_Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  True if the secondary text of the dialog includes Pango markup. See
   --  pango_parse_markup.

   Text_Property : constant Glib.Properties.Property_String;
   --  The primary text of the message dialog. If the dialog has a secondary
   --  text, this will appear as the title.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  True if the primary text of the dialog includes Pango markup. See
   --  pango_parse_markup.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Secondary_Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("secondary-use-markup");
   Secondary_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("secondary-text");
   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type :=
     Gtk.Message_Dialog.Build ("message-type");
   Message_Area_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("message-area");
   Image_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Buttons_Property : constant Gtk.Message_Dialog.Property_Gtk_Buttons_Type :=
     Gtk.Message_Dialog.Build ("buttons");
end Gtk.Message_Dialog;
