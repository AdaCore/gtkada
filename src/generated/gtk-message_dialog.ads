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

--  `GtkMessageDialog` presents a dialog with some message text.
--
--  <picture> <source srcset="messagedialog-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkMessageDialog" src="messagedialog.png"> </picture>
--  It's simply a convenience widget; you could construct the equivalent of
--  `GtkMessageDialog` from `GtkDialog` without too much effort, but
--  `GtkMessageDialog` saves typing.
--
--  The easiest way to do a modal message dialog is to use the
--  Gtk.Dialog.Dialog_Modal flag, which will call [methodGtk.Window.set_modal]
--  internally. The dialog will prevent interaction with the parent window
--  until it's hidden or destroyed. You can use the
--  [signalGtk.Dialog::response] signal to know when the user dismissed the
--  dialog.
--
--  An example for using a modal dialog: ```c GtkDialogFlags flags =
--  GTK_DIALOG_DESTROY_WITH_PARENT | GTK_DIALOG_MODAL; dialog =
--  gtk_message_dialog_new (parent_window, flags, GTK_MESSAGE_ERROR,
--  GTK_BUTTONS_CLOSE, "Error reading "%s": %s", filename, g_strerror (errno));
--  // Destroy the dialog when the user responds to it // (e.g. clicks a
--  button)
--
--  g_signal_connect (dialog, "response", G_CALLBACK (gtk_window_destroy),
--  NULL); ```
--
--  You might do a non-modal `GtkMessageDialog` simply by omitting the
--  Gtk.Dialog.Dialog_Modal flag:
--
--  ```c GtkDialogFlags flags = GTK_DIALOG_DESTROY_WITH_PARENT; dialog =
--  gtk_message_dialog_new (parent_window, flags, GTK_MESSAGE_ERROR,
--  GTK_BUTTONS_CLOSE, "Error reading "%s": %s", filename, g_strerror (errno));
--
--  // Destroy the dialog when the user responds to it // (e.g. clicks a
--  button) g_signal_connect (dialog, "response", G_CALLBACK
--  (gtk_window_destroy), NULL); ```
--
--  # GtkMessageDialog as GtkBuildable
--
--  The `GtkMessageDialog` implementation of the `GtkBuildable` interface
--  exposes the message area as an internal child with the name "message_area".
--
--  <screenshot>messagedialog</screenshot>
--  <group>Dialogs</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                     use Gdk;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Native;              use Gtk.Native;
with Gtk.Root;                use Gtk.Root;
with Gtk.Shortcut_Manager;    use Gtk.Shortcut_Manager;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.Message_Dialog is

   pragma Obsolescent;
   --  Use [class@Gtk.AlertDialog] instead

   type Gtk_Message_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_Message_Dialog is access all Gtk_Message_Dialog_Record'Class;

   type Gtk_Message_Type is (
      Message_Info,
      Message_Warning,
      Message_Question,
      Message_Error,
      Message_Other);
   pragma Convention (C, Gtk_Message_Type);
   --  The type of message being displayed in a [classMessagedialog].

   type Gtk_Buttons_Type is (
      Buttons_None,
      Buttons_Ok,
      Buttons_Close,
      Buttons_Cancel,
      Buttons_Yes_No,
      Buttons_Ok_Cancel);
   pragma Convention (C, Gtk_Buttons_Type);
   --  Prebuilt sets of buttons for `GtkDialog`.
   --
   --  If none of these choices are appropriate, simply use
   --  Gtk.Message_Dialog.Buttons_None and call [methodGtk.Dialog.add_buttons].
   --
   --  > Please note that Gtk.Message_Dialog.Buttons_Ok,
   --  Gtk.Message_Dialog.Buttons_Yes_No > and
   --  Gtk.Message_Dialog.Buttons_Ok_Cancel are discouraged by the > [GNOME
   --  Human Interface Guidelines](https://developer.gnome.org/hig/).

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
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   procedure Initialize
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   --  Creates a new message dialog.
   --  This is a simple dialog with some text the user may want to see. When
   --  the user clicks a button a "response" signal is emitted with response
   --  IDs from [enumGtk.ResponseType]. See [classGtk.Dialog] for more details.
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Parent transient parent
   --  @param Flags flags
   --  @param The_Type type of message
   --  @param Buttons set of buttons to use
   --  @param Message printf-style format string

   function Gtk_Message_Dialog_New
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog;
   --  Creates a new message dialog.
   --  This is a simple dialog with some text the user may want to see. When
   --  the user clicks a button a "response" signal is emitted with response
   --  IDs from [enumGtk.ResponseType]. See [classGtk.Dialog] for more details.
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'.
   --  @param Parent transient parent
   --  @param Flags flags
   --  @param The_Type type of message
   --  @param Buttons set of buttons to use
   --  @param Message printf-style format string

   procedure Gtk_New_With_Markup
      (Dialog   : out Gtk_Message_Dialog;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   procedure Initialize_With_Markup
      (Dialog   : not null access Gtk_Message_Dialog_Record'Class;
       Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "");
   --  Creates a new message dialog.
   --  This is a simple dialog with some text that is marked up with Pango
   --  markup. When the user clicks a button a "response" signal is emitted
   --  with response IDs from [enumGtk.ResponseType]. See [classGtk.Dialog] for
   --  more details.
   --  Special XML characters in the printf arguments passed to this function
   --  will automatically be escaped as necessary. (See g_markup_printf_escaped
   --  for how this is implemented.) Usually this is what you want, but if you
   --  have an existing Pango markup string that you want to use literally as
   --  the label, then you need to use [methodGtk.MessageDialog.set_markup]
   --  instead, since you can't pass the markup string either as the format (it
   --  might contain "%" characters) or as a string argument.
   --  ```c GtkWidget *dialog; GtkDialogFlags flags =
   --  GTK_DIALOG_DESTROY_WITH_PARENT; dialog = gtk_message_dialog_new
   --  (parent_window, flags, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, NULL);
   --  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup); ```
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'. Use Set_Markup instead when the markup is
   --  already in hand.
   --  Initialize_With_Markup does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Parent transient parent
   --  @param Flags flags
   --  @param The_Type type of message
   --  @param Buttons set of buttons to use
   --  @param Message printf-style format string

   function Gtk_Message_Dialog_New_With_Markup
      (Parent   : access Gtk.Window.Gtk_Window_Record'Class;
       Flags    : Gtk.Dialog.Gtk_Dialog_Flags;
       The_Type : Gtk_Message_Type;
       Buttons  : Gtk_Buttons_Type;
       Message  : UTF8_String := "") return Gtk_Message_Dialog;
   --  Creates a new message dialog.
   --  This is a simple dialog with some text that is marked up with Pango
   --  markup. When the user clicks a button a "response" signal is emitted
   --  with response IDs from [enumGtk.ResponseType]. See [classGtk.Dialog] for
   --  more details.
   --  Special XML characters in the printf arguments passed to this function
   --  will automatically be escaped as necessary. (See g_markup_printf_escaped
   --  for how this is implemented.) Usually this is what you want, but if you
   --  have an existing Pango markup string that you want to use literally as
   --  the label, then you need to use [methodGtk.MessageDialog.set_markup]
   --  instead, since you can't pass the markup string either as the format (it
   --  might contain "%" characters) or as a string argument.
   --  ```c GtkWidget *dialog; GtkDialogFlags flags =
   --  GTK_DIALOG_DESTROY_WITH_PARENT; dialog = gtk_message_dialog_new
   --  (parent_window, flags, GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE, NULL);
   --  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (dialog), markup); ```
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'. Use Set_Markup instead when the markup is
   --  already in hand.
   --  @param Parent transient parent
   --  @param Flags flags
   --  @param The_Type type of message
   --  @param Buttons set of buttons to use
   --  @param Message printf-style format string

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_message_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Format_Secondary_Markup
      (Dialog  : not null access Gtk_Message_Dialog_Record;
       Message : UTF8_String := "");
   pragma Obsolescent (Format_Secondary_Markup);
   --  Sets the secondary text of the message dialog.
   --  The Message_Format is assumed to contain Pango markup.
   --  Due to an oversight, this function does not escape special XML
   --  characters like [ctorGtk.MessageDialog.new_with_markup] does. Thus, if
   --  the arguments may contain special XML characters, you should use
   --  g_markup_printf_escaped to escape it.
   --  ```c char *msg;
   --  msg = g_markup_printf_escaped (message_format, ...);
   --  gtk_message_dialog_format_secondary_markup (message_dialog, "%s", msg);
   --  g_free (msg); ```
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'.
   --  Deprecated since 4.10, 1
   --  @param Message printf-style string with Pango markup

   procedure Format_Secondary_Text
      (Dialog  : not null access Gtk_Message_Dialog_Record;
       Message : UTF8_String := "");
   pragma Obsolescent (Format_Secondary_Text);
   --  Sets the secondary text of the message dialog.
   --  Message is passed to printf as its format string, so a literal '%' in
   --  it must be doubled as '%%'.
   --  Deprecated since 4.10, 1
   --  @param Message printf-style format string

   function Get_Message_Area
      (Dialog : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Message_Area);
   --  Returns the message area of the dialog.
   --  This is the box where the dialog's primary and secondary labels are
   --  packed. You can add your own extra content to that box and it will
   --  appear below those labels. See [methodGtk.Dialog.get_content_area] for
   --  the corresponding function in the parent [classGtk.Dialog].
   --  Deprecated since 4.10, 1
   --  @return A `GtkBox` corresponding to the "message area" in the
   --  Message_Dialog. Has transfer-ownership='none'.

   procedure Set_Markup
      (Dialog : not null access Gtk_Message_Dialog_Record;
       Str    : UTF8_String);
   pragma Obsolescent (Set_Markup);
   --  Sets the text of the message dialog.
   --  Deprecated since 4.10, 1
   --  @param Str string with Pango markup

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Message_Dialog_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Message_Dialog_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Message_Dialog_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Message_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Message_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Message_Dialog_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Message_Dialog_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Message_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Message_Dialog_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Message_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Message_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Message_Dialog_Record);

   procedure Unrealize (Self : not null access Gtk_Message_Dialog_Record);

   function Get_Display
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_Message_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_Message_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Buttons_Property : constant Gtk.Message_Dialog.Property_Gtk_Buttons_Type;
   --  Type: Gtk_Buttons_Type
   --  Flags: write
   --  Set of buttons to display on the dialog.

   Message_Area_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The `GtkBox` that corresponds to the message area of this dialog.
   --
   --  See [methodGtk.MessageDialog.get_message_area] for a detailed
   --  description of this area.

   Message_Type_Property : constant Gtk.Message_Dialog.Property_Gtk_Message_Type;
   --  Type: Gtk_Message_Type
   --  The type of the message.

   Secondary_Text_Property : constant Glib.Properties.Property_String;
   --  The secondary text of the message dialog.

   Secondary_Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  True if the secondary text of the dialog includes Pango markup.
   --
   --  See [funcPango.parse_markup].

   Text_Property : constant Glib.Properties.Property_String;
   --  The primary text of the message dialog.
   --
   --  If the dialog has a secondary text, this will appear as the title.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  True if the primary text of the dialog includes Pango markup.
   --
   --  See [funcPango.parse_markup].

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Message_Dialog_Record, Gtk_Message_Dialog);
   function "+"
     (Widget : access Gtk_Message_Dialog_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Message_Dialog
   renames Implements_Gtk_Shortcut_Manager.To_Object;

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
   Buttons_Property : constant Gtk.Message_Dialog.Property_Gtk_Buttons_Type :=
     Gtk.Message_Dialog.Build ("buttons");
end Gtk.Message_Dialog;
