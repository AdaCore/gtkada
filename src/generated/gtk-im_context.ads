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

--  The interface for GTK input methods.
--
--  `GtkIMContext` is used by GTK text input widgets like `GtkText` to map
--  from key events to Unicode character strings.
--
--  An input method may consume multiple key events in sequence before finally
--  outputting the composed result. This is called *preediting*, and an input
--  method may provide feedback about this process by displaying the
--  intermediate composition states as preedit text. To do so, the
--  `GtkIMContext` will emit [signalGtk.IMContext::preedit-start],
--  [signalGtk.IMContext::preedit-changed] and
--  [signalGtk.IMContext::preedit-end] signals.
--
--  For instance, the built-in GTK input method [classGtk.IMContextSimple]
--  implements the input of arbitrary Unicode code points by holding down the
--  <kbd>Control</kbd> and <kbd>Shift</kbd> keys and then typing <kbd>u</kbd>
--  followed by the hexadecimal digits of the code point. When releasing the
--  <kbd>Control</kbd> and <kbd>Shift</kbd> keys, preediting ends and the
--  character is inserted as text. For example,
--
--  Ctrl+Shift+u 2 0 A C
--
--  results in the € sign.
--
--  Additional input methods can be made available for use by GTK widgets as
--  loadable modules. An input method module is a small shared library which
--  provides a `GIOExtension` for the extension point named "gtk-im-module".
--
--  To connect a widget to the users preferred input method, you should use
--  [classGtk.IMMulticontext].

pragma Warnings (Off, "*is already use-visible*");
with Gdk;              use Gdk;
with Gdk.Device;
with Gdk.Enums;        use Gdk.Enums;
with Gdk.Event;        use Gdk.Event;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Surface;
with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Types;     use Gtkada.Types;
with Pango.Attributes; use Pango.Attributes;

package Gtk.IM_Context is

   type Gtk_IM_Context_Record is new GObject_Record with null record;
   type Gtk_IM_Context is access all Gtk_IM_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_im_context_get_type");

   -------------
   -- Methods --
   -------------

   function Activate_Osk
      (Self  : not null access Gtk_IM_Context_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Requests the platform to show an on-screen keyboard for user input.
   --  This method will return True if this request was actually performed to
   --  the platform, other environmental factors may result in an on-screen
   --  keyboard effectively not showing up.
   --  Since: gtk+ 4.14
   --  @param Event a [classGdk.Event]
   --  @return True if an on-screen keyboard could be requested to the
   --  platform.

   function Delete_Surrounding
      (Self    : not null access Gtk_IM_Context_Record;
       Offset  : Glib.Gint;
       N_Chars : Glib.Gint) return Boolean;
   --  Asks the widget that the input context is attached to delete characters
   --  around the cursor position by emitting the `::delete_surrounding`
   --  signal.
   --  Note that Offset and N_Chars are in characters not in bytes which
   --  differs from the usage other places in `GtkIMContext`.
   --  In order to use this function, you should first call
   --  [methodGtk.IMContext.get_surrounding] to get the current context, and
   --  call this function immediately afterwards to make sure that you know
   --  what you are deleting. You should also account for the fact that even if
   --  the signal was handled, the input context might not have deleted all the
   --  characters that were requested to be deleted.
   --  This function is used by an input method that wants to make
   --  substitutions in the existing text in response to new input. It is not
   --  useful for applications.
   --  @param Offset offset from cursor position in chars; a negative value
   --  means start before the cursor.
   --  @param N_Chars number of characters to delete.
   --  @return True if the signal was handled.

   function Filter_Key
      (Self    : not null access Gtk_IM_Context_Record;
       Press   : Boolean;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class;
       Device  : not null access Gdk.Device.Gdk_Device_Record'Class;
       Time    : Guint32;
       Keycode : Guint;
       State   : Gdk.Enums.Gdk_Modifier_Type;
       Group   : Glib.Gint) return Boolean;
   --  Allow an input method to forward key press and release events to
   --  another input method without necessarily having a `GdkEvent` available.
   --  @param Press whether to forward a key press or release event
   --  @param Surface the surface the event is for
   --  @param Device the device that the event is for
   --  @param Time the timestamp for the event
   --  @param Keycode the keycode for the event
   --  @param State modifier state for the event
   --  @param Group the active keyboard group for the event
   --  @return True if the input method handled the key event.

   function Filter_Keypress
      (Self  : not null access Gtk_IM_Context_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Allow an input method to internally handle key press and release
   --  events.
   --  If this function returns True, then no further processing should be
   --  done for this key event.
   --  @param Event the key event
   --  @return True if the input method handled the key event.

   procedure Focus_In (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that the widget to which this input context
   --  corresponds has gained focus.
   --  The input method may, for example, change the displayed feedback to
   --  reflect this change.

   procedure Focus_Out (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that the widget to which this input context
   --  corresponds has lost focus.
   --  The input method may, for example, change the displayed feedback or
   --  reset the contexts state to reflect this change.

   procedure Get_Preedit_String
      (Self       : not null access Gtk_IM_Context_Record;
       Str        : out Gtkada.Types.Chars_Ptr;
       Attrs      : out Pango.Attributes.Pango_Attr_List;
       Cursor_Pos : out Glib.Gint);
   --  Retrieve the current preedit string for the input context, and a list
   --  of attributes to apply to the string.
   --  This string should be displayed inserted at the insertion point.
   --  @param Str location to store the retrieved string. The string retrieved
   --  must be freed with g_free.
   --  @param Attrs location to store the retrieved attribute list. When you
   --  are done with this list, you must unreference it with
   --  [methodPango.AttrList.unref]. Has transfer-ownership='full'.
   --  @param Cursor_Pos location to store position of cursor (in characters)
   --  within the preedit string.

   function Get_Surrounding_With_Selection
      (Self         : not null access Gtk_IM_Context_Record;
       Text         : out Gtkada.Types.Chars_Ptr;
       Cursor_Index : out Glib.Gint;
       Anchor_Index : out Glib.Gint) return Boolean;
   --  Retrieves context around the insertion point.
   --  Input methods typically want context in order to constrain input text
   --  based on existing text; this is important for languages such as Thai
   --  where only some sequences of characters are allowed.
   --  This function is implemented by emitting the
   --  [signalGtk.IMContext::retrieve-surrounding] signal on the input method;
   --  in response to this signal, a widget should provide as much context as
   --  is available, up to an entire paragraph, by calling
   --  [methodGtk.IMContext.set_surrounding_with_selection].
   --  Note that there is no obligation for a widget to respond to the
   --  `::retrieve-surrounding` signal, so input methods must be prepared to
   --  function without context.
   --  Since: gtk+ 4.2
   --  @param Text location to store a UTF-8 encoded string of text holding
   --  context around the insertion point. If the function returns True, then
   --  you must free the result stored in this location with g_free.
   --  @param Cursor_Index location to store byte index of the insertion
   --  cursor within Text.
   --  @param Anchor_Index location to store byte index of the selection bound
   --  within Text
   --  @return `TRUE` if surrounding text was provided; in this case you must
   --  free the result stored in `text`.

   procedure Set_Surrounding_With_Selection
      (Self         : not null access Gtk_IM_Context_Record;
       Text         : UTF8_String;
       Len          : Glib.Gint;
       Cursor_Index : Glib.Gint;
       Anchor_Index : Glib.Gint);
   --  Sets surrounding context around the insertion point and preedit string.
   --  This function is expected to be called in response to the
   --  [signalGtk.IMContext::retrieve_surrounding] signal, and will likely have
   --  no effect if called at other times.
   --  Since: gtk+ 4.2
   --  @param Text text surrounding the insertion point, as UTF-8. the preedit
   --  string should not be included within Text
   --  @param Len the length of Text, or -1 if Text is nul-terminated
   --  @param Cursor_Index the byte index of the insertion cursor within Text
   --  @param Anchor_Index the byte index of the selection bound within Text

   procedure Reset (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that a change such as a change in cursor
   --  position has been made.
   --  This will typically cause the input method to clear the preedit state.

   procedure Set_Client_Widget
      (Self   : not null access Gtk_IM_Context_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the client widget for the input context.
   --  This is the `GtkWidget` holding the input focus. This widget is used in
   --  order to correctly position status windows, and may also be used for
   --  purposes internal to the input method.
   --  @param Widget the client widget. This may be null to indicate that the
   --  previous client widget no longer exists.

   procedure Set_Cursor_Location
      (Self : not null access Gtk_IM_Context_Record;
       Area : Gdk.Rectangle.Gdk_Rectangle);
   --  Notify the input method that a change in cursor position has been made.
   --  The location is relative to the client widget.
   --  @param Area new location

   procedure Set_Use_Preedit
      (Self        : not null access Gtk_IM_Context_Record;
       Use_Preedit : Boolean);
   --  Sets whether the IM context should use the preedit string to display
   --  feedback.
   --  If Use_Preedit is False (default is True), then the IM context may use
   --  some other method to display feedback, such as displaying it in a child
   --  of the root window.
   --  @param Use_Preedit whether the IM context should use the preedit
   --  string.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;
   --  Additional hints that allow input methods to fine-tune their behaviour.

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;
   --  The purpose of the text field that the `GtkIMContext is connected to.
   --
   --  This property can be used by on-screen keyboards and other input
   --  methods to adjust their behaviour.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_IM_Context_UTF8_String_Void is not null access procedure
     (Self : access Gtk_IM_Context_Record'Class;
      Str  : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Str  : UTF8_String);

   Signal_Commit : constant Glib.Signal_Name := "commit";
   procedure On_Commit
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Commit
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::commit signal is emitted when a complete input sequence has been
   --  entered by the user.
   --
   --  If the commit comes after a preediting sequence, the ::commit signal is
   --  emitted after ::preedit-end.
   --
   --  This can be a single character immediately after a key press or the
   --  final result of preediting.

   type Cb_Gtk_IM_Context_Gint_Gint_Boolean is not null access function
     (Self    : access Gtk_IM_Context_Record'Class;
      Offset  : Glib.Gint;
      N_Chars : Glib.Gint) return Boolean;

   type Cb_GObject_Gint_Gint_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Offset  : Glib.Gint;
      N_Chars : Glib.Gint) return Boolean;

   Signal_Delete_Surrounding : constant Glib.Signal_Name := "delete-surrounding";
   procedure On_Delete_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Gint_Gint_Boolean;
       After : Boolean := False);
   procedure On_Delete_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Gint_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::delete-surrounding signal is emitted when the input method needs
   --  to delete all or part of the context surrounding the cursor.
   -- 
   --  Callback parameters:
   --    --  @param Offset the character offset from the cursor position of the text
   --    --  to be deleted. A negative value indicates a position before the cursor.
   --    --  @param N_Chars the number of characters to be deleted

   type Cb_Gtk_IM_Context_UTF8_String_Boolean is not null access function
     (Self : access Gtk_IM_Context_Record'Class;
      Str  : UTF8_String) return Boolean;

   type Cb_GObject_UTF8_String_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      Str  : UTF8_String) return Boolean;

   Signal_Invalid_Composition : constant Glib.Signal_Name := "invalid-composition";
   procedure On_Invalid_Composition
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_UTF8_String_Boolean;
       After : Boolean := False);
   procedure On_Invalid_Composition
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the filtered keys do not compose to a single valid
   --  character.
   -- 
   --  Callback parameters:
   --    --  @param Str the completed character(s) entered by the user

   type Cb_Gtk_IM_Context_Void is not null access procedure (Self : access Gtk_IM_Context_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Preedit_Changed : constant Glib.Signal_Name := "preedit-changed";
   procedure On_Preedit_Changed
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False);
   procedure On_Preedit_Changed
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::preedit-changed signal is emitted whenever the preedit sequence
   --  currently being entered has changed.
   --
   --  It is also emitted at the end of a preedit sequence, in which case
   --  [methodGtk.IMContext.get_preedit_string] returns the empty string.

   Signal_Preedit_End : constant Glib.Signal_Name := "preedit-end";
   procedure On_Preedit_End
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False);
   procedure On_Preedit_End
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::preedit-end signal is emitted when a preediting sequence has been
   --  completed or canceled.

   Signal_Preedit_Start : constant Glib.Signal_Name := "preedit-start";
   procedure On_Preedit_Start
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Void;
       After : Boolean := False);
   procedure On_Preedit_Start
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::preedit-start signal is emitted when a new preediting sequence
   --  starts.

   type Cb_Gtk_IM_Context_Boolean is not null access function
     (Self : access Gtk_IM_Context_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Retrieve_Surrounding : constant Glib.Signal_Name := "retrieve-surrounding";
   procedure On_Retrieve_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_Gtk_IM_Context_Boolean;
       After : Boolean := False);
   procedure On_Retrieve_Surrounding
      (Self  : not null access Gtk_IM_Context_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::retrieve-surrounding signal is emitted when the input method
   --  requires the context surrounding the cursor.
   --
   --  The callback should set the input method surrounding context by calling
   --  the [methodGtk.IMContext.set_surrounding] method.
   -- 
   --  Callback parameters:

private
   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose :=
     Gtk.Enums.Build ("input-purpose");
   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints :=
     Gtk.Enums.Build ("input-hints");
end Gtk.IM_Context;
