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
--  Gtk.IM_Context.Gtk_IM_Context defines the interface for GTK+ input
--  methods. An input method is used by GTK+ text input widgets like
--  Gtk.GEntry.Gtk_Entry to map from key events to Unicode character strings.
--
--  The default input method can be set programmatically via the
--  Gtk.Settings.Gtk_Settings:gtk-im-module GtkSettings property.
--  Alternatively, you may set the GTK_IM_MODULE environment variable as
--  documented in [Running GTK+ Applications][gtk-running].
--
--  The Gtk.GEntry.Gtk_Entry Gtk.GEntry.Gtk_Entry:im-module and
--  Gtk.Text_View.Gtk_Text_View Gtk.Text_View.Gtk_Text_View:im-module
--  properties may also be used to set input methods for specific widget
--  instances. For instance, a certain entry widget might be expected to
--  contain certain characters which would be easier to input with a certain
--  input method.
--
--  An input method may consume multiple key events in sequence and finally
--  output the composed result. This is called preediting, and an input method
--  may provide feedback about this process by displaying the intermediate
--  composition states as preedit text. For instance, the default GTK+ input
--  method implements the input of arbitrary Unicode code points by holding
--  down the Control and Shift keys and then typing "U" followed by the
--  hexadecimal digits of the code point. When releasing the Control and Shift
--  keys, preediting ends and the character is inserted as text.
--  Ctrl+Shift+u20AC for example results in the euro sign.
--
--  Additional input methods can be made available for use by GTK+ widgets as
--  loadable modules. An input method module is a small shared library which
--  implements a subclass of Gtk.IM_Context.Gtk_IM_Context or
--  Gtk.IM_Context_Simple.Gtk_IM_Context_Simple and exports these four
--  functions:
--
--  |[<!-- language="C" --> void im_module_init(GTypeModule *module); ]| This
--  function should register the GType of the Gtk.IM_Context.Gtk_IM_Context
--  subclass which implements the input method by means of
--  g_type_module_register_type. Note that g_type_register_static cannot be
--  used as the type needs to be registered dynamically.
--
--  |[<!-- language="C" --> void im_module_exit(void); ]| Here goes any
--  cleanup code your input method might require on module unload.
--
--  |[<!-- language="C" --> void im_module_list(const GtkIMContextInfo
--  ***contexts, int *n_contexts) { *contexts = info_list; *n_contexts =
--  G_N_ELEMENTS (info_list); } ]| This function returns the list of input
--  methods provided by the module. The example implementation above shows a
--  common solution and simply returns a pointer to statically defined array of
--  Gtk_IMContext_Info items for each provided input method.
--
--  |[<!-- language="C" --> GtkIMContext * im_module_create(const gchar
--  *context_id); ]| This function should return a pointer to a newly created
--  instance of the Gtk.IM_Context.Gtk_IM_Context subclass identified by
--  Context_Id. The context ID is the same as specified in the
--  Gtk_IMContext_Info array returned by im_module_list.
--
--  After a new loadable input method module has been installed on the system,
--  the configuration file `gtk.immodules` needs to be regenerated by
--  [gtk-query-immodules-3.0][gtk-query-immodules-3.0], in order for the new
--  input method to become available to GTK+ applications.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;           use Gdk;
with Gdk.Event;     use Gdk.Event;
with Gdk.Rectangle; use Gdk.Rectangle;
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Gtk.Enums;     use Gtk.Enums;

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

   function Delete_Surrounding
      (Self    : not null access Gtk_IM_Context_Record;
       Offset  : Glib.Gint;
       N_Chars : Glib.Gint) return Boolean;
   --  Asks the widget that the input context is attached to to delete
   --  characters around the cursor position by emitting the
   --  GtkIMContext::delete_surrounding signal. Note that Offset and N_Chars
   --  are in characters not in bytes which differs from the usage other places
   --  in Gtk.IM_Context.Gtk_IM_Context.
   --  In order to use this function, you should first call
   --  gtk_im_context_get_surrounding to get the current context, and call this
   --  function immediately afterwards to make sure that you know what you are
   --  deleting. You should also account for the fact that even if the signal
   --  was handled, the input context might not have deleted all the characters
   --  that were requested to be deleted.
   --  This function is used by an input method that wants to make
   --  subsitutions in the existing text in response to new input. It is not
   --  useful for applications.
   --  "offset": offset from cursor position in chars; a negative value means
   --  start before the cursor.
   --  "n_chars": number of characters to delete.

   function Filter_Keypress
      (Self  : not null access Gtk_IM_Context_Record;
       Event : Gdk.Event.Gdk_Event_Key) return Boolean;
   --  Allow an input method to internally handle key press and release
   --  events. If this function returns True, then no further processing should
   --  be done for this key event.
   --  "event": the key event

   procedure Focus_In (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that the widget to which this input context
   --  corresponds has gained focus. The input method may, for example, change
   --  the displayed feedback to reflect this change.

   procedure Focus_Out (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that the widget to which this input context
   --  corresponds has lost focus. The input method may, for example, change
   --  the displayed feedback or reset the contexts state to reflect this
   --  change.

   procedure Reset (Self : not null access Gtk_IM_Context_Record);
   --  Notify the input method that a change such as a change in cursor
   --  position has been made. This will typically cause the input method to
   --  clear the preedit state.

   procedure Set_Client_Window
      (Self   : not null access Gtk_IM_Context_Record;
       Window : Gdk.Gdk_Window);
   --  Set the client window for the input context; this is the Gdk.Gdk_Window
   --  in which the input appears. This window is used in order to correctly
   --  position status windows, and may also be used for purposes internal to
   --  the input method.
   --  "window": the client window. This may be null to indicate that the
   --  previous client window no longer exists.

   procedure Set_Cursor_Location
      (Self : not null access Gtk_IM_Context_Record;
       Area : Gdk.Rectangle.Gdk_Rectangle);
   --  Notify the input method that a change in cursor position has been made.
   --  The location is relative to the client window.
   --  "area": new location

   procedure Set_Surrounding
      (Self         : not null access Gtk_IM_Context_Record;
       Text         : UTF8_String;
       Len          : Glib.Gint;
       Cursor_Index : Glib.Gint);
   --  Sets surrounding context around the insertion point and preedit string.
   --  This function is expected to be called in response to the
   --  GtkIMContext::retrieve_surrounding signal, and will likely have no
   --  effect if called at other times.
   --  "text": text surrounding the insertion point, as UTF-8. the preedit
   --  string should not be included within Text.
   --  "len": the length of Text, or -1 if Text is nul-terminated
   --  "cursor_index": the byte index of the insertion cursor within Text.

   procedure Set_Use_Preedit
      (Self        : not null access Gtk_IM_Context_Record;
       Use_Preedit : Boolean);
   --  Sets whether the IM context should use the preedit string to display
   --  feedback. If Use_Preedit is FALSE (default is TRUE), then the IM context
   --  may use some other method to display feedback, such as displaying it in
   --  a child of the root window.
   --  "use_preedit": whether the IM context should use the preedit string.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints;

   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose;

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
   --  entered by the user. This can be a single character immediately after a
   --  key press or the final result of preediting.

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
   --    --  "offset": the character offset from the cursor position of the text to
   --    --  be deleted. A negative value indicates a position before the cursor.
   --    --  "n_chars": the number of characters to be deleted
   --    --  Returns True if the signal was handled.

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
   --  currently being entered has changed. It is also emitted at the end of a
   --  preedit sequence, in which case gtk_im_context_get_preedit_string
   --  returns the empty string.

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
   --  requires the context surrounding the cursor. The callback should set the
   --  input method surrounding context by calling the
   --  Gtk.IM_Context.Set_Surrounding method.
   -- 
   --  Callback parameters:
   --    --  Returns True if the signal was handled.

private
   Input_Purpose_Property : constant Gtk.Enums.Property_Gtk_Input_Purpose :=
     Gtk.Enums.Build ("input-purpose");
   Input_Hints_Property : constant Gtk.Enums.Property_Gtk_Input_Hints :=
     Gtk.Enums.Build ("input-hints");
end Gtk.IM_Context;
