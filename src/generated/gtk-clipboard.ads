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
--  The Gtk.Clipboard.Gtk_Clipboard object represents a clipboard of data
--  shared between different processes or between different widgets in the same
--  process. Each clipboard is identified by a name encoded as a
--  Gdk.Types.Gdk_Atom. (Conversion to and from strings can be done with
--  gdk_atom_intern and gdk_atom_name.) The default clipboard corresponds to
--  the "CLIPBOARD" atom; another commonly used clipboard is the "PRIMARY"
--  clipboard, which, in X, traditionally contains the currently selected text.
--
--  To support having a number of different formats on the clipboard at the
--  same time, the clipboard mechanism allows providing callbacks instead of
--  the actual data. When you set the contents of the clipboard, you can either
--  supply the data directly (via functions like Gtk.Clipboard.Set_Text), or
--  you can supply a callback to be called at a later time when the data is
--  needed (via gtk_clipboard_set_with_data or gtk_clipboard_set_with_owner.)
--  Providing a callback also avoids having to make copies of the data when it
--  is not needed.
--
--  gtk_clipboard_set_with_data and gtk_clipboard_set_with_owner are quite
--  similar; the choice between the two depends mostly on which is more
--  convenient in a particular situation. The former is most useful when you
--  want to have a blob of data with callbacks to convert it into the various
--  data types that you advertise. When the Clear_Func you provided is called,
--  you simply free the data blob. The latter is more useful when the contents
--  of clipboard reflect the internal state of a Glib.Object.GObject (As an
--  example, for the PRIMARY clipboard, when an entry widget provides the
--  clipboard's contents the contents are simply the text within the selected
--  region.) If the contents change, the entry widget can call
--  gtk_clipboard_set_with_owner to update the timestamp for clipboard
--  ownership, without having to worry about Clear_Func being called.
--
--  Requesting the data from the clipboard is essentially asynchronous. If the
--  contents of the clipboard are provided within the same process, then a
--  direct function call will be made to retrieve the data, but if they are
--  provided by another process, then the data needs to be retrieved from the
--  other process, which may take some time. To avoid blocking the user
--  interface, the call to request the selection,
--  Gtk.Clipboard.Request_Contents takes a callback that will be called when
--  the contents are received (or when the request fails.) If you don't want to
--  deal with providing a separate callback, you can also use
--  Gtk.Clipboard.Wait_For_Contents. What this does is run the GLib main loop
--  recursively waiting for the contents. This can simplify the code flow, but
--  you still have to be aware that other callbacks in your program can be
--  called while this recursive mainloop is running.
--
--  Along with the functions to get the clipboard contents as an arbitrary
--  data chunk, there are also functions to retrieve it as text,
--  Gtk.Clipboard.Request_Text and Gtk.Clipboard.Wait_For_Text. These functions
--  take care of determining which formats are advertised by the clipboard
--  provider, asking for the clipboard in the best available format and
--  converting the results into the UTF-8 encoding. (The standard form for
--  representing strings in GTK+.)
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;       use GNAT.Strings;
with Gdk.Display;        use Gdk.Display;
with Gdk.Event;          use Gdk.Event;
with Gdk.Pixbuf;         use Gdk.Pixbuf;
with Gdk.Types;          use Gdk.Types;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Selection_Data; use Gtk.Selection_Data;
with Gtk.Target_List;    use Gtk.Target_List;

package Gtk.Clipboard is

   type Gtk_Clipboard_Record is new GObject_Record with null record;
   type Gtk_Clipboard is access all Gtk_Clipboard_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Clipboard_Received_Func is access procedure
     (Clipboard      : not null access Gtk_Clipboard_Record'Class;
      Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data);
   --  A function to be called when the results of
   --  Gtk.Clipboard.Request_Contents are received, or when the request fails.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data containing
   --  the data was received. If retrieving the data failed, then then length
   --  field of Selection_Data will be negative.

   type Gtk_Clipboard_Image_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  A function to be called when the results of Gtk.Clipboard.Request_Image
   --  are received, or when the request fails.
   --  Since: gtk+ 2.6
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "pixbuf": the received image

   type Gtk_Clipboard_Rich_Text_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Format    : Gdk.Types.Gdk_Atom;
      Text      : UTF8_String := "";
      Length    : Gsize);
   --  A function to be called when the results of
   --  Gtk.Clipboard.Request_Rich_Text are received, or when the request fails.
   --  Since: gtk+ 2.10
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "format": The format of the rich text
   --  "text": the rich text received, as a UTF-8 encoded string, or null if
   --  retrieving the data failed.
   --  "length": Length of the text.

   type Gtk_Clipboard_Targets_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Atoms     : Gdk_Atom_Array;
      N_Atoms   : Glib.Gint);
   --  A function to be called when the results of
   --  Gtk.Clipboard.Request_Targets are received, or when the request fails.
   --  Since: gtk+ 2.4
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "atoms": the supported targets, as array of Gdk.Types.Gdk_Atom, or null
   --  if retrieving the data failed.
   --  "n_atoms": the length of the Atoms array.

   type Gtk_Clipboard_Text_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Text      : UTF8_String := "");
   --  A function to be called when the results of Gtk.Clipboard.Request_Text
   --  are received, or when the request fails.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "text": the text received, as a UTF-8 encoded string, or null if
   --  retrieving the data failed.

   type Gtk_Clipboard_Urireceived_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Uris      : GNAT.Strings.String_List);
   --  A function to be called when the results of Gtk.Clipboard.Request_Uris
   --  are received, or when the request fails.
   --  Since: gtk+ 2.14
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "uris": the received URIs

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_clipboard_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Clipboard : not null access Gtk_Clipboard_Record);
   --  Clears the contents of the clipboard. Generally this should only be
   --  called between the time you call gtk_clipboard_set_with_owner or
   --  gtk_clipboard_set_with_data, and when the Clear_Func you supplied is
   --  called. Otherwise, the clipboard may be owned by someone else.

   function Get_Display
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Display.Gdk_Display;
   --  Gets the Gdk.Display.Gdk_Display associated with Clipboard
   --  Since: gtk+ 2.2

   function Get_Owner
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Glib.Object.GObject;
   --  If the clipboard contents callbacks were set with
   --  gtk_clipboard_set_with_owner, and the gtk_clipboard_set_with_data or
   --  Gtk.Clipboard.Clear has not subsequently called, returns the owner set
   --  by gtk_clipboard_set_with_owner.

   function Get_Selection
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Types.Gdk_Atom;
   --  Gets the selection that this clipboard is for.
   --  Since: gtk+ 3.22

   procedure Request_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom;
       Callback  : Gtk_Clipboard_Received_Func);
   --  Requests the contents of clipboard as the given target. When the
   --  results of the result are later received the supplied callback will be
   --  called.
   --  "target": an atom representing the form into which the clipboard owner
   --  should convert the selection.
   --  "callback": A function to call when the results are received (or the
   --  retrieval fails). If the retrieval fails the length field of
   --  Selection_Data will be negative.

   procedure Request_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Image_Received_Func);
   --  Requests the contents of the clipboard as image. When the image is
   --  later received, it will be converted to a Gdk.Pixbuf.Gdk_Pixbuf, and
   --  Callback will be called.
   --  The Pixbuf parameter to Callback will contain the resulting
   --  Gdk.Pixbuf.Gdk_Pixbuf if the request succeeded, or null if it failed.
   --  This could happen for various reasons, in particular if the clipboard
   --  was empty or if the contents of the clipboard could not be converted
   --  into an image.
   --  Since: gtk+ 2.6
   --  "callback": a function to call when the image is received, or the
   --  retrieval fails. (It will always be called one way or the other.)

   procedure Request_Rich_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : not null access Glib.Object.GObject_Record'Class;
       Callback  : Gtk_Clipboard_Rich_Text_Received_Func);
   --  Requests the contents of the clipboard as rich text. When the rich text
   --  is later received, Callback will be called.
   --  The Text parameter to Callback will contain the resulting rich text if
   --  the request succeeded, or null if it failed. The Length parameter will
   --  contain Text's length. This function can fail for various reasons, in
   --  particular if the clipboard was empty or if the contents of the
   --  clipboard could not be converted into rich text form.
   --  Since: gtk+ 2.10
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer
   --  "callback": a function to call when the text is received, or the
   --  retrieval fails. (It will always be called one way or the other.)

   procedure Request_Targets
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Targets_Received_Func);
   --  Requests the contents of the clipboard as list of supported targets.
   --  When the list is later received, Callback will be called.
   --  The Targets parameter to Callback will contain the resulting targets if
   --  the request succeeded, or null if it failed.
   --  Since: gtk+ 2.4
   --  "callback": a function to call when the targets are received, or the
   --  retrieval fails. (It will always be called one way or the other.)

   procedure Request_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Text_Received_Func);
   --  Requests the contents of the clipboard as text. When the text is later
   --  received, it will be converted to UTF-8 if necessary, and Callback will
   --  be called.
   --  The Text parameter to Callback will contain the resulting text if the
   --  request succeeded, or null if it failed. This could happen for various
   --  reasons, in particular if the clipboard was empty or if the contents of
   --  the clipboard could not be converted into text form.
   --  "callback": a function to call when the text is received, or the
   --  retrieval fails. (It will always be called one way or the other.)

   procedure Request_Uris
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Urireceived_Func);
   --  Requests the contents of the clipboard as URIs. When the URIs are later
   --  received Callback will be called.
   --  The Uris parameter to Callback will contain the resulting array of URIs
   --  if the request succeeded, or null if it failed. This could happen for
   --  various reasons, in particular if the clipboard was empty or if the
   --  contents of the clipboard could not be converted into URI form.
   --  Since: gtk+ 2.14
   --  "callback": a function to call when the URIs are received, or the
   --  retrieval fails. (It will always be called one way or the other.)

   procedure Set_Can_Store
      (Clipboard : not null access Gtk_Clipboard_Record;
       Targets   : Gtk.Target_List.Target_Entry_Array;
       N_Targets : Glib.Gint);
   --  Hints that the clipboard data should be stored somewhere when the
   --  application exits or when gtk_clipboard_store () is called.
   --  This value is reset when the clipboard owner changes. Where the
   --  clipboard data is stored is platform dependent, see
   --  gdk_display_store_clipboard () for more information.
   --  Since: gtk+ 2.6
   --  "targets": array containing information about which forms should be
   --  stored or null to indicate that all forms should be stored.
   --  "n_targets": number of elements in Targets

   procedure Set_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the contents of the clipboard to the given Gdk.Pixbuf.Gdk_Pixbuf.
   --  GTK+ will take responsibility for responding for requests for the image,
   --  and for converting the image into the requested format.
   --  Since: gtk+ 2.6
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   procedure Set_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Text      : UTF8_String);
   --  Sets the contents of the clipboard to the given UTF-8 string. GTK+ will
   --  make a copy of the text and take responsibility for responding for
   --  requests for the text, and for converting the text into the requested
   --  format.
   --  "text": a UTF-8 string.

   procedure Store (Clipboard : not null access Gtk_Clipboard_Record);
   --  Stores the current clipboard data somewhere so that it will stay around
   --  after the application has quit.
   --  Since: gtk+ 2.6

   function Wait_For_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom)
       return Gtk.Selection_Data.Gtk_Selection_Data;
   --  Requests the contents of the clipboard using the given target. This
   --  function waits for the data to be received using the main loop, so
   --  events, timeouts, etc, may be dispatched during the wait.
   --  "target": an atom representing the form into which the clipboard owner
   --  should convert the selection.

   function Wait_For_Image
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Requests the contents of the clipboard as image and converts the result
   --  to a Gdk.Pixbuf.Gdk_Pixbuf. This function waits for the data to be
   --  received using the main loop, so events, timeouts, etc, may be
   --  dispatched during the wait.
   --  Since: gtk+ 2.6

   function Wait_For_Text
      (Clipboard : not null access Gtk_Clipboard_Record) return UTF8_String;
   --  Requests the contents of the clipboard as text and converts the result
   --  to UTF-8 if necessary. This function waits for the data to be received
   --  using the main loop, so events, timeouts, etc, may be dispatched during
   --  the wait.

   function Wait_For_Uris
      (Clipboard : not null access Gtk_Clipboard_Record)
       return GNAT.Strings.String_List;
   --  Requests the contents of the clipboard as URIs. This function waits for
   --  the data to be received using the main loop, so events, timeouts, etc,
   --  may be dispatched during the wait.
   --  Since: gtk+ 2.14

   function Wait_Is_Image_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;
   --  Test to see if there is an image available to be pasted This is done by
   --  requesting the TARGETS atom and checking if it contains any of the
   --  supported image targets. This function waits for the data to be received
   --  using the main loop, so events, timeouts, etc, may be dispatched during
   --  the wait.
   --  This function is a little faster than calling
   --  Gtk.Clipboard.Wait_For_Image since it doesn't need to retrieve the
   --  actual image data.
   --  Since: gtk+ 2.6

   function Wait_Is_Rich_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : not null access Glib.Object.GObject_Record'Class)
       return Boolean;
   --  Test to see if there is rich text available to be pasted This is done
   --  by requesting the TARGETS atom and checking if it contains any of the
   --  supported rich text targets. This function waits for the data to be
   --  received using the main loop, so events, timeouts, etc, may be
   --  dispatched during the wait.
   --  This function is a little faster than calling
   --  gtk_clipboard_wait_for_rich_text since it doesn't need to retrieve the
   --  actual text.
   --  Since: gtk+ 2.10
   --  "buffer": a Gtk.Text_Buffer.Gtk_Text_Buffer

   function Wait_Is_Target_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom) return Boolean;
   --  Checks if a clipboard supports pasting data of a given type. This
   --  function can be used to determine if a "Paste" menu item should be
   --  insensitive or not.
   --  If you want to see if there's text available on the clipboard, use
   --  gtk_clipboard_wait_is_text_available () instead.
   --  Since: gtk+ 2.6
   --  "target": A Gdk.Types.Gdk_Atom indicating which target to look for.

   function Wait_Is_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;
   --  Test to see if there is text available to be pasted This is done by
   --  requesting the TARGETS atom and checking if it contains any of the
   --  supported text targets. This function waits for the data to be received
   --  using the main loop, so events, timeouts, etc, may be dispatched during
   --  the wait.
   --  This function is a little faster than calling
   --  Gtk.Clipboard.Wait_For_Text since it doesn't need to retrieve the actual
   --  text.

   function Wait_Is_Uris_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;
   --  Test to see if there is a list of URIs available to be pasted This is
   --  done by requesting the TARGETS atom and checking if it contains the URI
   --  targets. This function waits for the data to be received using the main
   --  loop, so events, timeouts, etc, may be dispatched during the wait.
   --  This function is a little faster than calling
   --  Gtk.Clipboard.Wait_For_Uris since it doesn't need to retrieve the actual
   --  URI data.
   --  Since: gtk+ 2.14

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Wait_For_Targets
     (Clipboard : not null access Gtk_Clipboard_Record)
   return Gdk.Types.Gdk_Atom_Array;
   --  Returns a list of targets that are present on the clipboard, or an empty
   --  array if there aren't any targets available.
   --  This function waits for the data to be received using the main
   --  loop, so events, timeouts, etc, may be dispatched during the wait.

   ---------------
   -- Functions --
   ---------------

   function Get
      (Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard;
   --  Returns the clipboard object for the given selection. See
   --  Gtk.Clipboard.Get_For_Display for complete details.
   --  "selection": a Gdk.Types.Gdk_Atom which identifies the clipboard to use

   function Get_Default
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gtk_Clipboard;
   --  Returns the default clipboard object for use with cut/copy/paste menu
   --  items and keyboard shortcuts.
   --  Since: gtk+ 3.16
   --  "display": the Gdk.Display.Gdk_Display for which the clipboard is to be
   --  retrieved.

   function Get_For_Display
      (Display   : not null access Gdk.Display.Gdk_Display_Record'Class;
       Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard;
   --  Returns the clipboard object for the given selection. Cut/copy/paste
   --  menu items and keyboard shortcuts should use the default clipboard,
   --  returned by passing GDK_SELECTION_CLIPBOARD for Selection. (GDK_NONE is
   --  supported as a synonym for GDK_SELECTION_CLIPBOARD for backwards
   --  compatibility reasons.) The currently-selected object or text should be
   --  provided on the clipboard identified by GDK_SELECTION_PRIMARY.
   --  Cut/copy/paste menu items conceptually copy the contents of the
   --  GDK_SELECTION_PRIMARY clipboard to the default clipboard, i.e. they copy
   --  the selection to what the user sees as the clipboard.
   --  (Passing GDK_NONE is the same as using `gdk_atom_intern ("CLIPBOARD",
   --  FALSE)`.
   --  See the [FreeDesktop Clipboard
   --  Specification](http://www.freedesktop.org/Standards/clipboards-spec) for
   --  a detailed discussion of the "CLIPBOARD" vs. "PRIMARY" selections under
   --  the X window system. On Win32 the GDK_SELECTION_PRIMARY clipboard is
   --  essentially ignored.)
   --  It's possible to have arbitrary named clipboards; if you do invent new
   --  clipboards, you should prefix the selection name with an underscore
   --  (because the ICCCM requires that nonstandard atoms are
   --  underscore-prefixed), and namespace it as well. For example, if your
   --  application called "Foo" has a special-purpose clipboard, you might call
   --  it "_FOO_SPECIAL_CLIPBOARD".
   --  Since: gtk+ 2.2
   --  "display": the Gdk.Display.Gdk_Display for which the clipboard is to be
   --  retrieved or created.
   --  "selection": a Gdk.Types.Gdk_Atom which identifies the clipboard to
   --  use.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Clipboard_Gdk_Event_Owner_Change_Void is not null access procedure
     (Self  : access Gtk_Clipboard_Record'Class;
      Event : Gdk.Event.Gdk_Event_Owner_Change);

   type Cb_GObject_Gdk_Event_Owner_Change_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Owner_Change);

   Signal_Owner_Change : constant Glib.Signal_Name := "owner-change";
   procedure On_Owner_Change
      (Self  : not null access Gtk_Clipboard_Record;
       Call  : Cb_Gtk_Clipboard_Gdk_Event_Owner_Change_Void;
       After : Boolean := False);
   procedure On_Owner_Change
      (Self  : not null access Gtk_Clipboard_Record;
       Call  : Cb_GObject_Gdk_Event_Owner_Change_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::owner-change signal is emitted when GTK+ receives an event that
   --  indicates that the ownership of the selection associated with Clipboard
   --  has changed.

end Gtk.Clipboard;
