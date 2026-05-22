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

--  Represents data shared between applications or inside an application.
--
--  To get a `GdkClipboard` object, use [methodGdk.Display.get_clipboard] or
--  [methodGdk.Display.get_primary_clipboard]. You can find out about the data
--  that is currently available in a clipboard using
--  [methodGdk.Clipboard.get_formats].
--
--  To make text or image data available in a clipboard, use
--  [methodGdk.Clipboard.set_text] or [methodGdk.Clipboard.set_texture]. For
--  other data, you can use [methodGdk.Clipboard.set_content], which takes a
--  [classGdk.ContentProvider] object.
--
--  To read textual or image data from a clipboard, use
--  [methodGdk.Clipboard.read_text_async] or
--  [methodGdk.Clipboard.read_texture_async]. For other data, use
--  [methodGdk.Clipboard.read_async], which provides a `GInputStream` object.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;         use GNAT.Strings;
with Gdk.Content_Formats;  use Gdk.Content_Formats;
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Cancellable;     use Glib.Cancellable;
with Glib.Object;          use Glib.Object;
with Glib.Properties;      use Glib.Properties;
with Glib.Values;          use Glib.Values;

package Gdk.Clipboard is

   type Gdk_Clipboard_Record is new GObject_Record with null record;
   type Gdk_Clipboard is access all Gdk_Clipboard_Record'Class;

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
   pragma Import (C, Get_Type, "gdk_clipboard_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Content
      (Self : not null access Gdk_Clipboard_Record)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   --  Returns the `GdkContentProvider` currently set on Clipboard.
   --  If the Clipboard is empty or its contents are not owned by the current
   --  process, null will be returned.
   --  @return The content of a clipboard if the clipboard does not maintain
   --  any content

   function Set_Content
      (Self     : not null access Gdk_Clipboard_Record;
       Provider : access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class)
       return Boolean;
   --  Sets a new content provider on Clipboard.
   --  The clipboard will claim the `GdkDisplay`'s resources and advertise
   --  these new contents to other applications.
   --  In the rare case of a failure, this function will return False. The
   --  clipboard will then continue reporting its old contents and ignore
   --  Provider.
   --  If the contents are read by either an external application or the
   --  Clipboard's read functions, Clipboard will select the best format to
   --  transfer the contents and then request that format from Provider.
   --  @param Provider the new contents of Clipboard or null to clear the
   --  clipboard
   --  @return True if setting the clipboard succeeded

   function Get_Display
      (Self : not null access Gdk_Clipboard_Record) return Gdk.Gdk_Display;
   --  Gets the `GdkDisplay` that the clipboard was created for.
   --  @return a `GdkDisplay`

   function Get_Formats
      (Self : not null access Gdk_Clipboard_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats;
   --  Gets the formats that the clipboard can provide its current contents
   --  in.
   --  @return The formats of the clipboard

   function Is_Local
      (Self : not null access Gdk_Clipboard_Record) return Boolean;
   --  Returns if the clipboard is local.
   --  A clipboard is considered local if it was last claimed by the running
   --  application.
   --  Note that [methodGdk.Clipboard.get_content] may return null even on a
   --  local clipboard. In this case the clipboard is empty.
   --  @return True if the clipboard is local

   procedure Read_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Mime_Types  : GNAT.Strings.String_List;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously requests an input stream to read the Clipboard's
   --  contents from.
   --  The clipboard will choose the most suitable mime type from the given
   --  list to fulfill the request, preferring the ones listed first.
   --  @param Mime_Types a null-terminated array of mime types to choose from
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied

   procedure Read_Text_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously request the Clipboard contents converted to a string.
   --  This is a simple wrapper around [methodGdk.Clipboard.read_value_async].
   --  Use that function or [methodGdk.Clipboard.read_async] directly if you
   --  need more control over the operation.
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied

   function Read_Text_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return UTF8_String;
   --  Finishes an asynchronous clipboard read.
   --  See [methodGdk.Clipboard.read_text_async].
   --  @param Result a `GAsyncResult`
   --  @return a new string

   procedure Read_Texture_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously request the Clipboard contents converted to a
   --  `GdkPixbuf`.
   --  This is a simple wrapper around [methodGdk.Clipboard.read_value_async].
   --  Use that function or [methodGdk.Clipboard.read_async] directly if you
   --  need more control over the operation.
   --  @param Cancellable optional `GCancellable` object, null to ignore.
   --  @param Callback callback to call when the request is satisfied

   procedure Read_Value_Async
      (Self        : not null access Gdk_Clipboard_Record;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously request the Clipboard contents converted to the given
   --  Type.
   --  For local clipboard contents that are available in the given `GType`,
   --  the value will be copied directly. Otherwise, GDK will try to use
   --  [funcContent_Deserialize_Async] to convert the clipboard's data.
   --  @param The_Type a `GType` to read
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied

   function Read_Value_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return Glib.Values.GValue;
   --  Finishes an asynchronous clipboard read.
   --  See [methodGdk.Clipboard.read_value_async].
   --  @param Result a `GAsyncResult`
   --  @return a `GValue` containing the result.

   procedure Set_Text
      (Self : not null access Gdk_Clipboard_Record;
       Text : UTF8_String);
   --  Puts the given Text into the clipboard.
   --  @param Text Text to put into the clipboard

   procedure Set_Value
      (Self  : not null access Gdk_Clipboard_Record;
       Value : in out Glib.Values.GValue);
   --  Sets the Clipboard to contain the given Value.
   --  @param Value a `GValue` to set

   procedure Store_Async
      (Self        : not null access Gdk_Clipboard_Record;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Asynchronously instructs the Clipboard to store its contents remotely.
   --  If the clipboard is not local, this function does nothing but report
   --  success.
   --  The purpose of this call is to preserve clipboard contents beyond the
   --  lifetime of an application, so this function is typically called on
   --  exit. Depending on the platform, the functionality may not be available
   --  unless a "clipboard manager" is running.
   --  This function is called automatically when a
   --  [GtkApplication](../gtk4/class.Application.html) is shut down, so you
   --  likely don't need to call it.
   --  @param Io_Priority the I/O priority of the request
   --  @param Cancellable optional `GCancellable` object
   --  @param Callback callback to call when the request is satisfied

   function Store_Finish
      (Self   : not null access Gdk_Clipboard_Record;
       Result : Glib.G_Async_Result) return Boolean;
   --  Finishes an asynchronous clipboard store.
   --  See [methodGdk.Clipboard.store_async].
   --  @param Result a `GAsyncResult`
   --  @return True if storing was successful.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Content_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Provider
   --  The `GdkContentProvider` or null if the clipboard is empty or contents
   --  are provided otherwise.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` that the clipboard belongs to.

   Formats_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Content_Formats
   --  The possible formats that the clipboard can provide its data in.

   Local_Property : constant Glib.Properties.Property_Boolean;
   --  True if the contents of the clipboard are owned by this process.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Clipboard_Void is not null access procedure (Self : access Gdk_Clipboard_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gdk_Clipboard_Record;
       Call  : Cb_Gdk_Clipboard_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gdk_Clipboard_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the clipboard changes ownership.

private
   Local_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("local");
   Formats_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("formats");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Content_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("content");
end Gdk.Clipboard;
