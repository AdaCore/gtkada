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

--  Asynchronous API to open a uri with an application.
--
--  `GtkUriLauncher` collects the arguments that are needed to open the uri.
--
--  Depending on system configuration, user preferences and available APIs,
--  this may or may not show an app chooser dialog or launch the default
--  application right away.
--
--  The operation is started with the [methodGtk.UriLauncher.launch] function.
--
--  To launch a file, use [classGtk.FileLauncher].
--
--  <group>Dialogs</group>
--  <gtkada_demo>create_file_chooser.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Error;       use Glib.Error;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Gtk.Window;       use Gtk.Window;

package Gtk.Uri_Launcher is

   type Gtk_Uri_Launcher_Record is new GObject_Record with null record;
   type Gtk_Uri_Launcher is access all Gtk_Uri_Launcher_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gasync_Ready_Callback is access procedure
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);
   --  Type definition for a function that will be called back when an
   --  asynchronous operation within GIO has been completed.
   --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
   --  invoked in a later iteration of the thread-default main context (see
   --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask was
   --  created. All other users of Gasync_Ready_Callback must likewise call it
   --  asynchronously in a later iteration of the main context.
   --  The asynchronous operation is guaranteed to have held a reference to
   --  Source_Object from the time when the `*_async` function was called,
   --  until after this callback returns.
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Uri_Launcher; URI : UTF8_String := "");
   procedure Initialize
      (Self : not null access Gtk_Uri_Launcher_Record'Class;
       URI  : UTF8_String := "");
   --  Creates a new `GtkUriLauncher` object.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param URI the uri to open

   function Gtk_Uri_Launcher_New
      (URI : UTF8_String := "") return Gtk_Uri_Launcher;
   --  Creates a new `GtkUriLauncher` object.
   --  Since: gtk+ 4.10
   --  @param URI the uri to open

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_uri_launcher_get_type");

   -------------
   -- Methods --
   -------------

   function Can_Launch
      (Self   : not null access Gtk_Uri_Launcher_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class) return Boolean;
   --  Returns whether the launcher is likely to succeed in launching an
   --  application for its uri.
   --  This can be used to disable controls that trigger the launcher when
   --  they are known not to work.
   --  Since: gtk+ 4.20
   --  @param Parent the parent window
   --  @return false if the launcher is known not to support the uri, true
   --  otherwise

   function Get_Uri
      (Self : not null access Gtk_Uri_Launcher_Record) return UTF8_String;
   --  Gets the uri that will be opened.
   --  Since: gtk+ 4.10
   --  @return the uri

   procedure Set_Uri
      (Self : not null access Gtk_Uri_Launcher_Record;
       URI  : UTF8_String := "");
   --  Sets the uri that will be opened.
   --  Since: gtk+ 4.10
   --  @param URI the uri

   procedure Launch
      (Self        : not null access Gtk_Uri_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Launches an application to open the uri.
   --  This may present an app chooser dialog to the user.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Launch_Finish
      (Self   : not null access Gtk_Uri_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes the [methodGtk.UriLauncher.launch] call and returns the
   --  result.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return true if an application was launched

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   URI_Property : constant Glib.Properties.Property_String;
   --  The uri to launch.

private
   URI_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("uri");
end Gtk.Uri_Launcher;
