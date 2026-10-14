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

--  Asynchronous API to open a file with an application.
--
--  `GtkFileLauncher` collects the arguments that are needed to open the file.
--
--  Depending on system configuration, user preferences and available APIs,
--  this may or may not show an app chooser dialog or launch the default
--  application right away.
--
--  The operation is started with the [methodGtk.FileLauncher.launch]
--  function.
--
--  To launch uris that don't represent files, use [classGtk.UriLauncher].
--
--  <group>Dialogs</group>
--  <gtkada_demo>create_file_chooser.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Error;       use Glib.Error;
with Glib.GFile;       use Glib.GFile;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Gtk.Window;       use Gtk.Window;

package Gtk.File_Launcher is

   type Gtk_File_Launcher_Record is new GObject_Record with null record;
   type Gtk_File_Launcher is access all Gtk_File_Launcher_Record'Class;

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

   procedure Gtk_New (Self : out Gtk_File_Launcher; File : Glib.GFile.Gfile);
   procedure Initialize
      (Self : not null access Gtk_File_Launcher_Record'Class;
       File : Glib.GFile.Gfile);
   --  Creates a new `GtkFileLauncher` object.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param File the file to open

   function Gtk_File_Launcher_New
      (File : Glib.GFile.Gfile) return Gtk_File_Launcher;
   --  Creates a new `GtkFileLauncher` object.
   --  Since: gtk+ 4.10
   --  @param File the file to open

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_file_launcher_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Always_Ask
      (Self : not null access Gtk_File_Launcher_Record) return Boolean;
   --  Returns whether to ask the user which app to use.
   --  Since: gtk+ 4.12
   --  @return true if always asking the user

   procedure Set_Always_Ask
      (Self       : not null access Gtk_File_Launcher_Record;
       Always_Ask : Boolean);
   --  Sets whether to always ask the user which app to use.
   --  If false, the file might be opened with a default app or the previous
   --  choice.
   --  Since: gtk+ 4.12
   --  @param Always_Ask whether to always ask

   function Get_File
      (Self : not null access Gtk_File_Launcher_Record)
       return Glib.GFile.Gfile;
   --  Gets the file that will be opened.
   --  Since: gtk+ 4.10
   --  @return the file

   procedure Set_File
      (Self : not null access Gtk_File_Launcher_Record;
       File : Glib.GFile.Gfile);
   --  Sets the file that will be opened.
   --  Since: gtk+ 4.10
   --  @param File the file

   function Get_Writable
      (Self : not null access Gtk_File_Launcher_Record) return Boolean;
   --  Returns whether to make the file writable for the handler.
   --  Since: gtk+ 4.14
   --  @return true if the file will be made writable

   procedure Set_Writable
      (Self     : not null access Gtk_File_Launcher_Record;
       Writable : Boolean);
   --  Sets whether to make the file writable for the handler.
   --  Since: gtk+ 4.14
   --  @param Writable whether to make the file writable

   procedure Launch
      (Self        : not null access Gtk_File_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Launches an application to open the file.
   --  This may present an app chooser dialog to the user.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Launch_Finish
      (Self   : not null access Gtk_File_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes the [methodGtk.FileLauncher.launch] call and returns the
   --  result.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return true if an application was launched

   procedure Open_Containing_Folder
      (Self        : not null access Gtk_File_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Launches a file manager to show the file in its parent directory.
   --  This is only supported for native files. It will fail if File is e.g. a
   --  http:// uri.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete

   function Open_Containing_Folder_Finish
      (Self   : not null access Gtk_File_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean;
   --  Finishes the [methodGtk.FileLauncher.open_containing_folder] call and
   --  returns the result.
   --  Since: gtk+ 4.10
   --  @param Result the result
   --  @param Error the return location for a recoverable error
   --  @return true if an application was launched

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Always_Ask_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to ask the user to choose an app for opening the file. If
   --  `FALSE`, the file might be opened with a default app or the previous
   --  choice.

   File_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.GFile.Gfile
   --  The file to launch.

   Writable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to make the file writable for the handler.

private
   Writable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("writable");
   File_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("file");
   Always_Ask_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("always-ask");
end Gtk.File_Launcher;
