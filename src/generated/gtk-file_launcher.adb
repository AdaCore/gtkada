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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Error;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

use type Glib.Error.GError;

package body Gtk.File_Launcher is

   procedure C_Gtk_File_Launcher_Launch
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Launcher_Launch, "gtk_file_launcher_launch");
   --  Launches an application to open the file.
   --  This may present an app chooser dialog to the user.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   procedure C_Gtk_File_Launcher_Open_Containing_Folder
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_File_Launcher_Open_Containing_Folder, "gtk_file_launcher_open_containing_folder");
   --  Launches a file manager to show the file in its parent directory.
   --  This is only supported for native files. It will fail if File is e.g. a
   --  http:// uri.
   --  Since: gtk+ 4.10
   --  @param Parent the parent window
   --  @param Cancellable a cancellable to cancel the operation
   --  @param Callback a callback to call when the operation is complete
   --  @param User_Data data to pass to Callback

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       Data          : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gtk_File_Launcher is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_File_Launcher_Record);
   pragma Unreferenced (Type_Conversion_Gtk_File_Launcher);

   ---------------------------
   -- Gtk_File_Launcher_New --
   ---------------------------

   function Gtk_File_Launcher_New
      (File : Glib.GFile.Gfile) return Gtk_File_Launcher
   is
      Self : constant Gtk_File_Launcher := new Gtk_File_Launcher_Record;
   begin
      Gtk.File_Launcher.Initialize (Self, File);
      return Self;
   end Gtk_File_Launcher_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_File_Launcher; File : Glib.GFile.Gfile) is
   begin
      Self := new Gtk_File_Launcher_Record;
      Gtk.File_Launcher.Initialize (Self, File);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_File_Launcher_Record'Class;
       File : Glib.GFile.Gfile)
   is
      function Internal (File : Glib.GFile.Gfile) return System.Address;
      pragma Import (C, Internal, "gtk_file_launcher_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (File));
      end if;
   end Initialize;

   --------------------
   -- Get_Always_Ask --
   --------------------

   function Get_Always_Ask
      (Self : not null access Gtk_File_Launcher_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_launcher_get_always_ask");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Always_Ask;

   --------------
   -- Get_File --
   --------------

   function Get_File
      (Self : not null access Gtk_File_Launcher_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_file_launcher_get_file");
   begin
      return Internal (Get_Object (Self));
   end Get_File;

   ------------------
   -- Get_Writable --
   ------------------

   function Get_Writable
      (Self : not null access Gtk_File_Launcher_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_launcher_get_writable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Writable;

   ------------
   -- Launch --
   ------------

   procedure Launch
      (Self        : not null access Gtk_File_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Launcher_Launch (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Launcher_Launch (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Launch;

   -------------------
   -- Launch_Finish --
   -------------------

   function Launch_Finish
      (Self   : not null access Gtk_File_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_launcher_launch_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch_Finish;

   ----------------------------
   -- Open_Containing_Folder --
   ----------------------------

   procedure Open_Containing_Folder
      (Self        : not null access Gtk_File_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_File_Launcher_Open_Containing_Folder (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_File_Launcher_Open_Containing_Folder (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Open_Containing_Folder;

   -----------------------------------
   -- Open_Containing_Folder_Finish --
   -----------------------------------

   function Open_Containing_Folder_Finish
      (Self   : not null access Gtk_File_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_file_launcher_open_containing_folder_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Open_Containing_Folder_Finish;

   --------------------
   -- Set_Always_Ask --
   --------------------

   procedure Set_Always_Ask
      (Self       : not null access Gtk_File_Launcher_Record;
       Always_Ask : Boolean)
   is
      procedure Internal (Self : System.Address; Always_Ask : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_file_launcher_set_always_ask");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Always_Ask));
   end Set_Always_Ask;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
      (Self : not null access Gtk_File_Launcher_Record;
       File : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; File : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_file_launcher_set_file");
   begin
      Internal (Get_Object (Self), File);
   end Set_File;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
      (Self     : not null access Gtk_File_Launcher_Record;
       Writable : Boolean)
   is
      procedure Internal (Self : System.Address; Writable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_file_launcher_set_writable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Writable));
   end Set_Writable;

end Gtk.File_Launcher;
