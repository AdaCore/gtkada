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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Gtk.Uri_Launcher is

   procedure C_Gtk_Uri_Launcher_Launch
      (Self        : System.Address;
       Parent      : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gtk_Uri_Launcher_Launch, "gtk_uri_launcher_launch");
   --  Launches an application to open the uri.
   --  This may present an app chooser dialog to the user.
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

   package Type_Conversion_Gtk_Uri_Launcher is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Uri_Launcher_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Uri_Launcher);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Uri_Launcher; URI : UTF8_String := "") is
   begin
      Self := new Gtk_Uri_Launcher_Record;
      Gtk.Uri_Launcher.Initialize (Self, URI);
   end Gtk_New;

   --------------------------
   -- Gtk_Uri_Launcher_New --
   --------------------------

   function Gtk_Uri_Launcher_New
      (URI : UTF8_String := "") return Gtk_Uri_Launcher
   is
      Self : constant Gtk_Uri_Launcher := new Gtk_Uri_Launcher_Record;
   begin
      Gtk.Uri_Launcher.Initialize (Self, URI);
      return Self;
   end Gtk_Uri_Launcher_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Uri_Launcher_Record'Class;
       URI  : UTF8_String := "")
   is
      function Internal (URI : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_uri_launcher_new");
      Tmp_URI    : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_URI :=
           (if URI = ""
            then Gtkada.Types.Null_Ptr
            else New_String (URI));
         Tmp_Return := Internal (Tmp_URI);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_URI);
   end Initialize;

   ----------------
   -- Can_Launch --
   ----------------

   function Can_Launch
      (Self   : not null access Gtk_Uri_Launcher_Record;
       Parent : access Gtk.Window.Gtk_Window_Record'Class) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Parent : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_uri_launcher_can_launch");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Parent))) /= 0;
   end Can_Launch;

   -------------
   -- Get_Uri --
   -------------

   function Get_Uri
      (Self : not null access Gtk_Uri_Launcher_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_uri_launcher_get_uri");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Uri;

   ------------
   -- Launch --
   ------------

   procedure Launch
      (Self        : not null access Gtk_Uri_Launcher_Record;
       Parent      : access Gtk.Window.Gtk_Window_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Uri_Launcher_Launch (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Uri_Launcher_Launch (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Launch;

   -------------------
   -- Launch_Finish --
   -------------------

   function Launch_Finish
      (Self   : not null access Gtk_Uri_Launcher_Record;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_uri_launcher_launch_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch_Finish;

   -------------
   -- Set_Uri --
   -------------

   procedure Set_Uri
      (Self : not null access Gtk_Uri_Launcher_Record;
       URI  : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          URI  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_uri_launcher_set_uri");
      Tmp_URI : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_URI :=
        (if URI = ""
         then Gtkada.Types.Null_Ptr
         else New_String (URI));
      Internal (Get_Object (Self), Tmp_URI);
      Free (Tmp_URI);
   end Set_Uri;

end Gtk.Uri_Launcher;
