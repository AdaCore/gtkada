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
with Glib.App_Launch_Context;  use Glib.App_Launch_Context;
with Glib.Error;

use type Glib.Error.GError;

package body Glib.App_Info is

   function Convert (R : Glib.App_Info.Gapp_Info) return System.Address is
   begin
      return System.Address (R);
   end Convert;

   function Convert (R : System.Address) return Glib.App_Info.Gapp_Info is
   begin
      return Glib.App_Info.Gapp_Info (R);
   end Convert;

   procedure C_G_App_Info_Get_Default_For_Type_Async
      (Content_Type      : Gtkada.Types.Chars_Ptr;
       Must_Support_Uris : Glib.Gboolean;
       Cancellable       : System.Address;
       Callback          : System.Address;
       User_Data         : System.Address);
   pragma Import (C, C_G_App_Info_Get_Default_For_Type_Async, "g_app_info_get_default_for_type_async");
   --  Asynchronously gets the default Glib.App_Info.Gapp_Info for a given
   --  content type.
   --  Since: gtk+ 2.74
   --  @param Content_Type the content type to find a Glib.App_Info.Gapp_Info
   --  for
   --  @param Must_Support_Uris if True, the Glib.App_Info.Gapp_Info is
   --  expected to support URIs
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done
   --  @param User_Data data to pass to Callback

   procedure C_G_App_Info_Get_Default_For_Uri_Scheme_Async
      (Uri_Scheme  : Gtkada.Types.Chars_Ptr;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_App_Info_Get_Default_For_Uri_Scheme_Async, "g_app_info_get_default_for_uri_scheme_async");
   --  Asynchronously gets the default application for handling URIs with the
   --  given URI scheme. A URI scheme is the initial part of the URI, up to but
   --  not including the ':', e.g. "http", "ftp" or "sip".
   --  Since: gtk+ 2.74
   --  @param Uri_Scheme a string containing a URI scheme.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done
   --  @param User_Data data to pass to Callback

   procedure C_G_App_Info_Launch_Default_For_Uri_Async
      (URI         : Gtkada.Types.Chars_Ptr;
       Context     : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_App_Info_Launch_Default_For_Uri_Async, "g_app_info_launch_default_for_uri_async");
   --  Async version of Glib.App_Info.Launch_Default_For_Uri.
   --  This version is useful if you are interested in receiving error
   --  information in the case where the application is sandboxed and the
   --  portal may present an application chooser dialog to the user.
   --  This is also useful if you want to be sure that the D-Bus–activated
   --  applications are really started before termination and if you are
   --  interested in receiving error information from their activation.
   --  Since: gtk+ 2.50
   --  @param URI the uri to show
   --  @param Context an optional Glib.App_Launch_Context.Gapp_Launch_Context
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done
   --  @param User_Data data to pass to Callback

   procedure C_G_App_Info_Launch_Uris_Async
      (Self        : Gapp_Info;
       Uris        : System.Address;
       Context     : System.Address;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_App_Info_Launch_Uris_Async, "g_app_info_launch_uris_async");
   --  Async version of Glib.App_Info.Launch_Uris.
   --  The Callback is invoked immediately after the application launch, but
   --  it waits for activation in case of D-Bus–activated applications and also
   --  provides extended error information for sandboxed applications, see
   --  notes for Glib.App_Info.Launch_Default_For_Uri_Async.
   --  Since: gtk+ 2.60
   --  @param Uris a GList containing URIs to launch.
   --  @param Context a Glib.App_Launch_Context.Gapp_Launch_Context or null
   --  @param Cancellable a Glib.Cancellable.Gcancellable
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  done
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

   -----------------------
   -- Add_Supports_Type --
   -----------------------

   function Add_Supports_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self         : Gapp_Info;
          Content_Type : Gtkada.Types.Chars_Ptr;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_add_supports_type");
      Acc_Error        : aliased Glib.Error.GError;
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Content_Type, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Content_Type);
      return Tmp_Return /= 0;
   end Add_Supports_Type;

   ----------------
   -- Can_Delete --
   ----------------

   function Can_Delete (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_can_delete");
   begin
      return Internal (Self) /= 0;
   end Can_Delete;

   ------------------------------
   -- Can_Remove_Supports_Type --
   ------------------------------

   function Can_Remove_Supports_Type (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_can_remove_supports_type");
   begin
      return Internal (Self) /= 0;
   end Can_Remove_Supports_Type;

   ------------
   -- Delete --
   ------------

   function Delete (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_delete");
   begin
      return Internal (Self) /= 0;
   end Delete;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gapp_Info; Appinfo2 : Gapp_Info) return Boolean is
      function Internal
         (Self     : Gapp_Info;
          Appinfo2 : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_equal");
   begin
      return Internal (Self, Appinfo2) /= 0;
   end Equal;

   ---------------------
   -- Get_Commandline --
   ---------------------

   function Get_Commandline (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_commandline");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Commandline;

   --------------------------------
   -- Get_Default_For_Type_Async --
   --------------------------------

   procedure Get_Default_For_Type_Async
      (Content_Type      : UTF8_String;
       Must_Support_Uris : Boolean;
       Cancellable       : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback          : Gasync_Ready_Callback)
   is
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
   begin
      if Callback = null then
         C_G_App_Info_Get_Default_For_Type_Async (Tmp_Content_Type, Boolean'Pos (Must_Support_Uris), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Content_Type);
      else
         C_G_App_Info_Get_Default_For_Type_Async (Tmp_Content_Type, Boolean'Pos (Must_Support_Uris), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Content_Type);
      end if;
   end Get_Default_For_Type_Async;

   package body Get_Default_For_Type_Async_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gasync_Ready_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gasync_Ready_Callback, System.Address);

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask
      --  was created. All other users of Gasync_Ready_Callback must likewise
      --  call it asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was
      --  started with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      --------------------------------
      -- Get_Default_For_Type_Async --
      --------------------------------

      procedure Get_Default_For_Type_Async
         (Content_Type      : UTF8_String;
          Must_Support_Uris : Boolean;
          Cancellable       : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback          : Gasync_Ready_Callback;
          User_Data         : User_Data_Type)
      is
         Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
         D                : System.Address;
      begin
         if Callback = null then
            C_G_App_Info_Get_Default_For_Type_Async (Tmp_Content_Type, Boolean'Pos (Must_Support_Uris), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
            Free (Tmp_Content_Type);
         else
            D := Users.Build (To_Address (Callback), User_Data);
            C_G_App_Info_Get_Default_For_Type_Async (Tmp_Content_Type, Boolean'Pos (Must_Support_Uris), Get_Object_Or_Null (GObject (Cancellable)), Internal_Cb'Address, D);
            Free (Tmp_Content_Type);
            Users.Free_Data (D);
         end if;
      end Get_Default_For_Type_Async;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address)
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         To_Gasync_Ready_Callback (D.Func) (Get_User_Data (Source_Object, Stub_GObject), Res, D.Data.all);
      end Internal_Cb;

   end Get_Default_For_Type_Async_User_Data;

   --------------------------------------
   -- Get_Default_For_Uri_Scheme_Async --
   --------------------------------------

   procedure Get_Default_For_Uri_Scheme_Async
      (Uri_Scheme  : UTF8_String;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_Uri_Scheme : Gtkada.Types.Chars_Ptr := New_String (Uri_Scheme);
   begin
      if Callback = null then
         C_G_App_Info_Get_Default_For_Uri_Scheme_Async (Tmp_Uri_Scheme, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_Uri_Scheme);
      else
         C_G_App_Info_Get_Default_For_Uri_Scheme_Async (Tmp_Uri_Scheme, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_Uri_Scheme);
      end if;
   end Get_Default_For_Uri_Scheme_Async;

   package body Get_Default_For_Uri_Scheme_Async_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gasync_Ready_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gasync_Ready_Callback, System.Address);

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask
      --  was created. All other users of Gasync_Ready_Callback must likewise
      --  call it asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was
      --  started with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      --------------------------------------
      -- Get_Default_For_Uri_Scheme_Async --
      --------------------------------------

      procedure Get_Default_For_Uri_Scheme_Async
         (Uri_Scheme  : UTF8_String;
          Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback    : Gasync_Ready_Callback;
          User_Data   : User_Data_Type)
      is
         Tmp_Uri_Scheme : Gtkada.Types.Chars_Ptr := New_String (Uri_Scheme);
         D              : System.Address;
      begin
         if Callback = null then
            C_G_App_Info_Get_Default_For_Uri_Scheme_Async (Tmp_Uri_Scheme, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
            Free (Tmp_Uri_Scheme);
         else
            D := Users.Build (To_Address (Callback), User_Data);
            C_G_App_Info_Get_Default_For_Uri_Scheme_Async (Tmp_Uri_Scheme, Get_Object_Or_Null (GObject (Cancellable)), Internal_Cb'Address, D);
            Free (Tmp_Uri_Scheme);
            Users.Free_Data (D);
         end if;
      end Get_Default_For_Uri_Scheme_Async;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address)
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         To_Gasync_Ready_Callback (D.Func) (Get_User_Data (Source_Object, Stub_GObject), Res, D.Data.all);
      end Internal_Cb;

   end Get_Default_For_Uri_Scheme_Async_User_Data;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_description");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Description;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Display_Name;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_executable");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Executable;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_id");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Id;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gapp_Info) return UTF8_String is
      function Internal (Self : Gapp_Info) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_app_info_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Name;

   -------------------------
   -- Get_Supported_Types --
   -------------------------

   function Get_Supported_Types
      (Self : Gapp_Info) return GNAT.Strings.String_List
   is
      function Internal (Self : Gapp_Info) return chars_ptr_array_access;
      pragma Import (C, Internal, "g_app_info_get_supported_types");
   begin
      return To_String_List (Internal (Self).all);
   end Get_Supported_Types;

   ------------
   -- Launch --
   ------------

   function Launch
      (Self    : Gapp_Info;
       Files   : Glib.GFile.Gfile_List.Glist;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Error   : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gapp_Info;
          Files     : System.Address;
          Context   : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_launch");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Glib.GFile.Gfile_List.Get_Object (Files), Get_Object_Or_Null (GObject (Context)), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch;

   ----------------------------------
   -- Launch_Default_For_Uri_Async --
   ----------------------------------

   procedure Launch_Default_For_Uri_Async
      (URI         : UTF8_String;
       Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
      Tmp_URI : Gtkada.Types.Chars_Ptr := New_String (URI);
   begin
      if Callback = null then
         C_G_App_Info_Launch_Default_For_Uri_Async (Tmp_URI, Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
         Free (Tmp_URI);
      else
         C_G_App_Info_Launch_Default_For_Uri_Async (Tmp_URI, Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
         Free (Tmp_URI);
      end if;
   end Launch_Default_For_Uri_Async;

   package body Launch_Default_For_Uri_Async_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gasync_Ready_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gasync_Ready_Callback, System.Address);

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Type definition for a function that will be called back when an
      --  asynchronous operation within GIO has been completed.
      --  Gasync_Ready_Callback callbacks from Gtask.Gtask are guaranteed to be
      --  invoked in a later iteration of the thread-default main context (see
      --  [methodGlib.MainContext.push_thread_default]) where the Gtask.Gtask
      --  was created. All other users of Gasync_Ready_Callback must likewise
      --  call it asynchronously in a later iteration of the main context.
      --  The asynchronous operation is guaranteed to have held a reference to
      --  Source_Object from the time when the `*_async` function was called,
      --  until after this callback returns.
      --  @param Source_Object the object the asynchronous operation was
      --  started with.
      --  @param Res a Glib.G_Async_Result.
      --  @param Data user data passed to the callback.

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Source_Object : System.Address;
          Res           : Glib.G_Async_Result;
          Data          : System.Address)
      is
         D            : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_GObject : Glib.Object.GObject_Record;
      begin
         To_Gasync_Ready_Callback (D.Func) (Get_User_Data (Source_Object, Stub_GObject), Res, D.Data.all);
      end Internal_Cb;

      ----------------------------------
      -- Launch_Default_For_Uri_Async --
      ----------------------------------

      procedure Launch_Default_For_Uri_Async
         (URI         : UTF8_String;
          Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
          Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
          Callback    : Gasync_Ready_Callback;
          User_Data   : User_Data_Type)
      is
         Tmp_URI : Gtkada.Types.Chars_Ptr := New_String (URI);
         D       : System.Address;
      begin
         if Callback = null then
            C_G_App_Info_Launch_Default_For_Uri_Async (Tmp_URI, Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
            Free (Tmp_URI);
         else
            D := Users.Build (To_Address (Callback), User_Data);
            C_G_App_Info_Launch_Default_For_Uri_Async (Tmp_URI, Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Cb'Address, D);
            Free (Tmp_URI);
            Users.Free_Data (D);
         end if;
      end Launch_Default_For_Uri_Async;

   end Launch_Default_For_Uri_Async_User_Data;

   -----------------
   -- Launch_Uris --
   -----------------

   function Launch_Uris
      (Self    : Gapp_Info;
       Uris    : Gtk.Enums.String_List.Glist;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Error   : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gapp_Info;
          Uris      : System.Address;
          Context   : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_launch_uris");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Gtk.Enums.String_List.Get_Object (Uris), Get_Object_Or_Null (GObject (Context)), Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch_Uris;

   -----------------------
   -- Launch_Uris_Async --
   -----------------------

   procedure Launch_Uris_Async
      (Self        : Gapp_Info;
       Uris        : Gtk.Enums.String_List.Glist;
       Context     : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_App_Info_Launch_Uris_Async (Self, Gtk.Enums.String_List.Get_Object (Uris), Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_App_Info_Launch_Uris_Async (Self, Gtk.Enums.String_List.Get_Object (Uris), Get_Object_Or_Null (GObject (Context)), Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Launch_Uris_Async;

   ------------------------
   -- Launch_Uris_Finish --
   ------------------------

   function Launch_Uris_Finish
      (Self   : Gapp_Info;
       Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gapp_Info;
          Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_launch_uris_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch_Uris_Finish;

   --------------------------
   -- Remove_Supports_Type --
   --------------------------

   function Remove_Supports_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self         : Gapp_Info;
          Content_Type : Gtkada.Types.Chars_Ptr;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_remove_supports_type");
      Acc_Error        : aliased Glib.Error.GError;
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Content_Type, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Content_Type);
      return Tmp_Return /= 0;
   end Remove_Supports_Type;

   ----------------------------------
   -- Set_As_Default_For_Extension --
   ----------------------------------

   function Set_As_Default_For_Extension
      (Self      : Gapp_Info;
       Extension : UTF8_String;
       Error     : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : Gapp_Info;
          Extension : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_set_as_default_for_extension");
      Acc_Error     : aliased Glib.Error.GError;
      Tmp_Extension : Gtkada.Types.Chars_Ptr := New_String (Extension);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Extension, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Extension);
      return Tmp_Return /= 0;
   end Set_As_Default_For_Extension;

   -----------------------------
   -- Set_As_Default_For_Type --
   -----------------------------

   function Set_As_Default_For_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self         : Gapp_Info;
          Content_Type : Gtkada.Types.Chars_Ptr;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_set_as_default_for_type");
      Acc_Error        : aliased Glib.Error.GError;
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Content_Type, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Content_Type);
      return Tmp_Return /= 0;
   end Set_As_Default_For_Type;

   -------------------------------
   -- Set_As_Last_Used_For_Type --
   -------------------------------

   function Set_As_Last_Used_For_Type
      (Self         : Gapp_Info;
       Content_Type : UTF8_String;
       Error        : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self         : Gapp_Info;
          Content_Type : Gtkada.Types.Chars_Ptr;
          Acc_Error    : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_set_as_last_used_for_type");
      Acc_Error        : aliased Glib.Error.GError;
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Content_Type, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Content_Type);
      return Tmp_Return /= 0;
   end Set_As_Last_Used_For_Type;

   -----------------
   -- Should_Show --
   -----------------

   function Should_Show (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_should_show");
   begin
      return Internal (Self) /= 0;
   end Should_Show;

   --------------------
   -- Supports_Files --
   --------------------

   function Supports_Files (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_supports_files");
   begin
      return Internal (Self) /= 0;
   end Supports_Files;

   -------------------
   -- Supports_Uris --
   -------------------

   function Supports_Uris (Self : Gapp_Info) return Boolean is
      function Internal (Self : Gapp_Info) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_supports_uris");
   begin
      return Internal (Self) /= 0;
   end Supports_Uris;

   -----------------------------
   -- Create_From_Commandline --
   -----------------------------

   function Create_From_Commandline
      (Commandline      : UTF8_String;
       Application_Name : UTF8_String := "";
       Flags            : Create_Flags;
       Error            : out Glib.Error.GError) return Gapp_Info
   is
      function Internal
         (Commandline      : Gtkada.Types.Chars_Ptr;
          Application_Name : Gtkada.Types.Chars_Ptr;
          Flags            : Create_Flags;
          Acc_Error        : access Glib.Error.GError) return Gapp_Info;
      pragma Import (C, Internal, "g_app_info_create_from_commandline");
      Acc_Error            : aliased Glib.Error.GError;
      Tmp_Commandline      : Gtkada.Types.Chars_Ptr := New_String (Commandline);
      Tmp_Application_Name : Gtkada.Types.Chars_Ptr;
      Tmp_Return           : Gapp_Info;
   begin
      Tmp_Application_Name :=
        (if Application_Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Application_Name));
      Tmp_Return := Internal (Tmp_Commandline, Tmp_Application_Name, Flags, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Application_Name);
      Free (Tmp_Commandline);
      return
        (if Error = null
         then Tmp_Return
         else Null_Gapp_Info);
   end Create_From_Commandline;

   -------------
   -- Get_All --
   -------------

   function Get_All return App_Info_List.Glist is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_app_info_get_all");
      Tmp_Return : App_Info_List.Glist;
   begin
      Glib.App_Info.App_Info_List.Set_Object (Tmp_Return, Internal);
      return Tmp_Return;
   end Get_All;

   ----------------------
   -- Get_All_For_Type --
   ----------------------

   function Get_All_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist
   is
      function Internal
         (Content_Type : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_app_info_get_all_for_type");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : App_Info_List.Glist;
   begin
      Glib.App_Info.App_Info_List.Set_Object (Tmp_Return, Internal (Tmp_Content_Type));
      Free (Tmp_Content_Type);
      return Tmp_Return;
   end Get_All_For_Type;

   --------------------------
   -- Get_Default_For_Type --
   --------------------------

   function Get_Default_For_Type
      (Content_Type      : UTF8_String;
       Must_Support_Uris : Boolean) return Gapp_Info
   is
      function Internal
         (Content_Type      : Gtkada.Types.Chars_Ptr;
          Must_Support_Uris : Glib.Gboolean) return Gapp_Info;
      pragma Import (C, Internal, "g_app_info_get_default_for_type");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : Gapp_Info;
   begin
      Tmp_Return := Internal (Tmp_Content_Type, Boolean'Pos (Must_Support_Uris));
      Free (Tmp_Content_Type);
      return Tmp_Return;
   end Get_Default_For_Type;

   --------------------------------
   -- Get_Default_For_Uri_Scheme --
   --------------------------------

   function Get_Default_For_Uri_Scheme
      (Uri_Scheme : UTF8_String) return Gapp_Info
   is
      function Internal
         (Uri_Scheme : Gtkada.Types.Chars_Ptr) return Gapp_Info;
      pragma Import (C, Internal, "g_app_info_get_default_for_uri_scheme");
      Tmp_Uri_Scheme : Gtkada.Types.Chars_Ptr := New_String (Uri_Scheme);
      Tmp_Return     : Gapp_Info;
   begin
      Tmp_Return := Internal (Tmp_Uri_Scheme);
      Free (Tmp_Uri_Scheme);
      return Tmp_Return;
   end Get_Default_For_Uri_Scheme;

   ---------------------------
   -- Get_Fallback_For_Type --
   ---------------------------

   function Get_Fallback_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist
   is
      function Internal
         (Content_Type : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_app_info_get_fallback_for_type");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : App_Info_List.Glist;
   begin
      Glib.App_Info.App_Info_List.Set_Object (Tmp_Return, Internal (Tmp_Content_Type));
      Free (Tmp_Content_Type);
      return Tmp_Return;
   end Get_Fallback_For_Type;

   ------------------------------
   -- Get_Recommended_For_Type --
   ------------------------------

   function Get_Recommended_For_Type
      (Content_Type : UTF8_String) return App_Info_List.Glist
   is
      function Internal
         (Content_Type : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_app_info_get_recommended_for_type");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
      Tmp_Return       : App_Info_List.Glist;
   begin
      Glib.App_Info.App_Info_List.Set_Object (Tmp_Return, Internal (Tmp_Content_Type));
      Free (Tmp_Content_Type);
      return Tmp_Return;
   end Get_Recommended_For_Type;

   ----------------------------
   -- Launch_Default_For_Uri --
   ----------------------------

   function Launch_Default_For_Uri
      (URI     : UTF8_String;
       Context : access Glib.App_Launch_Context.Gapp_Launch_Context_Record'Class;
       Error   : out Glib.Error.GError) return Boolean
   is
      function Internal
         (URI       : Gtkada.Types.Chars_Ptr;
          Context   : System.Address;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_launch_default_for_uri");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_URI, Get_Object_Or_Null (GObject (Context)), Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_URI);
      return Tmp_Return /= 0;
   end Launch_Default_For_Uri;

   -----------------------------------
   -- Launch_Default_For_Uri_Finish --
   -----------------------------------

   function Launch_Default_For_Uri_Finish
      (Result : Glib.G_Async_Result;
       Error  : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Result    : Glib.G_Async_Result;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "g_app_info_launch_default_for_uri_finish");
      Acc_Error  : aliased Glib.Error.GError;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Result, Acc_Error'Access);
      Error := Acc_Error;
      return Tmp_Return /= 0;
   end Launch_Default_For_Uri_Finish;

   -----------------------------
   -- Reset_Type_Associations --
   -----------------------------

   procedure Reset_Type_Associations (Content_Type : UTF8_String) is
      procedure Internal (Content_Type : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_app_info_reset_type_associations");
      Tmp_Content_Type : Gtkada.Types.Chars_Ptr := New_String (Content_Type);
   begin
      Internal (Tmp_Content_Type);
      Free (Tmp_Content_Type);
   end Reset_Type_Associations;

   function "+" (W : Gapp_Info) return Gapp_Info is
   begin
      return W;
   end "+";

end Glib.App_Info;
