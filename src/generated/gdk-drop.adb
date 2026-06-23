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
with Gdk.Device;
with Gdk.Display;
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with System;

package body Gdk.Drop is

   procedure C_Gdk_Drop_Read_Value_Async
      (Self        : System.Address;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_Gdk_Drop_Read_Value_Async, "gdk_drop_read_value_async");
   --  Asynchronously request the drag operation's contents converted to the
   --  given Type.
   --  For local drag-and-drop operations that are available in the given
   --  `GType`, the value will be copied directly. Otherwise, GDK will try to
   --  use [funcGdk.content_deserialize_async] to convert the data.
   --  @param The_Type a `GType` to read
   --  @param Io_Priority the I/O priority of the request.
   --  @param Cancellable optional `GCancellable` object, null to ignore.
   --  @param Callback callback to call when the request is satisfied
   --  @param User_Data the data to pass to callback function

   function To_Gasync_Ready_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gasync_Ready_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gasync_Ready_Callback, System.Address);

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address);
   pragma Convention (C, Internal_Gasync_Ready_Callback);
   --  @param Source_Object the object the asynchronous operation was started
   --  with.
   --  @param Res a Glib.G_Async_Result.
   --  @param User_Data user data passed to the callback.

   ------------------------------------
   -- Internal_Gasync_Ready_Callback --
   ------------------------------------

   procedure Internal_Gasync_Ready_Callback
      (Source_Object : System.Address;
       Res           : Glib.G_Async_Result;
       User_Data     : System.Address)
   is
      Func         : constant Gasync_Ready_Callback := To_Gasync_Ready_Callback (User_Data);
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      Func (Get_User_Data (Source_Object, Stub_GObject), Res);
   end Internal_Gasync_Ready_Callback;

   package Type_Conversion_Gdk_Drop is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Drop_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Drop);

   ------------
   -- Finish --
   ------------

   procedure Finish
      (Self   : not null access Gdk_Drop_Record;
       Action : Gdk.Drag.Drag_Action)
   is
      procedure Internal
         (Self   : System.Address;
          Action : Gdk.Drag.Drag_Action);
      pragma Import (C, Internal, "gdk_drop_finish");
   begin
      Internal (Get_Object (Self), Action);
   end Finish;

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
      (Self : not null access Gdk_Drop_Record) return Gdk.Drag.Drag_Action
   is
      function Internal (Self : System.Address) return Gdk.Drag.Drag_Action;
      pragma Import (C, Internal, "gdk_drop_get_actions");
   begin
      return Internal (Get_Object (Self));
   end Get_Actions;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drop_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drop_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------
   -- Get_Drag --
   --------------

   function Get_Drag
      (Self : not null access Gdk_Drop_Record) return Gdk.Drag.Gdk_Drag
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drop_get_drag");
      Stub_Gdk_Drag : Gdk.Drag.Gdk_Drag_Record;
   begin
      return Gdk.Drag.Gdk_Drag (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Drag));
   end Get_Drag;

   -----------------
   -- Get_Formats --
   -----------------

   function Get_Formats
      (Self : not null access Gdk_Drop_Record)
       return Gdk.Content_Formats.Gdk_Content_Formats
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drop_get_formats");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Formats;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gdk_Drop_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_drop_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ----------------------
   -- Read_Value_Async --
   ----------------------

   procedure Read_Value_Async
      (Self        : not null access Gdk_Drop_Record;
       The_Type    : GType;
       Io_Priority : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_Gdk_Drop_Read_Value_Async (Get_Object (Self), The_Type, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_Gdk_Drop_Read_Value_Async (Get_Object (Self), The_Type, Io_Priority, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Read_Value_Async;

   -----------------------
   -- Read_Value_Finish --
   -----------------------

   function Read_Value_Finish
      (Self   : not null access Gdk_Drop_Record;
       Result : Glib.G_Async_Result) return Glib.Values.GValue
   is
      function Internal
         (Self   : System.Address;
          Result : Glib.G_Async_Result) return Glib.Values.GValue;
      pragma Import (C, Internal, "gdk_drop_read_value_finish");
   begin
      return Internal (Get_Object (Self), Result);
   end Read_Value_Finish;

   ------------
   -- Status --
   ------------

   procedure Status
      (Self      : not null access Gdk_Drop_Record;
       Actions   : Gdk.Drag.Drag_Action;
       Preferred : Gdk.Drag.Drag_Action)
   is
      procedure Internal
         (Self      : System.Address;
          Actions   : Gdk.Drag.Drag_Action;
          Preferred : Gdk.Drag.Drag_Action);
      pragma Import (C, Internal, "gdk_drop_status");
   begin
      Internal (Get_Object (Self), Actions, Preferred);
   end Status;

end Gdk.Drop;
