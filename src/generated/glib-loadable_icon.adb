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

package body Glib.Loadable_Icon is

   procedure C_G_Loadable_Icon_Load_Async
      (Self        : Gloadable_Icon;
       Size        : Glib.Gint;
       Cancellable : System.Address;
       Callback    : System.Address;
       User_Data   : System.Address);
   pragma Import (C, C_G_Loadable_Icon_Load_Async, "g_loadable_icon_load_async");
   --  Loads an icon asynchronously. To finish this function, see
   --  Glib.Loadable_Icon.Load_Finish. For the synchronous, blocking version of
   --  this function, see Glib.Loadable_Icon.Load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

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

   ----------
   -- Load --
   ----------

   function Load
      (Self        : Gloadable_Icon;
       Size        : Glib.Gint;
       The_Type    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal
         (Self        : Gloadable_Icon;
          Size        : Glib.Gint;
          The_Type    : access Gtkada.Types.Chars_Ptr;
          Cancellable : System.Address) return System.Address;
      pragma Import (C, Internal, "g_loadable_icon_load");
      Tmp_The_Type       : aliased Gtkada.Types.Chars_Ptr;
      Acc_The_Type       : constant access Gtkada.Types.Chars_Ptr := (if The_Type /= null then Tmp_The_Type'Access else null);
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Self, Size, Acc_The_Type, Get_Object_Or_Null (GObject (Cancellable)));
      if The_Type /= null then
         The_Type.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_The_Type);
      end if;
      return Glib.Input_Stream.Ginput_Stream (Get_User_Data (Tmp_Return, Stub_Ginput_Stream));
   end Load;

   ----------------
   -- Load_Async --
   ----------------

   procedure Load_Async
      (Self        : Gloadable_Icon;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback)
   is
   begin
      if Callback = null then
         C_G_Loadable_Icon_Load_Async (Self, Size, Get_Object_Or_Null (GObject (Cancellable)), System.Null_Address, System.Null_Address);
      else
         C_G_Loadable_Icon_Load_Async (Self, Size, Get_Object_Or_Null (GObject (Cancellable)), Internal_Gasync_Ready_Callback'Address, To_Address (Callback));
      end if;
   end Load_Async;

   -----------------
   -- Load_Finish --
   -----------------

   function Load_Finish
      (Self     : Gloadable_Icon;
       Res      : Glib.G_Async_Result;
       The_Type : access UTF8_String := null)
       return Glib.Input_Stream.Ginput_Stream
   is
      function Internal
         (Self     : Gloadable_Icon;
          Res      : Glib.G_Async_Result;
          The_Type : access Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_loadable_icon_load_finish");
      Tmp_The_Type       : aliased Gtkada.Types.Chars_Ptr;
      Acc_The_Type       : constant access Gtkada.Types.Chars_Ptr := (if The_Type /= null then Tmp_The_Type'Access else null);
      Stub_Ginput_Stream : Glib.Input_Stream.Ginput_Stream_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Self, Res, Acc_The_Type);
      if The_Type /= null then
         The_Type.all := Gtkada.Bindings.Value_Allowing_Null (Tmp_The_Type);
      end if;
      return Glib.Input_Stream.Ginput_Stream (Get_User_Data (Tmp_Return, Stub_Ginput_Stream));
   end Load_Finish;

   function "+" (W : Gloadable_Icon) return Gloadable_Icon is
   begin
      return W;
   end "+";

end Glib.Loadable_Icon;
