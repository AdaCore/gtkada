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

--  Extends the Glib.G_Icon.G_Icon interface and adds the ability to load
--  icons from streams.

pragma Warnings (Off, "*is already use-visible*");
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Object;      use Glib.Object;
with Glib.Types;       use Glib.Types;
with Gtkada.Bindings;  use Gtkada.Bindings;
with Gtkada.Types;     use Gtkada.Types;

package Glib.Loadable_Icon is

   type Gloadable_Icon is new Glib.Types.GType_Interface;
   Null_Gloadable_Icon : constant Gloadable_Icon;

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
   pragma Import (C, Get_Type, "g_loadable_icon_get_type");

   -------------
   -- Methods --
   -------------

   procedure Load_Async
      (Self        : Gloadable_Icon;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Loads an icon asynchronously. To finish this function, see
   --  g_loadable_icon_load_finish. For the synchronous, blocking version of
   --  this function, see g_loadable_icon_load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gloadable_Icon"

   function "+" (W : Gloadable_Icon) return Gloadable_Icon;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Load is access function
     (Self        : Gloadable_Icon;
      Size        : Glib.Gint;
      The_Type    : access Gtkada.Types.Chars_Ptr;
      Cancellable : System.Address) return System.Address;
   pragma Convention (C, Virtual_Load);
   --  Loads a loadable icon. For the asynchronous version of this function,
   --  see Glib.Loadable_Icon.Load_Async.
   --  @param Size an integer.
   --  @param The_Type a location to store the type of the loaded icon, null
   --  to ignore.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @return a Ginput.Stream.Ginput_Stream to read the icon from.

   type Virtual_Load_Async is access procedure
     (Self        : Gloadable_Icon;
      Size        : Glib.Gint;
      Cancellable : System.Address;
      Callback    : System.Address;
      User_Data   : System.Address);
   pragma Convention (C, Virtual_Load_Async);
   --  Loads an icon asynchronously. To finish this function, see
   --  g_loadable_icon_load_finish. For the synchronous, blocking version of
   --  this function, see g_loadable_icon_load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied
   --  @param User_Data the data to pass to callback function

   type Virtual_Load_Finish is access function
     (Self     : Gloadable_Icon;
      Res      : Glib.G_Async_Result;
      The_Type : access Gtkada.Types.Chars_Ptr) return System.Address;
   pragma Convention (C, Virtual_Load_Finish);
   --  Finishes an asynchronous icon load started in
   --  Glib.Loadable_Icon.Load_Async.
   --  @param Res a Glib.G_Async_Result.
   --  @param The_Type a location to store the type of the loaded icon, null
   --  to ignore.
   --  @return a Ginput.Stream.Ginput_Stream to read the icon from.

   subtype Loadable_Icon_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Load
     (Self    : Loadable_Icon_Interface_Descr;
      Handler : Virtual_Load);
   pragma Import (C, Set_Load, "gtkada_Loadable_Icon_set_load");

   procedure Set_Load_Async
     (Self    : Loadable_Icon_Interface_Descr;
      Handler : Virtual_Load_Async);
   pragma Import (C, Set_Load_Async, "gtkada_Loadable_Icon_set_load_async");

   procedure Set_Load_Finish
     (Self    : Loadable_Icon_Interface_Descr;
      Handler : Virtual_Load_Finish);
   pragma Import (C, Set_Load_Finish, "gtkada_Loadable_Icon_set_load_finish");
   --  See Glib.Object.Add_Interface

private

Null_Gloadable_Icon : constant Gloadable_Icon :=
   Gloadable_Icon (Glib.Types.Null_Interface);
end Glib.Loadable_Icon;
