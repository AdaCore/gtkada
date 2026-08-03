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

--  A `GdkTexture` representing a DMA buffer.
--
--  To create a `GdkDmabufTexture`, use the auxiliary
--  [classGdk.DmabufTextureBuilder] object.
--
--  Dma-buf textures can only be created on Linux.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable;      use Gdk.Paintable;
with Gdk.Snapshot;       use Gdk.Snapshot;
with Gdk.Texture;        use Gdk.Texture;
with Glib;               use Glib;
with Glib.Cancellable;   use Glib.Cancellable;
with Glib.Input_Stream;  use Glib.Input_Stream;
with Glib.Loadable_Icon; use Glib.Loadable_Icon;
with Glib.Object;        use Glib.Object;
with Glib.Types;         use Glib.Types;

package Gdk.Dmabuf_Texture is

   type Gdk_Dmabuf_Texture_Record is new Gdk_Texture_Record with null record;
   type Gdk_Dmabuf_Texture is access all Gdk_Dmabuf_Texture_Record'Class;

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
   pragma Import (C, Get_Type, "gdk_dmabuf_texture_get_type");

   -------------
   -- Methods --
   -------------

   procedure Load_Async
      (Self        : not null access Gdk_Dmabuf_Texture_Record;
       Size        : Glib.Gint;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class;
       Callback    : Gasync_Ready_Callback);
   --  Loads an icon asynchronously. To finish this function, see
   --  Glib.Loadable_Icon.Load_Finish. For the synchronous, blocking version of
   --  this function, see Glib.Loadable_Icon.Load.
   --  @param Size an integer.
   --  @param Cancellable optional Glib.Cancellable.Gcancellable object, null
   --  to ignore.
   --  @param Callback a Gasync_Ready_Callback to call when the request is
   --  satisfied

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gdk_Dmabuf_Texture_Record;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble);

   function Get_Current_Image
      (Self : not null access Gdk_Dmabuf_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable;

   function Get_Flags
      (Self : not null access Gdk_Dmabuf_Texture_Record)
       return Gdk.Paintable.Gdk_Paintable_Flags;

   function Get_Intrinsic_Aspect_Ratio
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Gdouble;

   function Get_Intrinsic_Height
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Glib.Gint;

   function Get_Intrinsic_Width
      (Self : not null access Gdk_Dmabuf_Texture_Record) return Glib.Gint;

   procedure Invalidate_Contents
      (Self : not null access Gdk_Dmabuf_Texture_Record);

   procedure Invalidate_Size
      (Self : not null access Gdk_Dmabuf_Texture_Record);

   procedure Snapshot
      (Self     : not null access Gdk_Dmabuf_Texture_Record;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble);

   function Load
      (Self        : not null access Gdk_Dmabuf_Texture_Record;
       Size        : Glib.Gint;
       The_Type    : access UTF8_String := null;
       Cancellable : access Glib.Cancellable.Gcancellable_Record'Class)
       return Glib.Input_Stream.Ginput_Stream;

   function Load_Finish
      (Self     : not null access Gdk_Dmabuf_Texture_Record;
       Res      : Glib.G_Async_Result;
       The_Type : access UTF8_String := null)
       return Glib.Input_Stream.Ginput_Stream;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdk.Paintable"
   --
   --  - "Gio.LoadableIcon"

   package Implements_Gdk_Paintable is new Glib.Types.Implements
     (Gdk.Paintable.Gdk_Paintable, Gdk_Dmabuf_Texture_Record, Gdk_Dmabuf_Texture);
   function "+"
     (Widget : access Gdk_Dmabuf_Texture_Record'Class)
   return Gdk.Paintable.Gdk_Paintable
   renames Implements_Gdk_Paintable.To_Interface;
   function "-"
     (Interf : Gdk.Paintable.Gdk_Paintable)
   return Gdk_Dmabuf_Texture
   renames Implements_Gdk_Paintable.To_Object;

   package Implements_Gloadable_Icon is new Glib.Types.Implements
     (Glib.Loadable_Icon.Gloadable_Icon, Gdk_Dmabuf_Texture_Record, Gdk_Dmabuf_Texture);
   function "+"
     (Widget : access Gdk_Dmabuf_Texture_Record'Class)
   return Glib.Loadable_Icon.Gloadable_Icon
   renames Implements_Gloadable_Icon.To_Interface;
   function "-"
     (Interf : Glib.Loadable_Icon.Gloadable_Icon)
   return Gdk_Dmabuf_Texture
   renames Implements_Gloadable_Icon.To_Object;

end Gdk.Dmabuf_Texture;
