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

--  Assists in creating [classGsk.RenderNode]s for widgets.
--
--  It functions in a similar way to a cairo context, and maintains a stack of
--  render nodes and their associated transformations.
--
--  The node at the top of the stack is the one that `gtk_snapshot_append_…`
--  functions operate on. Use the `gtk_snapshot_push_…` functions and
--  [methodSnapshot.pop] to change the current node.
--
--  The typical way to obtain a `GtkSnapshot` object is as an argument to the
--  [vfuncGtk.Widget.snapshot] vfunc. If you need to create your own
--  `GtkSnapshot`, use [ctorGtk.Snapshot.new].
--
--  Note that `GtkSnapshot` applies some optimizations, so the node it
--  produces may not match the API calls 1:1. For example, it will omit clip
--  nodes if the child node is entirely contained within the clip rectangle.

pragma Warnings (Off, "*is already use-visible*");
with Cairo;         use Cairo;
with Gdk.Paintable; use Gdk.Paintable;
with Gdk.RGBA;      use Gdk.RGBA;
with Gdk.Snapshot;  use Gdk.Snapshot;
with Gdk.Texture;   use Gdk.Texture;
with Glib;          use Glib;
with Gtkada.Types;  use Gtkada.Types;
with Interfaces.C;  use Interfaces.C;
with Pango.Layout;  use Pango.Layout;

package Gtk.Snapshot is

   type Gtk_Snapshot_Record is new Gdk_Snapshot_Record with null record;
   type Gtk_Snapshot is access all Gtk_Snapshot_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Snapshot);
   procedure Initialize (Self : not null access Gtk_Snapshot_Record'Class);
   --  Creates a new `GtkSnapshot`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Snapshot_New return Gtk_Snapshot;
   --  Creates a new `GtkSnapshot`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_snapshot_get_type");

   -------------
   -- Methods --
   -------------

   function Append_Cairo
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t) return Cairo.Cairo_Context;
   --  Creates a new [classGsk.CairoNode] and appends it to the current render
   --  node of Snapshot, without changing the current node.
   --  @param Bounds the bounds for the new node
   --  @return a `cairo_t` suitable for drawing the contents of the newly
   --  created render node

   procedure Append_Color
      (Self   : not null access Gtk_Snapshot_Record;
       Color  : Gdk.RGBA.Gdk_RGBA;
       Bounds : in out graphene_rect_t);
   --  Creates a new render node drawing the Color into the given Bounds and
   --  appends it to the current render node of Snapshot.
   --  You should try to avoid calling this function if Color is transparent.
   --  @param Color the color to draw
   --  @param Bounds the bounds for the new node

   procedure Append_Layout
      (Self   : not null access Gtk_Snapshot_Record;
       Layout : not null access Pango.Layout.Pango_Layout_Record'Class;
       Color  : Gdk.RGBA.Gdk_RGBA);
   --  Creates render nodes for rendering Layout in the given foregound Color
   --  and appends them to the current node of Snapshot without changing the
   --  current node. The current theme's foreground color for a widget can be
   --  obtained with [methodGtk.Widget.get_color].
   --  Note that if the layout does not produce any visible output, then nodes
   --  may not be added to the Snapshot.
   --  @param Layout the `PangoLayout` to render
   --  @param Color the foreground color to render the layout in

   procedure Append_Paste
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t;
       Nth    : Gsize);
   --  Creates a new render node that pastes the contents copied by a previous
   --  call to [methodGtk.Snapshot.push_copy]
   --  Since: gtk+ 4.22
   --  @param Bounds the bounds for the new node
   --  @param Nth the index of the copy, with 0 being the latest copy, 1 being
   --  the copy before that, and so on.

   procedure Append_Texture
      (Self    : not null access Gtk_Snapshot_Record;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class;
       Bounds  : in out graphene_rect_t);
   --  Creates a new render node drawing the Texture into the given Bounds and
   --  appends it to the current render node of Snapshot.
   --  If the texture needs to be scaled to fill Bounds, linear filtering is
   --  used. See [methodGtk.Snapshot.append_scaled_texture] if you need other
   --  filtering, such as nearest-neighbour.
   --  @param Texture the texture to render
   --  @param Bounds the bounds for the new node

   procedure Gl_Shader_Pop_Texture
      (Self : not null access Gtk_Snapshot_Record);
   pragma Obsolescent (Gl_Shader_Pop_Texture);
   --  Removes the top element from the stack of render nodes and adds it to
   --  the nearest [classGsk.GLShaderNode] below it.
   --  This must be called the same number of times as the number of textures
   --  is needed for the shader in [methodGtk.Snapshot.push_gl_shader].
   --  Deprecated since 4.16, 1

   procedure Perspective
      (Self  : not null access Gtk_Snapshot_Record;
       Depth : Interfaces.C.C_float);
   --  Applies a perspective projection transform.
   --  See [methodGsk.Transform.perspective] for a discussion on the details.
   --  @param Depth distance of the z=0 plane

   procedure Pop (Self : not null access Gtk_Snapshot_Record);
   --  Removes the top element from the stack of render nodes, and appends it
   --  to the node underneath it.

   procedure Push_Blur
      (Self   : not null access Gtk_Snapshot_Record;
       Radius : Gdouble);
   --  Blurs an image.
   --  The image is recorded until the next call to [methodGtk.Snapshot.pop].
   --  @param Radius the blur radius to use. Must be positive

   procedure Push_Clip
      (Self   : not null access Gtk_Snapshot_Record;
       Bounds : in out graphene_rect_t);
   --  Clips an image to a rectangle.
   --  The image is recorded until the next call to [methodGtk.Snapshot.pop].
   --  @param Bounds the rectangle to clip to

   procedure Push_Copy (Self : not null access Gtk_Snapshot_Record);
   --  Stores the current rendering state for later pasting via
   --  [methodGtk.Snapshot.append_paste].
   --  Pasting is possible until the matching call to
   --  [methodGtk.Snapshot.pop].
   --  Since: gtk+ 4.22

   procedure Push_Cross_Fade
      (Self     : not null access Gtk_Snapshot_Record;
       Progress : Gdouble);
   --  Snapshots a cross-fade operation between two images with the given
   --  Progress.
   --  Until the first call to [methodGtk.Snapshot.pop], the start image will
   --  be snapshot. After that call, the end image will be recorded until the
   --  second call to [methodGtk.Snapshot.pop].
   --  Calling this function requires two subsequent calls to
   --  [methodGtk.Snapshot.pop].
   --  @param Progress progress between 0.0 and 1.0

   procedure Push_Opacity
      (Self    : not null access Gtk_Snapshot_Record;
       Opacity : Gdouble);
   --  Modifies the opacity of an image.
   --  The image is recorded until the next call to [methodGtk.Snapshot.pop].
   --  @param Opacity the opacity to use

   procedure Push_Repeat
      (Self         : not null access Gtk_Snapshot_Record;
       Bounds       : in out graphene_rect_t;
       Child_Bounds : in out graphene_rect_t);
   --  Creates a node that repeats the child node.
   --  The child is recorded until the next call to [methodGtk.Snapshot.pop].
   --  @param Bounds the bounds within which to repeat
   --  @param Child_Bounds the bounds of the child or null to use the full
   --  size of the collected child node

   procedure Restore (Self : not null access Gtk_Snapshot_Record);
   --  Restores Snapshot to the state saved by a preceding call to
   --  [methodSnapshot.save] and removes that state from the stack of saved
   --  states.

   procedure Rotate
      (Self  : not null access Gtk_Snapshot_Record;
       Angle : Interfaces.C.C_float);
   --  Rotates @Snapshot's coordinate system by Angle degrees in 2D space - or
   --  in 3D speak, rotates around the Z axis. The rotation happens around the
   --  origin point of (0, 0) in the Snapshot's current coordinate system.
   --  To rotate around axes other than the Z axis, use
   --  [methodGsk.Transform.rotate_3d].
   --  @param Angle the rotation angle, in degrees (clockwise)

   procedure Save (Self : not null access Gtk_Snapshot_Record);
   --  Makes a copy of the current state of Snapshot and saves it on an
   --  internal stack.
   --  When [methodGtk.Snapshot.restore] is called, Snapshot will be restored
   --  to the saved state.
   --  Multiple calls to [methodGtk.Snapshot.save] and
   --  [methodGtk.Snapshot.restore] can be nested; each call to
   --  `gtk_snapshot_restore` restores the state from the matching paired
   --  `gtk_snapshot_save`.
   --  It is necessary to clear all saved states with corresponding calls to
   --  `gtk_snapshot_restore`.

   procedure Scale
      (Self     : not null access Gtk_Snapshot_Record;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float);
   --  Scales Snapshot's coordinate system in 2-dimensional space by the given
   --  factors.
   --  Use [methodGtk.Snapshot.scale_3d] to scale in all 3 dimensions.
   --  @param Factor_X scaling factor on the X axis
   --  @param Factor_Y scaling factor on the Y axis

   procedure Scale_3D
      (Self     : not null access Gtk_Snapshot_Record;
       Factor_X : Interfaces.C.C_float;
       Factor_Y : Interfaces.C.C_float;
       Factor_Z : Interfaces.C.C_float);
   --  Scales Snapshot's coordinate system by the given factors.
   --  @param Factor_X scaling factor on the X axis
   --  @param Factor_Y scaling factor on the Y axis
   --  @param Factor_Z scaling factor on the Z axis

   function To_Paintable
      (Self : not null access Gtk_Snapshot_Record;
       Size : in out graphene_size_t) return Gdk.Paintable.Gdk_Paintable;
   --  Returns a paintable encapsulating the render node that was constructed
   --  by Snapshot.
   --  After calling this function, it is no longer possible to add more nodes
   --  to Snapshot. The only function that should be called after this is
   --  [methodGobject.Object.unref].
   --  @param Size The size of the resulting paintable or null to use the
   --  bounds of the snapshot
   --  @return a new `GdkPaintable`

   procedure Translate
      (Self  : not null access Gtk_Snapshot_Record;
       Point : in out graphene_point_t);
   --  Translates Snapshot's coordinate system by Point in 2-dimensional
   --  space.
   --  @param Point the point to translate the snapshot by

end Gtk.Snapshot;
