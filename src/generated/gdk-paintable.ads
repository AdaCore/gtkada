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

--  An interface for content that can be painted.
--
--  The content of a `GdkPaintable` can be painted anywhere at any size
--  without requiring any sort of layout. The interface is inspired by similar
--  concepts elsewhere, such as
--  [ClutterContent](https://developer.gnome.org/clutter/stable/ClutterContent.html),
--  [HTML/CSS Paint Sources](https://www.w3.org/TR/css-images-4/paint-source),
--  or [SVG Paint Servers](https://www.w3.org/TR/SVG2/pservers.html).
--
--  A `GdkPaintable` can be snapshot at any time and size using
--  [methodGdk.Paintable.snapshot]. How the paintable interprets that size and
--  if it scales or centers itself into the given rectangle is implementation
--  defined, though if you are implementing a `GdkPaintable` and don't know
--  what to do, it is suggested that you scale your paintable ignoring any
--  potential aspect ratio.
--
--  The contents that a `GdkPaintable` produces may depend on the
--  [classGdk.Snapshot] passed to it. For example, paintables may decide to use
--  more detailed images on higher resolution screens or when OpenGL is
--  available. A `GdkPaintable` will however always produce the same output for
--  the same snapshot.
--
--  A `GdkPaintable` may change its contents, meaning that it will now produce
--  a different output with the same snapshot. Once that happens, it will call
--  [methodGdk.Paintable.invalidate_contents] which will emit the
--  [signalGdk.Paintable::invalidate-contents] signal. If a paintable is known
--  to never change its contents, it will set the
--  Gdk.Paintable.Gdk_Paintable_Static_Contents flag. If a consumer cannot deal
--  with changing contents, it may call [methodGdk.Paintable.get_current_image]
--  which will return a static paintable and use that.
--
--  A paintable can report an intrinsic (or preferred) size or aspect ratio it
--  wishes to be rendered at, though it doesn't have to. Consumers of the
--  interface can use this information to layout thepaintable appropriately.
--  Just like the contents, the size of a paintable can change. A paintable
--  will indicate this by calling [methodGdk.Paintable.invalidate_size] which
--  will emit the [signalGdk.Paintable::invalidate-size] signal. And just like
--  for contents, if a paintable is known to never change its size, it will set
--  the Gdk.Paintable.Gdk_Paintable_Static_Size flag.
--
--  Besides API for applications, there are some functions that are only
--  useful for implementing subclasses and should not be used by applications:
--  [methodGdk.Paintable.invalidate_contents],
--  [methodGdk.Paintable.invalidate_size], [funcGdk.Paintable.new_empty].

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Snapshot;            use Gdk.Snapshot;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Types;              use Glib.Types;
with System;

package Gdk.Paintable is

   type Gdk_Paintable is new Glib.Types.GType_Interface;
   Null_Gdk_Paintable : constant Gdk_Paintable;

   type Gdk_Paintable_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Paintable_Flags);
   --  Flags about a paintable object.
   --
   --  Implementations use these for optimizations such as caching.

   Gdk_Paintable_Static_Size : constant Gdk_Paintable_Flags := 1;
   Gdk_Paintable_Static_Contents : constant Gdk_Paintable_Flags := 2;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Paintable_Flags_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Paintable_Flags);
   type Property_Gdk_Paintable_Flags is new Gdk_Paintable_Flags_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_paintable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Compute_Concrete_Size
      (Self             : Gdk_Paintable;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble);
   pragma Import (C, Compute_Concrete_Size, "gdk_paintable_compute_concrete_size");
   --  Compute a concrete size for the `GdkPaintable`.
   --  Applies the sizing algorithm outlined in the [CSS Image
   --  spec](https://drafts.csswg.org/css-images-3/default-sizing) to the given
   --  Paintable. See that link for more details.
   --  It is not necessary to call this function when both Specified_Width and
   --  Specified_Height are known, but it is useful to call this function in
   --  GtkWidget:measure implementations to compute the other dimension when
   --  only one dimension is given.
   --  @param Specified_Width the width Paintable could be drawn into or 0.0
   --  if unknown
   --  @param Specified_Height the height Paintable could be drawn into or 0.0
   --  if unknown
   --  @param Default_Width the width Paintable would be drawn into if no
   --  other constraints were given
   --  @param Default_Height the height Paintable would be drawn into if no
   --  other constraints were given
   --  @param Concrete_Width will be set to the concrete width computed
   --  @param Concrete_Height will be set to the concrete height computed

   function Get_Current_Image (Self : Gdk_Paintable) return Gdk_Paintable;
   pragma Import (C, Get_Current_Image, "gdk_paintable_get_current_image");
   --  Gets an immutable paintable for the current contents displayed by
   --  Paintable.
   --  This is useful when you want to retain the current state of an
   --  animation, for example to take a screenshot of a running animation.
   --  If the Paintable is already immutable, it will return itself.
   --  @return An immutable paintable for the current contents of Paintable

   function Get_Flags (Self : Gdk_Paintable) return Gdk_Paintable_Flags;
   pragma Import (C, Get_Flags, "gdk_paintable_get_flags");
   --  Get flags for the paintable.
   --  This is oftentimes useful for optimizations.
   --  See [flagsGdk.PaintableFlags] for the flags and what they mean.
   --  @return The `GdkPaintableFlags` for this paintable

   function Get_Intrinsic_Aspect_Ratio (Self : Gdk_Paintable) return Gdouble;
   pragma Import (C, Get_Intrinsic_Aspect_Ratio, "gdk_paintable_get_intrinsic_aspect_ratio");
   --  Gets the preferred aspect ratio the Paintable would like to be
   --  displayed at.
   --  The aspect ratio is the width divided by the height, so a value of 0.5
   --  means that the Paintable prefers to be displayed twice as high as it is
   --  wide. Consumers of this interface can use this to preserve aspect ratio
   --  when displaying the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  Usually when a Paintable returns nonzero values from
   --  [methodGdk.Paintable.get_intrinsic_width] and
   --  [methodGdk.Paintable.get_intrinsic_height] the aspect ratio should
   --  conform to those values, though that is not required.
   --  If the Paintable does not have a preferred aspect ratio, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic aspect ratio of Paintable or 0 if none.

   function Get_Intrinsic_Height (Self : Gdk_Paintable) return Glib.Gint;
   pragma Import (C, Get_Intrinsic_Height, "gdk_paintable_get_intrinsic_height");
   --  Gets the preferred height the Paintable would like to be displayed at.
   --  Consumers of this interface can use this to reserve enough space to
   --  draw the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  If the Paintable does not have a preferred height, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic height of Paintable or 0 if none.

   function Get_Intrinsic_Width (Self : Gdk_Paintable) return Glib.Gint;
   pragma Import (C, Get_Intrinsic_Width, "gdk_paintable_get_intrinsic_width");
   --  Gets the preferred width the Paintable would like to be displayed at.
   --  Consumers of this interface can use this to reserve enough space to
   --  draw the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  If the Paintable does not have a preferred width, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic width of Paintable or 0 if none.

   procedure Invalidate_Contents (Self : Gdk_Paintable);
   pragma Import (C, Invalidate_Contents, "gdk_paintable_invalidate_contents");
   --  Called by implementations of `GdkPaintable` to invalidate their
   --  contents.
   --  Unless the contents are invalidated, implementations must guarantee
   --  that multiple calls of [methodGdk.Paintable.snapshot] produce the same
   --  output.
   --  This function will emit the [signalGdk.Paintable::invalidate-contents]
   --  signal.
   --  If a Paintable reports the Gdk.Paintable.Gdk_Paintable_Static_Contents
   --  flag, it must not call this function.

   procedure Invalidate_Size (Self : Gdk_Paintable);
   pragma Import (C, Invalidate_Size, "gdk_paintable_invalidate_size");
   --  Called by implementations of `GdkPaintable` to invalidate their size.
   --  As long as the size is not invalidated, Paintable must return the same
   --  values for its intrinsic width, height and aspect ratio.
   --  This function will emit the [signalGdk.Paintable::invalidate-size]
   --  signal.
   --  If a Paintable reports the Gdk.Paintable.Gdk_Paintable_Static_Size
   --  flag, it must not call this function.

   procedure Snapshot
      (Self     : Gdk_Paintable;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble);
   --  Snapshots the given paintable with the given Width and Height.
   --  The paintable is drawn at the current (0,0) offset of the Snapshot. If
   --  Width and Height are not larger than zero, this function will do
   --  nothing.
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in

   ---------------
   -- Functions --
   ---------------

   function New_Empty
      (Intrinsic_Width  : Glib.Gint;
       Intrinsic_Height : Glib.Gint) return Gdk_Paintable;
   pragma Import (C, New_Empty, "gdk_paintable_new_empty");
   --  Returns a paintable that has the given intrinsic size and draws
   --  nothing.
   --  This is often useful for implementing the
   --  [vfuncGdk.Paintable.get_current_image] virtual function when the
   --  paintable is in an incomplete state (like a
   --  [GtkMediaStream](../gtk4/class.MediaStream.html) before receiving the
   --  first frame).
   --  @param Intrinsic_Width The intrinsic width to report. Can be 0 for no
   --  width.
   --  @param Intrinsic_Height The intrinsic height to report. Can be 0 for no
   --  height.
   --  @return a `GdkPaintable`

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Paintable_Void is not null access procedure (Self : Gdk_Paintable);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Invalidate_Contents : constant Glib.Signal_Name := "invalidate-contents";
   procedure On_Invalidate_Contents
      (Self  : Gdk_Paintable;
       Call  : Cb_Gdk_Paintable_Void;
       After : Boolean := False);
   procedure On_Invalidate_Contents
      (Self  : Gdk_Paintable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the contents of the Paintable change.
   --
   --  Examples for such an event would be videos changing to the next frame
   --  or the icon theme for an icon changing.

   Signal_Invalidate_Size : constant Glib.Signal_Name := "invalidate-size";
   procedure On_Invalidate_Size
      (Self  : Gdk_Paintable;
       Call  : Cb_Gdk_Paintable_Void;
       After : Boolean := False);
   procedure On_Invalidate_Size
      (Self  : Gdk_Paintable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the intrinsic size of the Paintable changes.
   --
   --  This means the values reported by at least one of
   --  [methodGdk.Paintable.get_intrinsic_width],
   --  [methodGdk.Paintable.get_intrinsic_height] or
   --  [methodGdk.Paintable.get_intrinsic_aspect_ratio] has changed.
   --
   --  Examples for such an event would be a paintable displaying the contents
   --  of a toplevel surface being resized.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdk_Paintable"

   function "+" (W : Gdk_Paintable) return Gdk_Paintable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Current_Image is access function (Self : Gdk_Paintable) return Gdk_Paintable;
   pragma Convention (C, Virtual_Get_Current_Image);
   --  Gets an immutable paintable for the current contents displayed by
   --  Paintable.
   --  This is useful when you want to retain the current state of an
   --  animation, for example to take a screenshot of a running animation.
   --  If the Paintable is already immutable, it will return itself.
   --  @return An immutable paintable for the current contents of Paintable

   type Virtual_Get_Flags is access function (Self : Gdk_Paintable) return Gdk_Paintable_Flags;
   pragma Convention (C, Virtual_Get_Flags);
   --  Get flags for the paintable.
   --  This is oftentimes useful for optimizations.
   --  See [flagsGdk.PaintableFlags] for the flags and what they mean.
   --  @return The `GdkPaintableFlags` for this paintable

   type Virtual_Get_Intrinsic_Aspect_Ratio is access function (Self : Gdk_Paintable) return Gdouble;
   pragma Convention (C, Virtual_Get_Intrinsic_Aspect_Ratio);
   --  Gets the preferred aspect ratio the Paintable would like to be
   --  displayed at.
   --  The aspect ratio is the width divided by the height, so a value of 0.5
   --  means that the Paintable prefers to be displayed twice as high as it is
   --  wide. Consumers of this interface can use this to preserve aspect ratio
   --  when displaying the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  Usually when a Paintable returns nonzero values from
   --  [methodGdk.Paintable.get_intrinsic_width] and
   --  [methodGdk.Paintable.get_intrinsic_height] the aspect ratio should
   --  conform to those values, though that is not required.
   --  If the Paintable does not have a preferred aspect ratio, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic aspect ratio of Paintable or 0 if none.

   type Virtual_Get_Intrinsic_Height is access function (Self : Gdk_Paintable) return Glib.Gint;
   pragma Convention (C, Virtual_Get_Intrinsic_Height);
   --  Gets the preferred height the Paintable would like to be displayed at.
   --  Consumers of this interface can use this to reserve enough space to
   --  draw the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  If the Paintable does not have a preferred height, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic height of Paintable or 0 if none.

   type Virtual_Get_Intrinsic_Width is access function (Self : Gdk_Paintable) return Glib.Gint;
   pragma Convention (C, Virtual_Get_Intrinsic_Width);
   --  Gets the preferred width the Paintable would like to be displayed at.
   --  Consumers of this interface can use this to reserve enough space to
   --  draw the paintable.
   --  This is a purely informational value and does not in any way limit the
   --  values that may be passed to [methodGdk.Paintable.snapshot].
   --  If the Paintable does not have a preferred width, it returns 0.
   --  Negative values are never returned.
   --  @return the intrinsic width of Paintable or 0 if none.

   type Virtual_Snapshot is access procedure
     (Self     : Gdk_Paintable;
      Snapshot : System.Address;
      Width    : Gdouble;
      Height   : Gdouble);
   pragma Convention (C, Virtual_Snapshot);
   --  Snapshots the given paintable with the given Width and Height.
   --  The paintable is drawn at the current (0,0) offset of the Snapshot. If
   --  Width and Height are not larger than zero, this function will do
   --  nothing.
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in

   subtype Paintable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Current_Image
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Get_Current_Image);
   pragma Import (C, Set_Get_Current_Image, "gtkada_Paintable_set_get_current_image");

   procedure Set_Get_Flags
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Get_Flags);
   pragma Import (C, Set_Get_Flags, "gtkada_Paintable_set_get_flags");

   procedure Set_Get_Intrinsic_Aspect_Ratio
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Get_Intrinsic_Aspect_Ratio);
   pragma Import (C, Set_Get_Intrinsic_Aspect_Ratio, "gtkada_Paintable_set_get_intrinsic_aspect_ratio");

   procedure Set_Get_Intrinsic_Height
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Get_Intrinsic_Height);
   pragma Import (C, Set_Get_Intrinsic_Height, "gtkada_Paintable_set_get_intrinsic_height");

   procedure Set_Get_Intrinsic_Width
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Get_Intrinsic_Width);
   pragma Import (C, Set_Get_Intrinsic_Width, "gtkada_Paintable_set_get_intrinsic_width");

   procedure Set_Snapshot
     (Self    : Paintable_Interface_Descr;
      Handler : Virtual_Snapshot);
   pragma Import (C, Set_Snapshot, "gtkada_Paintable_set_snapshot");
   --  See Glib.Object.Add_Interface

private

   Null_Gdk_Paintable : constant Gdk_Paintable :=
      Gdk_Paintable (Glib.Types.Null_Interface);
end Gdk.Paintable;
