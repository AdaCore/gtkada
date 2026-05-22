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

--  Represents a rectangular region on the screen.
--
--  It's a low-level object, used to implement high-level objects such as
--  [GtkWindow](../gtk4/class.Window.html).
--
--  The surfaces you see in practice are either [ifaceGdk.Toplevel] or
--  [ifaceGdk.Popup], and those interfaces provide much of the required API to
--  interact with these surfaces. Other, more specialized surface types exist,
--  but you will rarely interact with them directly.

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Gdk.Cursor;      use Gdk.Cursor;
with Gdk.Device;      use Gdk.Device;
with Gdk.Frame_Clock; use Gdk.Frame_Clock;
with Gdk.GLContext;   use Gdk.GLContext;
with Gdk.Monitor;     use Gdk.Monitor;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with System;

package Gdk.Surface is

   type Gdk_Surface_Record is new GObject_Record with null record;
   type Gdk_Surface is access all Gdk_Surface_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New_Popup
      (Self     : out Gdk_Surface;
       Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean);
   --  Create a new popup surface.
   --  The surface will be attached to Parent and can be positioned relative
   --  to it using [methodGdk.Popup.present].
   --  @param Parent the parent surface to attach the surface to
   --  @param Autohide whether to hide the surface on outside clicks

   procedure Initialize_Popup
      (Self     : not null access Gdk_Surface_Record'Class;
       Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean);
   --  Create a new popup surface.
   --  The surface will be attached to Parent and can be positioned relative
   --  to it using [methodGdk.Popup.present].
   --  Initialize_Popup does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param Parent the parent surface to attach the surface to
   --  @param Autohide whether to hide the surface on outside clicks

   function Gdk_Surface_New_Popup
      (Parent   : not null access Gdk_Surface_Record'Class;
       Autohide : Boolean) return Gdk_Surface;
   --  Create a new popup surface.
   --  The surface will be attached to Parent and can be positioned relative
   --  to it using [methodGdk.Popup.present].
   --  @param Parent the parent surface to attach the surface to
   --  @param Autohide whether to hide the surface on outside clicks

   procedure Gdk_New_Toplevel
      (Self    : out Gdk_Surface;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   --  Creates a new toplevel surface.
   --  @param Display the display to create the surface on

   procedure Initialize_Toplevel
      (Self    : not null access Gdk_Surface_Record'Class;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class);
   --  Creates a new toplevel surface.
   --  Initialize_Toplevel does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param Display the display to create the surface on

   function Gdk_Surface_New_Toplevel
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gdk_Surface;
   --  Creates a new toplevel surface.
   --  @param Display the display to create the surface on

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_surface_get_type");

   -------------
   -- Methods --
   -------------

   procedure Beep (Self : not null access Gdk_Surface_Record);
   --  Emits a short beep associated to Surface.
   --  If the display of Surface does not support per-surface beeps, emits a
   --  short beep on the display just as [methodGdk.Display.beep].

   function Create_Gl_Context
      (Self : not null access Gdk_Surface_Record)
       return Gdk.GLContext.Gdk_GLContext;
   --  Creates a new `GdkGLContext` for the `GdkSurface`.
   --  The context is disconnected from any particular surface or surface. If
   --  the creation of the `GdkGLContext` failed, Error will be set. Before
   --  using the returned `GdkGLContext`, you will need to call
   --  [methodGdk.GLContext.make_current] or [methodGdk.GLContext.realize].
   --  @return the newly created `GdkGLContext`

   procedure Destroy (Self : not null access Gdk_Surface_Record);
   --  Destroys the window system resources associated with Surface and
   --  decrements Surface's reference count.
   --  The window system resources for all children of Surface are also
   --  destroyed, but the children's reference counts are not decremented.
   --  Note that a surface will not be destroyed automatically when its
   --  reference count reaches zero. You must call this function yourself
   --  before that happens.

   function Get_Cursor
      (Self : not null access Gdk_Surface_Record)
       return Gdk.Cursor.Gdk_Cursor;
   --  Retrieves a `GdkCursor` pointer for the cursor currently set on the
   --  `GdkSurface`.
   --  If the return value is null then there is no custom cursor set on the
   --  surface, and it is using the cursor for its parent surface.
   --  Use [methodGdk.Surface.set_cursor] to unset the cursor of the surface.
   --  @return a `GdkCursor`

   procedure Set_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Cursor : access Gdk.Cursor.Gdk_Cursor_Record'Class);
   --  Sets the default mouse pointer for a `GdkSurface`.
   --  Passing null for the Cursor argument means that Surface will use the
   --  cursor of its parent surface. Most surfaces should use this default.
   --  Note that Cursor must be for the same display as Surface.
   --  Use [ctorGdk.Cursor.new_from_name] or [ctorGdk.Cursor.new_from_texture]
   --  to create the cursor. To make the cursor invisible, use
   --  GDK_BLANK_CURSOR.
   --  @param Cursor a `GdkCursor`

   function Get_Device_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Cursor.Gdk_Cursor;
   --  Retrieves a `GdkCursor` pointer for the Device currently set on the
   --  specified `GdkSurface`.
   --  If the return value is null then there is no custom cursor set on the
   --  specified surface, and it is using the cursor for its parent surface.
   --  Use [methodGdk.Surface.set_cursor] to unset the cursor of the surface.
   --  @param Device a pointer `GdkDevice`
   --  @return a `GdkCursor`

   procedure Set_Device_Cursor
      (Self   : not null access Gdk_Surface_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Cursor : not null access Gdk.Cursor.Gdk_Cursor_Record'Class);
   --  Sets a specific `GdkCursor` for a given device when it gets inside
   --  Surface.
   --  Passing null for the Cursor argument means that Surface will use the
   --  cursor of its parent surface. Most surfaces should use this default.
   --  Use [ctorGdk.Cursor.new_from_name] or [ctorGdk.Cursor.new_from_texture]
   --  to create the cursor. To make the cursor invisible, use
   --  GDK_BLANK_CURSOR.
   --  @param Device a pointer `GdkDevice`
   --  @param Cursor a `GdkCursor`

   function Get_Display
      (Self : not null access Gdk_Surface_Record) return Gdk.Gdk_Display;
   --  Gets the `GdkDisplay` associated with a `GdkSurface`.
   --  @return the `GdkDisplay` associated with Surface

   function Get_Frame_Clock
      (Self : not null access Gdk_Surface_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock;
   --  Gets the frame clock for the surface.
   --  The frame clock for a surface never changes unless the surface is
   --  reparented to a new toplevel surface.
   --  @return the frame clock

   function Get_Height
      (Self : not null access Gdk_Surface_Record) return Glib.Gint;
   --  Returns the height of the given Surface.
   --  Surface size is reported in "application pixels", not "device pixels"
   --  (see [methodGdk.Surface.get_scale_factor]).
   --  @return The height of Surface

   function Get_Mapped
      (Self : not null access Gdk_Surface_Record) return Boolean;
   --  Checks whether the surface has been mapped.
   --  A surface is mapped with [methodGdk.Toplevel.present] or
   --  [methodGdk.Popup.present].
   --  @return True if the surface is mapped

   function Get_Scale
      (Self : not null access Gdk_Surface_Record) return Gdouble;
   --  Returns the internal scale that maps from surface coordinates to the
   --  actual device pixels.
   --  When the scale is bigger than 1, the windowing system prefers to get
   --  buffers with a resolution that is bigger than the surface size (e.g. to
   --  show the surface on a high-resolution display, or in a magnifier).
   --  Compare with [methodGdk.Surface.get_scale_factor], which returns the
   --  next larger integer.
   --  The scale may change during the lifetime of the surface.
   --  Since: gtk+ 4.12
   --  @return the scale

   function Get_Scale_Factor
      (Self : not null access Gdk_Surface_Record) return Glib.Gint;
   --  Returns the internal scale factor that maps from surface coordinates to
   --  the actual device pixels.
   --  On traditional systems this is 1, but on very high density outputs this
   --  can be a higher value (often 2). A higher value means that drawing is
   --  automatically scaled up to a higher resolution, so any code doing
   --  drawing will automatically look nicer. However, if you are supplying
   --  pixel-based data the scale value can be used to determine whether to use
   --  a pixel resource with higher resolution data.
   --  The scale factor may change during the lifetime of the surface.
   --  @return the scale factor

   function Get_Width
      (Self : not null access Gdk_Surface_Record) return Glib.Gint;
   --  Returns the width of the given Surface.
   --  Surface size is reported in "application pixels", not "device pixels"
   --  (see [methodGdk.Surface.get_scale_factor]).
   --  @return The width of Surface

   procedure Hide (Self : not null access Gdk_Surface_Record);
   --  Hide the surface.
   --  For toplevel surfaces, withdraws them, so they will no longer be known
   --  to the window manager; for all surfaces, unmaps them, so they won't be
   --  displayed. Normally done automatically as part of
   --  [gtk_widget_hide](../gtk4/method.Widget.hide.html).

   function Is_Destroyed
      (Self : not null access Gdk_Surface_Record) return Boolean;
   --  Check to see if a surface is destroyed.
   --  @return True if the surface is destroyed

   procedure Queue_Render (Self : not null access Gdk_Surface_Record);
   --  Forces a [signalGdk.Surface::render] signal emission for Surface to be
   --  scheduled.
   --  This function is useful for implementations that track invalid regions
   --  on their own.

   procedure Request_Layout (Self : not null access Gdk_Surface_Record);
   --  Request a layout phase from the surface's frame clock.
   --  See [methodGdk.FrameClock.request_phase].

   procedure Set_Input_Region
      (Self   : not null access Gdk_Surface_Record;
       Region : Cairo.Region.Cairo_Region);
   --  Apply the region to the surface for the purpose of event handling.
   --  Mouse events which happen while the pointer position corresponds to an
   --  unset bit in the mask will be passed on the surface below Surface.
   --  An input region is typically used with RGBA surfaces. The alpha channel
   --  of the surface defines which pixels are invisible and allows for nicely
   --  antialiased borders, and the input region controls where the surface is
   --  "clickable".
   --  Use [methodGdk.Display.supports_input_shapes] to find out if a
   --  particular backend supports input regions.
   --  @param Region region of surface to be reactive, or null to make the
   --  entire surface reactive

   procedure Set_Opaque_Region
      (Self   : not null access Gdk_Surface_Record;
       Region : Cairo.Region.Cairo_Region);
   pragma Obsolescent (Set_Opaque_Region);
   --  Marks a region of the `GdkSurface` as opaque.
   --  For optimisation purposes, compositing window managers may like to not
   --  draw obscured regions of surfaces, or turn off blending during for these
   --  regions. With RGB windows with no transparency, this is just the shape
   --  of the window, but with ARGB32 windows, the compositor does not know
   --  what regions of the window are transparent or not.
   --  This function only works for toplevel surfaces.
   --  GTK will update this property automatically if the Surface background
   --  is opaque, as we know where the opaque regions are. If your surface
   --  background is not opaque, please update this property in your
   --  [GtkWidgetClass.css_changed](../gtk4/vfunc.Widget.css_changed.html)
   --  handler.
   --  Deprecated since 4.16, 1
   --  @param Region a region, or null to make the entire surface opaque

   function Translate_Coordinates
      (Self : not null access Gdk_Surface_Record;
       To   : not null access Gdk_Surface_Record'Class;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean;
   --  Translates coordinates between two surfaces.
   --  Note that this only works if To and From are popups or transient-for to
   --  the same toplevel (directly or indirectly).
   --  @param To the target surface
   --  @param X coordinates to translate
   --  @param Y coordinates to translate
   --  @return True if the coordinates were successfully translated

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cursor_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cursor
   --  The mouse pointer for the `GdkSurface`.

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` connection of the surface.

   Frame_Clock_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Frame_Clock
   --  The `GdkFrameClock` of the surface.

   Height_Property : constant Glib.Properties.Property_Int;
   --  The height of the surface, in pixels.

   Mapped_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the surface is mapped.

   Scale_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The scale of the surface.

   Scale_Factor_Property : constant Glib.Properties.Property_Int;
   --  The scale factor of the surface.
   --
   --  The scale factor is the next larger integer, compared to
   --  [propertyGdk.Surface:scale].

   Width_Property : constant Glib.Properties.Property_Int;
   --  The width of the surface in pixels.

   -------------
   -- Signals --
   -------------

   type Cb_Gdk_Surface_Gdk_Monitor_Void is not null access procedure
     (Self    : access Gdk_Surface_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);

   type Cb_GObject_Gdk_Monitor_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Monitor : not null access Gdk.Monitor.Gdk_Monitor_Record'Class);

   Signal_Enter_Monitor : constant Glib.Signal_Name := "enter-monitor";
   procedure On_Enter_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After : Boolean := False);
   procedure On_Enter_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gdk_Monitor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Surface starts being present on the monitor.

   type Cb_Gdk_Surface_Address_Boolean is not null access function
     (Self  : access Gdk_Surface_Record'Class;
      Event : System.Address) return Boolean;

   type Cb_GObject_Address_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : System.Address) return Boolean;

   Signal_Event : constant Glib.Signal_Name := "event";
   procedure On_Event
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Address_Boolean;
       After : Boolean := False);
   procedure On_Event
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Address_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when GDK receives an input event for Surface.
   -- 
   --  Callback parameters:
   --    --  @param Event an input event

   type Cb_Gdk_Surface_Gint_Gint_Void is not null access procedure
     (Self   : access Gdk_Surface_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   type Cb_GObject_Gint_Gint_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   Signal_Layout : constant Glib.Signal_Name := "layout";
   procedure On_Layout
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Layout
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the size of Surface is changed, or when relayout should be
   --  performed.
   --
   --  Surface size is reported in "application pixels", not "device pixels"
   --  (see Gdk.Surface.Get_Scale_Factor).
   -- 
   --  Callback parameters:
   --    --  @param Width the current width
   --    --  @param Height the current height

   Signal_Leave_Monitor : constant Glib.Signal_Name := "leave-monitor";
   procedure On_Leave_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_Gdk_Surface_Gdk_Monitor_Void;
       After : Boolean := False);
   procedure On_Leave_Monitor
      (Self  : not null access Gdk_Surface_Record;
       Call  : Cb_GObject_Gdk_Monitor_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Surface stops being present on the monitor.

   Signal_Render : constant Glib.Signal_Name := "render";
   --  Emitted when part of the surface needs to be redrawn.
   --    function Handler
   --       (Self   : access Gdk_Surface_Record'Class;
   --        Region : cairo.Region) return Boolean
   -- 
   --  Callback parameters:
   --    --  @param Region the region that needs to be redrawn

private
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Scale_Factor_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale-factor");
   Scale_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("scale");
   Mapped_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("mapped");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Frame_Clock_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("frame-clock");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
   Cursor_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("cursor");
end Gdk.Surface;
