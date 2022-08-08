------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Gtk.GLArea.Gtk_GLArea is a widget that allows drawing with OpenGL.
--
--  Gtk.GLArea.Gtk_GLArea sets up its own Gdk.GLContext.Gdk_GLContext for the
--  window it creates, and creates a custom GL framebuffer that the widget will
--  do GL rendering onto. It also ensures that this framebuffer is the default
--  GL rendering target when rendering.
--
--  In order to draw, you have to connect to the Gtk.GLArea.Gtk_GLArea::render
--  signal, or subclass Gtk.GLArea.Gtk_GLArea and override the
--  Gtkglareaclass.render virtual function.
--
--  The Gtk.GLArea.Gtk_GLArea widget ensures that the
--  Gdk.GLContext.Gdk_GLContext is associated with the widget's drawing area,
--  and it is kept updated when the size and position of the drawing area
--  changes.
--
--  ## Drawing with GtkGLArea ##
--
--  The simplest way to draw using OpenGL commands in a Gtk.GLArea.Gtk_GLArea
--  is to create a widget instance and connect to the
--  Gtk.GLArea.Gtk_GLArea::render signal:
--
--  |[<!-- language="C" --> // create a GtkGLArea instance GtkWidget *gl_area
--  = gtk_gl_area_new ();
--
--  // connect to the "render" signal g_signal_connect (gl_area, "render",
--  G_CALLBACK (render), NULL); ]|
--
--  The `render` function will be called when the Gtk.GLArea.Gtk_GLArea is
--  ready for you to draw its content:
--
--  |[<!-- language="C" --> static gboolean render (GtkGLArea *area,
--  GdkGLContext *context) { // inside this function it's safe to use GL; the
--  given // Gdk.GLContext.Gdk_GLContext has been made current to the drawable
--  // surface used by the Gtk.GLArea.Gtk_GLArea and the viewport has //
--  already been set to be the size of the allocation
--
--  // we can start by clearing the buffer glClearColor (0, 0, 0, 0); glClear
--  (GL_COLOR_BUFFER_BIT);
--
--  // draw your object draw_an_object ();
--
--  // we completed our drawing; the draw commands will be // flushed at the
--  end of the signal emission chain, and // the buffers will be drawn on the
--  window return TRUE; } ]|
--
--  If you need to initialize OpenGL state, e.g. buffer objects or shaders,
--  you should use the Gtk.Widget.Gtk_Widget::realize signal; you can use the
--  Gtk.Widget.Gtk_Widget::unrealize signal to clean up. Since the
--  Gdk.GLContext.Gdk_GLContext creation and initialization may fail, you will
--  need to check for errors, using Gtk.GLArea.Get_Error. An example of how to
--  safely initialize the GL state is:
--
--  |[<!-- language="C" --> static void on_realize (GtkGLarea *area) { // We
--  need to make the context current if we want to // call GL API
--  gtk_gl_area_make_current (area);
--
--  // If there were errors during the initialization or // when trying to
--  make the context current, this // function will return a Gerror.Gerror for
--  you to catch if (gtk_gl_area_get_error (area) != NULL) return;
--
--  // You can also use Gtk.GLArea.Set_Error in order // to show eventual
--  initialization errors on the // GtkGLArea widget itself GError
--  *internal_error = NULL; init_buffer_objects (&error); if (error != NULL) {
--  gtk_gl_area_set_error (area, error); g_error_free (error); return; }
--
--  init_shaders (&error); if (error != NULL) { gtk_gl_area_set_error (area,
--  error); g_error_free (error); return; } } ]|
--
--  If you need to change the options for creating the
--  Gdk.GLContext.Gdk_GLContext you should use the
--  Gtk.GLArea.Gtk_GLArea::create-context signal.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.GLContext;   use Gdk.GLContext;
with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.GLArea is

   type Gtk_GLArea_Record is new Gtk_Widget_Record with null record;
   type Gtk_GLArea is access all Gtk_GLArea_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_GLArea);
   procedure Initialize (Self : not null access Gtk_GLArea_Record'Class);
   --  Creates a new Gtk.GLArea.Gtk_GLArea widget.
   --  Since: gtk+ 3.16
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_GLArea_New return Gtk_GLArea;
   --  Creates a new Gtk.GLArea.Gtk_GLArea widget.
   --  Since: gtk+ 3.16

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gl_area_get_type");

   -------------
   -- Methods --
   -------------

   procedure Attach_Buffers (Self : not null access Gtk_GLArea_Record);
   --  Ensures that the Area framebuffer object is made the current draw and
   --  read target, and that all the required buffers for the Area are created
   --  and bound to the frambuffer.
   --  This function is automatically called before emitting the
   --  Gtk.GLArea.Gtk_GLArea::render signal, and doesn't normally need to be
   --  called by application code.
   --  Since: gtk+ 3.16

   function Get_Auto_Render
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area is in auto render mode or not.
   --  Since: gtk+ 3.16

   procedure Set_Auto_Render
      (Self        : not null access Gtk_GLArea_Record;
       Auto_Render : Boolean);
   --  If Auto_Render is True the Gtk.GLArea.Gtk_GLArea::render signal will be
   --  emitted every time the widget draws. This is the default and is useful
   --  if drawing the widget is faster.
   --  If Auto_Render is False the data from previous rendering is kept around
   --  and will be used for drawing the widget the next time, unless the window
   --  is resized. In order to force a rendering Gtk.GLArea.Queue_Render must
   --  be called. This mode is useful when the scene changes seldomly, but
   --  takes a long time to redraw.
   --  Since: gtk+ 3.16
   --  "auto_render": a boolean

   function Get_Context
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLContext;
   --  Retrieves the Gdk.GLContext.Gdk_GLContext used by Area.
   --  Since: gtk+ 3.16

   function Get_Error
      (Self : not null access Gtk_GLArea_Record) return Glib.Error.GError;
   --  Gets the current error set on the Area.
   --  Since: gtk+ 3.16

   procedure Set_Error
      (Self  : not null access Gtk_GLArea_Record;
       Error : Glib.Error.GError);
   --  Sets an error on the area which will be shown instead of the GL
   --  rendering. This is useful in the Gtk.GLArea.Gtk_GLArea::create-context
   --  signal if GL context creation fails.
   --  Since: gtk+ 3.16
   --  "error": a new Gerror.Gerror, or null to unset the error

   function Get_Has_Alpha
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area has an alpha component.
   --  Since: gtk+ 3.16

   procedure Set_Has_Alpha
      (Self      : not null access Gtk_GLArea_Record;
       Has_Alpha : Boolean);
   --  If Has_Alpha is True the buffer allocated by the widget will have an
   --  alpha channel component, and when rendering to the window the result
   --  will be composited over whatever is below the widget.
   --  If Has_Alpha is False there will be no alpha channel, and the buffer
   --  will fully replace anything below the widget.
   --  Since: gtk+ 3.16
   --  "has_alpha": True to add an alpha component

   function Get_Has_Depth_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area has a depth buffer.
   --  Since: gtk+ 3.16

   procedure Set_Has_Depth_Buffer
      (Self             : not null access Gtk_GLArea_Record;
       Has_Depth_Buffer : Boolean);
   --  If Has_Depth_Buffer is True the widget will allocate and enable a depth
   --  buffer for the target framebuffer. Otherwise there will be none.
   --  Since: gtk+ 3.16
   --  "has_depth_buffer": True to add a depth buffer

   function Get_Has_Stencil_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area has a stencil buffer.
   --  Since: gtk+ 3.16

   procedure Set_Has_Stencil_Buffer
      (Self               : not null access Gtk_GLArea_Record;
       Has_Stencil_Buffer : Boolean);
   --  If Has_Stencil_Buffer is True the widget will allocate and enable a
   --  stencil buffer for the target framebuffer. Otherwise there will be none.
   --  Since: gtk+ 3.16
   --  "has_stencil_buffer": True to add a stencil buffer

   procedure Get_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves the required version of OpenGL set using
   --  Gtk.GLArea.Set_Required_Version.
   --  Since: gtk+ 3.16
   --  "major": return location for the required major version
   --  "minor": return location for the required minor version

   procedure Set_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint);
   --  Sets the required version of OpenGL to be used when creating the
   --  context for the widget.
   --  This function must be called before the area has been realized.
   --  Since: gtk+ 3.16
   --  "major": the major version
   --  "minor": the minor version

   function Get_Use_Es
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Retrieves the value set by Gtk.GLArea.Set_Use_Es.
   --  Since: gtk+ 3.22

   procedure Set_Use_Es
      (Self   : not null access Gtk_GLArea_Record;
       Use_Es : Boolean);
   --  Sets whether the Area should create an OpenGL or an OpenGL ES context.
   --  You should check the capabilities of the Gdk.GLContext.Gdk_GLContext
   --  before drawing with either API.
   --  Since: gtk+ 3.22
   --  "use_es": whether to use OpenGL or OpenGL ES

   procedure Make_Current (Self : not null access Gtk_GLArea_Record);
   --  Ensures that the Gdk.GLContext.Gdk_GLContext used by Area is associated
   --  with the Gtk.GLArea.Gtk_GLArea.
   --  This function is automatically called before emitting the
   --  Gtk.GLArea.Gtk_GLArea::render signal, and doesn't normally need to be
   --  called by application code.
   --  Since: gtk+ 3.16

   procedure Queue_Render (Self : not null access Gtk_GLArea_Record);
   --  Marks the currently rendered data (if any) as invalid, and queues a
   --  redraw of the widget, ensuring that the Gtk.GLArea.Gtk_GLArea::render
   --  signal is emitted during the draw.
   --  This is only needed when the Gtk.GLArea.Set_Auto_Render has been called
   --  with a False value. The default behaviour is to emit
   --  Gtk.GLArea.Gtk_GLArea::render on each draw.
   --  Since: gtk+ 3.16

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Auto_Render_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the Gtk.GLArea.Gtk_GLArea::render signal will be emitted
   --  every time the widget draws. This is the default and is useful if
   --  drawing the widget is faster.
   --
   --  If set to False the data from previous rendering is kept around and
   --  will be used for drawing the widget the next time, unless the window is
   --  resized. In order to force a rendering Gtk.GLArea.Queue_Render must be
   --  called. This mode is useful when the scene changes seldomly, but takes a
   --  long time to redraw.

   Context_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.GLContext.Gdk_GLContext
   --  The Gdk.GLContext.Gdk_GLContext used by the Gtk.GLArea.Gtk_GLArea
   --  widget.
   --
   --  The Gtk.GLArea.Gtk_GLArea widget is responsible for creating the
   --  Gdk.GLContext.Gdk_GLContext instance. If you need to render with other
   --  kinds of buffers (stencil, depth, etc), use render buffers.

   Has_Alpha_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the buffer allocated by the widget will have an alpha
   --  channel component, and when rendering to the window the result will be
   --  composited over whatever is below the widget.
   --
   --  If set to False there will be no alpha channel, and the buffer will
   --  fully replace anything below the widget.

   Has_Depth_Buffer_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will allocate and enable a depth buffer for
   --  the target framebuffer.

   Has_Stencil_Buffer_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will allocate and enable a stencil buffer for
   --  the target framebuffer.

   Use_Es_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will try to create a
   --  Gdk.GLContext.Gdk_GLContext using OpenGL ES instead of OpenGL.
   --
   --  See also: Gdk.GLContext.Set_Use_Es

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_GLArea_Gdk_GLContext is not null access function
     (Self : access Gtk_GLArea_Record'Class)
   return Gdk.GLContext.Gdk_GLContext;

   type Cb_GObject_Gdk_GLContext is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Gdk.GLContext.Gdk_GLContext;

   Signal_Create_Context : constant Glib.Signal_Name := "create-context";
   procedure On_Create_Context
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gdk_GLContext;
       After : Boolean := False);
   procedure On_Create_Context
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gdk_GLContext;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::create-context signal is emitted when the widget is being
   --  realized, and allows you to override how the GL context is created. This
   --  is useful when you want to reuse an existing GL context, or if you want
   --  to try creating different kinds of GL options.
   --
   --  If context creation fails then the signal handler can use
   --  Gtk.GLArea.Set_Error to register a more detailed error of how the
   --  construction failed.
   -- 
   --  Callback parameters:
   --    --  Returns a newly created Gdk.GLContext.Gdk_GLContext;
   --     the Gtk.GLArea.Gtk_GLArea widget will take ownership of the returned value.

   type Cb_Gtk_GLArea_Gdk_GLContext_Boolean is not null access function
     (Self    : access Gtk_GLArea_Record'Class;
      Context : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
   return Boolean;

   type Cb_GObject_Gdk_GLContext_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : not null access Gdk.GLContext.Gdk_GLContext_Record'Class)
   return Boolean;

   Signal_Render : constant Glib.Signal_Name := "render";
   procedure On_Render
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gdk_GLContext_Boolean;
       After : Boolean := False);
   procedure On_Render
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gdk_GLContext_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::render signal is emitted every time the contents of the
   --  Gtk.GLArea.Gtk_GLArea should be redrawn.
   --
   --  The Context is bound to the Area prior to emitting this function, and
   --  the buffers are painted to the window once the emission terminates.
   -- 
   --  Callback parameters:
   --    --  "context": the Gdk.GLContext.Gdk_GLContext used by Area
   --    --  Returns True to stop other handlers from being invoked for the event.
   --   False to propagate the event further.

   type Cb_Gtk_GLArea_Gint_Gint_Void is not null access procedure
     (Self   : access Gtk_GLArea_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   type Cb_GObject_Gint_Gint_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   Signal_Resize : constant Glib.Signal_Name := "resize";
   procedure On_Resize
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_Gtk_GLArea_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Resize
      (Self  : not null access Gtk_GLArea_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::resize signal is emitted once when the widget is realized, and
   --  then each time the widget is changed while realized. This is useful in
   --  order to keep GL state up to date with the widget size, like for
   --  instance camera properties which may depend on the width/height ratio.
   --
   --  The GL context for the area is guaranteed to be current when this
   --  signal is emitted.
   --
   --  The default handler sets up the GL viewport.
   -- 
   --  Callback parameters:
   --    --  "width": the width of the viewport
   --    --  "height": the height of the viewport

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_GLArea_Record, Gtk_GLArea);
   function "+"
     (Widget : access Gtk_GLArea_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_GLArea
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Es_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-es");
   Has_Stencil_Buffer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-stencil-buffer");
   Has_Depth_Buffer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-depth-buffer");
   Has_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-alpha");
   Context_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("context");
   Auto_Render_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("auto-render");
end Gtk.GLArea;
