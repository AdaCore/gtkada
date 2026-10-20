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

--  Allows drawing with OpenGL.
--
--  <picture> <source srcset="glarea-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkGLArea" src="glarea.png"> </picture>
--  `GtkGLArea` sets up its own [classGdk.GLContext], and creates a custom GL
--  framebuffer that the widget will do GL rendering onto. It also ensures that
--  this framebuffer is the default GL rendering target when rendering. The
--  completed rendering is integrated into the larger GTK scene graph as a
--  texture.
--
--  In order to draw, you have to connect to the [signalGtk.GLArea::render]
--  signal, or subclass `GtkGLArea` and override the GtkGLAreaClass.render
--  virtual function.
--
--  The `GtkGLArea` widget ensures that the `GdkGLContext` is associated with
--  the widget's drawing area, and it is kept updated when the size and
--  position of the drawing area changes.
--
--  ## Drawing with GtkGLArea
--
--  The simplest way to draw using OpenGL commands in a `GtkGLArea` is to
--  create a widget instance and connect to the [signalGtk.GLArea::render]
--  signal:
--
--  The `render` function will be called when the `GtkGLArea` is ready for you
--  to draw its content:
--
--  The initial contents of the framebuffer are transparent.
--
--  ```c static gboolean render (GtkGLArea *area, GdkGLContext *context) { //
--  inside this function it's safe to use GL; the given // GdkGLContext has
--  been made current to the drawable // surface used by the `GtkGLArea` and
--  the viewport has // already been set to be the size of the allocation
--
--  // we can start by clearing the buffer glClearColor (0, 0, 0, 0); glClear
--  (GL_COLOR_BUFFER_BIT);
--
--  // record the active framebuffer ID, so we can return to it // with
--  `glBindFramebuffer (GL_FRAMEBUFFER, screen_fb)` should // we, for instance,
--  intend on utilizing the results of an // intermediate render texture pass
--  GLuint screen_fb = 0; glGetIntegerv (GL_FRAMEBUFFER_BINDING, &screen_fb);
--
--  // draw your object // draw_an_object ();
--
--  // we completed our drawing; the draw commands will be // flushed at the
--  end of the signal emission chain, and // the buffers will be drawn on the
--  window return TRUE; }
--
--  void setup_glarea (void) { // create a GtkGLArea instance GtkWidget
--  *gl_area = gtk_gl_area_new ();
--
--  // connect to the "render" signal g_signal_connect (gl_area, "render",
--  G_CALLBACK (render), NULL); } ```
--
--  If you need to initialize OpenGL state, e.g. buffer objects or shaders,
--  you should use the [signalGtk.Widget::realize] signal; you can use the
--  [signalGtk.Widget::unrealize] signal to clean up. Since the `GdkGLContext`
--  creation and initialization may fail, you will need to check for errors,
--  using [methodGtk.GLArea.get_error].
--
--  An example of how to safely initialize the GL state is:
--
--  ```c static void on_realize (GtkGLArea *area) { // We need to make the
--  context current if we want to // call GL API gtk_gl_area_make_current
--  (area);
--
--  // If there were errors during the initialization or // when trying to
--  make the context current, this // function will return a GError for you to
--  catch if (gtk_gl_area_get_error (area) != NULL) return;
--
--  // You can also use Gtk.GLArea.Set_Error in order // to show eventual
--  initialization errors on the // GtkGLArea widget itself GError
--  *internal_error = NULL; init_buffer_objects (&error); if (error != NULL) {
--  gtk_gl_area_set_error (area, error); g_error_free (error); return; }
--
--  init_shaders (&error); if (error != NULL) { gtk_gl_area_set_error (area,
--  error); g_error_free (error); return; } } ```
--
--  If you need to change the options for creating the `GdkGLContext` you
--  should use the [signalGtk.GLArea::create-context] signal.
--
--  <gtkada_demo>create_gl.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.GLContext;         use Gdk.GLContext;
with Glib;                  use Glib;
with Glib.Error;            use Glib.Error;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.GLArea is

   type Gtk_GLArea_Record is new Gtk_Widget_Record with null record;
   type Gtk_GLArea is access all Gtk_GLArea_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_GLArea);
   procedure Initialize (Self : not null access Gtk_GLArea_Record'Class);
   --  Creates a new `GtkGLArea` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_GLArea_New return Gtk_GLArea;
   --  Creates a new `GtkGLArea` widget.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gl_area_get_type");

   -------------
   -- Methods --
   -------------

   procedure Attach_Buffers (Self : not null access Gtk_GLArea_Record);
   --  Binds buffers to the framebuffer.
   --  Ensures that the Area framebuffer object is made the current draw and
   --  read target, and that all the required buffers for the Area are created
   --  and bound to the framebuffer.
   --  This function is automatically called before emitting the
   --  [signalGtk.GLArea::render] signal, and doesn't normally need to be
   --  called by application code.

   function Get_Allowed_Apis
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLAPI;
   --  Gets the allowed APIs.
   --  See [methodGtk.GLArea.set_allowed_apis].
   --  Since: gtk+ 4.12
   --  @return the allowed APIs

   procedure Set_Allowed_Apis
      (Self : not null access Gtk_GLArea_Record;
       Apis : Gdk.GLContext.Gdk_GLAPI);
   --  Sets the allowed APIs to create a context with.
   --  You should check [propertyGtk.GLArea:api] before drawing with either
   --  API.
   --  By default, all APIs are allowed.
   --  Since: gtk+ 4.12
   --  @param Apis the allowed APIs

   function Get_Api
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLAPI;
   --  Gets the API that is currently in use.
   --  If the GL area has not been realized yet, 0 is returned.
   --  Since: gtk+ 4.12
   --  @return the currently used API

   function Get_Auto_Render
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area is in auto render mode or not.
   --  @return True if the Area is auto rendering, False otherwise

   procedure Set_Auto_Render
      (Self        : not null access Gtk_GLArea_Record;
       Auto_Render : Boolean);
   --  Sets whether the `GtkGLArea` is in auto render mode.
   --  If Auto_Render is True the [signalGtk.GLArea::render] signal will be
   --  emitted every time the widget draws. This is the default and is useful
   --  if drawing the widget is faster.
   --  If Auto_Render is False the data from previous rendering is kept around
   --  and will be used for drawing the widget the next time, unless the window
   --  is resized. In order to force a rendering
   --  [methodGtk.GLArea.queue_render] must be called. This mode is useful when
   --  the scene changes seldom, but takes a long time to redraw.
   --  @param Auto_Render a boolean

   function Get_Context
      (Self : not null access Gtk_GLArea_Record)
       return Gdk.GLContext.Gdk_GLContext;
   --  Retrieves the `GdkGLContext` used by Area.
   --  @return the `GdkGLContext`. Has transfer-ownership='none'.

   function Get_Error
      (Self : not null access Gtk_GLArea_Record) return Glib.Error.GError;
   --  Gets the current error set on the Area.
   --  @return the `GError`

   procedure Set_Error
      (Self  : not null access Gtk_GLArea_Record;
       Error : Glib.Error.GError);
   --  Sets an error on the area which will be shown instead of the GL
   --  rendering.
   --  This is useful in the [signalGtk.GLArea::create-context] signal if GL
   --  context creation fails.
   --  @param Error a new `GError`, or null to unset the error

   function Get_Has_Depth_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area has a depth buffer.
   --  @return True if the Area has a depth buffer, False otherwise

   procedure Set_Has_Depth_Buffer
      (Self             : not null access Gtk_GLArea_Record;
       Has_Depth_Buffer : Boolean);
   --  Sets whether the `GtkGLArea` should use a depth buffer.
   --  If Has_Depth_Buffer is True the widget will allocate and enable a depth
   --  buffer for the target framebuffer. Otherwise there will be none.
   --  @param Has_Depth_Buffer True to add a depth buffer

   function Get_Has_Stencil_Buffer
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   --  Returns whether the area has a stencil buffer.
   --  @return True if the Area has a stencil buffer, False otherwise

   procedure Set_Has_Stencil_Buffer
      (Self               : not null access Gtk_GLArea_Record;
       Has_Stencil_Buffer : Boolean);
   --  Sets whether the `GtkGLArea` should use a stencil buffer.
   --  If Has_Stencil_Buffer is True the widget will allocate and enable a
   --  stencil buffer for the target framebuffer. Otherwise there will be none.
   --  @param Has_Stencil_Buffer True to add a stencil buffer

   procedure Get_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves the required version of OpenGL.
   --  See [methodGtk.GLArea.set_required_version].
   --  @param Major return location for the required major version
   --  @param Minor return location for the required minor version

   procedure Set_Required_Version
      (Self  : not null access Gtk_GLArea_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint);
   --  Sets the required version of OpenGL to be used when creating the
   --  context for the widget.
   --  This function must be called before the area has been realized.
   --  @param Major the major version
   --  @param Minor the minor version

   function Get_Use_Es
      (Self : not null access Gtk_GLArea_Record) return Boolean;
   pragma Obsolescent (Get_Use_Es);
   --  Returns whether the `GtkGLArea` should use OpenGL ES.
   --  See [methodGtk.GLArea.set_use_es].
   --  Deprecated since 4.12, 1
   --  @return True if the `GtkGLArea` should create an OpenGL ES context and
   --  False otherwise

   procedure Set_Use_Es
      (Self   : not null access Gtk_GLArea_Record;
       Use_Es : Boolean);
   pragma Obsolescent (Set_Use_Es);
   --  Sets whether the Area should create an OpenGL or an OpenGL ES context.
   --  You should check the capabilities of the `GdkGLContext` before drawing
   --  with either API.
   --  Deprecated since 4.12, 1
   --  @param Use_Es whether to use OpenGL or OpenGL ES

   procedure Make_Current (Self : not null access Gtk_GLArea_Record);
   --  Ensures that the `GdkGLContext` used by Area is associated with the
   --  `GtkGLArea`.
   --  This function is automatically called before emitting the
   --  [signalGtk.GLArea::render] signal, and doesn't normally need to be
   --  called by application code.

   procedure Queue_Render (Self : not null access Gtk_GLArea_Record);
   --  Marks the currently rendered data (if any) as invalid, and queues a
   --  redraw of the widget.
   --  This ensures that the [signalGtk.GLArea::render] signal is emitted
   --  during the draw.
   --  This is only needed when [methodGtk.GLArea.set_auto_render] has been
   --  called with a False value. The default behaviour is to emit
   --  [signalGtk.GLArea::render] on each draw.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_GLArea_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_GLArea_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_GLArea_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_GLArea_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_GLArea_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_GLArea_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_GLArea_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_GLArea_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_GLArea_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Allowed_Apis_Property : constant Gdk.GLContext.Property_Gdk_GLAPI;
   --  Type: Gdk.GLContext.Gdk_GLAPI
   --  The allowed APIs.

   Api_Property : constant Gdk.GLContext.Property_Gdk_GLAPI;
   --  Type: Gdk.GLContext.Gdk_GLAPI
   --  The API currently in use.

   Auto_Render_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the ::render signal will be emitted every time the
   --  widget draws.
   --
   --  This is the default and is useful if drawing the widget is faster.
   --
   --  If set to False the data from previous rendering is kept around and
   --  will be used for drawing the widget the next time, unless the window is
   --  resized. In order to force a rendering [methodGtk.GLArea.queue_render]
   --  must be called. This mode is useful when the scene changes seldom, but
   --  takes a long time to redraw.

   Context_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.GLContext.Gdk_GLContext
   --  The `GdkGLContext` used by the `GtkGLArea` widget.
   --
   --  The `GtkGLArea` widget is responsible for creating the `GdkGLContext`
   --  instance. If you need to render with other kinds of buffers (stencil,
   --  depth, etc), use render buffers.

   Has_Depth_Buffer_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will allocate and enable a depth buffer for
   --  the target framebuffer.
   --
   --  Setting this property will enable GL's depth testing as a side effect.
   --  If you don't need depth testing, you should call
   --  `glDisable(GL_DEPTH_TEST)` in your `GtkGLArea::render` handler.

   Has_Stencil_Buffer_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will allocate and enable a stencil buffer for
   --  the target framebuffer.

   Use_Es_Property : constant Glib.Properties.Property_Boolean;
   --  If set to True the widget will try to create a `GdkGLContext` using
   --  OpenGL ES instead of OpenGL.

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
   --  Emitted when the widget is being realized.
   --
   --  This allows you to override how the GL context is created. This is
   --  useful when you want to reuse an existing GL context, or if you want to
   --  try creating different kinds of GL options.
   --
   --  If context creation fails then the signal handler can use
   --  [methodGtk.GLArea.set_error] to register a more detailed error of how
   --  the construction failed.
   -- 
   --  Callback parameters:

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
   --  Emitted every time the contents of the `GtkGLArea` should be redrawn.
   --
   --  The Context is bound to the Area prior to emitting this function, and
   --  the buffers are painted to the window once the emission terminates.
   -- 
   --  Callback parameters:
   --    --  @param Context the `GdkGLContext` used by Area

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
   --  Emitted once when the widget is realized, and then each time the widget
   --  is changed while realized.
   --
   --  This is useful in order to keep GL state up to date with the widget
   --  size, like for instance camera properties which may depend on the
   --  width/height ratio.
   --
   --  The GL context for the area is guaranteed to be current when this
   --  signal is emitted.
   --
   --  The default handler sets up the GL viewport.
   -- 
   --  Callback parameters:
   --    --  @param Width the width of the viewport
   --    --  @param Height the height of the viewport

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_GLArea_Record, Gtk_GLArea);
   function "+"
     (Widget : access Gtk_GLArea_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_GLArea
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_GLArea_Record, Gtk_GLArea);
   function "+"
     (Widget : access Gtk_GLArea_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_GLArea
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Es_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-es");
   Has_Stencil_Buffer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-stencil-buffer");
   Has_Depth_Buffer_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-depth-buffer");
   Context_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("context");
   Auto_Render_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("auto-render");
   Api_Property : constant Gdk.GLContext.Property_Gdk_GLAPI :=
     Gdk.GLContext.Build ("api");
   Allowed_Apis_Property : constant Gdk.GLContext.Property_Gdk_GLAPI :=
     Gdk.GLContext.Build ("allowed-apis");
end Gtk.GLArea;
