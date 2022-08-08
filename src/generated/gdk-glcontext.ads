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
--  Gdk.GLContext.Gdk_GLContext is an object representing the
--  platform-specific OpenGL drawing context.
--
--  Gdk_GLContexts are created for a Gdk.Gdk_Window using
--  Gdk.Window.Create_Gl_Context, and the context will match the
--  Gdk.Visual.Gdk_Visual of the window.
--
--  A Gdk.GLContext.Gdk_GLContext is not tied to any particular normal
--  framebuffer. For instance, it cannot draw to the Gdk.Gdk_Window back
--  buffer. The GDK repaint system is in full control of the painting to that.
--  Instead, you can create render buffers or textures and use
--  gdk_cairo_draw_from_gl in the draw function of your widget to draw them.
--  Then GDK will handle the integration of your rendering with that of other
--  widgets.
--
--  Support for Gdk.GLContext.Gdk_GLContext is platform-specific, context
--  creation can fail, returning null context.
--
--  A Gdk.GLContext.Gdk_GLContext has to be made "current" in order to start
--  using it, otherwise any OpenGL call will be ignored.
--
--  ## Creating a new OpenGL context ##
--
--  In order to create a new Gdk.GLContext.Gdk_GLContext instance you need a
--  Gdk.Gdk_Window, which you typically get during the realize call of a
--  widget.
--
--  A Gdk.GLContext.Gdk_GLContext is not realized until either
--  Gdk.GLContext.Make_Current, or until it is realized using
--  Gdk.GLContext.Realize. It is possible to specify details of the GL context
--  like the OpenGL version to be used, or whether the GL context should have
--  extra state validation enabled after calling Gdk.Window.Create_Gl_Context
--  by calling Gdk.GLContext.Realize. If the realization fails you have the
--  option to change the settings of the Gdk.GLContext.Gdk_GLContext and try
--  again.
--
--  ## Using a GdkGLContext ##
--
--  You will need to make the Gdk.GLContext.Gdk_GLContext the current context
--  before issuing OpenGL calls; the system sends OpenGL commands to whichever
--  context is current. It is possible to have multiple contexts, so you always
--  need to ensure that the one which you want to draw with is the current one
--  before issuing commands:
--
--  |[<!-- language="C" --> gdk_gl_context_make_current (context); ]|
--
--  You can now perform your drawing using OpenGL commands.
--
--  You can check which Gdk.GLContext.Gdk_GLContext is the current one by
--  using Gdk.GLContext.Get_Current; you can also unset any
--  Gdk.GLContext.Gdk_GLContext that is currently set by calling
--  Gdk.GLContext.Clear_Current.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Display;     use Gdk.Display;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.GLContext is

   type Gdk_GLContext_Record is new GObject_Record with null record;
   type Gdk_GLContext is access all Gdk_GLContext_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_gl_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Debug_Enabled
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Retrieves the value set using Gdk.GLContext.Set_Debug_Enabled.
   --  Since: gtk+ 3.16

   procedure Set_Debug_Enabled
      (Self    : not null access Gdk_GLContext_Record;
       Enabled : Boolean);
   --  Sets whether the Gdk.GLContext.Gdk_GLContext should perform extra
   --  validations and run time checking. This is useful during development,
   --  but has additional overhead.
   --  The Gdk.GLContext.Gdk_GLContext must not be realized or made current
   --  prior to calling this function.
   --  Since: gtk+ 3.16
   --  "enabled": whether to enable debugging in the context

   function Get_Display
      (Self : not null access Gdk_GLContext_Record)
       return Gdk.Display.Gdk_Display;
   --  Retrieves the Gdk.Display.Gdk_Display the Context is created for
   --  Since: gtk+ 3.16

   function Get_Forward_Compatible
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Retrieves the value set using Gdk.GLContext.Set_Forward_Compatible.
   --  Since: gtk+ 3.16

   procedure Set_Forward_Compatible
      (Self       : not null access Gdk_GLContext_Record;
       Compatible : Boolean);
   --  Sets whether the Gdk.GLContext.Gdk_GLContext should be forward
   --  compatible.
   --  Forward compatibile contexts must not support OpenGL functionality that
   --  has been marked as deprecated in the requested version; non-forward
   --  compatible contexts, on the other hand, must support both deprecated and
   --  non deprecated functionality.
   --  The Gdk.GLContext.Gdk_GLContext must not be realized or made current
   --  prior to calling this function.
   --  Since: gtk+ 3.16
   --  "compatible": whether the context should be forward compatible

   procedure Get_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves the major and minor version requested by calling
   --  Gdk.GLContext.Set_Required_Version.
   --  Since: gtk+ 3.16
   --  "major": return location for the major version to request
   --  "minor": return location for the minor version to request

   procedure Set_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint);
   --  Sets the major and minor version of OpenGL to request.
   --  Setting Major and Minor to zero will use the default values.
   --  The Gdk.GLContext.Gdk_GLContext must not be realized or made current
   --  prior to calling this function.
   --  Since: gtk+ 3.16
   --  "major": the major version to request
   --  "minor": the minor version to request

   function Get_Shared_Context
      (Self : not null access Gdk_GLContext_Record) return Gdk_GLContext;
   --  Retrieves the Gdk.GLContext.Gdk_GLContext that this Context share data
   --  with.
   --  Since: gtk+ 3.16

   function Get_Use_Es
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Checks whether the Context is using an OpenGL or OpenGL ES profile.
   --  Since: gtk+ 3.22

   procedure Set_Use_Es
      (Self   : not null access Gdk_GLContext_Record;
       Use_Es : Glib.Gint);
   --  Requests that GDK create a OpenGL ES context instead of an OpenGL one,
   --  if the platform and windowing system allows it.
   --  The Context must not have been realized.
   --  By default, GDK will attempt to automatically detect whether the
   --  underlying GL implementation is OpenGL or OpenGL ES once the Context is
   --  realized.
   --  You should check the return value of Gdk.GLContext.Get_Use_Es after
   --  calling Gdk.GLContext.Realize to decide whether to use the OpenGL or
   --  OpenGL ES API, extensions, or shaders.
   --  Since: gtk+ 3.22
   --  "use_es": whether the context should use OpenGL ES instead of OpenGL,
   --  or -1 to allow auto-detection

   procedure Get_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves the OpenGL version of the Context.
   --  The Context must be realized prior to calling this function.
   --  Since: gtk+ 3.16
   --  "major": return location for the major version
   --  "minor": return location for the minor version

   function Get_Window
      (Self : not null access Gdk_GLContext_Record) return Gdk.Gdk_Window;
   --  Retrieves the Gdk.Gdk_Window used by the Context.
   --  Since: gtk+ 3.16

   function Is_Legacy
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Whether the Gdk.GLContext.Gdk_GLContext is in legacy mode or not.
   --  The Gdk.GLContext.Gdk_GLContext must be realized before calling this
   --  function.
   --  When realizing a GL context, GDK will try to use the OpenGL 3.2 core
   --  profile; this profile removes all the OpenGL API that was deprecated
   --  prior to the 3.2 version of the specification. If the realization is
   --  successful, this function will return False.
   --  If the underlying OpenGL implementation does not support core profiles,
   --  GDK will fall back to a pre-3.2 compatibility profile, and this function
   --  will return True.
   --  You can use the value returned by this function to decide which kind of
   --  OpenGL API to use, or whether to do extension discovery, or what kind of
   --  shader programs to load.
   --  Since: gtk+ 3.20

   procedure Make_Current (Self : not null access Gdk_GLContext_Record);
   --  Makes the Context the current one.
   --  Since: gtk+ 3.16

   function Realize
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Realizes the given Gdk.GLContext.Gdk_GLContext.
   --  It is safe to call this function on a realized
   --  Gdk.GLContext.Gdk_GLContext.
   --  Since: gtk+ 3.16

   ---------------
   -- Functions --
   ---------------

   procedure Clear_Current;
   --  Clears the current Gdk.GLContext.Gdk_GLContext.
   --  Any OpenGL call after this function returns will be ignored until
   --  Gdk.GLContext.Make_Current is called.
   --  Since: gtk+ 3.16

   function Get_Current return Gdk_GLContext;
   --  Retrieves the current Gdk.GLContext.Gdk_GLContext.
   --  Since: gtk+ 3.16

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The Gdk.Display.Gdk_Display used to create the
   --  Gdk.GLContext.Gdk_GLContext.

   Shared_Context_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLContext
   --  The Gdk.GLContext.Gdk_GLContext that this context is sharing data with,
   --  or null

   Window_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Window.Gtk_Window
   --  The Gdk.Gdk_Window the gl context is bound to.

private
   Window_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("window");
   Shared_Context_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("shared-context");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.GLContext;
