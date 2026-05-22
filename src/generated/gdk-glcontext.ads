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

--  Represents a platform-specific OpenGL draw context.
--
--  `GdkGLContext`s are created for a surface using
--  [methodGdk.Surface.create_gl_context], and the context will match the
--  characteristics of the surface.
--
--  A `GdkGLContext` is not tied to any particular normal framebuffer. For
--  instance, it cannot draw to the surface back buffer. The GDK repaint system
--  is in full control of the painting to that. Instead, you can create render
--  buffers or textures and use [funcCairo_Draw_From_Gl] in the draw function
--  of your widget to draw them. Then GDK will handle the integration of your
--  rendering with that of other widgets.
--
--  Support for `GdkGLContext` is platform-specific and context creation can
--  fail, returning null context.
--
--  A `GdkGLContext` has to be made "current" in order to start using it,
--  otherwise any OpenGL call will be ignored.
--
--  ## Creating a new OpenGL context
--
--  In order to create a new `GdkGLContext` instance you need a `GdkSurface`,
--  which you typically get during the realize call of a widget.
--
--  A `GdkGLContext` is not realized until either
--  [methodGdk.GLContext.make_current] or [methodGdk.GLContext.realize] is
--  called. It is possible to specify details of the GL context like the OpenGL
--  version to be used, or whether the GL context should have extra state
--  validation enabled after calling [methodGdk.Surface.create_gl_context] by
--  calling [methodGdk.GLContext.realize]. If the realization fails you have
--  the option to change the settings of the `GdkGLContext` and try again.
--
--  ## Using a GdkGLContext
--
--  You will need to make the `GdkGLContext` the current context before
--  issuing OpenGL calls; the system sends OpenGL commands to whichever context
--  is current. It is possible to have multiple contexts, so you always need to
--  ensure that the one which you want to draw with is the current one before
--  issuing commands:
--
--  ```c gdk_gl_context_make_current (context); ```
--
--  You can now perform your drawing using OpenGL commands.
--
--  You can check which `GdkGLContext` is the current one by using
--  [funcGdk.GLContext.get_current]; you can also unset any `GdkGLContext` that
--  is currently set by calling [funcGdk.GLContext.clear_current].

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Draw_Context;        use Gdk.Draw_Context;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;

package Gdk.GLContext is

   type Gdk_GLContext_Record is new Gdk_Draw_Context_Record with null record;
   type Gdk_GLContext is access all Gdk_GLContext_Record'Class;

   type Gdk_GLAPI is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_GLAPI);
   --  The list of the different APIs that GdkGLContext can potentially
   --  support.

   Gdk_Gl_Api_Gl : constant Gdk_GLAPI := 1;
   Gdk_Gl_Api_Gles : constant Gdk_GLAPI := 2;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_GLAPI_Properties is
      new Generic_Internal_Discrete_Property (Gdk_GLAPI);
   type Property_Gdk_GLAPI is new Gdk_GLAPI_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_gl_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Allowed_Apis
      (Self : not null access Gdk_GLContext_Record) return Gdk_GLAPI;
   --  Gets the allowed APIs set via Gdk.GLContext.Set_Allowed_Apis.
   --  Since: gtk+ 4.6
   --  @return the allowed APIs

   procedure Set_Allowed_Apis
      (Self : not null access Gdk_GLContext_Record;
       Apis : Gdk_GLAPI);
   --  Sets the allowed APIs. When Gdk.GLContext.Realize is called, only the
   --  allowed APIs will be tried. If you set this to 0, realizing will always
   --  fail.
   --  If you set it on a realized context, the property will not have any
   --  effect. It is only relevant during Gdk.GLContext.Realize.
   --  By default, all APIs are allowed.
   --  Since: gtk+ 4.6
   --  @param Apis the allowed APIs

   function Get_Api
      (Self : not null access Gdk_GLContext_Record) return Gdk_GLAPI;
   --  Gets the API currently in use.
   --  If the renderer has not been realized yet, 0 is returned.
   --  Since: gtk+ 4.6
   --  @return the currently used API

   function Get_Debug_Enabled
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Retrieves whether the context is doing extra validations and runtime
   --  checking.
   --  See [methodGdk.GLContext.set_debug_enabled].
   --  @return True if debugging is enabled

   procedure Set_Debug_Enabled
      (Self    : not null access Gdk_GLContext_Record;
       Enabled : Boolean);
   --  Sets whether the `GdkGLContext` should perform extra validations and
   --  runtime checking.
   --  This is useful during development, but has additional overhead.
   --  The `GdkGLContext` must not be realized or made current prior to
   --  calling this function.
   --  @param Enabled whether to enable debugging in the context

   function Get_Display
      (Self : not null access Gdk_GLContext_Record) return Gdk.Gdk_Display;
   --  Retrieves the display the Context is created for
   --  @return a `GdkDisplay`

   function Get_Forward_Compatible
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Retrieves whether the context is forward-compatible.
   --  See [methodGdk.GLContext.set_forward_compatible].
   --  @return True if the context should be forward-compatible

   procedure Set_Forward_Compatible
      (Self       : not null access Gdk_GLContext_Record;
       Compatible : Boolean);
   --  Sets whether the `GdkGLContext` should be forward-compatible.
   --  Forward-compatible contexts must not support OpenGL functionality that
   --  has been marked as deprecated in the requested version; non-forward
   --  compatible contexts, on the other hand, must support both deprecated and
   --  non deprecated functionality.
   --  The `GdkGLContext` must not be realized or made current prior to
   --  calling this function.
   --  @param Compatible whether the context should be forward-compatible

   procedure Get_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves required OpenGL version set as a requirement for the Context
   --  realization. It will not change even if a greater OpenGL version is
   --  supported and used after the Context is realized. See
   --  [methodGdk.GLContext.get_version] for the real version in use.
   --  See [methodGdk.GLContext.set_required_version].
   --  @param Major return location for the major version to request
   --  @param Minor return location for the minor version to request

   procedure Set_Required_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : Glib.Gint;
       Minor : Glib.Gint);
   --  Sets the major and minor version of OpenGL to request.
   --  Setting Major and Minor to zero will use the default values.
   --  Setting Major and Minor lower than the minimum versions required by GTK
   --  will result in the context choosing the minimum version.
   --  The Context must not be realized or made current prior to calling this
   --  function.
   --  @param Major the major version to request
   --  @param Minor the minor version to request

   function Get_Shared_Context
      (Self : not null access Gdk_GLContext_Record) return Gdk_GLContext;
   pragma Obsolescent (Get_Shared_Context);
   --  Used to retrieves the `GdkGLContext` that this Context share data with.
   --  As many contexts can share data now and no single shared context exists
   --  anymore, this function has been deprecated and now always returns null.
   --  Deprecated since 4.4, 1
   --  @return null

   function Get_Surface
      (Self : not null access Gdk_GLContext_Record) return Gdk.Gdk_Surface;
   --  Retrieves the surface used by the Context.
   --  @return a `GdkSurface`

   function Get_Use_Es
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Checks whether the Context is using an OpenGL or OpenGL ES profile.
   --  @return True if the `GdkGLContext` is using an OpenGL ES profile; False
   --  if other profile is in use of if the Context has not yet been realized.

   procedure Set_Use_Es
      (Self   : not null access Gdk_GLContext_Record;
       Use_Es : Glib.Gint);
   --  Requests that GDK create an OpenGL ES context instead of an OpenGL one.
   --  Not all platforms support OpenGL ES.
   --  The Context must not have been realized.
   --  By default, GDK will attempt to automatically detect whether the
   --  underlying GL implementation is OpenGL or OpenGL ES once the Context is
   --  realized.
   --  You should check the return value of [methodGdk.GLContext.get_use_es]
   --  after calling [methodGdk.GLContext.realize] to decide whether to use the
   --  OpenGL or OpenGL ES API, extensions, or shaders.
   --  @param Use_Es whether the context should use OpenGL ES instead of
   --  OpenGL, or -1 to allow auto-detection

   procedure Get_Version
      (Self  : not null access Gdk_GLContext_Record;
       Major : out Glib.Gint;
       Minor : out Glib.Gint);
   --  Retrieves the OpenGL version of the Context.
   --  The Context must be realized prior to calling this function.
   --  @param Major return location for the major version
   --  @param Minor return location for the minor version

   function Is_Legacy
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Whether the `GdkGLContext` is in legacy mode or not.
   --  The `GdkGLContext` must be realized before calling this function.
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
   --  @return True if the GL context is in legacy mode

   function Is_Shared
      (Self  : not null access Gdk_GLContext_Record;
       Other : not null access Gdk_GLContext_Record'Class) return Boolean;
   --  Checks if the two GL contexts can share resources.
   --  When they can, the texture IDs from Other can be used in Self. This is
   --  particularly useful when passing `GdkGLTexture` objects between
   --  different contexts.
   --  Contexts created for the same display with the same properties will
   --  always be compatible, even if they are created for different surfaces.
   --  For other contexts it depends on the GL backend.
   --  Both contexts must be realized for this check to succeed. If either one
   --  is not, this function will return False.
   --  Since: gtk+ 4.4
   --  @param Other the `GdkGLContext` that should be compatible with Self
   --  @return True if the two GL contexts are compatible.

   procedure Make_Current (Self : not null access Gdk_GLContext_Record);
   --  Makes the Context the current one.

   function Realize
      (Self : not null access Gdk_GLContext_Record) return Boolean;
   --  Realizes the given `GdkGLContext`.
   --  It is safe to call this function on a realized `GdkGLContext`.
   --  @return True if the context is realized

   ---------------
   -- Functions --
   ---------------

   procedure Clear_Current;
   --  Clears the current `GdkGLContext`.
   --  Any OpenGL call after this function returns will be ignored until
   --  [methodGdk.GLContext.make_current] is called.

   function Get_Current return Gdk_GLContext;
   --  Retrieves the current `GdkGLContext`.
   --  @return the current `GdkGLContext`

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Allowed_Apis_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLAPI
   --  The allowed APIs.

   Api_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLAPI
   --  The API currently in use.

   Shared_Context_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GLContext
   --  Always null
   --
   --  As many contexts can share data now and no single shared context exists
   --  anymore, this function has been deprecated and now always returns null.

private
   Shared_Context_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("shared-context");
   Api_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("api");
   Allowed_Apis_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("allowed-apis");
end Gdk.GLContext;
