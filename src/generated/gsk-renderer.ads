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

--  Renders a scene graph defined via a tree of [classGsk.RenderNode]
--  instances.
--
--  Typically you will use a `GskRenderer` instance to repeatedly call
--  [methodGsk.Renderer.render] to update the contents of its associated
--  [classGdk.Surface].
--
--  It is necessary to realize a `GskRenderer` instance using
--  [methodGsk.Renderer.realize] before calling [methodGsk.Renderer.render], in
--  order to create the appropriate windowing system resources needed to render
--  the scene.

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Gdk;             use Gdk;
with Gdk.Display;
with Gdk.Surface;
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Graphene.Rect;   use Graphene.Rect;
with Gsk.Render_Node; use Gsk.Render_Node;

package Gsk.Renderer is

   type Gsk_Renderer_Record is new GObject_Record with null record;
   type Gsk_Renderer is access all Gsk_Renderer_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_For_Surface
      (Self    : out Gsk_Renderer;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class);
   procedure Initialize_For_Surface
      (Self    : not null access Gsk_Renderer_Record'Class;
       Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class);
   --  Creates an appropriate `GskRenderer` instance for the given surface.
   --  If the `GSK_RENDERER` environment variable is set, GSK will try that
   --  renderer first, before trying the backend-specific default. The ultimate
   --  fallback is the cairo renderer.
   --  The renderer will be realized before it is returned.
   --  Initialize_For_Surface does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Surface a surface

   function Gsk_Renderer_New_For_Surface
      (Surface : not null access Gdk.Surface.Gdk_Surface_Record'Class)
       return Gsk_Renderer;
   --  Creates an appropriate `GskRenderer` instance for the given surface.
   --  If the `GSK_RENDERER` environment variable is set, GSK will try that
   --  renderer first, before trying the backend-specific default. The ultimate
   --  fallback is the cairo renderer.
   --  The renderer will be realized before it is returned.
   --  @param Surface a surface

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_renderer_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Surface
      (Self : not null access Gsk_Renderer_Record) return Gdk.Gdk_Surface;
   --  Retrieves the surface that the renderer is associated with.
   --  If the renderer has not been realized yet, `NULL` will be returned.
   --  @return the surface. Has transfer-ownership='none'.

   function Is_Realized
      (Self : not null access Gsk_Renderer_Record) return Boolean;
   --  Checks whether the renderer is realized or not.
   --  @return true if the renderer was realized, false otherwise

   function Realize
      (Self    : not null access Gsk_Renderer_Record;
       Surface : access Gdk.Surface.Gdk_Surface_Record'Class;
       Error   : out Glib.Error.GError) return Boolean;
   --  Creates the resources needed by the renderer.
   --  Since GTK 4.6, the surface may be `NULL`, which allows using renderers
   --  without having to create a surface. Since GTK 4.14, it is recommended to
   --  use [methodGsk.Renderer.realize_for_display] for this case.
   --  Note that it is mandatory to call [methodGsk.Renderer.unrealize] before
   --  destroying the renderer.
   --  @param Surface the surface that renderer will be used on
   --  @param Error the return location for a recoverable error
   --  @return whether the renderer was successfully realized

   function Realize_For_Display
      (Self    : not null access Gsk_Renderer_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       Error   : out Glib.Error.GError) return Boolean;
   --  Creates the resources needed by the renderer.
   --  Note that it is mandatory to call [methodGsk.Renderer.unrealize] before
   --  destroying the renderer.
   --  Since: gtk+ 4.14
   --  @param Display the display that the renderer will be used on
   --  @param Error the return location for a recoverable error
   --  @return whether the renderer was successfully realized

   procedure Render
      (Self   : not null access Gsk_Renderer_Record;
       Root   : Gsk.Render_Node.Gsk_Render_Node;
       Region : Cairo.Region.Cairo_Region);
   --  Renders the scene graph, described by a tree of `GskRenderNode`
   --  instances to the renderer's surface, ensuring that the given region gets
   --  redrawn.
   --  If the renderer has no associated surface, this function does nothing.
   --  Renderers must ensure that changes of the contents given by the Root
   --  node as well as the area given by Region are redrawn. They are however
   --  free to not redraw any pixel outside of Region if they can guarantee
   --  that it didn't change.
   --  The renderer will acquire a reference on the `GskRenderNode` tree while
   --  the rendering is in progress.
   --  @param Root the render node to render
   --  @param Region the `cairo_region_t` that must be redrawn or `NULL` for
   --  the whole surface

   function Render_Texture
      (Self     : not null access Gsk_Renderer_Record;
       Root     : Gsk.Render_Node.Gsk_Render_Node;
       Viewport : access Graphene.Rect.Graphene_Rect_T)
       return Gdk.Texture.Gdk_Texture;
   --  Renders a scene graph, described by a tree of `GskRenderNode`
   --  instances, to a texture.
   --  The renderer will acquire a reference on the `GskRenderNode` tree while
   --  the rendering is in progress.
   --  If you want to apply any transformations to Root, you should put it
   --  into a transform node and pass that node instead.
   --  @param Root the render node to render
   --  @param Viewport the section to draw or `NULL` to use Root's bounds
   --  @return a texture with the rendered contents of Root. Has
   --  transfer-ownership='full'.

   procedure Unrealize (Self : not null access Gsk_Renderer_Record);
   --  Releases all the resources created by [methodGsk.Renderer.realize].

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Realized_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the renderer has been associated with a surface or draw
   --  context.

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Surface
   --  The surface associated with renderer.

private
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Realized_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("realized");
end Gsk.Renderer;
