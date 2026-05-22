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

--  Base class for objects implementing different rendering methods.
--
--  `GdkDrawContext` is the base object used by contexts implementing
--  different rendering methods, such as [classGdk.CairoContext] or
--  [classGdk.GLContext]. It provides shared functionality between those
--  contexts.
--
--  You will always interact with one of those subclasses.
--
--  A `GdkDrawContext` is always associated with a single toplevel surface.

pragma Warnings (Off, "*is already use-visible*");
with Cairo.Region;    use Cairo.Region;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;

package Gdk.Draw_Context is

   type Gdk_Draw_Context_Record is new GObject_Record with null record;
   type Gdk_Draw_Context is access all Gdk_Draw_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_draw_context_get_type");

   -------------
   -- Methods --
   -------------

   procedure Begin_Frame
      (Self   : not null access Gdk_Draw_Context_Record;
       Region : Cairo.Region.Cairo_Region);
   pragma Obsolescent (Begin_Frame);
   --  Indicates that you are beginning the process of redrawing Region on the
   --  Context's surface.
   --  Calling this function begins a drawing operation using Context on the
   --  surface that Context was created from. The actual requirements and
   --  guarantees for the drawing operation vary for different implementations
   --  of drawing, so a [classGdk.CairoContext] and a [classGdk.GLContext] need
   --  to be treated differently.
   --  A call to this function is a requirement for drawing and must be
   --  followed by a call to [methodGdk.DrawContext.end_frame], which will
   --  complete the drawing operation and ensure the contents become visible on
   --  screen.
   --  Note that the Region passed to this function is the minimum region that
   --  needs to be drawn and depending on implementation, windowing system and
   --  hardware in use, it might be necessary to draw a larger region. Drawing
   --  implementation must use [methodGdk.DrawContext.get_frame_region] to
   --  query the region that must be drawn.
   --  When using GTK, the widget system automatically places calls to
   --  Gdk.Draw_Context.Begin_Frame and Gdk.Draw_Context.End_Frame via the use
   --  of [GskRenderer](../gsk4/class.Renderer.html)s, so application code does
   --  not need to call these functions explicitly.
   --  Deprecated since 4.16, 1
   --  @param Region minimum region that should be drawn

   procedure End_Frame (Self : not null access Gdk_Draw_Context_Record);
   pragma Obsolescent (End_Frame);
   --  Ends a drawing operation started with Gdk.Draw_Context.Begin_Frame.
   --  This makes the drawing available on screen. See
   --  [methodGdk.DrawContext.begin_frame] for more details about drawing.
   --  When using a [classGdk.GLContext], this function may call `glFlush`
   --  implicitly before returning; it is not recommended to call `glFlush`
   --  explicitly before calling this function.
   --  Deprecated since 4.16, 1

   function Get_Display
      (Self : not null access Gdk_Draw_Context_Record)
       return Gdk.Gdk_Display;
   --  Retrieves the `GdkDisplay` the Context is created for
   --  @return the `GdkDisplay`

   function Get_Frame_Region
      (Self : not null access Gdk_Draw_Context_Record)
       return Cairo.Region.Cairo_Region;
   pragma Obsolescent (Get_Frame_Region);
   --  Retrieves the region that is currently being repainted.
   --  After a call to [methodGdk.DrawContext.begin_frame] this function will
   --  return a union of the region passed to that function and the area of the
   --  surface that the Context determined needs to be repainted.
   --  If Context is not in between calls to
   --  [methodGdk.DrawContext.begin_frame] and
   --  [methodGdk.DrawContext.end_frame], null will be returned.
   --  Deprecated since 4.16, 1
   --  @return a Cairo region

   function Is_In_Frame
      (Self : not null access Gdk_Draw_Context_Record) return Boolean;
   pragma Obsolescent (Is_In_Frame);
   --  Returns True if Context is in the process of drawing to its surface.
   --  This is the case between calls to [methodGdk.DrawContext.begin_frame]
   --  and [methodGdk.DrawContext.end_frame]. In this situation, drawing
   --  commands may be effecting the contents of the Context's surface.
   --  Deprecated since 4.16, 1
   --  @return True if the context is between
   --  [methodGdk.DrawContext.begin_frame] and
   --  [methodGdk.DrawContext.end_frame] calls.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The `GdkDisplay` used to create the `GdkDrawContext`.

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Surface
   --  The `GdkSurface` the context is bound to.

private
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.Draw_Context;
