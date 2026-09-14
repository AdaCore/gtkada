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

--  Allows drawing with cairo.
--
--  <picture> <source srcset="drawingarea-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkDrawingArea"
--  src="drawingarea.png"> </picture>
--  It's essentially a blank widget; you can draw on it. After creating a
--  drawing area, the application may want to connect to:
--
--  - The [signalGtk.Widget::realize] signal to take any necessary actions
--  when the widget is instantiated on a particular display. (Create GDK
--  resources in response to this signal.)
--
--  - The [signalGtk.DrawingArea::resize] signal to take any necessary actions
--  when the widget changes size.
--
--  - Call [methodGtk.DrawingArea.set_draw_func] to handle redrawing the
--  contents of the widget.
--
--  The following code portion demonstrates using a drawing area to display a
--  circle in the normal widget foreground color.
--
--  ## Simple GtkDrawingArea usage
--
--  ```c static void draw_function (GtkDrawingArea *area, cairo_t *cr, int
--  width, int height, gpointer data) { GdkRGBA color;
--
--  cairo_arc (cr, width / 2.0, height / 2.0, MIN (width, height) / 2.0, 0, 2
--  * G_PI);
--
--  gtk_widget_get_color (GTK_WIDGET (area), &color);
--  gdk_cairo_set_source_rgba (cr, &color);
--
--  cairo_fill (cr); }
--
--  int main (int argc, char **argv) { gtk_init ();
--
--  GtkWidget *area = gtk_drawing_area_new ();
--  gtk_drawing_area_set_content_width (GTK_DRAWING_AREA (area), 100);
--  gtk_drawing_area_set_content_height (GTK_DRAWING_AREA (area), 100);
--  gtk_drawing_area_set_draw_func (GTK_DRAWING_AREA (area), draw_function,
--  NULL, NULL); return 0; } ```
--
--  The draw function is normally called when a drawing area first comes
--  onscreen, or when it's covered by another window and then uncovered. You
--  can also force a redraw by adding to the "damage region" of the drawing
--  area's window using [methodGtk.Widget.queue_draw]. This will cause the
--  drawing area to call the draw function again.
--
--  The available routines for drawing are documented in the [Cairo
--  documentation](https://www.cairographics.org/manual/); GDK offers
--  additional API to integrate with Cairo, like
--  [funcGdk.cairo_set_source_rgba] or [funcGdk.cairo_set_source_pixbuf].
--
--  To receive mouse events on a drawing area, you will need to use event
--  controllers. To receive keyboard events, you will need to set the
--  "can-focus" property on the drawing area, and you should probably draw some
--  user-visible indication that the drawing area is focused.
--
--  If you need more complex control over your widget, you should consider
--  creating your own `GtkWidget` subclass.
--
--  <gtkada_demo>create_drawing_area.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                 use Cairo;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Drawing_Area is

   type Gtk_Drawing_Area_Record is new Gtk_Widget_Record with null record;
   type Gtk_Drawing_Area is access all Gtk_Drawing_Area_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Drawing_Area_Draw_Func is access procedure
     (Drawing_Area : not null access Gtk_Drawing_Area_Record'Class;
      Cr           : Cairo.Cairo_Context;
      Width        : Glib.Gint;
      Height       : Glib.Gint);
   --  Whenever Drawing_Area needs to redraw, this function will be called.
   --  This function should exclusively redraw the contents of the drawing
   --  area and must not call any widget functions that cause changes.
   --  @param Drawing_Area the `GtkDrawingArea` to redraw
   --  @param Cr the context to draw to
   --  @param Width the actual width of the contents. This value will be at
   --  least as wide as GtkDrawingArea:width.
   --  @param Height the actual height of the contents. This value will be at
   --  least as wide as GtkDrawingArea:height.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Drawing_Area);
   procedure Initialize
      (Self : not null access Gtk_Drawing_Area_Record'Class);
   --  Creates a new drawing area.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Drawing_Area_New return Gtk_Drawing_Area;
   --  Creates a new drawing area.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_drawing_area_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Content_Height
      (Self : not null access Gtk_Drawing_Area_Record) return Glib.Gint;
   --  Retrieves the content height of the `GtkDrawingArea`.
   --  @return The height requested for content of the drawing area

   procedure Set_Content_Height
      (Self   : not null access Gtk_Drawing_Area_Record;
       Height : Glib.Gint);
   --  Sets the desired height of the contents of the drawing area.
   --  Note that because widgets may be allocated larger sizes than they
   --  requested, it is possible that the actual height passed to your draw
   --  function is larger than the height set here. You can use
   --  [methodGtk.Widget.set_valign] to avoid that.
   --  If the height is set to 0 (the default), the drawing area may
   --  disappear.
   --  @param Height the height of contents

   function Get_Content_Width
      (Self : not null access Gtk_Drawing_Area_Record) return Glib.Gint;
   --  Retrieves the content width of the `GtkDrawingArea`.
   --  @return The width requested for content of the drawing area

   procedure Set_Content_Width
      (Self  : not null access Gtk_Drawing_Area_Record;
       Width : Glib.Gint);
   --  Sets the desired width of the contents of the drawing area.
   --  Note that because widgets may be allocated larger sizes than they
   --  requested, it is possible that the actual width passed to your draw
   --  function is larger than the width set here. You can use
   --  [methodGtk.Widget.set_halign] to avoid that.
   --  If the width is set to 0 (the default), the drawing area may disappear.
   --  @param Width the width of contents

   procedure Set_Draw_Func
      (Self      : not null access Gtk_Drawing_Area_Record;
       Draw_Func : Gtk_Drawing_Area_Draw_Func);
   --  Setting a draw function is the main thing you want to do when using a
   --  drawing area.
   --  The draw function is called whenever GTK needs to draw the contents of
   --  the drawing area to the screen.
   --  The draw function will be called during the drawing stage of GTK. In
   --  the drawing stage it is not allowed to change properties of any GTK
   --  widgets or call any functions that would cause any properties to be
   --  changed. You should restrict yourself exclusively to drawing your
   --  contents in the draw function.
   --  If what you are drawing does change, call [methodGtk.Widget.queue_draw]
   --  on the drawing area. This will cause a redraw and will call Draw_Func
   --  again.
   --  @param Draw_Func callback that lets you draw the drawing area's
   --  contents

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Draw_Func_User_Data is

      type Gtk_Drawing_Area_Draw_Func is access procedure
        (Drawing_Area : not null access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
         Cr           : Cairo.Cairo_Context;
         Width        : Glib.Gint;
         Height       : Glib.Gint;
         User_Data    : User_Data_Type);
      --  Whenever Drawing_Area needs to redraw, this function will be called.
      --  This function should exclusively redraw the contents of the drawing
      --  area and must not call any widget functions that cause changes.
      --  @param Drawing_Area the `GtkDrawingArea` to redraw
      --  @param Cr the context to draw to
      --  @param Width the actual width of the contents. This value will be at
      --  least as wide as GtkDrawingArea:width.
      --  @param Height the actual height of the contents. This value will be at
      --  least as wide as GtkDrawingArea:height.
      --  @param User_Data user data

      procedure Set_Draw_Func
         (Self      : not null access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
          Draw_Func : Gtk_Drawing_Area_Draw_Func;
          User_Data : User_Data_Type);
      --  Setting a draw function is the main thing you want to do when using
      --  a drawing area.
      --  The draw function is called whenever GTK needs to draw the contents
      --  of the drawing area to the screen.
      --  The draw function will be called during the drawing stage of GTK. In
      --  the drawing stage it is not allowed to change properties of any GTK
      --  widgets or call any functions that would cause any properties to be
      --  changed. You should restrict yourself exclusively to drawing your
      --  contents in the draw function.
      --  If what you are drawing does change, call
      --  [methodGtk.Widget.queue_draw] on the drawing area. This will cause a
      --  redraw and will call Draw_Func again.
      --  @param Draw_Func callback that lets you draw the drawing area's
      --  contents
      --  @param User_Data user data passed to Draw_Func

   end Set_Draw_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Drawing_Area_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Drawing_Area_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Drawing_Area_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Drawing_Area_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Drawing_Area_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Drawing_Area_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Drawing_Area_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Drawing_Area_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Drawing_Area_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Content_Height_Property : constant Glib.Properties.Property_Int;
   --  The content height.

   Content_Width_Property : constant Glib.Properties.Property_Int;
   --  The content width.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Drawing_Area_Gint_Gint_Void is not null access procedure
     (Self   : access Gtk_Drawing_Area_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   type Cb_GObject_Gint_Gint_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Width  : Glib.Gint;
      Height : Glib.Gint);

   Signal_Resize : constant Glib.Signal_Name := "resize";
   procedure On_Resize
      (Self  : not null access Gtk_Drawing_Area_Record;
       Call  : Cb_Gtk_Drawing_Area_Gint_Gint_Void;
       After : Boolean := False);
   procedure On_Resize
      (Self  : not null access Gtk_Drawing_Area_Record;
       Call  : Cb_GObject_Gint_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted once when the widget is realized, and then each time the widget
   --  is changed while realized.
   --
   --  This is useful in order to keep state up to date with the widget size,
   --  like for instance a backing surface.
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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Drawing_Area
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Drawing_Area
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Drawing_Area_Record, Gtk_Drawing_Area);
   function "+"
     (Widget : access Gtk_Drawing_Area_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Drawing_Area
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Content_Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("content-width");
   Content_Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("content-height");
end Gtk.Drawing_Area;
