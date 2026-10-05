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

--  Places "overlay" widgets on top of a single main child.
--
--  <picture> <source srcset="overlay-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkOverlay" src="overlay.png"> </picture>
--  The position of each overlay widget is determined by its
--  [propertyGtk.Widget:halign] and [propertyGtk.Widget:valign] properties.
--  E.g. a widget with both alignments set to Gtk.Widget.Align_Start will be
--  placed at the top left corner of the `GtkOverlay` container, whereas an
--  overlay with halign set to Gtk.Widget.Align_Center and valign set to
--  Gtk.Widget.Align_End will be placed a the bottom edge of the `GtkOverlay`,
--  horizontally centered. The position can be adjusted by setting the margin
--  properties of the child to non-zero values.
--
--  More complicated placement of overlays is possible by connecting to the
--  [signalGtk.Overlay::get-child-position] signal.
--
--  An overlay's minimum and natural sizes are those of its main child. The
--  sizes of overlay children are not considered when measuring these preferred
--  sizes.
--
--  # GtkOverlay as GtkBuildable
--
--  The `GtkOverlay` implementation of the `GtkBuildable` interface supports
--  placing a child as an overlay by specifying "overlay" as the "type"
--  attribute of a `<child>` element.
--
--  # CSS nodes
--
--  `GtkOverlay` has a single CSS node with the name "overlay". Overlay
--  children whose alignments cause them to be positioned at an edge get the
--  style classes ".left", ".right", ".top", and/or ".bottom" according to
--  their position.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_overlay.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Overlay is

   type Gtk_Overlay_Record is new Gtk_Widget_Record with null record;
   type Gtk_Overlay is access all Gtk_Overlay_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Overlay);
   procedure Initialize (Self : not null access Gtk_Overlay_Record'Class);
   --  Creates a new `GtkOverlay`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Overlay_New return Gtk_Overlay;
   --  Creates a new `GtkOverlay`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_overlay_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Widget to Overlay.
   --  The widget will be stacked on top of the main widget added with
   --  [methodGtk.Overlay.set_child].
   --  The position at which Widget is placed is determined from its
   --  [propertyGtk.Widget:halign] and [propertyGtk.Widget:valign] properties.
   --  @param Widget a `GtkWidget` to be added to the container

   function Get_Child
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Overlay.
   --  @return the child widget of Overlay. Has transfer-ownership='none'.

   procedure Set_Child
      (Self  : not null access Gtk_Overlay_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Overlay.
   --  @param Child the child widget

   function Get_Clip_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether Widget should be clipped within the parent.
   --  @param Widget an overlay child of `GtkOverlay`
   --  @return whether the widget is clipped within the parent.

   procedure Set_Clip_Overlay
      (Self         : not null access Gtk_Overlay_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Clip_Overlay : Boolean);
   --  Sets whether Widget should be clipped within the parent.
   --  @param Widget an overlay child of `GtkOverlay`
   --  @param Clip_Overlay whether the child should be clipped

   function Get_Measure_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Gets whether Widget's size is included in the measurement of Overlay.
   --  @param Widget an overlay child of `GtkOverlay`
   --  @return whether the widget is measured

   procedure Set_Measure_Overlay
      (Self    : not null access Gtk_Overlay_Record;
       Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Measure : Boolean);
   --  Sets whether Widget is included in the measured size of Overlay.
   --  The overlay will request the size of the largest child that has this
   --  property set to True. Children who are not included may be drawn outside
   --  of Overlay's allocation if they are too large.
   --  @param Widget an overlay child of `GtkOverlay`
   --  @param Measure whether the child should be measured

   procedure Remove_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Removes an overlay that was added with Gtk.Overlay.Add_Overlay.
   --  @param Widget a `GtkWidget` to be removed

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Overlay_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Overlay_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Overlay_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Overlay_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Overlay_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Overlay_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Overlay_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Overlay_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Overlay_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Overlay_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The main child widget.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean is not null access function
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : out Gdk.Rectangle.Gdk_Rectangle)
   return Boolean;

   type Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean is not null access function
     (Self       : access Glib.Object.GObject_Record'Class;
      Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : out Gdk.Rectangle.Gdk_Rectangle)
   return Boolean;

   Signal_Get_Child_Position : constant Glib.Signal_Name := "get-child-position";
   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After : Boolean := False);
   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted to determine the position and size of any overlay child
   --  widgets.
   --
   --  A handler for this signal should fill Allocation with the desired
   --  position and size for Widget, relative to the 'main' child of Overlay.
   --
   --  The default handler for this signal uses the Widget's halign and valign
   --  properties to determine the position and gives the widget its natural
   --  size (except that an alignment of Gtk.Widget.Align_Fill will cause the
   --  overlay to be full-width/height). If the main child is a
   --  `GtkScrolledWindow`, the overlays are placed relative to its contents.
   -- 
   --  Callback parameters:
   --    --  @param Widget the child widget to position
   --    --  @param Allocation return location for the allocation

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Overlay_Record, Gtk_Overlay);
   function "+"
     (Widget : access Gtk_Overlay_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Overlay
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Overlay_Record, Gtk_Overlay);
   function "+"
     (Widget : access Gtk_Overlay_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Overlay
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Overlay_Record, Gtk_Overlay);
   function "+"
     (Widget : access Gtk_Overlay_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Overlay
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Overlay;
