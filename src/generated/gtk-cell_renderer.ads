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

--  An object for rendering a single cell
--
--  The `GtkCellRenderer` is a base class of a set of objects used for
--  rendering a cell to a `cairo_t`. These objects are used primarily by the
--  `GtkTreeView` widget, though they aren't tied to them in any specific way.
--  It is worth noting that `GtkCellRenderer` is not a `GtkWidget` and cannot
--  be treated as such.
--
--  The primary use of a `GtkCellRenderer` is for drawing a certain graphical
--  elements on a `cairo_t`. Typically, one cell renderer is used to draw many
--  cells on the screen. To this extent, it isn't expected that a CellRenderer
--  keep any permanent state around. Instead, any state is set just prior to
--  use using `GObject`s property system. Then, the cell is measured using
--  Gtk.Cell_Renderer.Get_Preferred_Size. Finally, the cell is rendered in the
--  correct location using gtk_cell_renderer_snapshot.
--
--  There are a number of rules that must be followed when writing a new
--  `GtkCellRenderer`. First and foremost, it's important that a certain set of
--  properties will always yield a cell renderer of the same size, barring a
--  style change. The `GtkCellRenderer` also has a number of generic properties
--  that are expected to be honored by all children.
--
--  Beyond merely rendering a cell, cell renderers can optionally provide
--  active user interface elements. A cell renderer can be "activatable" like
--  `GtkCellRenderer`Toggle, which toggles when it gets activated by a mouse
--  click, or it can be "editable" like `GtkCellRenderer`Text, which allows the
--  user to edit the text using a widget implementing the `GtkCellEditable`
--  interface, e.g. `GtkEntry`. To make a cell renderer activatable or
--  editable, you have to implement the `GtkCellRenderer`Class.activate or
--  `GtkCellRenderer`Class.start_editing virtual functions, respectively.
--
--  Many properties of `GtkCellRenderer` and its subclasses have a
--  corresponding "set" property, e.g. "cell-background-set" corresponds to
--  "cell-background". These "set" properties reflect whether a property has
--  been set or not. You should not set them independently.
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Glist;              use Glib.Glist;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gtk.Cell_Editable;       use Gtk.Cell_Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;

package Gtk.Cell_Renderer is

   pragma Obsolescent;
   --  List views use widgets for displaying their contents

   type Gtk_Cell_Renderer_Record is new GObject_Record with null record;
   type Gtk_Cell_Renderer is access all Gtk_Cell_Renderer_Record'Class;

   type Gtk_Cell_Renderer_State is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Cell_Renderer_State);
   --  Tells how a cell is to be rendered.

   Cell_Renderer_Selected : constant Gtk_Cell_Renderer_State := 1;
   Cell_Renderer_Prelit : constant Gtk_Cell_Renderer_State := 2;
   Cell_Renderer_Insensitive : constant Gtk_Cell_Renderer_State := 4;
   Cell_Renderer_Sorted : constant Gtk_Cell_Renderer_State := 8;
   Cell_Renderer_Focused : constant Gtk_Cell_Renderer_State := 16;
   Cell_Renderer_Expandable : constant Gtk_Cell_Renderer_State := 32;
   Cell_Renderer_Expanded : constant Gtk_Cell_Renderer_State := 64;

   type Gtk_Cell_Renderer_Mode is (
      Cell_Renderer_Mode_Inert,
      Cell_Renderer_Mode_Activatable,
      Cell_Renderer_Mode_Editable);
   pragma Convention (C, Gtk_Cell_Renderer_Mode);
   --  Identifies how the user can interact with a particular cell.

   function Convert (R : Gtk.Cell_Renderer.Gtk_Cell_Renderer) return System.Address;
   function Convert (R : System.Address) return Gtk.Cell_Renderer.Gtk_Cell_Renderer;
   package Cell_Renderer_List is new Generic_List (Gtk.Cell_Renderer.Gtk_Cell_Renderer);

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Cell_Renderer_State_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Cell_Renderer_State);
   type Property_Gtk_Cell_Renderer_State is new Gtk_Cell_Renderer_State_Properties.Property;

   package Gtk_Cell_Renderer_Mode_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Cell_Renderer_Mode);
   type Property_Gtk_Cell_Renderer_Mode is new Gtk_Cell_Renderer_Mode_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Aligned_Area
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Flags        : Gtk_Cell_Renderer_State;
       Cell_Area    : Gdk.Rectangle.Gdk_Rectangle;
       Aligned_Area : out Gdk.Rectangle.Gdk_Rectangle);
   pragma Obsolescent (Get_Aligned_Area);
   --  Gets the aligned area used by Cell inside Cell_Area. Used for finding
   --  the appropriate edit and focus rectangle.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Flags render flags
   --  @param Cell_Area cell area which would be passed to
   --  gtk_cell_renderer_render
   --  @param Aligned_Area the return location for the space inside Cell_Area
   --  that would actually be used to render.

   procedure Get_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : out Interfaces.C.C_float;
       Yalign : out Interfaces.C.C_float);
   pragma Obsolescent (Get_Alignment);
   --  Fills in Xalign and Yalign with the appropriate values of Cell.
   --  Deprecated since 4.10, 1
   --  @param Xalign location to fill in with the x alignment of the cell
   --  @param Yalign location to fill in with the y alignment of the cell

   procedure Set_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : Interfaces.C.C_float;
       Yalign : Interfaces.C.C_float);
   pragma Obsolescent (Set_Alignment);
   --  Sets the renderer's alignment within its available space.
   --  Deprecated since 4.10, 1
   --  @param Xalign the x alignment of the cell renderer
   --  @param Yalign the y alignment of the cell renderer

   procedure Get_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   pragma Obsolescent (Get_Fixed_Size);
   --  Fills in Width and Height with the appropriate size of Cell.
   --  Deprecated since 4.10, 1
   --  @param Width location to fill in with the fixed width of the cell
   --  @param Height location to fill in with the fixed height of the cell

   procedure Set_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   pragma Obsolescent (Set_Fixed_Size);
   --  Sets the renderer size to be explicit, independent of the properties
   --  set.
   --  Deprecated since 4.10, 1
   --  @param Width the width of the cell renderer, or -1
   --  @param Height the height of the cell renderer, or -1

   function Get_Is_Expanded
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   pragma Obsolescent (Get_Is_Expanded);
   --  Checks whether the given `GtkCellRenderer` is expanded.
   --  Deprecated since 4.10, 1
   --  @return True if the cell renderer is expanded

   procedure Set_Is_Expanded
      (Cell        : not null access Gtk_Cell_Renderer_Record;
       Is_Expanded : Boolean);
   pragma Obsolescent (Set_Is_Expanded);
   --  Sets whether the given `GtkCellRenderer` is expanded.
   --  Deprecated since 4.10, 1
   --  @param Is_Expanded whether Cell should be expanded

   function Get_Is_Expander
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   pragma Obsolescent (Get_Is_Expander);
   --  Checks whether the given `GtkCellRenderer` is an expander.
   --  Deprecated since 4.10, 1
   --  @return True if Cell is an expander, and False otherwise

   procedure Set_Is_Expander
      (Cell        : not null access Gtk_Cell_Renderer_Record;
       Is_Expander : Boolean);
   pragma Obsolescent (Set_Is_Expander);
   --  Sets whether the given `GtkCellRenderer` is an expander.
   --  Deprecated since 4.10, 1
   --  @param Is_Expander whether Cell is an expander

   procedure Get_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : out Glib.Gint;
       Ypad : out Glib.Gint);
   pragma Obsolescent (Get_Padding);
   --  Fills in Xpad and Ypad with the appropriate values of Cell.
   --  Deprecated since 4.10, 1
   --  @param Xpad location to fill in with the x padding of the cell
   --  @param Ypad location to fill in with the y padding of the cell

   procedure Set_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : Glib.Gint;
       Ypad : Glib.Gint);
   pragma Obsolescent (Set_Padding);
   --  Sets the renderer's padding.
   --  Deprecated since 4.10, 1
   --  @param Xpad the x padding of the cell renderer
   --  @param Ypad the y padding of the cell renderer

   procedure Get_Preferred_Height
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height);
   --  Retrieves a renderer's natural size when rendered to Widget.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Minimum_Size location to store the minimum size
   --  @param Natural_Size location to store the natural size

   procedure Get_Preferred_Height_For_Width
      (Cell           : not null access Gtk_Cell_Renderer_Record;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Height_For_Width);
   --  Retrieves a cell renderers's minimum and natural height if it were
   --  rendered to Widget with the specified Width.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Width the size which is available for allocation
   --  @param Minimum_Height location for storing the minimum size
   --  @param Natural_Height location for storing the preferred size

   procedure Get_Preferred_Size
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Gtk.Widget.Gtk_Requisition;
       Natural_Size : out Gtk.Widget.Gtk_Requisition);
   pragma Obsolescent (Get_Preferred_Size);
   --  Retrieves the minimum and natural size of a cell taking into account
   --  the widget's preference for height-for-width management.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Minimum_Size location for storing the minimum size
   --  @param Natural_Size location for storing the natural size

   procedure Get_Preferred_Width
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width);
   --  Retrieves a renderer's natural size when rendered to Widget.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Minimum_Size location to store the minimum size
   --  @param Natural_Size location to store the natural size

   procedure Get_Preferred_Width_For_Height
      (Cell          : not null access Gtk_Cell_Renderer_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   pragma Obsolescent (Get_Preferred_Width_For_Height);
   --  Retrieves a cell renderers's minimum and natural width if it were
   --  rendered to Widget with the specified Height.
   --  Deprecated since 4.10, 1
   --  @param Widget the `GtkWidget` this cell will be rendering to
   --  @param Height the size which is available for allocation
   --  @param Minimum_Width location for storing the minimum size
   --  @param Natural_Width location for storing the preferred size

   function Get_Request_Mode
      (Cell : not null access Gtk_Cell_Renderer_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   pragma Obsolescent (Get_Request_Mode);
   --  Gets whether the cell renderer prefers a height-for-width layout or a
   --  width-for-height layout.
   --  Deprecated since 4.10, 1
   --  @return The `GtkSizeRequestMode` preferred by this renderer.

   function Get_Sensitive
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   pragma Obsolescent (Get_Sensitive);
   --  Returns the cell renderer's sensitivity.
   --  Deprecated since 4.10, 1
   --  @return True if the cell renderer is sensitive

   procedure Set_Sensitive
      (Cell      : not null access Gtk_Cell_Renderer_Record;
       Sensitive : Boolean);
   pragma Obsolescent (Set_Sensitive);
   --  Sets the cell renderer's sensitivity.
   --  Deprecated since 4.10, 1
   --  @param Sensitive the sensitivity of the cell

   function Get_State
      (Cell       : not null access Gtk_Cell_Renderer_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_State : Gtk_Cell_Renderer_State)
       return Gtk.Enums.Gtk_State_Flags;
   pragma Obsolescent (Get_State);
   --  Translates the cell renderer state to `GtkStateFlags`, based on the
   --  cell renderer and widget sensitivity, and the given
   --  `GtkCellRenderer`State.
   --  Deprecated since 4.10, 1
   --  @param Widget a `GtkWidget`
   --  @param Cell_State cell renderer state
   --  @return the widget state flags applying to Cell

   function Get_Visible
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   pragma Obsolescent (Get_Visible);
   --  Returns the cell renderer's visibility.
   --  Deprecated since 4.10, 1
   --  @return True if the cell renderer is visible

   procedure Set_Visible
      (Cell    : not null access Gtk_Cell_Renderer_Record;
       Visible : Boolean);
   pragma Obsolescent (Set_Visible);
   --  Sets the cell renderer's visibility.
   --  Deprecated since 4.10, 1
   --  @param Visible the visibility of the cell

   function Is_Activatable
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   pragma Obsolescent (Is_Activatable);
   --  Checks whether the cell renderer can do something when activated.
   --  Deprecated since 4.10, 1
   --  @return True if the cell renderer can do anything when activated

   procedure Stop_Editing
      (Cell     : not null access Gtk_Cell_Renderer_Record;
       Canceled : Boolean);
   pragma Obsolescent (Stop_Editing);
   --  Informs the cell renderer that the editing is stopped. If Canceled is
   --  True, the cell renderer will emit the
   --  `GtkCellRenderer`::editing-canceled signal.
   --  This function should be called by cell renderer implementations in
   --  response to the `GtkCellEditable::editing-done` signal of
   --  `GtkCellEditable`.
   --  Deprecated since 4.10, 1
   --  @param Canceled True if the editing has been canceled

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cell_Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Cell_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Cell background as a `GdkRGBA`

   Cell_Background_Set_Property : constant Glib.Properties.Property_Boolean;

   Editing_Property : constant Glib.Properties.Property_Boolean;

   Height_Property : constant Glib.Properties.Property_Int;

   Is_Expanded_Property : constant Glib.Properties.Property_Boolean;

   Is_Expander_Property : constant Glib.Properties.Property_Boolean;

   Mode_Property : constant Gtk.Cell_Renderer.Property_Gtk_Cell_Renderer_Mode;
   --  Type: Gtk_Cell_Renderer_Mode

   Sensitive_Property : constant Glib.Properties.Property_Boolean;

   Visible_Property : constant Glib.Properties.Property_Boolean;

   Width_Property : constant Glib.Properties.Property_Int;

   Xalign_Property : constant Glib.Properties.Property_Float;

   Xpad_Property : constant Glib.Properties.Property_Uint;

   Yalign_Property : constant Glib.Properties.Property_Float;

   Ypad_Property : constant Glib.Properties.Property_Uint;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Renderer_Void is not null access procedure
     (Self : access Gtk_Cell_Renderer_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Editing_Canceled : constant Glib.Signal_Name := "editing-canceled";
   procedure On_Editing_Canceled
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_Gtk_Cell_Renderer_Void;
       After : Boolean := False);
   procedure On_Editing_Canceled
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal gets emitted when the user cancels the process of editing a
   --  cell. For example, an editable cell renderer could be written to cancel
   --  editing when the user presses Escape.
   --
   --  See also: Gtk.Cell_Renderer.Stop_Editing.

   type Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void is not null access procedure
     (Self     : access Gtk_Cell_Renderer_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Path     : UTF8_String);

   type Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Editable : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Path     : UTF8_String);

   Signal_Editing_Started : constant Glib.Signal_Name := "editing-started";
   procedure On_Editing_Started
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Editing_Started
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal gets emitted when a cell starts to be edited. The intended
   --  use of this signal is to do special setup on Editable, e.g. adding a
   --  `GtkEntryCompletion` or setting up additional columns in a
   --  `GtkComboBox`.
   --
   --  See gtk_cell_editable_start_editing for information on the lifecycle of
   --  the Editable and a way to do setup that doesn't depend on the Renderer.
   --
   --  Note that GTK doesn't guarantee that cell renderers will continue to
   --  use the same kind of widget for editing in future releases, therefore
   --  you should check the type of Editable before doing any specific setup,
   --  as in the following example:
   --
   --  ```c static void text_editing_started (GtkCellRenderer *cell,
   --  GtkCellEditable *editable, const char *path, gpointer data) { if
   --  (GTK_IS_ENTRY (editable)) { GtkEntry *entry = GTK_ENTRY (editable);
   --
   --  // ... create a GtkEntryCompletion
   --
   --  gtk_entry_set_completion (entry, completion); } } ```
   -- 
   --  Callback parameters:
   --    --  @param Editable the `GtkCellEditable`
   --    --  @param Path the path identifying the edited cell

private
   Ypad_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("ypad");
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xpad_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("xpad");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Width_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("width");
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Sensitive_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("sensitive");
   Mode_Property : constant Gtk.Cell_Renderer.Property_Gtk_Cell_Renderer_Mode :=
     Gtk.Cell_Renderer.Build ("mode");
   Is_Expander_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-expander");
   Is_Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-expanded");
   Height_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("height");
   Editing_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing");
   Cell_Background_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cell-background-set");
   Cell_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("cell-background-rgba");
   Cell_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("cell-background");
end Gtk.Cell_Renderer;
