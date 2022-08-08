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
--  The Gtk.Cell_Renderer.Gtk_Cell_Renderer is a base class of a set of
--  objects used for rendering a cell to a cairo_t. These objects are used
--  primarily by the Gtk.Tree_View.Gtk_Tree_View widget, though they aren't
--  tied to them in any specific way. It is worth noting that
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer is not a Gtk.Widget.Gtk_Widget and
--  cannot be treated as such.
--
--  The primary use of a Gtk.Cell_Renderer.Gtk_Cell_Renderer is for drawing a
--  certain graphical elements on a cairo_t. Typically, one cell renderer is
--  used to draw many cells on the screen. To this extent, it isn't expected
--  that a CellRenderer keep any permanent state around. Instead, any state is
--  set just prior to use using GObjects property system. Then, the cell is
--  measured using Gtk.Cell_Renderer.Get_Size. Finally, the cell is rendered in
--  the correct location using Gtk.Cell_Renderer.Render.
--
--  There are a number of rules that must be followed when writing a new
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer. First and foremost, it's important
--  that a certain set of properties will always yield a cell renderer of the
--  same size, barring a Gtk.Style.Gtk_Style change. The
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer also has a number of generic properties
--  that are expected to be honored by all children.
--
--  Beyond merely rendering a cell, cell renderers can optionally provide
--  active user interface elements. A cell renderer can be "activatable" like
--  Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle, which toggles when it
--  gets activated by a mouse click, or it can be "editable" like
--  Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text, which allows the user to
--  edit the text using a widget implementing the
--  Gtk.Cell_Editable.Gtk_Cell_Editable interface, e.g. Gtk.GEntry.Gtk_Entry.
--  To make a cell renderer activatable or editable, you have to implement the
--  Gtk.Cell_Renderer_Class.Gtk_Cell_Renderer_Class.activate or
--  Gtk.Cell_Renderer_Class.Gtk_Cell_Renderer_Class.start_editing virtual
--  functions, respectively.
--
--  Many properties of Gtk.Cell_Renderer.Gtk_Cell_Renderer and its subclasses
--  have a corresponding "set" property, e.g. "cell-background-set" corresponds
--  to "cell-background". These "set" properties reflect whether a property has
--  been set or not. You should not set them independently.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with Gdk.Color;               use Gdk.Color;
with Gdk.Event;               use Gdk.Event;
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

package Gtk.Cell_Renderer is

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

   function Activate
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Event           : Gdk.Event.Gdk_Event;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Path            : UTF8_String;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State) return Boolean;
   --  Passes an activate event to the cell renderer for possible processing.
   --  Some cell renderers may use events; for example,
   --  Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle toggles when it gets a
   --  mouse click.
   --  "event": a Gdk.Event.Gdk_Event
   --  "widget": widget that received the event
   --  "path": widget-dependent string representation of the event location;
   --  e.g. for Gtk.Tree_View.Gtk_Tree_View, a string representation of
   --  Gtk.Tree_Model.Gtk_Tree_Path
   --  "background_area": background area as passed to
   --  Gtk.Cell_Renderer.Render
   --  "cell_area": cell area as passed to Gtk.Cell_Renderer.Render
   --  "flags": render flags

   procedure Get_Aligned_Area
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Flags        : Gtk_Cell_Renderer_State;
       Cell_Area    : Gdk.Rectangle.Gdk_Rectangle;
       Aligned_Area : out Gdk.Rectangle.Gdk_Rectangle);
   --  Gets the aligned area used by Cell inside Cell_Area. Used for finding
   --  the appropriate edit and focus rectangle.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "flags": render flags
   --  "cell_area": cell area which would be passed to
   --  Gtk.Cell_Renderer.Render
   --  "aligned_area": the return location for the space inside Cell_Area that
   --  would acually be used to render.

   procedure Get_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   --  Fills in Xalign and Yalign with the appropriate values of Cell.
   --  Since: gtk+ 2.18
   --  "xalign": location to fill in with the x alignment of the cell, or null
   --  "yalign": location to fill in with the y alignment of the cell, or null

   procedure Set_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   --  Sets the renderer's alignment within its available space.
   --  Since: gtk+ 2.18
   --  "xalign": the x alignment of the cell renderer
   --  "yalign": the y alignment of the cell renderer

   procedure Get_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint);
   --  Fills in Width and Height with the appropriate size of Cell.
   --  "width": location to fill in with the fixed width of the cell, or null
   --  "height": location to fill in with the fixed height of the cell, or
   --  null

   procedure Set_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint);
   --  Sets the renderer size to be explicit, independent of the properties
   --  set.
   --  "width": the width of the cell renderer, or -1
   --  "height": the height of the cell renderer, or -1

   procedure Get_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : out Glib.Gint;
       Ypad : out Glib.Gint);
   --  Fills in Xpad and Ypad with the appropriate values of Cell.
   --  Since: gtk+ 2.18
   --  "xpad": location to fill in with the x padding of the cell, or null
   --  "ypad": location to fill in with the y padding of the cell, or null

   procedure Set_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : Glib.Gint;
       Ypad : Glib.Gint);
   --  Sets the renderer's padding.
   --  Since: gtk+ 2.18
   --  "xpad": the x padding of the cell renderer
   --  "ypad": the y padding of the cell renderer

   procedure Get_Preferred_Height
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   --  Retreives a renderer's natural size when rendered to Widget.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "minimum_size": location to store the minimum size, or null
   --  "natural_size": location to store the natural size, or null

   procedure Get_Preferred_Height_For_Width
      (Cell           : not null access Gtk_Cell_Renderer_Record;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint);
   --  Retreives a cell renderers's minimum and natural height if it were
   --  rendered to Widget with the specified Width.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "width": the size which is available for allocation
   --  "minimum_height": location for storing the minimum size, or null
   --  "natural_height": location for storing the preferred size, or null

   procedure Get_Preferred_Size
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Gtk.Widget.Gtk_Requisition;
       Natural_Size : out Gtk.Widget.Gtk_Requisition);
   --  Retrieves the minimum and natural size of a cell taking into account
   --  the widget's preference for height-for-width management.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "minimum_size": location for storing the minimum size, or null
   --  "natural_size": location for storing the natural size, or null

   procedure Get_Preferred_Width
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint);
   --  Retreives a renderer's natural size when rendered to Widget.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "minimum_size": location to store the minimum size, or null
   --  "natural_size": location to store the natural size, or null

   procedure Get_Preferred_Width_For_Height
      (Cell          : not null access Gtk_Cell_Renderer_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint);
   --  Retreives a cell renderers's minimum and natural width if it were
   --  rendered to Widget with the specified Height.
   --  Since: gtk+ 3.0
   --  "widget": the Gtk.Widget.Gtk_Widget this cell will be rendering to
   --  "height": the size which is available for allocation
   --  "minimum_width": location for storing the minimum size, or null
   --  "natural_width": location for storing the preferred size, or null

   function Get_Request_Mode
      (Cell : not null access Gtk_Cell_Renderer_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Gets whether the cell renderer prefers a height-for-width layout or a
   --  width-for-height layout.
   --  Since: gtk+ 3.0

   function Get_Sensitive
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   --  Returns the cell renderer's sensitivity.
   --  Since: gtk+ 2.18

   procedure Set_Sensitive
      (Cell      : not null access Gtk_Cell_Renderer_Record;
       Sensitive : Boolean);
   --  Sets the cell renderer's sensitivity.
   --  Since: gtk+ 2.18
   --  "sensitive": the sensitivity of the cell

   procedure Get_Size
      (Cell      : not null access Gtk_Cell_Renderer_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area : in out Gdk.Rectangle.Gdk_Rectangle;
       X_Offset  : out Glib.Gint;
       Y_Offset  : out Glib.Gint;
       Width     : out Glib.Gint;
       Height    : out Glib.Gint);
   pragma Obsolescent (Get_Size);
   --  Obtains the width and height needed to render the cell. Used by view
   --  widgets to determine the appropriate size for the cell_area passed to
   --  Gtk.Cell_Renderer.Render. If Cell_Area is not null, fills in the x and y
   --  offsets (if set) of the cell relative to this location.
   --  Please note that the values set in Width and Height, as well as those
   --  in X_Offset and Y_Offset are inclusive of the xpad and ypad properties.
   --  Deprecated since 3.0, 1
   --  "widget": the widget the renderer is rendering to
   --  "cell_area": The area a cell will be allocated, or null
   --  "x_offset": location to return x offset of cell relative to Cell_Area,
   --  or null
   --  "y_offset": location to return y offset of cell relative to Cell_Area,
   --  or null
   --  "width": location to return width needed to render a cell, or null
   --  "height": location to return height needed to render a cell, or null

   function Get_State
      (Cell       : not null access Gtk_Cell_Renderer_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_State : Gtk_Cell_Renderer_State)
       return Gtk.Enums.Gtk_State_Flags;
   --  Translates the cell renderer state to Gtk.Enums.Gtk_State_Flags, based
   --  on the cell renderer and widget sensitivity, and the given
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer_State.
   --  Since: gtk+ 3.0
   --  "widget": a Gtk.Widget.Gtk_Widget, or null
   --  "cell_state": cell renderer state

   function Get_Visible
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   --  Returns the cell renderer's visibility.
   --  Since: gtk+ 2.18

   procedure Set_Visible
      (Cell    : not null access Gtk_Cell_Renderer_Record;
       Visible : Boolean);
   --  Sets the cell renderer's visibility.
   --  Since: gtk+ 2.18
   --  "visible": the visibility of the cell

   function Is_Activatable
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean;
   --  Checks whether the cell renderer can do something when activated.
   --  Since: gtk+ 3.0

   procedure Render
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Cr              : Cairo.Cairo_Context;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State);
   --  Invokes the virtual render function of the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer. The three passed-in rectangles are
   --  areas in Cr. Most renderers will draw within Cell_Area; the xalign,
   --  yalign, xpad, and ypad fields of the Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  should be honored with respect to Cell_Area. Background_Area includes
   --  the blank space around the cell, and also the area containing the tree
   --  expander; so the Background_Area rectangles for all cells tile to cover
   --  the entire Window.
   --  "cr": a cairo context to draw to
   --  "widget": the widget owning Window
   --  "background_area": entire cell area (including tree expanders and maybe
   --  padding on the sides)
   --  "cell_area": area normally rendered by a cell renderer
   --  "flags": flags that affect rendering

   function Start_Editing
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Event           : Gdk.Event.Gdk_Event;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Path            : UTF8_String;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State)
       return Gtk.Cell_Editable.Gtk_Cell_Editable;
   --  Starts editing the contents of this Cell, through a new
   --  Gtk.Cell_Editable.Gtk_Cell_Editable widget created by the
   --  Gtk.Cell_Renderer_Class.Gtk_Cell_Renderer_Class.start_editing virtual
   --  function.
   --  "event": a Gdk.Event.Gdk_Event
   --  "widget": widget that received the event
   --  "path": widget-dependent string representation of the event location;
   --  e.g. for Gtk.Tree_View.Gtk_Tree_View, a string representation of
   --  Gtk.Tree_Model.Gtk_Tree_Path
   --  "background_area": background area as passed to
   --  Gtk.Cell_Renderer.Render
   --  "cell_area": cell area as passed to Gtk.Cell_Renderer.Render
   --  "flags": render flags

   procedure Stop_Editing
      (Cell     : not null access Gtk_Cell_Renderer_Record;
       Canceled : Boolean);
   --  Informs the cell renderer that the editing is stopped. If Canceled is
   --  True, the cell renderer will emit the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer::editing-canceled signal.
   --  This function should be called by cell renderer implementations in
   --  response to the Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal
   --  of Gtk.Cell_Editable.Gtk_Cell_Editable.
   --  Since: gtk+ 2.6
   --  "canceled": True if the editing has been canceled

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Cell_Background_Property : constant Glib.Properties.Property_String;
   --  Flags: write

   Cell_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  Cell background as a Gdk.Color.Gdk_Color

   Cell_Background_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  Cell background as a Gdk.RGBA.Gdk_RGBA

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
   --  Gtk.Entry_Completion.Gtk_Entry_Completion or setting up additional
   --  columns in a Gtk.Combo_Box.Gtk_Combo_Box.
   --
   --  See Gtk.Cell_Editable.Start_Editing for information on the lifecycle of
   --  the Editable and a way to do setup that doesn't depend on the Renderer.
   --
   --  Note that GTK+ doesn't guarantee that cell renderers will continue to
   --  use the same kind of widget for editing in future releases, therefore
   --  you should check the type of Editable before doing any specific setup,
   --  as in the following example: |[<!-- language="C" --> static void
   --  text_editing_started (GtkCellRenderer *cell, GtkCellEditable *editable,
   --  const gchar *path, gpointer data) { if (GTK_IS_ENTRY (editable)) {
   --  GtkEntry *entry = GTK_ENTRY (editable); // ... create a
   --  GtkEntryCompletion gtk_entry_set_completion (entry, completion); } } ]|
   -- 
   --  Callback parameters:
   --    --  "editable": the Gtk.Cell_Editable.Gtk_Cell_Editable
   --    --  "path": the path identifying the edited cell

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
   Cell_Background_Gdk_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("cell-background-gdk");
   Cell_Background_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("cell-background");
end Gtk.Cell_Renderer;
