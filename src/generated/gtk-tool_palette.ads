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
--  A Gtk.Tool_Palette.Gtk_Tool_Palette allows you to add Gtk_Tool_Items to a
--  palette-like container with different categories and drag and drop support.
--
--  A Gtk.Tool_Palette.Gtk_Tool_Palette is created with a call to
--  Gtk.Tool_Palette.Gtk_New.
--
--  Gtk_Tool_Items cannot be added directly to a
--  Gtk.Tool_Palette.Gtk_Tool_Palette - instead they are added to a
--  Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which can than be added to a
--  Gtk.Tool_Palette.Gtk_Tool_Palette. To add a
--  Gtk.Tool_Item_Group.Gtk_Tool_Item_Group to a
--  Gtk.Tool_Palette.Gtk_Tool_Palette, use Gtk.Container.Add.
--
--  |[<!-- language="C" --> GtkWidget *palette, *group; GtkToolItem *item;
--
--  palette = gtk_tool_palette_new (); group = gtk_tool_item_group_new
--  (_("Test Category")); gtk_container_add (GTK_CONTAINER (palette), group);
--
--  item = gtk_tool_button_new (NULL, _("_Open"));
--  gtk_tool_button_set_icon_name (GTK_TOOL_BUTTON (item), "document-open");
--  gtk_tool_item_group_insert (GTK_TOOL_ITEM_GROUP (group), item, -1); ]|
--
--  The easiest way to use drag and drop with
--  Gtk.Tool_Palette.Gtk_Tool_Palette is to call Gtk.Tool_Palette.Add_Drag_Dest
--  with the desired drag source Palette and the desired drag target Widget.
--  Then Gtk.Tool_Palette.Get_Drag_Item can be used to get the dragged item in
--  the Gtk.Widget.Gtk_Widget::drag-data-received signal handler of the drag
--  target.
--
--  |[<!-- language="C" --> static void passive_canvas_drag_data_received
--  (GtkWidget *widget, GdkDragContext *context, gint x, gint y,
--  GtkSelectionData *selection, guint info, guint time, gpointer data) {
--  GtkWidget *palette; GtkWidget *item;
--
--  // Get the dragged item palette = gtk_widget_get_ancestor
--  (gtk_drag_get_source_widget (context), GTK_TYPE_TOOL_PALETTE); if (palette
--  != NULL) item = gtk_tool_palette_get_drag_item (GTK_TOOL_PALETTE (palette),
--  selection);
--
--  // Do something with item }
--
--  GtkWidget *target, palette;
--
--  palette = gtk_tool_palette_new (); target = gtk_drawing_area_new ();
--
--  g_signal_connect (G_OBJECT (target), "drag-data-received", G_CALLBACK
--  (passive_canvas_drag_data_received), NULL); gtk_tool_palette_add_drag_dest
--  (GTK_TOOL_PALETTE (palette), target, GTK_DEST_DEFAULT_ALL,
--  GTK_TOOL_PALETTE_DRAG_ITEMS, GDK_ACTION_COPY); ]|
--
--  # CSS nodes
--
--  GtkToolPalette has a single CSS node named toolpalette.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Drag_Contexts;       use Gdk.Drag_Contexts;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Orientable;          use Gtk.Orientable;
with Gtk.Scrollable;          use Gtk.Scrollable;
with Gtk.Selection_Data;      use Gtk.Selection_Data;
with Gtk.Style;               use Gtk.Style;
with Gtk.Target_Entry;        use Gtk.Target_Entry;
with Gtk.Tool_Item;           use Gtk.Tool_Item;
with Gtk.Tool_Item_Group;     use Gtk.Tool_Item_Group;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Tool_Palette is

   type Gtk_Tool_Palette_Record is new Gtk_Container_Record with null record;
   type Gtk_Tool_Palette is access all Gtk_Tool_Palette_Record'Class;

   type Gtk_Dest_Defaults is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Dest_Defaults);
   --  The Gtk.Tool_Palette.Gtk_Dest_Defaults enumeration specifies the
   --  various types of action that will be taken on behalf of the user for a
   --  drag destination site.

   Dest_Default_Motion : constant Gtk_Dest_Defaults := 1;
   Dest_Default_Highlight : constant Gtk_Dest_Defaults := 2;
   Dest_Default_Drop : constant Gtk_Dest_Defaults := 4;
   Dest_Default_All : constant Gtk_Dest_Defaults := 7;

   type Gtk_Tool_Palette_Drag_Targets is mod 2 ** Integer'Size;
   pragma Convention (C, Gtk_Tool_Palette_Drag_Targets);
   --  Flags used to specify the supported drag targets.

   Tool_Palette_Drag_Items : constant Gtk_Tool_Palette_Drag_Targets := 1;
   Tool_Palette_Drag_Groups : constant Gtk_Tool_Palette_Drag_Targets := 2;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Dest_Defaults_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Dest_Defaults);
   type Property_Gtk_Dest_Defaults is new Gtk_Dest_Defaults_Properties.Property;

   package Gtk_Tool_Palette_Drag_Targets_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Tool_Palette_Drag_Targets);
   type Property_Gtk_Tool_Palette_Drag_Targets is new Gtk_Tool_Palette_Drag_Targets_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Tool_Palette);
   procedure Initialize
      (Self : not null access Gtk_Tool_Palette_Record'Class);
   --  Creates a new tool palette.
   --  Since: gtk+ 2.20
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Tool_Palette_New return Gtk_Tool_Palette;
   --  Creates a new tool palette.
   --  Since: gtk+ 2.20

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_palette_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Drag_Dest
      (Self    : not null access Gtk_Tool_Palette_Record;
       Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Flags   : Gtk_Dest_Defaults;
       Targets : Gtk_Tool_Palette_Drag_Targets;
       Actions : Gdk.Drag_Contexts.Gdk_Drag_Action);
   --  Sets Palette as drag source (see Gtk.Tool_Palette.Set_Drag_Source) and
   --  sets Widget as a drag destination for drags from Palette. See
   --  gtk_drag_dest_set.
   --  Since: gtk+ 2.20
   --  "widget": a Gtk.Widget.Gtk_Widget which should be a drag destination
   --  for Palette
   --  "flags": the flags that specify what actions GTK+ should take for drops
   --  on that widget
   --  "targets": the Gtk.Tool_Palette.Gtk_Tool_Palette_Drag_Targets which the
   --  widget should support
   --  "actions": the Gdk_Drag_Actions which the widget should suppport

   function Get_Drag_Item
      (Self      : not null access Gtk_Tool_Palette_Record;
       Selection : Gtk.Selection_Data.Gtk_Selection_Data)
       return Gtk.Widget.Gtk_Widget;
   --  Get the dragged item from the selection. This could be a
   --  Gtk.Tool_Item.Gtk_Tool_Item or a
   --  Gtk.Tool_Item_Group.Gtk_Tool_Item_Group.
   --  Since: gtk+ 2.20
   --  "selection": a Gtk.Selection_Data.Gtk_Selection_Data

   function Get_Drop_Group
      (Self : not null access Gtk_Tool_Palette_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item_Group.Gtk_Tool_Item_Group;
   --  Gets the group at position (x, y).
   --  Since: gtk+ 2.20
   --  "x": the x position
   --  "y": the y position

   function Get_Drop_Item
      (Self : not null access Gtk_Tool_Palette_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item;
   --  Gets the item at position (x, y). See Gtk.Tool_Palette.Get_Drop_Group.
   --  Since: gtk+ 2.20
   --  "x": the x position
   --  "y": the y position

   function Get_Exclusive
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Boolean;
   --  Gets whether Group is exclusive or not. See
   --  Gtk.Tool_Palette.Set_Exclusive.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which is a child of
   --  palette

   procedure Set_Exclusive
      (Self      : not null access Gtk_Tool_Palette_Record;
       Group     : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Exclusive : Boolean);
   --  Sets whether the group should be exclusive or not. If an exclusive
   --  group is expanded all other groups are collapsed.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which is a child of
   --  palette
   --  "exclusive": whether the group should be exclusive or not

   function Get_Expand
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Boolean;
   --  Gets whether group should be given extra space. See
   --  Gtk.Tool_Palette.Set_Expand.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which is a child of
   --  palette

   procedure Set_Expand
      (Self   : not null access Gtk_Tool_Palette_Record;
       Group  : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Expand : Boolean);
   --  Sets whether the group should be given extra space.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which is a child of
   --  palette
   --  "expand": whether the group should be given extra space

   function Get_Group_Position
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Glib.Gint;
   --  Gets the position of Group in Palette as index. See
   --  Gtk.Tool_Palette.Set_Group_Position.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group

   procedure Set_Group_Position
      (Self     : not null access Gtk_Tool_Palette_Record;
       Group    : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Position : Glib.Gint);
   --  Sets the position of the group as an index of the tool palette. If
   --  position is 0 the group will become the first child, if position is -1
   --  it will become the last child.
   --  Since: gtk+ 2.20
   --  "group": a Gtk.Tool_Item_Group.Gtk_Tool_Item_Group which is a child of
   --  palette
   --  "position": a new index for group

   function Get_Icon_Size
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Icon_Size;
   --  Gets the size of icons in the tool palette. See
   --  Gtk.Tool_Palette.Set_Icon_Size.
   --  Since: gtk+ 2.20

   procedure Set_Icon_Size
      (Self      : not null access Gtk_Tool_Palette_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size);
   --  Sets the size of icons in the tool palette.
   --  Since: gtk+ 2.20
   --  "icon_size": the Gtk.Enums.Gtk_Icon_Size that icons in the tool palette
   --  shall have

   function Get_Style
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Toolbar_Style;
   --  Gets the style (icons, text or both) of items in the tool palette.
   --  Since: gtk+ 2.20

   procedure Set_Style
      (Self  : not null access Gtk_Tool_Palette_Record;
       Style : Gtk.Enums.Gtk_Toolbar_Style);
   --  Sets the style (text, icons or both) of items in the tool palette.
   --  Since: gtk+ 2.20
   --  "style": the Gtk.Enums.Gtk_Toolbar_Style that items in the tool palette
   --  shall have

   procedure Set_Drag_Source
      (Self    : not null access Gtk_Tool_Palette_Record;
       Targets : Gtk_Tool_Palette_Drag_Targets);
   --  Sets the tool palette as a drag source. Enables all groups and items in
   --  the tool palette as drag sources on button 1 and button 3 press with
   --  copy and move actions. See gtk_drag_source_set.
   --  Since: gtk+ 2.20
   --  "targets": the Gtk.Tool_Palette.Gtk_Tool_Palette_Drag_Targets which the
   --  widget should support

   procedure Unset_Icon_Size
      (Self : not null access Gtk_Tool_Palette_Record);
   --  Unsets the tool palette icon size set with
   --  Gtk.Tool_Palette.Set_Icon_Size, so that user preferences will be used to
   --  determine the icon size.
   --  Since: gtk+ 2.20

   procedure Unset_Style (Self : not null access Gtk_Tool_Palette_Record);
   --  Unsets a toolbar style set with Gtk.Tool_Palette.Set_Style, so that
   --  user preferences will be used to determine the toolbar style.
   --  Since: gtk+ 2.20

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Tool_Palette_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   function Get_Border
      (Self   : not null access Gtk_Tool_Palette_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean;

   function Get_Hadjustment
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Tool_Palette_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Tool_Palette_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   function Get_Vadjustment
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Tool_Palette_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy;

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Tool_Palette_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy);

   ---------------
   -- Functions --
   ---------------

   function Get_Drag_Target_Group return Gtk.Target_Entry.Gtk_Target_Entry;
   --  Get the target entry for a dragged
   --  Gtk.Tool_Item_Group.Gtk_Tool_Item_Group.
   --  Since: gtk+ 2.20

   function Get_Drag_Target_Item return Gtk.Target_Entry.Gtk_Target_Entry;
   --  Gets the target entry for a dragged Gtk.Tool_Item.Gtk_Tool_Item.
   --  Since: gtk+ 2.20

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size;
   --  The size of the icons in a tool palette. When this property is set, it
   --  overrides the default setting.
   --
   --  This should only be used for special-purpose tool palettes, normal
   --  application tool palettes should respect the user preferences for the
   --  size of icons.

   Icon_Size_Set_Property : constant Glib.Properties.Property_Boolean;
   --  Is True if the Gtk.Tool_Palette.Gtk_Tool_Palette:icon-size property has
   --  been set.

   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style;
   --  The style of items in the tool palette.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"
   --
   --  - "Scrollable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tool_Palette_Record, Gtk_Tool_Palette);
   function "+"
     (Widget : access Gtk_Tool_Palette_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tool_Palette
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Tool_Palette_Record, Gtk_Tool_Palette);
   function "+"
     (Widget : access Gtk_Tool_Palette_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Tool_Palette
   renames Implements_Gtk_Orientable.To_Object;

   package Implements_Gtk_Scrollable is new Glib.Types.Implements
     (Gtk.Scrollable.Gtk_Scrollable, Gtk_Tool_Palette_Record, Gtk_Tool_Palette);
   function "+"
     (Widget : access Gtk_Tool_Palette_Record'Class)
   return Gtk.Scrollable.Gtk_Scrollable
   renames Implements_Gtk_Scrollable.To_Interface;
   function "-"
     (Interf : Gtk.Scrollable.Gtk_Scrollable)
   return Gtk_Tool_Palette
   renames Implements_Gtk_Scrollable.To_Object;

private
   Toolbar_Style_Property : constant Gtk.Enums.Property_Gtk_Toolbar_Style :=
     Gtk.Enums.Build ("toolbar-style");
   Icon_Size_Set_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("icon-size-set");
   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size :=
     Gtk.Enums.Build ("icon-size");
end Gtk.Tool_Palette;
