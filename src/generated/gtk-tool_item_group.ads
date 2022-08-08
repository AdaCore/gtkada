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
--  A Gtk.Tool_Item_Group.Gtk_Tool_Item_Group is used together with
--  Gtk.Tool_Palette.Gtk_Tool_Palette to add Gtk_Tool_Items to a palette like
--  container with different categories and drag and drop support.
--
--  # CSS nodes
--
--  GtkToolItemGroup has a single CSS node named toolitemgroup.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Container;   use Gtk.Container;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Size_Group;  use Gtk.Size_Group;
with Gtk.Tool_Item;   use Gtk.Tool_Item;
with Gtk.Tool_Shell;  use Gtk.Tool_Shell;
with Gtk.Widget;      use Gtk.Widget;
with Pango.Layout;    use Pango.Layout;

package Gtk.Tool_Item_Group is

   type Gtk_Tool_Item_Group_Record is new Gtk_Container_Record with null record;
   type Gtk_Tool_Item_Group is access all Gtk_Tool_Item_Group_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Tool_Item_Group; Label : UTF8_String);
   procedure Initialize
      (Self  : not null access Gtk_Tool_Item_Group_Record'Class;
       Label : UTF8_String);
   --  Creates a new tool item group with label Label.
   --  Since: gtk+ 2.20
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the label of the new group

   function Gtk_Tool_Item_Group_New
      (Label : UTF8_String) return Gtk_Tool_Item_Group;
   --  Creates a new tool item group with label Label.
   --  Since: gtk+ 2.20
   --  "label": the label of the new group

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tool_item_group_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Collapsed
      (Self : not null access Gtk_Tool_Item_Group_Record) return Boolean;
   --  Gets whether Group is collapsed or expanded.
   --  Since: gtk+ 2.20

   procedure Set_Collapsed
      (Self      : not null access Gtk_Tool_Item_Group_Record;
       Collapsed : Boolean);
   --  Sets whether the Group should be collapsed or expanded.
   --  Since: gtk+ 2.20
   --  "collapsed": whether the Group should be collapsed or expanded

   function Get_Drop_Item
      (Self : not null access Gtk_Tool_Item_Group_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item;
   --  Gets the tool item at position (x, y).
   --  Since: gtk+ 2.20
   --  "x": the x position
   --  "y": the y position

   function Get_Ellipsize
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   --  Gets the ellipsization mode of Group.
   --  Since: gtk+ 2.20

   procedure Set_Ellipsize
      (Self      : not null access Gtk_Tool_Item_Group_Record;
       Ellipsize : Pango.Layout.Pango_Ellipsize_Mode);
   --  Sets the ellipsization mode which should be used by labels in Group.
   --  Since: gtk+ 2.20
   --  "ellipsize": the Pango.Layout.Pango_Ellipsize_Mode labels in Group
   --  should use

   function Get_Header_Relief
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Relief_Style;
   --  Gets the relief mode of the header button of Group.
   --  Since: gtk+ 2.20

   procedure Set_Header_Relief
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Style : Gtk.Enums.Gtk_Relief_Style);
   --  Set the button relief of the group header. See Gtk.Button.Set_Relief
   --  for details.
   --  Since: gtk+ 2.20
   --  "style": the Gtk.Enums.Gtk_Relief_Style

   function Get_Item_Position
      (Self : not null access Gtk_Tool_Item_Group_Record;
       Item : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
       return Glib.Gint;
   --  Gets the position of Item in Group as index.
   --  Since: gtk+ 2.20
   --  "item": a Gtk.Tool_Item.Gtk_Tool_Item

   procedure Set_Item_Position
      (Self     : not null access Gtk_Tool_Item_Group_Record;
       Item     : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Position : Glib.Gint);
   --  Sets the position of Item in the list of children of Group.
   --  Since: gtk+ 2.20
   --  "item": the Gtk.Tool_Item.Gtk_Tool_Item to move to a new position,
   --  should be a child of Group.
   --  "position": the new position of Item in Group, starting with 0. The
   --  position -1 means end of list.

   function Get_Label
      (Self : not null access Gtk_Tool_Item_Group_Record) return UTF8_String;
   --  Gets the label of Group.
   --  Since: gtk+ 2.20

   procedure Set_Label
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Label : UTF8_String);
   --  Sets the label of the tool item group. The label is displayed in the
   --  header of the group.
   --  Since: gtk+ 2.20
   --  "label": the new human-readable label of of the group

   function Get_Label_Widget
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the label widget of Group. See
   --  Gtk.Tool_Item_Group.Set_Label_Widget.
   --  Since: gtk+ 2.20

   procedure Set_Label_Widget
      (Self         : not null access Gtk_Tool_Item_Group_Record;
       Label_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the label of the tool item group. The label widget is displayed in
   --  the header of the group, in place of the usual label.
   --  Since: gtk+ 2.20
   --  "label_widget": the widget to be displayed in place of the usual label

   function Get_N_Items
      (Self : not null access Gtk_Tool_Item_Group_Record) return Guint;
   --  Gets the number of tool items in Group.
   --  Since: gtk+ 2.20

   function Get_Nth_Item
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Index : Guint) return Gtk.Tool_Item.Gtk_Tool_Item;
   --  Gets the tool item at Index in group.
   --  Since: gtk+ 2.20
   --  "index": the index

   procedure Insert
      (Self     : not null access Gtk_Tool_Item_Group_Record;
       Item     : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Position : Glib.Gint);
   --  Inserts Item at Position in the list of children of Group.
   --  Since: gtk+ 2.20
   --  "item": the Gtk.Tool_Item.Gtk_Tool_Item to insert into Group
   --  "position": the position of Item in Group, starting with 0. The
   --  position -1 means end of list.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Ellipsize_Mode
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;

   function Get_Icon_Size
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Icon_Size;

   function Get_Orientation
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Orientation;

   function Get_Relief_Style
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Relief_Style;

   function Get_Style
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Toolbar_Style;

   function Get_Text_Alignment
      (Self : not null access Gtk_Tool_Item_Group_Record) return Gfloat;

   function Get_Text_Orientation
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Orientation;

   function Get_Text_Size_Group
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Size_Group.Gtk_Size_Group;

   procedure Rebuild_Menu
      (Self : not null access Gtk_Tool_Item_Group_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Collapsed_Property : constant Glib.Properties.Property_Boolean;

   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode;
   --  Type: Pango.Layout.Pango_Ellipsize_Mode

   Header_Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style;

   Label_Property : constant Glib.Properties.Property_String;

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "ToolShell"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Tool_Item_Group_Record, Gtk_Tool_Item_Group);
   function "+"
     (Widget : access Gtk_Tool_Item_Group_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Tool_Item_Group
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Tool_Shell is new Glib.Types.Implements
     (Gtk.Tool_Shell.Gtk_Tool_Shell, Gtk_Tool_Item_Group_Record, Gtk_Tool_Item_Group);
   function "+"
     (Widget : access Gtk_Tool_Item_Group_Record'Class)
   return Gtk.Tool_Shell.Gtk_Tool_Shell
   renames Implements_Gtk_Tool_Shell.To_Interface;
   function "-"
     (Interf : Gtk.Tool_Shell.Gtk_Tool_Shell)
   return Gtk_Tool_Item_Group
   renames Implements_Gtk_Tool_Shell.To_Object;

private
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Header_Relief_Property : constant Gtk.Enums.Property_Gtk_Relief_Style :=
     Gtk.Enums.Build ("header-relief");
   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode :=
     Pango.Layout.Build ("ellipsize");
   Collapsed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("collapsed");
end Gtk.Tool_Item_Group;
