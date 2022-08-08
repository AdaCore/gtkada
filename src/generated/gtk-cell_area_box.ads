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
--  The Gtk.Cell_Area_Box.Gtk_Cell_Area_Box renders cell renderers into a row
--  or a column depending on its Gtk.Enums.Gtk_Orientation.
--
--  GtkCellAreaBox uses a notion of packing. Packing refers to adding cell
--  renderers with reference to a particular position in a
--  Gtk.Cell_Area_Box.Gtk_Cell_Area_Box. There are two reference positions: the
--  start and the end of the box. When the Gtk.Cell_Area_Box.Gtk_Cell_Area_Box
--  is oriented in the Gtk.Enums.Orientation_Vertical orientation, the start is
--  defined as the top of the box and the end is defined as the bottom. In the
--  Gtk.Enums.Orientation_Horizontal orientation start is defined as the left
--  side and the end is defined as the right side.
--
--  Alignments of Gtk_Cell_Renderers rendered in adjacent rows can be
--  configured by configuring the Gtk.Cell_Area_Box.Gtk_Cell_Area_Box align
--  child cell property with Gtk.Cell_Area.Cell_Set_Property or by specifying
--  the "align" argument to Gtk.Cell_Area_Box.Pack_Start and
--  Gtk.Cell_Area_Box.Pack_End.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Cell_Area;     use Gtk.Cell_Area;
with Gtk.Cell_Layout;   use Gtk.Cell_Layout;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Orientable;    use Gtk.Orientable;
with Gtk.Tree_Model;    use Gtk.Tree_Model;

package Gtk.Cell_Area_Box is

   type Gtk_Cell_Area_Box_Record is new Gtk_Cell_Area_Record with null record;
   type Gtk_Cell_Area_Box is access all Gtk_Cell_Area_Box_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Cell_Layout_Data_Func is access procedure
     (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
      Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  A function which should set the value of Cell_Layout's cell renderer(s)
   --  as appropriate.
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Area_Box);
   procedure Initialize
      (Self : not null access Gtk_Cell_Area_Box_Record'Class);
   --  Creates a new Gtk.Cell_Area_Box.Gtk_Cell_Area_Box.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Area_Box_New return Gtk_Cell_Area_Box;
   --  Creates a new Gtk.Cell_Area_Box.Gtk_Cell_Area_Box.
   --  Since: gtk+ 3.0

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_area_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Spacing
      (Self : not null access Gtk_Cell_Area_Box_Record) return Glib.Gint;
   --  Gets the spacing added between cell renderers.
   --  Since: gtk+ 3.0

   procedure Set_Spacing
      (Self    : not null access Gtk_Cell_Area_Box_Record;
       Spacing : Glib.Gint);
   --  Sets the spacing to add between cell renderers in Box.
   --  Since: gtk+ 3.0
   --  "spacing": the space to add between Gtk_Cell_Renderers

   procedure Pack_End
      (Self     : not null access Gtk_Cell_Area_Box_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand   : Boolean;
       Align    : Boolean;
       Fixed    : Boolean);
   --  Adds Renderer to Box, packed with reference to the end of Box.
   --  The Renderer is packed after (away from end of) any other
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer packed with reference to the end of
   --  Box.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add
   --  "expand": whether Renderer should receive extra space when the area
   --  receives more than its natural size
   --  "align": whether Renderer should be aligned in adjacent rows
   --  "fixed": whether Renderer should have the same size in all rows

   procedure Pack_Start
      (Self     : not null access Gtk_Cell_Area_Box_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand   : Boolean;
       Align    : Boolean;
       Fixed    : Boolean);
   --  Adds Renderer to Box, packed with reference to the start of Box.
   --  The Renderer is packed after any other
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer packed with reference to the start
   --  of Box.
   --  Since: gtk+ 3.0
   --  "renderer": the Gtk.Cell_Renderer.Gtk_Cell_Renderer to add
   --  "expand": whether Renderer should receive extra space when the area
   --  receives more than its natural size
   --  "align": whether Renderer should be aligned in adjacent rows
   --  "fixed": whether Renderer should have the same size in all rows

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func);
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Gtk_Cell_Layout_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);
      --  A function which should set the value of Cell_Layout's cell renderer(s)
      --  as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      procedure Set_Cell_Data_Func
         (Cell_Layout : not null access Gtk.Cell_Area_Box.Gtk_Cell_Area_Box_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
      --  This function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate.
      --  Func may be null to remove a previously set function.
      --  Since: gtk+ 2.4
      --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
      --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
      --  "func_data": user data for Func

   end Set_Cell_Data_Func_User_Data;

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint);

   procedure Clear (Cell_Layout : not null access Gtk_Cell_Area_Box_Record);

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class);

   function Get_Cells
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist;

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);

   procedure Reorder
      (Cell_Layout : not null access Gtk_Cell_Area_Box_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint);

   function Get_Orientation
      (Self : not null access Gtk_Cell_Area_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Cell_Area_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Spacing_Property : constant Glib.Properties.Property_Int;
   --  The amount of space to reserve between cells.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "CellLayout"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Cell_Area_Box_Record, Gtk_Cell_Area_Box);
   function "+"
     (Widget : access Gtk_Cell_Area_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Cell_Area_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Cell_Layout is new Glib.Types.Implements
     (Gtk.Cell_Layout.Gtk_Cell_Layout, Gtk_Cell_Area_Box_Record, Gtk_Cell_Area_Box);
   function "+"
     (Widget : access Gtk_Cell_Area_Box_Record'Class)
   return Gtk.Cell_Layout.Gtk_Cell_Layout
   renames Implements_Gtk_Cell_Layout.To_Interface;
   function "-"
     (Interf : Gtk.Cell_Layout.Gtk_Cell_Layout)
   return Gtk_Cell_Area_Box
   renames Implements_Gtk_Cell_Layout.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Cell_Area_Box_Record, Gtk_Cell_Area_Box);
   function "+"
     (Widget : access Gtk_Cell_Area_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Cell_Area_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
end Gtk.Cell_Area_Box;
