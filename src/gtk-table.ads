-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A Gtk_Table is a container that can contain any number of children.
--  Each of them is attached to a specific row and a specific column in
--  widget.
--  Every row in the table must have the same height, and every column must
--  have the same width if the table was said as Homogeneous. But you can
--  also decide to have an heterogeneous table, where the width and height
--  are set by the children contained in the table.
--  Check out the Gtk_Sheet widget for a different kind of table that can
--  also contain text and images in a more efficient way.
--  </description>
--  <c_version>1.3.6</c_version>

with Glib.Properties;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Table is

   use Gtk.Enums;

   type Gtk_Table_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Table is access all Gtk_Table_Record'Class;

   procedure Gtk_New
     (Widget      : out Gtk_Table;
      Rows        : Guint;
      Columns     : Guint;
      Homogeneous : Boolean);
   --  Create a new table.
   --  The width allocated to the table is divided into Columns columns, which
   --  all have the same width if Homogeneous is True. If Homogeneous is False,
   --  the width will be calculated with the children contained in the table.
   --  Same behavior for the rows.

   procedure Initialize
     (Widget      : access Gtk_Table_Record'Class;
      Rows        : Guint;
      Columns     : Guint;
      Homogeneous : Boolean);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Table.

   procedure Resize
     (Table       : access Gtk_Table_Record;
      Rows        : Guint;
      Columns     : Guint);
   --  Modify the number of rows and columns in the table.

   procedure Attach
     (Table         : access Gtk_Table_Record;
      Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint;
      Xoptions      : Gtk_Attach_Options := Expand or Fill;
      Yoptions      : Gtk_Attach_Options := Expand or Fill;
      Xpadding      : Guint := 0;
      Ypadding      : Guint := 0);
   --  Insert a new widget in the table.
   --  All the attachments are relative to the separations between columns and
   --  rows (for instance, to insert a widget spanning the first two columns
   --  in the table, you should put Left_Attach=0 and Right_Attach=2).
   --  Same behavior for the rows.
   --  Xoptions and Yoptions indicate the behavior of the child when the table
   --  is resized (whether the child can shrink or expand). See the description
   --  in Gtk.Box for more information on the possible values.
   --  Xpadding and Ypadding are the amount of space left around the child.

   procedure Attach_Defaults
     (Table         : access Gtk_Table_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint);
   --  Insert a new widget in the table, with default values.
   --  No padding is put around the child, and the options are set to
   --  Expand and Fill.
   --  This call is similar to Attach with default values and is only provided
   --  for compatibility.

   procedure Set_Row_Spacing
     (Table   : access Gtk_Table_Record;
      Row     : Guint;
      Spacing : Guint);
   --  Set the spacing insert between Row and the next one.
   --  Spacing is in pixels.

   procedure Set_Col_Spacing
     (Table   : access Gtk_Table_Record;
      Column  : Guint;
      Spacing : Guint);
   --  Set the spacing in pixels between Column and the next one.

   procedure Set_Row_Spacings
     (Table : access Gtk_Table_Record; Spacing : Guint);
   --  Set the spacing for all the rows.

   procedure Set_Col_Spacings
     (Table : access Gtk_Table_Record; Spacing : Guint);
   --  Set the spacing for all the columns.

   procedure Set_Homogeneous
     (Table : access Gtk_Table_Record; Homogeneous : Boolean);
   --  Indicate the homogeneous status of the table.
   --  If Homogeneous is True, the rows and columns of the table will all
   --  be allocated the same width or height.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  N_Rows_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The number of rows in the table
   --    See also: Resize
   --
   --  - Name:  N_Columns_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The number of columns in the table
   --    See also: Resize
   --
   --  - Name:  Row_Spacing_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The amount of space between two consecutive rows
   --    See also: Set_Row_Spacing
   --
   --  - Name:  Column_Spacing_Property
   --    Type:  Guint
   --    Flags: read-write
   --    Descr: The amount of space between two consecutive columns
   --    See also: Set_Row_Spacing
   --
   --  - Name:  Homogeneous_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If TRUE this means the table cells are all the same
   --           width/height
   --    See also: Set_Homogeneous
   --
   --  </properties>

   N_Rows_Property         : constant Glib.Properties.Property_Uint;
   N_Columns_Property      : constant Glib.Properties.Property_Uint;
   Row_Spacing_Property    : constant Glib.Properties.Property_Uint;
   Column_Spacing_Property : constant Glib.Properties.Property_Uint;
   Homogeneous_Property    : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Table_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   N_Rows_Property         : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n_rows");
   N_Columns_Property      : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n_columns");
   Row_Spacing_Property    : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("row_spacing");
   Column_Spacing_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("column_spacing");
   Homogeneous_Property    : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("homogeneous");

   pragma Import (C, Get_Type, "gtk_table_get_type");
end Gtk.Table;
