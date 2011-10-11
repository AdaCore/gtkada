-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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

pragma Ada_05;
--  <description>
--  Gtk_Cell_Layout is an interface to be implemented by all objects which
--  want to provide a Gtk_Tree_View_Column like API for packing cells, setting
--  attributes and data funcs.
--
--  The rendering of the widget is done through various Gtk_Cell_Renderer, and
--  by reading data from a Gtk_Tree_Model.
--
--  </description>
--  <group>Trees and Lists</group>
--  <testgtk>create_cell_view.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Types;        use Glib.Types;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Tree_Model;    use Gtk.Tree_Model;

package Gtk.Cell_Layout is

   type Gtk_Cell_Layout is new Glib.Types.GType_Interface;

   type Cell_Data_Func is access procedure
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter        : Gtk.Tree_Model.Gtk_Tree_Iter);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_layout_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Attribute
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Gint);
   --  Adds an attribute mapping to the list in Cell_Layout. The Column is the
   --  column of the model to get a value from, and the Attribute is the
   --  parameter on Cell to be set from the value. So for example if column 2
   --  of the model contains strings, you could have the "text" attribute of a
   --  Gtk.Cellrenderertext.Gtk_Cellrenderertext get its values from column 2.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
   --  "attribute": An attribute on the renderer.
   --  "column": The column position on the model to get the attribute from.

   procedure Clear (Cell_Layout : Gtk_Cell_Layout);
   pragma Import (C, Clear, "gtk_cell_layout_clear");
   --  Unsets all the mappings on all renderers on Cell_Layout and removes all
   --  renderers from Cell_Layout.
   --  Since: gtk+ 2.4

   procedure Clear_Attributes
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      ;
   --  Clears all existing attributes previously set with
   --  gtk_cell_layout_set_attributes.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer to clear the attribute
   --  mapping on.

   function Get_Cells
      (Cell_Layout : Gtk_Cell_Layout)
       return Glib.Object.Object_Simple_List.GList;
   --  Returns the cell renderers which have been added to Cell_Layout.
   --  renderers has been newly allocated and should be freed with g_list_free
   --  when no longer needed.
   --  Since: gtk+ 2.12

   procedure Pack_End
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);
   --  Adds the Cell to the end of Cell_Layout. If Expand is False, then the
   --  divided evenly between cells for which Expand is True. Note that reusing
   --  the same cell renderer is not supported.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
   --  "expand": True if Cell is to be given extra space allocated to
   --  Cell_Layout.

   procedure Pack_Start
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean);
   --  Packs the Cell into the beginning of Cell_Layout. If Expand is False,
   --  then the Cell is allocated no more space than it needs. Any unused space
   --  is divided evenly between cells for which Expand is True. Note that
   --  reusing the same cell renderer is not supported.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
   --  "expand": True if Cell is to be given extra space allocated to
   --  Cell_Layout.

   procedure Reorder
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Gint);
   --  Re-inserts Cell at Position. Note that Cell has already to be packed
   --  into Cell_Layout for this to function properly.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer to reorder.
   --  "position": New position to insert Cell at.

   procedure Set_Cell_Data_Func
      (Cell_Layout : Gtk_Cell_Layout;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Cell_Data_Func);
   --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout. This
   --  function is used instead of the standard attributes mapping for setting
   --  the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate. Func may be null to remove and older one.
   --  Since: gtk+ 2.4
   --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
   --  "func": The Gtk.Cell_Layout.Cell_Data_Func to use.

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Set_Cell_Data_Func_User_Data is

      type Cell_Data_Func is access procedure
        (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
         Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
         Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data        : User_Data_Type);

      procedure Set_Cell_Data_Func
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Cell_Data_Func;
          Func_Data   : User_Data_Type);
      --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout. This
      --  function is used instead of the standard attributes mapping for
      --  setting the column value, and should set the value of Cell_Layout's
      --  cell renderer(s) as appropriate. Func may be null to remove and older
      --  one.
      --  Since: gtk+ 2.4
      --  "cell": A Gtk.Cell_Renderer.Gtk_Cell_Renderer.
      --  "func": The Gtk.Cell_Layout.Cell_Data_Func to use.
      --  "func_data": The user data for Func.

   end Set_Cell_Data_Func_User_Data;

end Gtk.Cell_Layout;
