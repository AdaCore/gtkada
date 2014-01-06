------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Types;         use Glib.Types;
with Gtk.Selection_Data; use Gtk.Selection_Data;
with Gtk.Tree_Model;     use Gtk.Tree_Model;

package Gtk.Tree_Drag_Source is

   type Gtk_Tree_Drag_Source is new Glib.Types.GType_Interface;
   Null_Gtk_Tree_Drag_Source : constant Gtk_Tree_Drag_Source;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_drag_source_get_type");

   -------------
   -- Methods --
   -------------

   function Drag_Data_Delete
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Asks the Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source to delete the row at
   --  Path, because it was moved somewhere else via drag-and-drop. Returns
   --  False if the deletion fails because Path no longer exists, or for some
   --  model-specific reason. Should robustly handle a Path no longer found in
   --  the model!
   --  "path": row that was being dragged

   function Drag_Data_Get
      (Self           : Gtk_Tree_Drag_Source;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;
   --  Asks the Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source to fill in
   --  Selection_Data with a representation of the row at Path.
   --  Selection_Data->target gives the required type of the data. Should
   --  robustly handle a Path no longer found in the model!
   --  "path": row that was dragged
   --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data to fill with
   --  data from the dragged row

   function Row_Draggable
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Asks the Gtk.Tree_Drag_Source.Gtk_Tree_Drag_Source whether a particular
   --  row can be used as the source of a DND operation. If the source doesn't
   --  implement this interface, the row is assumed draggable.
   --  "path": row on which user is initiating a drag

   ---------------
   -- Functions --
   ---------------

   function Set_Row_Drag_Data
      (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
       Tree_Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path           : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   --  Sets selection data of target type GTK_TREE_MODEL_ROW. Normally used in
   --  a drag_data_get handler.
   --  "selection_data": some Gtk.Selection_Data.Gtk_Selection_Data
   --  "tree_model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": a row in Tree_Model

   procedure Get_Row_Drag_Data
      (Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
       Tree_Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
       Path           : out Gtk.Tree_Model.Gtk_Tree_Path;
       succes         : out Boolean);
   --  Obtains a Tree_Model and Path from selection data of target type
   --  GTK_TREE_MODEL_ROW. Normally called from a drag_data_received handler.
   --  This function can only be used if Selection_Data originates from the
   --  same process that's calling this function, because a pointer to the tree
   --  model is being passed around. If you aren't in the same process, then
   --  you'll get memory corruption. In the
   --  Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest drag_data_received handler, you
   --  can assume that selection data of type GTK_TREE_MODEL_ROW is in from the
   --  current process. The returned path must be freed with
   --  Gtk.Tree_Model.Path_Free.
   --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data
   --  "tree_model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": row in Tree_Model

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Drag_Source"

   function "+" (W : Gtk_Tree_Drag_Source) return Gtk_Tree_Drag_Source;
   pragma Inline ("+");

private

Null_Gtk_Tree_Drag_Source : constant Gtk_Tree_Drag_Source :=
   Gtk_Tree_Drag_Source (Glib.Types.Null_Interface);
end Gtk.Tree_Drag_Source;
