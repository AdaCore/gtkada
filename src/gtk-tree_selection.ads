-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Glib.Object;
with Gtk;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Tree_Model;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Tree_Selection is

   type Gtk_Tree_Selection_Record is
     new Glib.Object.GObject_Record with private;
   type Gtk_Tree_Selection is access all Gtk_Tree_Selection_Record'Class;

   procedure Set_Mode
     (Selection : access Gtk_Tree_Selection_Record;
      The_Type  : Gtk_Selection_Mode);
   --  Sets the selection mode of the Selection.
   --  If the previous type was Gtk_Selection_Multiple,
   --  then the anchor is kept selected, if it was  previously selected.

   function Get_Mode (Selection : access Gtk_Tree_Selection_Record)
                     return Gtk_Selection_Mode;
   --  Gets the selection mode for Selection. See  Set_Mode.

   function Get_Tree_View (Selection : access Gtk_Tree_Selection_Record)
                          return Gtk.Widget.Gtk_Widget;
   --  Return the tree view associated with Selection.

   function Get_Selected
     (Selection : access Gtk_Tree_Selection_Record;
      Model     : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;
   --  Sets Iter to the currently selected node if Selection
   --  is set to Gtk_Selection_Single or Gtk_Selection_Browse.
   --  Iter may be Null_Iter if you just want to test if Selection has any
   --  selected nodes.  Model is filled with the current model as a
   --  convenience.  This function will not work if you  use Selection is
   --  Gtk_Selection_Multiple.

   procedure Select_Path
     (Selection : access Gtk_Tree_Selection_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Select the row at path.

   procedure Unselect_Path
     (Selection : access Gtk_Tree_Selection_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Unselects the row at path.

   procedure Select_Iter
     (Selection : access Gtk_Tree_Selection_Record;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Selects the specified iterator.

   procedure Unselect_Iter
     (Selection : access Gtk_Tree_Selection_Record;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Unselects the specified iterator.

   function Path_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path)
      return Boolean;
   --  Returns True if the row pointed to by path is currently selected.
   --  If path does not point to a valid location, False is returned

   function Iter_Is_Selected
     (Selection : access Gtk_Tree_Selection_Record;
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;
   --  Returns %TRUE if the row pointed to by path is currently selected.

   procedure Select_All (Selection : access Gtk_Tree_Selection_Record);
   --  Selects all the nodes.  Selection is must be set to
   --  Gtk_Selection_Multiple mode.

   procedure Unselect_All (Selection : access Gtk_Tree_Selection_Record);
   --  Unselects all the nodes.

   procedure Select_Range
     (Selection  : access Gtk_Tree_Selection_Record;
      Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Selects a range of nodes, determined by Start_Path and End_Path
   --  inclusive.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "changed"
   --    procedure Handler (Widget : access Gtk_Tree_Selection_Record'Class);
   --
   --  </signals>

private
   type Gtk_Tree_Selection_Record is
     new Glib.Object.GObject_Record with null record;

end Gtk.Tree_Selection;

--   ??? Missing :
--
--    procedure Set_Select_Function
--      (Selection : access Gtk_Tree_Selection_Record;
--       Func      : Gtk_Tree_Selection_Func;
--       Data      : gpointer;
--       Destroy   : Gtk_Destroy_Notify);
--
--    function Get_User_Data (Selection : access Gtk_Tree_Selection_Record)
--                            return gpointer;
--
--    procedure Selected_Foreach
--      (Selection : access Gtk_Tree_Selection_Record;
--       Func      : Gtk_Tree_Selection_Foreach_Func;
--       Data      : gpointer);
