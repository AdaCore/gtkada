------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

package Gtk.Tree_Drag_Dest is

   type Gtk_Tree_Drag_Dest is new Glib.Types.GType_Interface;
   Null_Gtk_Tree_Drag_Dest : constant Gtk_Tree_Drag_Dest;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_drag_dest_get_type");

   -------------
   -- Methods --
   -------------

   function Drag_Data_Received
      (Self           : Gtk_Tree_Drag_Dest;
       Dest           : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;
   --  Asks the Gtk.Tree_Drag_Dest.Gtk_Tree_Drag_Dest to insert a row before
   --  the path Dest, deriving the contents of the row from Selection_Data. If
   --  Dest is outside the tree so that inserting before it is impossible,
   --  False will be returned. Also, False may be returned if the new row is
   --  not created for some model-specific reason. Should robustly handle a
   --  Dest no longer found in the model!
   --  "dest": row to drop in front of
   --  "selection_data": data to drop

   function Row_Drop_Possible
      (Self           : Gtk_Tree_Drag_Dest;
       Dest_Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data)
       return Boolean;
   --  Determines whether a drop is possible before the given Dest_Path, at
   --  the same depth as Dest_Path. i.e., can we drop the data in
   --  Selection_Data at that location. Dest_Path does not have to exist; the
   --  return value will almost certainly be False if the parent of Dest_Path
   --  doesn't exist, though.
   --  "dest_path": destination row
   --  "selection_data": the data being dragged

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Drag_Dest"

   function "+" (W : Gtk_Tree_Drag_Dest) return Gtk_Tree_Drag_Dest;
   pragma Inline ("+");

private

Null_Gtk_Tree_Drag_Dest : constant Gtk_Tree_Drag_Dest :=
   Gtk_Tree_Drag_Dest (Glib.Types.Null_Interface);
end Gtk.Tree_Drag_Dest;
