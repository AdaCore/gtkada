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

--  Interface for Drag-and-Drop destinations in `GtkTreeView`.
--
--  <group>Trees and Lists</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Glib.Types;     use Glib.Types;
with Glib.Values;    use Glib.Values;
with Gtk.Tree_Model; use Gtk.Tree_Model;

package Gtk.Tree_Drag_Dest is

   pragma Obsolescent;
   --  List views use widgets to display their contents. You can use [class@Gtk.DropTarget] to implement a drop destination

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
      (Self  : Gtk_Tree_Drag_Dest;
       Dest  : Gtk.Tree_Model.Gtk_Tree_Path;
       Value : in out Glib.Values.GValue) return Boolean;
   pragma Obsolescent (Drag_Data_Received);
   --  Asks the `GtkTreeDragDest` to insert a row before the path Dest,
   --  deriving the contents of the row from Value. If Dest is outside the tree
   --  so that inserting before it is impossible, False will be returned. Also,
   --  False may be returned if the new row is not created for some
   --  model-specific reason. Should robustly handle a Dest no longer found in
   --  the model!
   --  Deprecated since 4.10, 1
   --  @param Dest row to drop in front of
   --  @param Value data to drop
   --  @return whether a new row was created before position Dest

   function Row_Drop_Possible
      (Self      : Gtk_Tree_Drag_Dest;
       Dest_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       Value     : in out Glib.Values.GValue) return Boolean;
   pragma Obsolescent (Row_Drop_Possible);
   --  Determines whether a drop is possible before the given Dest_Path, at
   --  the same depth as Dest_Path. i.e., can we drop the data in Value at that
   --  location. Dest_Path does not have to exist; the return value will almost
   --  certainly be False if the parent of Dest_Path doesn't exist, though.
   --  Deprecated since 4.10, 1
   --  @param Dest_Path destination row
   --  @param Value the data being dropped
   --  @return True if a drop is possible before Dest_Path

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Drag_Dest"

   function "+" (W : Gtk_Tree_Drag_Dest) return Gtk_Tree_Drag_Dest;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Drag_Data_Received is access function
     (Self  : Gtk_Tree_Drag_Dest;
      Dest  : System.Address;
      Value : in out Glib.Values.GValue) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Drag_Data_Received);
   pragma Convention (C, Virtual_Drag_Data_Received);
   --  Asks the `GtkTreeDragDest` to insert a row before the path Dest,
   --  deriving the contents of the row from Value. If Dest is outside the tree
   --  so that inserting before it is impossible, False will be returned. Also,
   --  False may be returned if the new row is not created for some
   --  model-specific reason. Should robustly handle a Dest no longer found in
   --  the model!
   --  Deprecated since 4.10, 1
   --  @param Dest row to drop in front of
   --  @param Value data to drop
   --  @return whether a new row was created before position Dest

   type Virtual_Row_Drop_Possible is access function
     (Self      : Gtk_Tree_Drag_Dest;
      Dest_Path : System.Address;
      Value     : in out Glib.Values.GValue) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Row_Drop_Possible);
   pragma Convention (C, Virtual_Row_Drop_Possible);
   --  Determines whether a drop is possible before the given Dest_Path, at
   --  the same depth as Dest_Path. i.e., can we drop the data in Value at that
   --  location. Dest_Path does not have to exist; the return value will almost
   --  certainly be False if the parent of Dest_Path doesn't exist, though.
   --  Deprecated since 4.10, 1
   --  @param Dest_Path destination row
   --  @param Value the data being dropped
   --  @return True if a drop is possible before Dest_Path

   subtype Tree_Drag_Dest_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Drag_Data_Received
     (Self    : Tree_Drag_Dest_Interface_Descr;
      Handler : Virtual_Drag_Data_Received);
   pragma Import (C, Set_Drag_Data_Received, "gtkada_Tree_Drag_Dest_set_drag_data_received");

   procedure Set_Row_Drop_Possible
     (Self    : Tree_Drag_Dest_Interface_Descr;
      Handler : Virtual_Row_Drop_Possible);
   pragma Import (C, Set_Row_Drop_Possible, "gtkada_Tree_Drag_Dest_set_row_drop_possible");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Tree_Drag_Dest : constant Gtk_Tree_Drag_Dest :=
      Gtk_Tree_Drag_Dest (Glib.Types.Null_Interface);
end Gtk.Tree_Drag_Dest;
