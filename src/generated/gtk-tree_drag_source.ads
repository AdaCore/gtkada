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
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Types;           use Glib.Types;
with Gtk.Tree_Model;       use Gtk.Tree_Model;

package Gtk.Tree_Drag_Source is

   pragma Obsolescent;
   --  List views use widgets to display their contents. You can use [class@Gtk.DragSource] to implement a drag source

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
   pragma Obsolescent (Drag_Data_Delete);
   --  Asks the `GtkTreeDragSource` to delete the row at Path, because it was
   --  moved somewhere else via drag-and-drop. Returns False if the deletion
   --  fails because Path no longer exists, or for some model-specific reason.
   --  Should robustly handle a Path no longer found in the model!
   --  Deprecated since 4.10, 1
   --  @param Path row that was being dragged
   --  @return True if the row was successfully deleted

   function Drag_Data_Get
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
       return Gdk.Content_Provider.Gdk_Content_Provider;
   pragma Obsolescent (Drag_Data_Get);
   --  Asks the `GtkTreeDragSource` to return a `GdkContentProvider`
   --  representing the row at Path. Should robustly handle a Path no longer
   --  found in the model!
   --  Deprecated since 4.10, 1
   --  @param Path row that was dragged
   --  @return a `GdkContentProvider` for the given Path

   function Row_Draggable
      (Self : Gtk_Tree_Drag_Source;
       Path : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean;
   pragma Obsolescent (Row_Draggable);
   --  Asks the `GtkTreeDragSource` whether a particular row can be used as
   --  the source of a DND operation. If the source doesn't implement this
   --  interface, the row is assumed draggable.
   --  Deprecated since 4.10, 1
   --  @param Path row on which user is initiating a drag
   --  @return True if the row can be dragged

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Tree_Drag_Source"

   function "+" (W : Gtk_Tree_Drag_Source) return Gtk_Tree_Drag_Source;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Drag_Data_Delete is access function
     (Self : Gtk_Tree_Drag_Source;
      Path : System.Address) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Drag_Data_Delete);
   pragma Convention (C, Virtual_Drag_Data_Delete);
   --  Asks the `GtkTreeDragSource` to delete the row at Path, because it was
   --  moved somewhere else via drag-and-drop. Returns False if the deletion
   --  fails because Path no longer exists, or for some model-specific reason.
   --  Should robustly handle a Path no longer found in the model!
   --  Deprecated since 4.10, 1
   --  @param Path row that was being dragged
   --  @return True if the row was successfully deleted

   type Virtual_Drag_Data_Get is access function
     (Self : Gtk_Tree_Drag_Source;
      Path : System.Address) return System.Address;
   pragma Obsolescent (Virtual_Drag_Data_Get);
   pragma Convention (C, Virtual_Drag_Data_Get);
   --  Asks the `GtkTreeDragSource` to return a `GdkContentProvider`
   --  representing the row at Path. Should robustly handle a Path no longer
   --  found in the model!
   --  Deprecated since 4.10, 1
   --  @param Path row that was dragged
   --  @return a `GdkContentProvider` for the given Path

   type Virtual_Row_Draggable is access function
     (Self : Gtk_Tree_Drag_Source;
      Path : System.Address) return Glib.Gboolean;
   pragma Obsolescent (Virtual_Row_Draggable);
   pragma Convention (C, Virtual_Row_Draggable);
   --  Asks the `GtkTreeDragSource` whether a particular row can be used as
   --  the source of a DND operation. If the source doesn't implement this
   --  interface, the row is assumed draggable.
   --  Deprecated since 4.10, 1
   --  @param Path row on which user is initiating a drag
   --  @return True if the row can be dragged

   subtype Tree_Drag_Source_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Drag_Data_Delete
     (Self    : Tree_Drag_Source_Interface_Descr;
      Handler : Virtual_Drag_Data_Delete);
   pragma Import (C, Set_Drag_Data_Delete, "gtkada_Tree_Drag_Source_set_drag_data_delete");

   procedure Set_Drag_Data_Get
     (Self    : Tree_Drag_Source_Interface_Descr;
      Handler : Virtual_Drag_Data_Get);
   pragma Import (C, Set_Drag_Data_Get, "gtkada_Tree_Drag_Source_set_drag_data_get");

   procedure Set_Row_Draggable
     (Self    : Tree_Drag_Source_Interface_Descr;
      Handler : Virtual_Row_Draggable);
   pragma Import (C, Set_Row_Draggable, "gtkada_Tree_Drag_Source_set_row_draggable");
   --  See Glib.Object.Add_Interface

private

Null_Gtk_Tree_Drag_Source : constant Gtk_Tree_Drag_Source :=
   Gtk_Tree_Drag_Source (Glib.Types.Null_Interface);
end Gtk.Tree_Drag_Source;
