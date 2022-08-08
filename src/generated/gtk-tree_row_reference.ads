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
--  A GtkTreeRowReference tracks model changes so that it always refers to the
--  same row (a Gtk.Tree_Model.Gtk_Tree_Path refers to a position, not a fixed
--  row). Create a new GtkTreeRowReference with gtk_tree_row_reference_new.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Gtk.Tree_Model; use Gtk.Tree_Model;

package Gtk.Tree_Row_Reference is

   type Gtk_Tree_Row_Reference is new Glib.C_Boxed with null record;
   Null_Gtk_Tree_Row_Reference : constant Gtk_Tree_Row_Reference;

   function From_Object (Object : System.Address) return Gtk_Tree_Row_Reference;
   function From_Object_Free (B : access Gtk_Tree_Row_Reference'Class) return Gtk_Tree_Row_Reference;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Reference : out Gtk_Tree_Row_Reference;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Creates a row reference based on Path.
   --  This reference will keep pointing to the node pointed to by Path, so
   --  long as it exists. Any changes that occur on Model are propagated, and
   --  the path is updated appropriately. If Path isn't a valid path in Model,
   --  then null is returned.
   --  "model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": a valid Gtk.Tree_Model.Gtk_Tree_Path-struct to monitor

   function Gtk_Tree_Row_Reference_New
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk_Tree_Row_Reference;
   --  Creates a row reference based on Path.
   --  This reference will keep pointing to the node pointed to by Path, so
   --  long as it exists. Any changes that occur on Model are propagated, and
   --  the path is updated appropriately. If Path isn't a valid path in Model,
   --  then null is returned.
   --  "model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": a valid Gtk.Tree_Model.Gtk_Tree_Path-struct to monitor

   procedure Gtk_New_Proxy
      (Reference : out Gtk_Tree_Row_Reference;
       Proxy     : not null access Glib.Object.GObject_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path);
   --  You do not need to use this function.
   --  Creates a row reference based on Path.
   --  This reference will keep pointing to the node pointed to by Path, so
   --  long as it exists. If Path isn't a valid path in Model, then null is
   --  returned. However, unlike references created with
   --  gtk_tree_row_reference_new, it does not listen to the model for changes.
   --  The creator of the row reference must do this explicitly using
   --  Gtk.Tree_Row_Reference.Inserted, Gtk.Tree_Row_Reference.Deleted,
   --  Gtk.Tree_Row_Reference.Reordered.
   --  These functions must be called exactly once per proxy when the
   --  corresponding signal on the model is emitted. This single call updates
   --  all row references for that proxy. Since built-in GTK+ objects like
   --  Gtk.Tree_View.Gtk_Tree_View already use this mechanism internally, using
   --  them as the proxy object will produce unpredictable results. Further
   --  more, passing the same object as Model and Proxy doesn't work for
   --  reasons of internal implementation.
   --  This type of row reference is primarily meant by structures that need
   --  to carefully monitor exactly when a row reference updates itself, and is
   --  not generally needed by most applications.
   --  "proxy": a proxy Glib.Object.GObject
   --  "model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": a valid Gtk.Tree_Model.Gtk_Tree_Path-struct to monitor

   function Gtk_Tree_Row_Reference_New_Proxy
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk_Tree_Row_Reference;
   --  You do not need to use this function.
   --  Creates a row reference based on Path.
   --  This reference will keep pointing to the node pointed to by Path, so
   --  long as it exists. If Path isn't a valid path in Model, then null is
   --  returned. However, unlike references created with
   --  gtk_tree_row_reference_new, it does not listen to the model for changes.
   --  The creator of the row reference must do this explicitly using
   --  Gtk.Tree_Row_Reference.Inserted, Gtk.Tree_Row_Reference.Deleted,
   --  Gtk.Tree_Row_Reference.Reordered.
   --  These functions must be called exactly once per proxy when the
   --  corresponding signal on the model is emitted. This single call updates
   --  all row references for that proxy. Since built-in GTK+ objects like
   --  Gtk.Tree_View.Gtk_Tree_View already use this mechanism internally, using
   --  them as the proxy object will produce unpredictable results. Further
   --  more, passing the same object as Model and Proxy doesn't work for
   --  reasons of internal implementation.
   --  This type of row reference is primarily meant by structures that need
   --  to carefully monitor exactly when a row reference updates itself, and is
   --  not generally needed by most applications.
   --  "proxy": a proxy Glib.Object.GObject
   --  "model": a Gtk.Tree_Model.Gtk_Tree_Model
   --  "path": a valid Gtk.Tree_Model.Gtk_Tree_Path-struct to monitor

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_tree_row_reference_get_type");

   -------------
   -- Methods --
   -------------

   function Copy
      (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Row_Reference;
   --  Copies a Gtk.Tree_Row_Reference.Gtk_Tree_Row_Reference.
   --  Since: gtk+ 2.2

   procedure Free (Reference : Gtk_Tree_Row_Reference);
   --  Free's Reference. Reference may be null

   function Get_Model
      (Reference : Gtk_Tree_Row_Reference)
       return Gtk.Tree_Model.Gtk_Tree_Model;
   --  Returns the model that the row reference is monitoring.
   --  Since: gtk+ 2.8

   function Get_Path
      (Reference : Gtk_Tree_Row_Reference)
       return Gtk.Tree_Model.Gtk_Tree_Path;
   --  Returns a path that the row reference currently points to, or null if
   --  the path pointed to is no longer valid.

   function Valid (Reference : Gtk_Tree_Row_Reference) return Boolean;
   --  Returns True if the Reference is non-null and refers to a current valid
   --  path.

   ---------------
   -- Functions --
   ---------------

   procedure Deleted
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Lets a set of row reference created by gtk_tree_row_reference_new_proxy
   --  know that the model emitted the
   --  Gtk.Tree_Model.Gtk_Tree_Model::row-deleted signal.
   --  "proxy": a Glib.Object.GObject
   --  "path": the path position that was deleted

   procedure Inserted
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path);
   --  Lets a set of row reference created by gtk_tree_row_reference_new_proxy
   --  know that the model emitted the
   --  Gtk.Tree_Model.Gtk_Tree_Model::row-inserted signal.
   --  "proxy": a Glib.Object.GObject
   --  "path": the row position that was inserted

   procedure Reordered
      (Proxy     : not null access Glib.Object.GObject_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order : Gint_Array);
   --  Lets a set of row reference created by gtk_tree_row_reference_new_proxy
   --  know that the model emitted the
   --  Gtk.Tree_Model.Gtk_Tree_Model::rows-reordered signal.
   --  "proxy": a Glib.Object.GObject
   --  "path": the parent path of the reordered signal
   --  "iter": the iter pointing to the parent of the reordered
   --  "new_order": the new order of rows

private

   Null_Gtk_Tree_Row_Reference : constant Gtk_Tree_Row_Reference := (Glib.C_Boxed with null record);

end Gtk.Tree_Row_Reference;
