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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Tree_Row_Reference is

   function From_Object_Free
     (B : access Gtk_Tree_Row_Reference'Class) return Gtk_Tree_Row_Reference
   is
      Result : constant Gtk_Tree_Row_Reference := Gtk_Tree_Row_Reference (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Tree_Row_Reference is
      S : Gtk_Tree_Row_Reference;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Reference : out Gtk_Tree_Row_Reference;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_new");
   begin
      Reference.Set_Object (Internal (Model, Get_Object (Path)));
   end Gtk_New;

   -------------------
   -- Gtk_New_Proxy --
   -------------------

   procedure Gtk_New_Proxy
      (Reference : out Gtk_Tree_Row_Reference;
       Proxy     : not null access Glib.Object.GObject_Record'Class;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      function Internal
         (Proxy : System.Address;
          Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_new_proxy");
   begin
      Reference.Set_Object (Internal (Get_Object (Proxy), Model, Get_Object (Path)));
   end Gtk_New_Proxy;

   --------------------------------
   -- Gtk_Tree_Row_Reference_New --
   --------------------------------

   function Gtk_Tree_Row_Reference_New
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk_Tree_Row_Reference
   is
      function Internal
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_new");
      Reference : Gtk_Tree_Row_Reference;
   begin
      Reference.Set_Object (Internal (Model, Get_Object (Path)));
      return Reference;
   end Gtk_Tree_Row_Reference_New;

   --------------------------------------
   -- Gtk_Tree_Row_Reference_New_Proxy --
   --------------------------------------

   function Gtk_Tree_Row_Reference_New_Proxy
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk_Tree_Row_Reference
   is
      function Internal
         (Proxy : System.Address;
          Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_new_proxy");
      Reference : Gtk_Tree_Row_Reference;
   begin
      Reference.Set_Object (Internal (Get_Object (Proxy), Model, Get_Object (Path)));
      return Reference;
   end Gtk_Tree_Row_Reference_New_Proxy;

   ----------
   -- Copy --
   ----------

   function Copy
      (Reference : Gtk_Tree_Row_Reference) return Gtk_Tree_Row_Reference
   is
      function Internal (Reference : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_copy");
   begin
      return From_Object (Internal (Get_Object (Reference)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Reference : Gtk_Tree_Row_Reference) is
      procedure Internal (Reference : System.Address);
      pragma Import (C, Internal, "gtk_tree_row_reference_free");
   begin
      Internal (Get_Object (Reference));
   end Free;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Reference : Gtk_Tree_Row_Reference)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Reference : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_tree_row_reference_get_model");
   begin
      return Internal (Get_Object (Reference));
   end Get_Model;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Reference : Gtk_Tree_Row_Reference)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal (Reference : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_row_reference_get_path");
   begin
      return From_Object (Internal (Get_Object (Reference)));
   end Get_Path;

   -----------
   -- Valid --
   -----------

   function Valid (Reference : Gtk_Tree_Row_Reference) return Boolean is
      function Internal (Reference : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_row_reference_valid");
   begin
      return Internal (Get_Object (Reference)) /= 0;
   end Valid;

   -------------
   -- Deleted --
   -------------

   procedure Deleted
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Proxy : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_row_reference_deleted");
   begin
      Internal (Get_Object (Proxy), Get_Object (Path));
   end Deleted;

   --------------
   -- Inserted --
   --------------

   procedure Inserted
      (Proxy : not null access Glib.Object.GObject_Record'Class;
       Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Proxy : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_row_reference_inserted");
   begin
      Internal (Get_Object (Proxy), Get_Object (Path));
   end Inserted;

   ---------------
   -- Reordered --
   ---------------

   procedure Reordered
      (Proxy     : not null access Glib.Object.GObject_Record'Class;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
       New_Order : Gint_Array)
   is
      procedure Internal
         (Proxy     : System.Address;
          Path      : System.Address;
          Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
          New_Order : System.Address);
      pragma Import (C, Internal, "gtk_tree_row_reference_reordered");
   begin
      Internal (Get_Object (Proxy), Get_Object (Path), Iter, New_Order (New_Order'First)'Address);
   end Reordered;

end Gtk.Tree_Row_Reference;
