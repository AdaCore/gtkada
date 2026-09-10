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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Tree_List_Row is

   package Type_Conversion_Gtk_Tree_List_Row is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_List_Row_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_List_Row);

   -------------------
   -- Get_Child_Row --
   -------------------

   function Get_Child_Row
      (Self     : not null access Gtk_Tree_List_Row_Record;
       Position : Guint) return Gtk_Tree_List_Row
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_row_get_child_row");
      Stub_Gtk_Tree_List_Row : Gtk_Tree_List_Row_Record;
   begin
      return Gtk.Tree_List_Row.Gtk_Tree_List_Row (Get_User_Data (Internal (Get_Object (Self), Position), Stub_Gtk_Tree_List_Row));
   end Get_Child_Row;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_tree_list_row_get_children");
   begin
      return Internal (Get_Object (Self));
   end Get_Children;

   ---------------
   -- Get_Depth --
   ---------------

   function Get_Depth
      (Self : not null access Gtk_Tree_List_Row_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_tree_list_row_get_depth");
   begin
      return Internal (Get_Object (Self));
   end Get_Depth;

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
      (Self : not null access Gtk_Tree_List_Row_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_list_row_get_expanded");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Expanded;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_row_get_item");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Item;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
      (Self : not null access Gtk_Tree_List_Row_Record)
       return Gtk_Tree_List_Row
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_row_get_parent");
      Stub_Gtk_Tree_List_Row : Gtk_Tree_List_Row_Record;
   begin
      return Gtk.Tree_List_Row.Gtk_Tree_List_Row (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Tree_List_Row));
   end Get_Parent;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Tree_List_Row_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_tree_list_row_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   -------------------
   -- Is_Expandable --
   -------------------

   function Is_Expandable
      (Self : not null access Gtk_Tree_List_Row_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_list_row_is_expandable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Expandable;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded
      (Self     : not null access Gtk_Tree_List_Row_Record;
       Expanded : Boolean)
   is
      procedure Internal (Self : System.Address; Expanded : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tree_list_row_set_expanded");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Expanded));
   end Set_Expanded;

end Gtk.Tree_List_Row;
