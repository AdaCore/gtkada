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

package body Gtk.Tree_List_Row_Sorter is

   package Type_Conversion_Gtk_Tree_List_Row_Sorter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_List_Row_Sorter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_List_Row_Sorter);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Tree_List_Row_Sorter;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
   begin
      Self := new Gtk_Tree_List_Row_Sorter_Record;
      Gtk.Tree_List_Row_Sorter.Initialize (Self, Sorter);
   end Gtk_New;

   ----------------------------------
   -- Gtk_Tree_List_Row_Sorter_New --
   ----------------------------------

   function Gtk_Tree_List_Row_Sorter_New
      (Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
       return Gtk_Tree_List_Row_Sorter
   is
      Self : constant Gtk_Tree_List_Row_Sorter := new Gtk_Tree_List_Row_Sorter_Record;
   begin
      Gtk.Tree_List_Row_Sorter.Initialize (Self, Sorter);
      return Self;
   end Gtk_Tree_List_Row_Sorter_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Tree_List_Row_Sorter_Record'Class;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      function Internal (Sorter : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_row_sorter_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Sorter))));
      end if;
   end Initialize;

   ----------------
   -- Get_Sorter --
   ----------------

   function Get_Sorter
      (Self : not null access Gtk_Tree_List_Row_Sorter_Record)
       return Gtk.Sorter.Gtk_Sorter
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_list_row_sorter_get_sorter");
      Stub_Gtk_Sorter : Gtk.Sorter.Gtk_Sorter_Record;
   begin
      return Gtk.Sorter.Gtk_Sorter (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Sorter));
   end Get_Sorter;

   ----------------
   -- Set_Sorter --
   ----------------

   procedure Set_Sorter
      (Self   : not null access Gtk_Tree_List_Row_Sorter_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      procedure Internal (Self : System.Address; Sorter : System.Address);
      pragma Import (C, Internal, "gtk_tree_list_row_sorter_set_sorter");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Sorter)));
   end Set_Sorter;

end Gtk.Tree_List_Row_Sorter;
