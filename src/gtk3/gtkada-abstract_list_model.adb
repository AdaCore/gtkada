------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Gtk.Tree_Model; use Gtk.Tree_Model;

package body Gtkada.Abstract_List_Model is

   --------------
   -- Children --
   --------------

   function Children
     (Self   : access Gtk_Abstract_List_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      if Parent = Null_Iter then
         declare
            Path : Gtk.Tree_Model.Gtk_Tree_Path;
            Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            Gtk_New_First (Path);
            Iter := Gtk.Tree_Model.Get_Iter (+Self, Path);
            Gtk.Tree_Model.Path_Free (Path);
            return Iter;
         end;
      end if;
      return Gtk.Tree_Model.Null_Iter;
   end Children;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Self : access Gtk_Abstract_List_Model_Record)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      pragma Unreferenced (Self);

   begin
      return Gtk.Tree_Model.Tree_Model_List_Only;
   end Get_Flags;

   ---------------
   -- Has_Child --
   ---------------

   function Has_Child
     (Self : access Gtk_Abstract_List_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
   begin
      if Iter = Null_Iter then
         return Gtk.Tree_Model.Children (+Self, (Iter)) /= Null_Iter;
      else
         return False;
      end if;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Abstract_List_Model_Record'Class) is
   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);
   end Initialize;

   ------------
   -- Parent --
   ------------

   function Parent
     (Self  : access Gtk_Abstract_List_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self, Child);

   begin
      return Gtk.Tree_Model.Null_Iter;
   end Parent;

end Gtkada.Abstract_List_Model;
