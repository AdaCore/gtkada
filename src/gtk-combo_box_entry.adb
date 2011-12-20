------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Glib.Type_Conversion_Hooks;

package body Gtk.Combo_Box_Entry is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Combo_Box_Entry_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Combo : out Gtk_Combo_Box_Entry) is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Gtk.Combo_Box_Entry.Initialize (Combo);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Combo : access Gtk_Combo_Box_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new");
   begin
      Set_Object (Combo, Internal);
   end Initialize;

   ------------------
   -- Gtk_New_Text --
   ------------------

   procedure Gtk_New_Text (Combo : out Gtk_Combo_Box_Entry) is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Gtk.Combo_Box_Entry.Initialize_Text (Combo);
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text
     (Combo : access Gtk_Combo_Box_Entry_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new_text");
   begin
      Set_Object (Combo, Internal);
   end Initialize_Text;

   ------------------------
   -- Gtk_New_With_Model --
   ------------------------

   procedure Gtk_New_With_Model
     (Combo       : out Gtk_Combo_Box_Entry;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint)
   is
   begin
      Combo := new Gtk_Combo_Box_Entry_Record;
      Initialize_With_Model (Combo, Model, Text_Column);
   end Gtk_New_With_Model;

   ---------------------------
   -- Initialize_With_Model --
   ---------------------------

   procedure Initialize_With_Model
     (Combo       : access Gtk_Combo_Box_Entry_Record'Class;
      Model       : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Text_Column : Gint)
   is
      function Internal
        (Model       : System.Address;
         Text_Column : Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_combo_box_entry_new_with_model");
   begin
      Set_Object
        (Combo, Internal (Get_Object (Model), Text_Column));
   end Initialize_With_Model;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record; Text_Column : Gint)
   is
      procedure Internal
        (Entry_Box   : System.Address;
         Text_Column : Gint);
      pragma Import (C, Internal, "gtk_combo_box_entry_set_text_column");
   begin
      Internal (Get_Object (Entry_Box), Text_Column);
   end Set_Text_Column;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
     (Entry_Box : access Gtk_Combo_Box_Entry_Record)  return Gint
   is
      function Internal (Entry_Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_combo_box_entry_get_text_column");
   begin
      return Internal (Get_Object (Entry_Box));
   end Get_Text_Column;

end Gtk.Combo_Box_Entry;
