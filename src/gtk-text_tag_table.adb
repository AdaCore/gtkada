------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with System;
with Gtk.Text_Tag;  use Gtk.Text_Tag;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Tag_Table is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Tag_Table_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Table : out Gtk_Text_Tag_Table) is
   begin
      Table := new Gtk_Text_Tag_Table_Record;
      Gtk.Text_Tag_Table.Initialize (Table);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Table : access Gtk_Text_Tag_Table_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_tag_table_new");

   begin
      Set_Object (Table, Internal);
   end Initialize;

   ---------
   -- Add --
   ---------

   procedure Add
     (Table : access Gtk_Text_Tag_Table_Record;
      Tag   : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
   is
      procedure Internal (Table : System.Address; Tag : System.Address);
      pragma Import (C, Internal, "gtk_text_tag_table_add");

   begin
      Internal (Get_Object (Table), Get_Object (Tag));
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Table : access Gtk_Text_Tag_Table_Record;
      Tag   : access Gtk.Text_Tag.Gtk_Text_Tag_Record'Class)
   is
      procedure Internal (Table : System.Address; Tag : System.Address);
      pragma Import (C, Internal, "gtk_text_tag_table_remove");

   begin
      Internal (Get_Object (Table), Get_Object (Tag));
   end Remove;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Table : access Gtk_Text_Tag_Table_Record;
      Name  : String) return Gtk.Text_Tag.Gtk_Text_Tag
   is
      function Internal
        (Table : System.Address; Name : String) return System.Address;
      pragma Import (C, Internal, "gtk_text_tag_table_lookup");

      Stub : Gtk.Text_Tag.Gtk_Text_Tag_Record;

      S : System.Address;
      use type System.Address;

   begin
      S := Internal (Get_Object (Table), Name & ASCII.NUL);

      if S = System.Null_Address then
         return null;
      else
         return Gtk.Text_Tag.Gtk_Text_Tag (Get_User_Data_Fast (S, Stub));
      end if;
   end Lookup;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Table : access Gtk_Text_Tag_Table_Record) return Gint is
      function Internal (Table : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_text_tag_table_get_size");

   begin
      return Internal (Get_Object (Table));
   end Get_Size;

   --------------
   -- Iterator --
   --------------

   package body Iterator is

      ---------------------------------------
      -- C_Gtk_Text_Tag_Table_Foreach_Proc --
      ---------------------------------------

      procedure C_Gtk_Text_Tag_Table_Foreach_Proc
        (C_Tag  : System.Address;
         C_Data : Foreach_Proc_Record_Access)
      is
         Stub : Gtk_Text_Tag_Record;
         Tag  : constant Gtk_Text_Tag :=
           Gtk_Text_Tag (Get_User_Data_Fast (C_Tag, Stub));

      begin
         C_Data.Proc (Tag, C_Data.Data);
      end C_Gtk_Text_Tag_Table_Foreach_Proc;

      -------------
      -- Foreach --
      -------------

      procedure Foreach
        (Table : access Gtk_Text_Tag_Table_Record;
         Proc  : Gtk_Text_Tag_Table_Proc;
         Data  : Data_Type_Access)
      is
         procedure Internal
           (Table : System.Address;
            Proc  : System.Address;
            Data  : System.Address);
         pragma Import (C, Internal, "gtk_text_tag_table_foreach");

         C_Proc_Address : System.Address;
         Local_Data : aliased constant Foreach_Proc_Record :=
           (Proc => Proc, Data => Data);

      begin
         if Proc = null then
            C_Proc_Address := System.Null_Address;
         else
            C_Proc_Address := C_Gtk_Text_Tag_Table_Foreach_Proc'Address;
         end if;

         Internal (Get_Object (Table), C_Proc_Address, Local_Data'Address);
      end Foreach;

   end Iterator;

end Gtk.Text_Tag_Table;
