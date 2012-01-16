------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Check_Menu_Item is

   -----------------------
   -- Local Subprograms --
   -----------------------

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Check_Menu_Item_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean
   is
      function Internal (Item : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_check_menu_item_get_active");
   begin
      return Internal (Get_Object (Check_Menu_Item)) /= 0;
   end Get_Active;

   ----------------------
   -- Get_Inconsistent --
   ----------------------

   function Get_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean
   is
      function Internal (Item : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_check_menu_item_get_inconsistent");
   begin
      return Internal (Get_Object (Check_Menu_Item)) /= 0;
   end Get_Inconsistent;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label           : UTF8_String := "") is
   begin
      Check_Menu_Item := new Gtk_Check_Menu_Item_Record;
      Initialize (Check_Menu_Item, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label           : UTF8_String := "")
   is
      function Internal (Label : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new_with_label");
      function Internal_No_Label return System.Address;
      pragma Import (C, Internal_No_Label, "gtk_check_menu_item_new");
   begin
      if Label = "" then
         Set_Object (Check_Menu_Item, Internal_No_Label);
      else
         Set_Object (Check_Menu_Item, Internal (Label & ASCII.NUL));
      end if;
   end Initialize;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Check_Menu_Item : out Gtk_Check_Menu_Item;
      Label           : UTF8_String) is
   begin
      Check_Menu_Item := new Gtk_Check_Menu_Item_Record;
      Initialize_With_Mnemonic (Check_Menu_Item, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_With_Mnemonic
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record'Class;
      Label           : UTF8_String)
   is
      function Internal (Label : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_check_menu_item_new_with_mnemonic");

   begin
      Set_Object (Check_Menu_Item, Internal (Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Is_Active       : Boolean)
   is
      procedure Internal (Check_Menu_Item : System.Address; Is_Active : Gint);
      pragma Import (C, Internal, "gtk_check_menu_item_set_active");

   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Is_Active));
   end Set_Active;

   ----------------------
   -- Set_Inconsistent --
   ----------------------

   procedure Set_Inconsistent
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Setting         : Boolean)
   is
      procedure Internal (Check_Menu_Item : System.Address; Setting : Gint);
      pragma Import (C, Internal, "gtk_check_menu_item_set_inconsistent");

   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Setting));
   end Set_Inconsistent;

   -------------
   -- Toggled --
   -------------

   procedure Toggled (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) is
      procedure Internal (Check_Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_check_menu_item_toggled");

   begin
      Internal (Get_Object (Check_Menu_Item));
   end Toggled;

   -----------------------
   -- Get_Draw_As_Radio --
   -----------------------

   function Get_Draw_As_Radio
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record) return Boolean
   is
      function Internal (Check_Menu_Item : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_check_menu_item_get_draw_as_radio");
   begin
      return Boolean'Val (Internal (Get_Object (Check_Menu_Item)));
   end Get_Draw_As_Radio;

   -----------------------
   -- Set_Draw_As_Radio --
   -----------------------

   procedure Set_Draw_As_Radio
     (Check_Menu_Item : access Gtk_Check_Menu_Item_Record;
      Draw_As_Radio   : Boolean)
   is
      procedure Internal
        (Check_Menu_Item : System.Address; Draw_As_Radio : Gboolean);
      pragma Import (C, Internal, "gtk_check_menu_item_set_draw_as_radio");
   begin
      Internal (Get_Object (Check_Menu_Item), Boolean'Pos (Draw_As_Radio));
   end Set_Draw_As_Radio;

end Gtk.Check_Menu_Item;
