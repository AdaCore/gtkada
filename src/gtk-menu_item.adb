-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Menu_Item is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   --------------
   -- Activate --
   --------------

   procedure Activate (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_activate");

   begin
      Internal (Get_Object (Menu_Item));
   end Activate;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_deselect");

   begin
      Internal (Get_Object (Menu_Item));
   end Deselect;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item; Label : String := "") is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Initialize (Menu_Item, Label);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Menu_Item : out Gtk_Menu_Item;
      Label     : String) is
   begin
      Menu_Item := new Gtk_Menu_Item_Record;
      Initialize_With_Mnemonic (Menu_Item, Label);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Gtk_Select --
   ----------------

   procedure Gtk_Select (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_select");

   begin
      Internal (Get_Object (Menu_Item));
   end Gtk_Select;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Label     : String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_label");

      function Internal2 return System.Address;
      pragma Import (C, Internal2, "gtk_menu_item_new");

   begin
      if Label = "" then
         Set_Object (Menu_Item, Internal2);
      else
         Set_Object (Menu_Item, Internal (Label & ASCII.NUL));
      end if;

      Initialize_User_Data (Menu_Item);
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Menu_Item : access Gtk_Menu_Item_Record'Class;
      Label     : String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_mnemonic");
   begin
      Set_Object (Menu_Item, Internal (Label & ASCII.NUL));
      Initialize_User_Data (Menu_Item);
   end Initialize_With_Mnemonic;

   -----------------
   -- Get_Submenu --
   -----------------

   function Get_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record) return Boolean
   is
      function Internal (Menu_Item : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_menu_item_get_right_justified");
   begin
      return To_Boolean (Internal (Get_Object (Menu_Item)));
   end Get_Right_Justified;

   -----------------
   -- Get_Submenu --
   -----------------

   function Get_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Item : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_get_submenu");
   begin
      return Gtk.Widget.Convert (Internal (Get_Object (Menu_Item)));
   end Get_Submenu;

   --------------------
   -- Remove_Submenu --
   --------------------

   procedure Remove_Submenu (Menu_Item : access Gtk_Menu_Item_Record) is
      procedure Internal (Menu_Item : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_remove_submenu");

   begin
      Internal (Get_Object (Menu_Item));
   end Remove_Submenu;

   -------------------
   -- Right_Justify --
   --------------------

   procedure Right_Justify (Menu_Item : access Gtk_Menu_Item_Record) is
   begin
      Set_Right_Justified (Menu_Item, True);
   end Right_Justify;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
     (Menu_Item  : access Gtk_Menu_Item_Record;
      Accel_Path : String)
   is
      procedure Internal
        (Menu_Item  : System.Address;
         Accel_Path : String);
      pragma Import (C, Internal, "gtk_menu_item_set_accel_path");

   begin
      Internal (Get_Object (Menu_Item), Accel_Path & ASCII.NUL);
   end Set_Accel_Path;

   -----------------
   -- Set_Submenu --
   -----------------

   procedure Set_Submenu
     (Menu_Item : access Gtk_Menu_Item_Record;
      Submenu   : access Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Menu_Item : System.Address;
         Submenu   : System.Address);
      pragma Import (C, Internal, "gtk_menu_item_set_submenu");

   begin
      Internal (Get_Object (Menu_Item), Get_Object (Submenu));
   end Set_Submenu;

   -------------------------
   -- Set_Right_Justified --
   -------------------------

   procedure Set_Right_Justified
     (Menu_Item : access Gtk_Menu_Item_Record;
      Justify   : Boolean := True)
   is
      procedure Internal
        (Menu_Item : System.Address;
         Justify   : Gboolean);
      pragma Import (C, Internal, "gtk_menu_item_set_right_justified");

   begin
      Internal (Get_Object (Menu_Item), To_Gboolean (Justify));
   end Set_Right_Justified;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkMenuItem" then
         return new Gtk_Menu_Item_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Menu_Item;
