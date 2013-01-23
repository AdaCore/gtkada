------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

package body Gtk.Stock is

   function From_Object_Free (B : access Gtk_Stock_Item) return Gtk_Stock_Item is
      Result : constant Gtk_Stock_Item := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   procedure Gtk_New
     (Item               : out Gtk_Stock_Item;
      Stock_Id           : String;
      Label              : UTF8_String;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : String) is
   begin
      Item.Stock_Id           := Interfaces.C.Strings.New_String (Stock_Id);
      Item.Label              := Interfaces.C.Strings.New_String (Label);
      Item.Modifier           := Modifier;
      Item.Keyval             := Guint (Keyval);
      Item.Translation_Domain := Interfaces.C.Strings.New_String (Translation_Domain);
   end Gtk_New;

   procedure Free (Item : in out Gtk_Stock_Item) is
   begin
      Interfaces.C.Strings.Free (Item.Stock_Id);
      Interfaces.C.Strings.Free (Item.Label);
      Interfaces.C.Strings.Free (Item.Translation_Domain);
   end Free;

   ---------
   -- Add --
   ---------

   procedure Add (Item : Gtk_Stock_Item) is
      procedure Internal (Item : Gtk_Stock_Item; N_Items : Guint := 1);
      pragma Import (C, Internal, "gtk_stock_add");

   begin
      Internal (Item);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Items : Gtk_Stock_Item_Array) is
      procedure Internal
         (Items   : Gtk_Stock_Item_Array;
          N_Items : Guint);
      pragma Import (C, Internal, "gtk_stock_add");
   begin
      Internal (Items, Items'Length);
   end Add;

   ----------------
   -- Add_Static --
   ----------------

   procedure Add_Static (Items : Gtk_Stock_Item_Array) is
      procedure Internal
         (Items   : Gtk_Stock_Item_Array;
          N_Items : Guint);
      pragma Import (C, Internal, "gtk_stock_add_static");
   begin
      Internal (Items, Items'Length);
   end Add_Static;

   ------------
   -- Lookup --
   ------------

   procedure Lookup
      (Stock_Id : UTF8_String;
       Item     : out Gtk_Stock_Item;
       Success  : out Boolean)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr;
          Acc_Item : access Gtk_Stock_Item) return Integer;
      pragma Import (C, Internal, "gtk_stock_lookup");
      Acc_Item     : aliased Gtk_Stock_Item;
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Acc_Item : aliased Gtk_Stock_Item;
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id, Tmp_Acc_Item'Access);
      Acc_Item := Tmp_Acc_Item;
      Free (Tmp_Stock_Id);
      Item := Acc_Item;
      Success := Tmp_Return /= 0;
   end Lookup;

end Gtk.Stock;
