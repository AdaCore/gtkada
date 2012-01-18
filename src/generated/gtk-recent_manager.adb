------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Recent_Manager is

   function Convert (R : Gtk.Recent_Info.Gtk_Recent_Info) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Recent_Info.Gtk_Recent_Info is
   begin
      return From_Object(R);
   end Convert;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Recent_Manager_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Recent_Manager) is
   begin
      Self := new Gtk_Recent_Manager_Record;
      Gtk.Recent_Manager.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Recent_Manager_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   --------------
   -- Add_Full --
   --------------

   function Add_Full
      (Self        : not null access Gtk_Recent_Manager_Record;
       URI         : UTF8_String;
       Recent_Data : Gtk_Recent_Data) return Boolean
   is
      function Internal
         (Self        : System.Address;
          URI         : Interfaces.C.Strings.chars_ptr;
          Recent_Data : Gtk_Recent_Data) return Integer;
      pragma Import (C, Internal, "gtk_recent_manager_add_full");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI, Recent_Data);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Add_Full;

   --------------
   -- Add_Item --
   --------------

   function Add_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          URI  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_manager_add_item");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Add_Item;

   ---------------
   -- Get_Items --
   ---------------

   function Get_Items
      (Self : not null access Gtk_Recent_Manager_Record)
       return Gtk_Recent_Info_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_items");
      Tmp_Return : Gtk_Recent_Info_List.Glist;
   begin
      Gtk.Recent_Manager.Gtk_Recent_Info_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Items;

   --------------
   -- Has_Item --
   --------------

   function Has_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          URI  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_manager_has_item");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Has_Item;

   -----------------
   -- Lookup_Item --
   -----------------

   function Lookup_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Gtk.Recent_Info.Gtk_Recent_Info
   is
      function Internal
         (Self : System.Address;
          URI  : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_lookup_item");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI);
      Free (Tmp_URI);
      return From_Object (Tmp_Return);
   end Lookup_Item;

   ---------------
   -- Move_Item --
   ---------------

   function Move_Item
      (Self    : not null access Gtk_Recent_Manager_Record;
       URI     : UTF8_String;
       New_Uri : UTF8_String) return Boolean
   is
      function Internal
         (Self    : System.Address;
          URI     : Interfaces.C.Strings.chars_ptr;
          New_Uri : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_manager_move_item");
      Tmp_URI     : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_New_Uri : Interfaces.C.Strings.chars_ptr := New_String (New_Uri);
      Tmp_Return  : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI, Tmp_New_Uri);
      Free (Tmp_URI);
      Free (Tmp_New_Uri);
      return Boolean'Val (Tmp_Return);
   end Move_Item;

   -----------------
   -- Purge_Items --
   -----------------

   function Purge_Items
      (Self : not null access Gtk_Recent_Manager_Record) return Gint
   is
      function Internal (Self : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_recent_manager_purge_items");
   begin
      return Internal (Get_Object (Self));
   end Purge_Items;

   -----------------
   -- Remove_Item --
   -----------------

   function Remove_Item
      (Self : not null access Gtk_Recent_Manager_Record;
       URI  : UTF8_String) return Boolean
   is
      function Internal
         (Self : System.Address;
          URI  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_manager_remove_item");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_URI);
      Free (Tmp_URI);
      return Boolean'Val (Tmp_Return);
   end Remove_Item;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Recent_Manager is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_default");
      Stub_Gtk_Recent_Manager : Gtk_Recent_Manager_Record;
   begin
      return Gtk.Recent_Manager.Gtk_Recent_Manager (Get_User_Data (Internal, Stub_Gtk_Recent_Manager));
   end Get_Default;

end Gtk.Recent_Manager;
