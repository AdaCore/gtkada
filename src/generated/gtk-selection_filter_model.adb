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

package body Gtk.Selection_Filter_Model is

   package Type_Conversion_Gtk_Selection_Filter_Model is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Selection_Filter_Model_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Selection_Filter_Model);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self  : out Gtk_Selection_Filter_Model;
       Model : Gtk.Selection_Model.Gtk_Selection_Model)
   is
   begin
      Self := new Gtk_Selection_Filter_Model_Record;
      Gtk.Selection_Filter_Model.Initialize (Self, Model);
   end Gtk_New;

   ------------------------------------
   -- Gtk_Selection_Filter_Model_New --
   ------------------------------------

   function Gtk_Selection_Filter_Model_New
      (Model : Gtk.Selection_Model.Gtk_Selection_Model)
       return Gtk_Selection_Filter_Model
   is
      Self : constant Gtk_Selection_Filter_Model := new Gtk_Selection_Filter_Model_Record;
   begin
      Gtk.Selection_Filter_Model.Initialize (Self, Model);
      return Self;
   end Gtk_Selection_Filter_Model_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Selection_Filter_Model_Record'Class;
       Model : Gtk.Selection_Model.Gtk_Selection_Model)
   is
      function Internal
         (Model : Gtk.Selection_Model.Gtk_Selection_Model)
          return System.Address;
      pragma Import (C, Internal, "gtk_selection_filter_model_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Model));
      end if;
   end Initialize;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return Gtk.Selection_Model.Gtk_Selection_Model
   is
      function Internal
         (Self : System.Address)
          return Gtk.Selection_Model.Gtk_Selection_Model;
      pragma Import (C, Internal, "gtk_selection_filter_model_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Selection_Filter_Model_Record;
       Model : Gtk.Selection_Model.Gtk_Selection_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Gtk.Selection_Model.Gtk_Selection_Model);
      pragma Import (C, Internal, "gtk_selection_filter_model_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Selection_Filter_Model_Record;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self), Position), Stub_GObject);
   end Get_Item;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return GType
   is
      function Internal (Self : System.Address) return GType;
      pragma Import (C, Internal, "g_list_model_get_item_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Item_Type;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Gtk_Selection_Filter_Model_Record)
       return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_Selection_Filter_Model_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Removed  : Guint;
          Added    : Guint);
      pragma Import (C, Internal, "g_list_model_items_changed");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Items_Changed;

end Gtk.Selection_Filter_Model;
