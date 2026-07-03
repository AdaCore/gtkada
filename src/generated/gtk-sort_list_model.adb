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

package body Gtk.Sort_List_Model is

   package Type_Conversion_Gtk_Sort_List_Model is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Sort_List_Model_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Sort_List_Model);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Sort_List_Model;
       Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
   begin
      Self := new Gtk_Sort_List_Model_Record;
      Gtk.Sort_List_Model.Initialize (Self, Model, Sorter);
   end Gtk_New;

   -----------------------------
   -- Gtk_Sort_List_Model_New --
   -----------------------------

   function Gtk_Sort_List_Model_New
      (Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
       return Gtk_Sort_List_Model
   is
      Self : constant Gtk_Sort_List_Model := new Gtk_Sort_List_Model_Record;
   begin
      Gtk.Sort_List_Model.Initialize (Self, Model, Sorter);
      return Self;
   end Gtk_Sort_List_Model_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Sort_List_Model_Record'Class;
       Model  : Glib.List_Model.Glist_Model;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      function Internal
         (Model  : Glib.List_Model.Glist_Model;
          Sorter : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sort_list_model_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Model, Get_Object_Or_Null (GObject (Sorter))));
      end if;
   end Initialize;

   ---------------------
   -- Get_Incremental --
   ---------------------

   function Get_Incremental
      (Self : not null access Gtk_Sort_List_Model_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_sort_list_model_get_incremental");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Incremental;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_sort_list_model_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   -----------------
   -- Get_Pending --
   -----------------

   function Get_Pending
      (Self : not null access Gtk_Sort_List_Model_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_sort_list_model_get_pending");
   begin
      return Internal (Get_Object (Self));
   end Get_Pending;

   ------------------------
   -- Get_Section_Sorter --
   ------------------------

   function Get_Section_Sorter
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Gtk.Sorter.Gtk_Sorter
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sort_list_model_get_section_sorter");
      Stub_Gtk_Sorter : Gtk.Sorter.Gtk_Sorter_Record;
   begin
      return Gtk.Sorter.Gtk_Sorter (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Sorter));
   end Get_Section_Sorter;

   ----------------
   -- Get_Sorter --
   ----------------

   function Get_Sorter
      (Self : not null access Gtk_Sort_List_Model_Record)
       return Gtk.Sorter.Gtk_Sorter
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sort_list_model_get_sorter");
      Stub_Gtk_Sorter : Gtk.Sorter.Gtk_Sorter_Record;
   begin
      return Gtk.Sorter.Gtk_Sorter (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Sorter));
   end Get_Sorter;

   ---------------------
   -- Set_Incremental --
   ---------------------

   procedure Set_Incremental
      (Self        : not null access Gtk_Sort_List_Model_Record;
       Incremental : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Incremental : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_sort_list_model_set_incremental");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Incremental));
   end Set_Incremental;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Sort_List_Model_Record;
       Model : Glib.List_Model.Glist_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Glib.List_Model.Glist_Model);
      pragma Import (C, Internal, "gtk_sort_list_model_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   ------------------------
   -- Set_Section_Sorter --
   ------------------------

   procedure Set_Section_Sorter
      (Self   : not null access Gtk_Sort_List_Model_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      procedure Internal (Self : System.Address; Sorter : System.Address);
      pragma Import (C, Internal, "gtk_sort_list_model_set_section_sorter");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Sorter)));
   end Set_Section_Sorter;

   ----------------
   -- Set_Sorter --
   ----------------

   procedure Set_Sorter
      (Self   : not null access Gtk_Sort_List_Model_Record;
       Sorter : access Gtk.Sorter.Gtk_Sorter_Record'Class)
   is
      procedure Internal (Self : System.Address; Sorter : System.Address);
      pragma Import (C, Internal, "gtk_sort_list_model_set_sorter");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Sorter)));
   end Set_Sorter;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Sort_List_Model_Record;
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
      (Self : not null access Gtk_Sort_List_Model_Record) return GType
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
      (Self : not null access Gtk_Sort_List_Model_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -----------------
   -- Get_Section --
   -----------------

   procedure Get_Section
      (Self      : not null access Gtk_Sort_List_Model_Record;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint)
   is
      procedure Internal
         (Self      : System.Address;
          Position  : Guint;
          Out_Start : out Guint;
          Out_End   : out Guint);
      pragma Import (C, Internal, "gtk_section_model_get_section");
   begin
      Internal (Get_Object (Self), Position, Out_Start, Out_End);
   end Get_Section;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_Sort_List_Model_Record;
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

   ----------------------
   -- Sections_Changed --
   ----------------------

   procedure Sections_Changed
      (Self     : not null access Gtk_Sort_List_Model_Record;
       Position : Guint;
       N_Items  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          N_Items  : Guint);
      pragma Import (C, Internal, "gtk_section_model_sections_changed");
   begin
      Internal (Get_Object (Self), Position, N_Items);
   end Sections_Changed;

end Gtk.Sort_List_Model;
