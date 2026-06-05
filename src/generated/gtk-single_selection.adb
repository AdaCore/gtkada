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

package body Gtk.Single_Selection is

   package Type_Conversion_Gtk_Single_Selection is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Single_Selection_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Single_Selection);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self  : out Gtk_Single_Selection;
       Model : Glib.List_Model.Glist_Model)
   is
   begin
      Self := new Gtk_Single_Selection_Record;
      Gtk.Single_Selection.Initialize (Self, Model);
   end Gtk_New;

   ------------------------------
   -- Gtk_Single_Selection_New --
   ------------------------------

   function Gtk_Single_Selection_New
      (Model : Glib.List_Model.Glist_Model) return Gtk_Single_Selection
   is
      Self : constant Gtk_Single_Selection := new Gtk_Single_Selection_Record;
   begin
      Gtk.Single_Selection.Initialize (Self, Model);
      return Self;
   end Gtk_Single_Selection_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Single_Selection_Record'Class;
       Model : Glib.List_Model.Glist_Model)
   is
      function Internal
         (Model : Glib.List_Model.Glist_Model) return System.Address;
      pragma Import (C, Internal, "gtk_single_selection_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Model));
      end if;
   end Initialize;

   --------------------
   -- Get_Autoselect --
   --------------------

   function Get_Autoselect
      (Self : not null access Gtk_Single_Selection_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_single_selection_get_autoselect");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Autoselect;

   ----------------------
   -- Get_Can_Unselect --
   ----------------------

   function Get_Can_Unselect
      (Self : not null access Gtk_Single_Selection_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_single_selection_get_can_unselect");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Can_Unselect;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Single_Selection_Record)
       return Glib.List_Model.Glist_Model
   is
      function Internal
         (Self : System.Address) return Glib.List_Model.Glist_Model;
      pragma Import (C, Internal, "gtk_single_selection_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected
      (Self : not null access Gtk_Single_Selection_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_single_selection_get_selected");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
      (Self : not null access Gtk_Single_Selection_Record)
       return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_single_selection_get_selected_item");
   begin
      return Internal (Get_Object (Self));
   end Get_Selected_Item;

   --------------------
   -- Set_Autoselect --
   --------------------

   procedure Set_Autoselect
      (Self       : not null access Gtk_Single_Selection_Record;
       Autoselect : Boolean)
   is
      procedure Internal (Self : System.Address; Autoselect : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_single_selection_set_autoselect");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Autoselect));
   end Set_Autoselect;

   ----------------------
   -- Set_Can_Unselect --
   ----------------------

   procedure Set_Can_Unselect
      (Self         : not null access Gtk_Single_Selection_Record;
       Can_Unselect : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Can_Unselect : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_single_selection_set_can_unselect");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Can_Unselect));
   end Set_Can_Unselect;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Single_Selection_Record;
       Model : Glib.List_Model.Glist_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Glib.List_Model.Glist_Model);
      pragma Import (C, Internal, "gtk_single_selection_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   ------------------
   -- Set_Selected --
   ------------------

   procedure Set_Selected
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint)
   is
      procedure Internal (Self : System.Address; Position : Guint);
      pragma Import (C, Internal, "gtk_single_selection_set_selected");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Selected;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint) return System.Address
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_item");
   begin
      return Internal (Get_Object (Self), Position);
   end Get_Item;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
      (Self : not null access Gtk_Single_Selection_Record) return GType
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
      (Self : not null access Gtk_Single_Selection_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self), Position), Stub_GObject);
   end Get_Object;

   -----------------
   -- Get_Section --
   -----------------

   procedure Get_Section
      (Self      : not null access Gtk_Single_Selection_Record;
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
   -- Get_Selection --
   -------------------

   function Get_Selection
      (Self : not null access Gtk_Single_Selection_Record)
       return Gtk.Bitset.Gtk_Bitset
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_selection_model_get_selection");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Selection;

   ----------------------------
   -- Get_Selection_In_Range --
   ----------------------------

   function Get_Selection_In_Range
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint;
       N_Items  : Guint) return Gtk.Bitset.Gtk_Bitset
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          N_Items  : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_selection_model_get_selection_in_range");
   begin
      return From_Object (Internal (Get_Object (Self), Position, N_Items));
   end Get_Selection_In_Range;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_is_selected");
   begin
      return Internal (Get_Object (Self), Position) /= 0;
   end Is_Selected;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_Single_Selection_Record;
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
      (Self     : not null access Gtk_Single_Selection_Record;
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

   ----------------
   -- Select_All --
   ----------------

   function Select_All
      (Self : not null access Gtk_Single_Selection_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_all");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Select_All;

   -----------------
   -- Select_Item --
   -----------------

   function Select_Item
      (Self          : not null access Gtk_Single_Selection_Record;
       Position      : Guint;
       Unselect_Rest : Boolean) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Position      : Guint;
          Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_item");
   begin
      return Internal (Get_Object (Self), Position, Boolean'Pos (Unselect_Rest)) /= 0;
   end Select_Item;

   ------------------
   -- Select_Range --
   ------------------

   function Select_Range
      (Self          : not null access Gtk_Single_Selection_Record;
       Position      : Guint;
       N_Items       : Guint;
       Unselect_Rest : Boolean) return Boolean
   is
      function Internal
         (Self          : System.Address;
          Position      : Guint;
          N_Items       : Guint;
          Unselect_Rest : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_select_range");
   begin
      return Internal (Get_Object (Self), Position, N_Items, Boolean'Pos (Unselect_Rest)) /= 0;
   end Select_Range;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint;
       N_Items  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          N_Items  : Guint);
      pragma Import (C, Internal, "gtk_selection_model_selection_changed");
   begin
      Internal (Get_Object (Self), Position, N_Items);
   end Selection_Changed;

   -------------------
   -- Set_Selection --
   -------------------

   function Set_Selection
      (Self     : not null access Gtk_Single_Selection_Record;
       Selected : Gtk.Bitset.Gtk_Bitset;
       Mask     : Gtk.Bitset.Gtk_Bitset) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Selected : System.Address;
          Mask     : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_set_selection");
   begin
      return Internal (Get_Object (Self), Get_Object (Selected), Get_Object (Mask)) /= 0;
   end Set_Selection;

   ------------------
   -- Unselect_All --
   ------------------

   function Unselect_All
      (Self : not null access Gtk_Single_Selection_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_all");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Unselect_All;

   -------------------
   -- Unselect_Item --
   -------------------

   function Unselect_Item
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_item");
   begin
      return Internal (Get_Object (Self), Position) /= 0;
   end Unselect_Item;

   --------------------
   -- Unselect_Range --
   --------------------

   function Unselect_Range
      (Self     : not null access Gtk_Single_Selection_Record;
       Position : Guint;
       N_Items  : Guint) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          N_Items  : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_selection_model_unselect_range");
   begin
      return Internal (Get_Object (Self), Position, N_Items) /= 0;
   end Unselect_Range;

end Gtk.Single_Selection;
