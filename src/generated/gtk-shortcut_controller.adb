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

package body Gtk.Shortcut_Controller is

   package Type_Conversion_Gtk_Shortcut_Controller is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Shortcut_Controller_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Shortcut_Controller);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Controller) is
   begin
      Self := new Gtk_Shortcut_Controller_Record;
      Gtk.Shortcut_Controller.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_For_Model --
   -----------------------

   procedure Gtk_New_For_Model
      (Self  : out Gtk_Shortcut_Controller;
       Model : Glib.List_Model.Glist_Model)
   is
   begin
      Self := new Gtk_Shortcut_Controller_Record;
      Gtk.Shortcut_Controller.Initialize_For_Model (Self, Model);
   end Gtk_New_For_Model;

   ---------------------------------
   -- Gtk_Shortcut_Controller_New --
   ---------------------------------

   function Gtk_Shortcut_Controller_New return Gtk_Shortcut_Controller is
      Self : constant Gtk_Shortcut_Controller := new Gtk_Shortcut_Controller_Record;
   begin
      Gtk.Shortcut_Controller.Initialize (Self);
      return Self;
   end Gtk_Shortcut_Controller_New;

   -------------------------------------------
   -- Gtk_Shortcut_Controller_New_For_Model --
   -------------------------------------------

   function Gtk_Shortcut_Controller_New_For_Model
      (Model : Glib.List_Model.Glist_Model) return Gtk_Shortcut_Controller
   is
      Self : constant Gtk_Shortcut_Controller := new Gtk_Shortcut_Controller_Record;
   begin
      Gtk.Shortcut_Controller.Initialize_For_Model (Self, Model);
      return Self;
   end Gtk_Shortcut_Controller_New_For_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Shortcut_Controller_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_controller_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_For_Model --
   --------------------------

   procedure Initialize_For_Model
      (Self  : not null access Gtk_Shortcut_Controller_Record'Class;
       Model : Glib.List_Model.Glist_Model)
   is
      function Internal
         (Model : Glib.List_Model.Glist_Model) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_controller_new_for_model");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Model));
      end if;
   end Initialize_For_Model;

   ------------------
   -- Add_Shortcut --
   ------------------

   procedure Add_Shortcut
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Shortcut : not null access Gtk.Shortcut.Gtk_Shortcut_Record'Class)
   is
      procedure Internal (Self : System.Address; Shortcut : System.Address);
      pragma Import (C, Internal, "gtk_shortcut_controller_add_shortcut");
   begin
      Internal (Get_Object (Self), Get_Object (Shortcut));
   end Add_Shortcut;

   -----------------------------
   -- Get_Mnemonics_Modifiers --
   -----------------------------

   function Get_Mnemonics_Modifiers
      (Self : not null access Gtk_Shortcut_Controller_Record)
       return Gdk.Enums.Gdk_Modifier_Type
   is
      function Internal
         (Self : System.Address) return Gdk.Enums.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_shortcut_controller_get_mnemonics_modifiers");
   begin
      return Internal (Get_Object (Self));
   end Get_Mnemonics_Modifiers;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope
      (Self : not null access Gtk_Shortcut_Controller_Record)
       return Gtk_Shortcut_Scope
   is
      function Internal (Self : System.Address) return Gtk_Shortcut_Scope;
      pragma Import (C, Internal, "gtk_shortcut_controller_get_scope");
   begin
      return Internal (Get_Object (Self));
   end Get_Scope;

   ---------------------
   -- Remove_Shortcut --
   ---------------------

   procedure Remove_Shortcut
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Shortcut : not null access Gtk.Shortcut.Gtk_Shortcut_Record'Class)
   is
      procedure Internal (Self : System.Address; Shortcut : System.Address);
      pragma Import (C, Internal, "gtk_shortcut_controller_remove_shortcut");
   begin
      Internal (Get_Object (Self), Get_Object (Shortcut));
   end Remove_Shortcut;

   -----------------------------
   -- Set_Mnemonics_Modifiers --
   -----------------------------

   procedure Set_Mnemonics_Modifiers
      (Self      : not null access Gtk_Shortcut_Controller_Record;
       Modifiers : Gdk.Enums.Gdk_Modifier_Type)
   is
      procedure Internal
         (Self      : System.Address;
          Modifiers : Gdk.Enums.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_shortcut_controller_set_mnemonics_modifiers");
   begin
      Internal (Get_Object (Self), Modifiers);
   end Set_Mnemonics_Modifiers;

   ---------------
   -- Set_Scope --
   ---------------

   procedure Set_Scope
      (Self  : not null access Gtk_Shortcut_Controller_Record;
       Scope : Gtk_Shortcut_Scope)
   is
      procedure Internal (Self : System.Address; Scope : Gtk_Shortcut_Scope);
      pragma Import (C, Internal, "gtk_shortcut_controller_set_scope");
   begin
      Internal (Get_Object (Self), Scope);
   end Set_Scope;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Shortcut_Controller_Record;
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
      (Self : not null access Gtk_Shortcut_Controller_Record) return GType
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
      (Self : not null access Gtk_Shortcut_Controller_Record) return Guint
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
      (Self     : not null access Gtk_Shortcut_Controller_Record;
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

end Gtk.Shortcut_Controller;
