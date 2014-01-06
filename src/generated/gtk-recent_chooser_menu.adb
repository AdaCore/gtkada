------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Ada.Unchecked_Conversion;
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Gtk.Recent_Chooser_Menu is

   procedure C_Gtk_Recent_Chooser_Set_Sort_Func
      (Chooser      : System.Address;
       Sort_Func    : System.Address;
       Sort_Data    : System.Address;
       Data_Destroy : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Recent_Chooser_Set_Sort_Func, "gtk_recent_chooser_set_sort_func");
   --  Sets the comparison function used when sorting to be Sort_Func. If the
   --  Chooser has the sort type set to GTK_RECENT_SORT_CUSTOM then the chooser
   --  will sort using this function.
   --  To the comparison function will be passed two
   --  Gtk.Recent_Info.Gtk_Recent_Info structs and Sort_Data; Sort_Func should
   --  return a positive integer if the first item comes before the second,
   --  zero if the two items are equal and a negative integer if the first item
   --  comes after the second.
   --  Since: gtk+ 2.10
   --  "sort_func": the comparison function
   --  "sort_data": user data to pass to Sort_Func, or null
   --  "data_destroy": destroy notifier for Sort_Data, or null

   function To_Gtk_Recent_Sort_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Recent_Sort_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Recent_Sort_Func, System.Address);

   function Internal_Gtk_Recent_Sort_Func
      (A         : System.Address;
       B         : System.Address;
       User_Data : System.Address) return Gint;
   pragma Convention (C, Internal_Gtk_Recent_Sort_Func);

   -----------------------------------
   -- Internal_Gtk_Recent_Sort_Func --
   -----------------------------------

   function Internal_Gtk_Recent_Sort_Func
      (A         : System.Address;
       B         : System.Address;
       User_Data : System.Address) return Gint
   is
      Func : constant Gtk_Recent_Sort_Func := To_Gtk_Recent_Sort_Func (User_Data);
   begin
      return Func (From_Object (A), From_Object (B));
   end Internal_Gtk_Recent_Sort_Func;

   package Type_Conversion_Gtk_Recent_Chooser_Menu is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Recent_Chooser_Menu_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Recent_Chooser_Menu);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Recent_Chooser_Menu) is
   begin
      Self := new Gtk_Recent_Chooser_Menu_Record;
      Gtk.Recent_Chooser_Menu.Initialize (Self);
   end Gtk_New;

   -------------------------
   -- Gtk_New_For_Manager --
   -------------------------

   procedure Gtk_New_For_Manager
      (Self    : out Gtk_Recent_Chooser_Menu;
       Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
   is
   begin
      Self := new Gtk_Recent_Chooser_Menu_Record;
      Gtk.Recent_Chooser_Menu.Initialize_For_Manager (Self, Manager);
   end Gtk_New_For_Manager;

   ---------------------------------
   -- Gtk_Recent_Chooser_Menu_New --
   ---------------------------------

   function Gtk_Recent_Chooser_Menu_New return Gtk_Recent_Chooser_Menu is
      Self : constant Gtk_Recent_Chooser_Menu := new Gtk_Recent_Chooser_Menu_Record;
   begin
      Gtk.Recent_Chooser_Menu.Initialize (Self);
      return Self;
   end Gtk_Recent_Chooser_Menu_New;

   ---------------------------------------------
   -- Gtk_Recent_Chooser_Menu_New_For_Manager --
   ---------------------------------------------

   function Gtk_Recent_Chooser_Menu_New_For_Manager
      (Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
       return Gtk_Recent_Chooser_Menu
   is
      Self : constant Gtk_Recent_Chooser_Menu := new Gtk_Recent_Chooser_Menu_Record;
   begin
      Gtk.Recent_Chooser_Menu.Initialize_For_Manager (Self, Manager);
      return Self;
   end Gtk_Recent_Chooser_Menu_New_For_Manager;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Recent_Chooser_Menu_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_menu_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------------
   -- Initialize_For_Manager --
   ----------------------------

   procedure Initialize_For_Manager
      (Self    : not null access Gtk_Recent_Chooser_Menu_Record'Class;
       Manager : not null access Gtk.Recent_Manager.Gtk_Recent_Manager_Record'Class)
   is
      function Internal (Manager : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_menu_new_for_manager");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Manager)));
      end if;
   end Initialize_For_Manager;

   ----------------------
   -- Get_Show_Numbers --
   ----------------------

   function Get_Show_Numbers
      (Self : not null access Gtk_Recent_Chooser_Menu_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_menu_get_show_numbers");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Show_Numbers;

   ----------------------
   -- Set_Show_Numbers --
   ----------------------

   procedure Set_Show_Numbers
      (Self         : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Numbers : Boolean)
   is
      procedure Internal (Self : System.Address; Show_Numbers : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_menu_set_show_numbers");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Show_Numbers));
   end Set_Show_Numbers;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Chooser      : not null access Gtk_Recent_Chooser_Menu_Record;
       Sort_Func    : Gtk_Recent_Sort_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Recent_Chooser_Set_Sort_Func (Get_Object (Chooser), System.Null_Address, System.Null_Address, Data_Destroy);
      else
         C_Gtk_Recent_Chooser_Set_Sort_Func (Get_Object (Chooser), Internal_Gtk_Recent_Sort_Func'Address, To_Address (Sort_Func), Data_Destroy);
      end if;
   end Set_Sort_Func;

   package body Set_Sort_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Recent_Sort_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Recent_Sort_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Recent_Sort_Func, System.Address);

      function Internal_Cb
         (A         : System.Address;
          B         : System.Address;
          User_Data : System.Address) return Gint;
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (A         : System.Address;
          B         : System.Address;
          User_Data : System.Address) return Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Recent_Sort_Func (D.Func) (From_Object (A), From_Object (B), D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Chooser      : not null access Gtk.Recent_Chooser_Menu.Gtk_Recent_Chooser_Menu_Record'Class;
          Sort_Func    : Gtk_Recent_Sort_Func;
          Sort_Data    : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address)
      is
      begin
         if Sort_Func = null then
            C_Gtk_Recent_Chooser_Set_Sort_Func (Get_Object (Chooser), System.Null_Address, System.Null_Address, Data_Destroy);
         else
            C_Gtk_Recent_Chooser_Set_Sort_Func (Get_Object (Chooser), Internal_Cb'Address, Users.Build (To_Address (Sort_Func), Sort_Data), Data_Destroy);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal (Chooser : System.Address; Filter : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_add_filter");
   begin
      Internal (Get_Object (Chooser), Get_Object (Filter));
   end Add_Filter;

   ---------------------------
   -- Do_Set_Related_Action --
   ---------------------------

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_do_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Do_Set_Related_Action;

   ----------------------
   -- Get_Current_Item --
   ----------------------

   function Get_Current_Item
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Info.Gtk_Recent_Info
   is
      function Internal (Chooser : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_item");
   begin
      return From_Object (Internal (Get_Object (Chooser)));
   end Get_Current_Item;

   ---------------------
   -- Get_Current_Uri --
   ---------------------

   function Get_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return UTF8_String
   is
      function Internal
         (Chooser : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Chooser)));
   end Get_Current_Uri;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter
   is
      function Internal (Chooser : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_filter");
      Stub_Gtk_Recent_Filter : Gtk.Recent_Filter.Gtk_Recent_Filter_Record;
   begin
      return Gtk.Recent_Filter.Gtk_Recent_Filter (Get_User_Data (Internal (Get_Object (Chooser)), Stub_Gtk_Recent_Filter));
   end Get_Filter;

   ---------------
   -- Get_Items --
   ---------------

   function Get_Items
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist
   is
      function Internal (Chooser : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_items");
      Tmp_Return : Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;
   begin
      Gtk.Recent_Manager.Gtk_Recent_Info_List.Set_Object (Tmp_Return, Internal (Get_Object (Chooser)));
      return Tmp_Return;
   end Get_Items;

   ---------------
   -- Get_Limit --
   ---------------

   function Get_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record) return Gint
   is
      function Internal (Chooser : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_recent_chooser_get_limit");
   begin
      return Internal (Get_Object (Chooser));
   end Get_Limit;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_local_only");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Local_Only;

   ------------------------
   -- Get_Related_Action --
   ------------------------

   function Get_Related_Action
      (Self : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Action.Gtk_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_activatable_get_related_action");
      Stub_Gtk_Action : Gtk.Action.Gtk_Action_Record;
   begin
      return Gtk.Action.Gtk_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Action));
   end Get_Related_Action;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_select_multiple");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Select_Multiple;

   --------------------
   -- Get_Show_Icons --
   --------------------

   function Get_Show_Icons
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_icons");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Show_Icons;

   ------------------------
   -- Get_Show_Not_Found --
   ------------------------

   function Get_Show_Not_Found
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_not_found");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Show_Not_Found;

   ----------------------
   -- Get_Show_Private --
   ----------------------

   function Get_Show_Private
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_private");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Show_Private;

   -------------------
   -- Get_Show_Tips --
   -------------------

   function Get_Show_Tips
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Boolean
   is
      function Internal (Chooser : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_tips");
   begin
      return Internal (Get_Object (Chooser)) /= 0;
   end Get_Show_Tips;

   -------------------
   -- Get_Sort_Type --
   -------------------

   function Get_Sort_Type
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Chooser.Gtk_Recent_Sort_Type
   is
      function Internal
         (Chooser : System.Address)
          return Gtk.Recent_Chooser.Gtk_Recent_Sort_Type;
      pragma Import (C, Internal, "gtk_recent_chooser_get_sort_type");
   begin
      return Internal (Get_Object (Chooser));
   end Get_Sort_Type;

   -------------------------------
   -- Get_Use_Action_Appearance --
   -------------------------------

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Recent_Chooser_Menu_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_activatable_get_use_action_appearance");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Action_Appearance;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist
   is
      function Internal (Chooser : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_list_filters");
      Tmp_Return : Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;
   begin
      Gtk.Recent_Filter.Gtk_Recent_Filter_List.Set_Object (Tmp_Return, Internal (Get_Object (Chooser)));
      return Tmp_Return;
   end List_Filters;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal (Chooser : System.Address; Filter : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_remove_filter");
   begin
      Internal (Get_Object (Chooser), Get_Object (Filter));
   end Remove_Filter;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
   is
      procedure Internal (Chooser : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_select_all");
   begin
      Internal (Get_Object (Chooser));
   end Select_All;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : System.Address;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_select_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Chooser), Tmp_URI);
      Free (Tmp_URI);
      return Tmp_Return /= 0;
   end Select_Uri;

   ---------------------
   -- Set_Current_Uri --
   ---------------------

   function Set_Current_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : System.Address;
          URI     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_recent_chooser_set_current_uri");
      Tmp_URI    : Interfaces.C.Strings.chars_ptr := New_String (URI);
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Chooser), Tmp_URI);
      Free (Tmp_URI);
      return Tmp_Return /= 0;
   end Set_Current_Uri;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal (Chooser : System.Address; Filter : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_set_filter");
   begin
      Internal (Get_Object (Chooser), Get_Object (Filter));
   end Set_Filter;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       Limit   : Gint)
   is
      procedure Internal (Chooser : System.Address; Limit : Gint);
      pragma Import (C, Internal, "gtk_recent_chooser_set_limit");
   begin
      Internal (Get_Object (Chooser), Limit);
   end Set_Limit;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only
      (Chooser    : not null access Gtk_Recent_Chooser_Menu_Record;
       Local_Only : Boolean)
   is
      procedure Internal (Chooser : System.Address; Local_Only : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_local_only");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Local_Only));
   end Set_Local_Only;

   ------------------------
   -- Set_Related_Action --
   ------------------------

   procedure Set_Related_Action
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_set_related_action");
   begin
      Internal (Get_Object (Self), Get_Object (Action));
   end Set_Related_Action;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
      (Chooser         : not null access Gtk_Recent_Chooser_Menu_Record;
       Select_Multiple : Boolean)
   is
      procedure Internal
         (Chooser         : System.Address;
          Select_Multiple : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_select_multiple");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   --------------------
   -- Set_Show_Icons --
   --------------------

   procedure Set_Show_Icons
      (Chooser    : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Icons : Boolean)
   is
      procedure Internal (Chooser : System.Address; Show_Icons : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_icons");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Show_Icons));
   end Set_Show_Icons;

   ------------------------
   -- Set_Show_Not_Found --
   ------------------------

   procedure Set_Show_Not_Found
      (Chooser        : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Not_Found : Boolean)
   is
      procedure Internal
         (Chooser        : System.Address;
          Show_Not_Found : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_not_found");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Show_Not_Found));
   end Set_Show_Not_Found;

   ----------------------
   -- Set_Show_Private --
   ----------------------

   procedure Set_Show_Private
      (Chooser      : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Private : Boolean)
   is
      procedure Internal (Chooser : System.Address; Show_Private : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_private");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Show_Private));
   end Set_Show_Private;

   -------------------
   -- Set_Show_Tips --
   -------------------

   procedure Set_Show_Tips
      (Chooser   : not null access Gtk_Recent_Chooser_Menu_Record;
       Show_Tips : Boolean)
   is
      procedure Internal (Chooser : System.Address; Show_Tips : Integer);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_tips");
   begin
      Internal (Get_Object (Chooser), Boolean'Pos (Show_Tips));
   end Set_Show_Tips;

   -------------------
   -- Set_Sort_Type --
   -------------------

   procedure Set_Sort_Type
      (Chooser   : not null access Gtk_Recent_Chooser_Menu_Record;
       Sort_Type : Gtk.Recent_Chooser.Gtk_Recent_Sort_Type)
   is
      procedure Internal
         (Chooser   : System.Address;
          Sort_Type : Gtk.Recent_Chooser.Gtk_Recent_Sort_Type);
      pragma Import (C, Internal, "gtk_recent_chooser_set_sort_type");
   begin
      Internal (Get_Object (Chooser), Sort_Type);
   end Set_Sort_Type;

   -------------------------------
   -- Set_Use_Action_Appearance --
   -------------------------------

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Recent_Chooser_Menu_Record;
       Use_Appearance : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Appearance : Integer);
      pragma Import (C, Internal, "gtk_activatable_set_use_action_appearance");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Appearance));
   end Set_Use_Action_Appearance;

   ----------------------------
   -- Sync_Action_Properties --
   ----------------------------

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Recent_Chooser_Menu_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_activatable_sync_action_properties");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Sync_Action_Properties;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record)
   is
      procedure Internal (Chooser : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_unselect_all");
   begin
      Internal (Get_Object (Chooser));
   end Unselect_All;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri
      (Chooser : not null access Gtk_Recent_Chooser_Menu_Record;
       URI     : UTF8_String)
   is
      procedure Internal
         (Chooser : System.Address;
          URI     : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_recent_chooser_unselect_uri");
      Tmp_URI : Interfaces.C.Strings.chars_ptr := New_String (URI);
   begin
      Internal (Get_Object (Chooser), Tmp_URI);
      Free (Tmp_URI);
   end Unselect_Uri;

end Gtk.Recent_Chooser_Menu;
