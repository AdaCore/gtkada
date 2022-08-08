------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Glib.Values;              use Glib.Values;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Bindings;          use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;             use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Recent_Chooser is

   procedure C_Gtk_Recent_Chooser_Set_Sort_Func
      (Chooser      : Gtk_Recent_Chooser;
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
       User_Data : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Recent_Sort_Func);

   -----------------------------------
   -- Internal_Gtk_Recent_Sort_Func --
   -----------------------------------

   function Internal_Gtk_Recent_Sort_Func
      (A         : System.Address;
       B         : System.Address;
       User_Data : System.Address) return Glib.Gint
   is
      Func : constant Gtk_Recent_Sort_Func := To_Gtk_Recent_Sort_Func (User_Data);
   begin
      return Func (From_Object (A), From_Object (B));
   end Internal_Gtk_Recent_Sort_Func;

   ----------------
   -- Add_Filter --
   ----------------

   procedure Add_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_Recent_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_add_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Add_Filter;

   ----------------------
   -- Get_Current_Item --
   ----------------------

   function Get_Current_Item
      (Chooser : Gtk_Recent_Chooser) return Gtk.Recent_Info.Gtk_Recent_Info
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_item");
   begin
      return From_Object (Internal (Chooser));
   end Get_Current_Item;

   ---------------------
   -- Get_Current_Uri --
   ---------------------

   function Get_Current_Uri
      (Chooser : Gtk_Recent_Chooser) return UTF8_String
   is
      function Internal
         (Chooser : Gtk_Recent_Chooser) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_chooser_get_current_uri");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Chooser));
   end Get_Current_Uri;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Filter.Gtk_Recent_Filter
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_filter");
      Stub_Gtk_Recent_Filter : Gtk.Recent_Filter.Gtk_Recent_Filter_Record;
   begin
      return Gtk.Recent_Filter.Gtk_Recent_Filter (Get_User_Data (Internal (Chooser), Stub_Gtk_Recent_Filter));
   end Get_Filter;

   ---------------
   -- Get_Items --
   ---------------

   function Get_Items
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_get_items");
      Tmp_Return : Gtk.Recent_Manager.Gtk_Recent_Info_List.Glist;
   begin
      Gtk.Recent_Manager.Gtk_Recent_Info_List.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end Get_Items;

   --------------------
   -- Get_Local_Only --
   --------------------

   function Get_Local_Only (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_local_only");
   begin
      return Internal (Chooser) /= 0;
   end Get_Local_Only;

   -------------------------
   -- Get_Select_Multiple --
   -------------------------

   function Get_Select_Multiple
      (Chooser : Gtk_Recent_Chooser) return Boolean
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_select_multiple");
   begin
      return Internal (Chooser) /= 0;
   end Get_Select_Multiple;

   --------------------
   -- Get_Show_Icons --
   --------------------

   function Get_Show_Icons (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_icons");
   begin
      return Internal (Chooser) /= 0;
   end Get_Show_Icons;

   ------------------------
   -- Get_Show_Not_Found --
   ------------------------

   function Get_Show_Not_Found (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_not_found");
   begin
      return Internal (Chooser) /= 0;
   end Get_Show_Not_Found;

   ----------------------
   -- Get_Show_Private --
   ----------------------

   function Get_Show_Private (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_private");
   begin
      return Internal (Chooser) /= 0;
   end Get_Show_Private;

   -------------------
   -- Get_Show_Tips --
   -------------------

   function Get_Show_Tips (Chooser : Gtk_Recent_Chooser) return Boolean is
      function Internal (Chooser : Gtk_Recent_Chooser) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_get_show_tips");
   begin
      return Internal (Chooser) /= 0;
   end Get_Show_Tips;

   ------------------
   -- List_Filters --
   ------------------

   function List_Filters
      (Chooser : Gtk_Recent_Chooser)
       return Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist
   is
      function Internal (Chooser : Gtk_Recent_Chooser) return System.Address;
      pragma Import (C, Internal, "gtk_recent_chooser_list_filters");
      Tmp_Return : Gtk.Recent_Filter.Gtk_Recent_Filter_List.GSlist;
   begin
      Gtk.Recent_Filter.Gtk_Recent_Filter_List.Set_Object (Tmp_Return, Internal (Chooser));
      return Tmp_Return;
   end List_Filters;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : not null access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_Recent_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_remove_filter");
   begin
      Internal (Chooser, Get_Object (Filter));
   end Remove_Filter;

   ----------------
   -- Select_Uri --
   ----------------

   function Select_Uri
      (Chooser : Gtk_Recent_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_Recent_Chooser;
          URI     : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_select_uri");
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Tmp_Return /= 0;
   end Select_Uri;

   ---------------------
   -- Set_Current_Uri --
   ---------------------

   function Set_Current_Uri
      (Chooser : Gtk_Recent_Chooser;
       URI     : UTF8_String) return Boolean
   is
      function Internal
         (Chooser : Gtk_Recent_Chooser;
          URI     : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_recent_chooser_set_current_uri");
      Tmp_URI    : Gtkada.Types.Chars_Ptr := New_String (URI);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
      return Tmp_Return /= 0;
   end Set_Current_Uri;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
      (Chooser : Gtk_Recent_Chooser;
       Filter  : access Gtk.Recent_Filter.Gtk_Recent_Filter_Record'Class)
   is
      procedure Internal
         (Chooser : Gtk_Recent_Chooser;
          Filter  : System.Address);
      pragma Import (C, Internal, "gtk_recent_chooser_set_filter");
   begin
      Internal (Chooser, Get_Object_Or_Null (GObject (Filter)));
   end Set_Filter;

   --------------------
   -- Set_Local_Only --
   --------------------

   procedure Set_Local_Only
      (Chooser    : Gtk_Recent_Chooser;
       Local_Only : Boolean)
   is
      procedure Internal
         (Chooser    : Gtk_Recent_Chooser;
          Local_Only : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_local_only");
   begin
      Internal (Chooser, Boolean'Pos (Local_Only));
   end Set_Local_Only;

   -------------------------
   -- Set_Select_Multiple --
   -------------------------

   procedure Set_Select_Multiple
      (Chooser         : Gtk_Recent_Chooser;
       Select_Multiple : Boolean)
   is
      procedure Internal
         (Chooser         : Gtk_Recent_Chooser;
          Select_Multiple : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_select_multiple");
   begin
      Internal (Chooser, Boolean'Pos (Select_Multiple));
   end Set_Select_Multiple;

   --------------------
   -- Set_Show_Icons --
   --------------------

   procedure Set_Show_Icons
      (Chooser    : Gtk_Recent_Chooser;
       Show_Icons : Boolean)
   is
      procedure Internal
         (Chooser    : Gtk_Recent_Chooser;
          Show_Icons : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_icons");
   begin
      Internal (Chooser, Boolean'Pos (Show_Icons));
   end Set_Show_Icons;

   ------------------------
   -- Set_Show_Not_Found --
   ------------------------

   procedure Set_Show_Not_Found
      (Chooser        : Gtk_Recent_Chooser;
       Show_Not_Found : Boolean)
   is
      procedure Internal
         (Chooser        : Gtk_Recent_Chooser;
          Show_Not_Found : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_not_found");
   begin
      Internal (Chooser, Boolean'Pos (Show_Not_Found));
   end Set_Show_Not_Found;

   ----------------------
   -- Set_Show_Private --
   ----------------------

   procedure Set_Show_Private
      (Chooser      : Gtk_Recent_Chooser;
       Show_Private : Boolean)
   is
      procedure Internal
         (Chooser      : Gtk_Recent_Chooser;
          Show_Private : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_private");
   begin
      Internal (Chooser, Boolean'Pos (Show_Private));
   end Set_Show_Private;

   -------------------
   -- Set_Show_Tips --
   -------------------

   procedure Set_Show_Tips
      (Chooser   : Gtk_Recent_Chooser;
       Show_Tips : Boolean)
   is
      procedure Internal
         (Chooser   : Gtk_Recent_Chooser;
          Show_Tips : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_recent_chooser_set_show_tips");
   begin
      Internal (Chooser, Boolean'Pos (Show_Tips));
   end Set_Show_Tips;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Chooser      : Gtk_Recent_Chooser;
       Sort_Func    : Gtk_Recent_Sort_Func;
       Data_Destroy : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Recent_Chooser_Set_Sort_Func (Chooser, System.Null_Address, System.Null_Address, Data_Destroy);
      else
         C_Gtk_Recent_Chooser_Set_Sort_Func (Chooser, Internal_Gtk_Recent_Sort_Func'Address, To_Address (Sort_Func), Data_Destroy);
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
          User_Data : System.Address) return Glib.Gint;
      pragma Convention (C, Internal_Cb);

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (A         : System.Address;
          B         : System.Address;
          User_Data : System.Address) return Glib.Gint
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return To_Gtk_Recent_Sort_Func (D.Func) (From_Object (A), From_Object (B), D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Chooser      : Gtk.Recent_Chooser.Gtk_Recent_Chooser;
          Sort_Func    : Gtk_Recent_Sort_Func;
          Sort_Data    : User_Data_Type;
          Data_Destroy : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Recent_Chooser_Set_Sort_Func (Chooser, System.Null_Address, System.Null_Address, Data_Destroy);
         else
            D := Users.Build (To_Address (Sort_Func), Sort_Data);
            C_Gtk_Recent_Chooser_Set_Sort_Func (Chooser, Internal_Cb'Address, D, Data_Destroy);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   ------------------
   -- Unselect_Uri --
   ------------------

   procedure Unselect_Uri (Chooser : Gtk_Recent_Chooser; URI : UTF8_String) is
      procedure Internal
         (Chooser : Gtk_Recent_Chooser;
          URI     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_recent_chooser_unselect_uri");
      Tmp_URI : Gtkada.Types.Chars_Ptr := New_String (URI);
   begin
      Internal (Chooser, Tmp_URI);
      Free (Tmp_URI);
   end Unselect_Uri;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Recent_Chooser_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Recent_Chooser_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : Gtk_Recent_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Recent_Chooser_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : Gtk_Recent_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Recent_Chooser_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Recent_Chooser_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : Gtk_Recent_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Recent_Chooser_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Recent_Chooser_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : Gtk_Recent_Chooser;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Glib.Types.GType_Interface (Object),
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -----------------------------------
   -- Marsh_Gtk_Recent_Chooser_Void --
   -----------------------------------

   procedure Marsh_Gtk_Recent_Chooser_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Recent_Chooser_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Recent_Chooser := Gtk_Recent_Chooser (Unchecked_To_Interface (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Recent_Chooser_Void;

   -----------------------
   -- On_Item_Activated --
   -----------------------

   procedure On_Item_Activated
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_Gtk_Recent_Chooser_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "item-activated" & ASCII.NUL, Call, After);
   end On_Item_Activated;

   -----------------------
   -- On_Item_Activated --
   -----------------------

   procedure On_Item_Activated
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "item-activated" & ASCII.NUL, Call, After, Slot);
   end On_Item_Activated;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_Gtk_Recent_Chooser_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-changed" & ASCII.NUL, Call, After);
   end On_Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
      (Self  : Gtk_Recent_Chooser;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-changed" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Changed;

   function "+" (W : Gtk_Recent_Chooser) return Gtk_Recent_Chooser is
   begin
      return W;
   end "+";

end Gtk.Recent_Chooser;
