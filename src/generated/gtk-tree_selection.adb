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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;

package body Gtk.Tree_Selection is

   procedure Get_Selected_Rows
     (Selection : access Gtk_Tree_Selection_Record;
      Model     : out Gtk_Tree_Model;
      Path_List : out Gtk_Tree_Path_List.Glist)
   is
      use type System.Address;
      function Internal
        (Selection : System.Address;
         Model     : access Gtk_Tree_Model) return System.Address;
      pragma Import (C, Internal, "gtk_tree_selection_get_selected_rows");
      M        : aliased Gtk_Tree_Model;
      Selected : System.Address;--  list of paths
   begin
      Selected := Internal (Get_Object (Selection), M'Access);
      Model := M;
      if Selected = System.Null_Address then
         Path_List := Gtk_Tree_Path_List.Null_List;
      else
         Gtk_Tree_Path_List.Set_Object (Path_List, Selected);
      end if;
   end Get_Selected_Rows;

   procedure C_Gtk_Tree_Selection_Selected_Foreach
      (Selection : System.Address;
       Func      : System.Address;
       Data      : System.Address);
   pragma Import (C, C_Gtk_Tree_Selection_Selected_Foreach, "gtk_tree_selection_selected_foreach");
   --  Calls a function for each selected node. Note that you cannot modify
   --  the tree or selection from within this function. As a result,
   --  gtk_tree_selection_get_selected_rows might be more useful.
   --  "func": The function to call for each selected node.
   --  "data": user data to pass to the function.

   procedure C_Gtk_Tree_Selection_Set_Select_Function
      (Selection : System.Address;
       Func      : System.Address;
       Data      : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Tree_Selection_Set_Select_Function, "gtk_tree_selection_set_select_function");
   --  Sets the selection function.
   --  If set, this function is called before any node is selected or
   --  unselected, giving some control over which nodes are selected. The
   --  select function should return True if the state of the node may be
   --  toggled, and False if the state of the node should be left unchanged.
   --  "func": The selection function. May be null
   --  "data": The selection function's data. May be null
   --  "destroy": The destroy function for user data. May be null

   function To_Gtk_Tree_Selection_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Selection_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Selection_Foreach_Func, System.Address);

   function To_Gtk_Tree_Selection_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tree_Selection_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tree_Selection_Func, System.Address);

   procedure Internal_Gtk_Tree_Selection_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address);
   pragma Convention (C, Internal_Gtk_Tree_Selection_Foreach_Func);
   --  "model": The Gtk.Tree_Model.Gtk_Tree_Model being viewed
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
   --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter pointing to a selected row
   --  "data": user data

   function Internal_Gtk_Tree_Selection_Func
      (Selection               : System.Address;
       Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
       Path                    : System.Address;
       Path_Currently_Selected : Glib.Gboolean;
       Data                    : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tree_Selection_Func);
   --  "selection": A Gtk.Tree_Selection.Gtk_Tree_Selection
   --  "model": A Gtk.Tree_Model.Gtk_Tree_Model being viewed
   --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of the row in question
   --  "path_currently_selected": True, if the path is currently selected
   --  "data": user data

   ----------------------------------------------
   -- Internal_Gtk_Tree_Selection_Foreach_Func --
   ----------------------------------------------

   procedure Internal_Gtk_Tree_Selection_Foreach_Func
      (Model : Gtk.Tree_Model.Gtk_Tree_Model;
       Path  : System.Address;
       Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data  : System.Address)
   is
      Func : constant Gtk_Tree_Selection_Foreach_Func := To_Gtk_Tree_Selection_Foreach_Func (Data);
   begin
      Func (Model, From_Object (Path), Iter.all);
   end Internal_Gtk_Tree_Selection_Foreach_Func;

   --------------------------------------
   -- Internal_Gtk_Tree_Selection_Func --
   --------------------------------------

   function Internal_Gtk_Tree_Selection_Func
      (Selection               : System.Address;
       Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
       Path                    : System.Address;
       Path_Currently_Selected : Glib.Gboolean;
       Data                    : System.Address) return Glib.Gboolean
   is
      Func                    : constant Gtk_Tree_Selection_Func := To_Gtk_Tree_Selection_Func (Data);
      Stub_Gtk_Tree_Selection : Gtk_Tree_Selection_Record;
   begin
      return Boolean'Pos (Func (Gtk.Tree_Selection.Gtk_Tree_Selection (Get_User_Data (Selection, Stub_Gtk_Tree_Selection)), Model, From_Object (Path), Path_Currently_Selected /= 0));
   end Internal_Gtk_Tree_Selection_Func;

   package Type_Conversion_Gtk_Tree_Selection is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tree_Selection_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tree_Selection);

   -------------------------
   -- Count_Selected_Rows --
   -------------------------

   function Count_Selected_Rows
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Glib.Gint
   is
      function Internal (Selection : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tree_selection_count_selected_rows");
   begin
      return Internal (Get_Object (Selection));
   end Count_Selected_Rows;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Gtk.Enums.Gtk_Selection_Mode
   is
      function Internal
         (Selection : System.Address) return Gtk.Enums.Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_tree_selection_get_mode");
   begin
      return Internal (Get_Object (Selection));
   end Get_Mode;

   -------------------------
   -- Get_Select_Function --
   -------------------------

   procedure Get_Select_Function
      (Selection : not null access Gtk_Tree_Selection_Record)
   is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_get_select_function");
   begin
      Internal (Get_Object (Selection));
   end Get_Select_Function;

   ------------------
   -- Get_Selected --
   ------------------

   procedure Get_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Model     : out Gtk.Tree_Model.Gtk_Tree_Model;
       Iter      : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      function Internal
        (Selection : System.Address;
         Model     : access Gtk_Tree_Model;
         Iter      : access Gtk_Tree_Iter) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_get_selected");
      M  : aliased Gtk_Tree_Model := Null_Gtk_Tree_Model;
      It : aliased Gtk_Tree_Iter;
   begin
      if Internal (Get_Object (Selection), M'Access, It'Access) = 0 then
         Iter  := Null_Iter;
      else
         Iter  := It;
      end if;
      Model := M;
   end Get_Selected;

   -------------------
   -- Get_Tree_View --
   -------------------

   function Get_Tree_View
      (Selection : not null access Gtk_Tree_Selection_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_selection_get_tree_view");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Selection)), Stub_Gtk_Widget));
   end Get_Tree_View;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
      (Selection : not null access Gtk_Tree_Selection_Record)
       return System.Address
   is
      function Internal (Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_selection_get_user_data");
   begin
      return Internal (Get_Object (Selection));
   end Get_User_Data;

   ----------------------
   -- Iter_Is_Selected --
   ----------------------

   function Iter_Is_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      function Internal
         (Selection : System.Address;
          Iter      : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_iter_is_selected");
   begin
      return Internal (Get_Object (Selection), Iter) /= 0;
   end Iter_Is_Selected;

   ----------------------
   -- Path_Is_Selected --
   ----------------------

   function Path_Is_Selected
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      function Internal
         (Selection : System.Address;
          Path      : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tree_selection_path_is_selected");
   begin
      return Internal (Get_Object (Selection), Get_Object (Path)) /= 0;
   end Path_Is_Selected;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All
      (Selection : not null access Gtk_Tree_Selection_Record)
   is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_select_all");
   begin
      Internal (Get_Object (Selection));
   end Select_All;

   -----------------
   -- Select_Iter --
   -----------------

   procedure Select_Iter
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Selection : System.Address;
          Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_selection_select_iter");
   begin
      Internal (Get_Object (Selection), Iter);
   end Select_Iter;

   -----------------
   -- Select_Path --
   -----------------

   procedure Select_Path
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Selection : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_select_path");
   begin
      Internal (Get_Object (Selection), Get_Object (Path));
   end Select_Path;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
      (Selection  : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Selection  : System.Address;
          Start_Path : System.Address;
          End_Path   : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_select_range");
   begin
      Internal (Get_Object (Selection), Get_Object (Start_Path), Get_Object (End_Path));
   end Select_Range;

   ----------------------
   -- Selected_Foreach --
   ----------------------

   procedure Selected_Foreach
      (Selection : not null access Gtk_Tree_Selection_Record;
       Func      : Gtk_Tree_Selection_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Selection_Selected_Foreach (Get_Object (Selection), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Selection_Selected_Foreach (Get_Object (Selection), Internal_Gtk_Tree_Selection_Foreach_Func'Address, To_Address (Func));
      end if;
   end Selected_Foreach;

   package body Selected_Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Selection_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Selection_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Selection_Foreach_Func, System.Address);

      procedure Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function used by Gtk.Tree_Selection.Selected_Foreach to map all
      --  selected rows. It will be called on every selected row in the view.
      --  "model": The Gtk.Tree_Model.Gtk_Tree_Model being viewed
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of a selected row
      --  "iter": A Gtk.Tree_Model.Gtk_Tree_Iter pointing to a selected row
      --  "data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Model : Gtk.Tree_Model.Gtk_Tree_Model;
          Path  : System.Address;
          Iter  : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data  : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Gtk_Tree_Selection_Foreach_Func (D.Func) (Model, From_Object (Path), Iter.all, D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Selected_Foreach --
      ----------------------

      procedure Selected_Foreach
         (Selection : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func      : Gtk_Tree_Selection_Foreach_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Selection_Selected_Foreach (Get_Object (Selection), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_Selection_Selected_Foreach (Get_Object (Selection), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Selected_Foreach;

   end Selected_Foreach_User_Data;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
      (Selection : not null access Gtk_Tree_Selection_Record;
       The_Type  : Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal
         (Selection : System.Address;
          The_Type  : Gtk.Enums.Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_tree_selection_set_mode");
   begin
      Internal (Get_Object (Selection), The_Type);
   end Set_Mode;

   -------------------------
   -- Set_Select_Function --
   -------------------------

   procedure Set_Select_Function
      (Selection : not null access Gtk_Tree_Selection_Record;
       Func      : Gtk_Tree_Selection_Func)
   is
   begin
      if Func = null then
         C_Gtk_Tree_Selection_Set_Select_Function (Get_Object (Selection), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Tree_Selection_Set_Select_Function (Get_Object (Selection), Internal_Gtk_Tree_Selection_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Select_Function;

   package body Set_Select_Function_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tree_Selection_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tree_Selection_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tree_Selection_Func, System.Address);

      function Internal_Cb
         (Selection               : System.Address;
          Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
          Path                    : System.Address;
          Path_Currently_Selected : Glib.Gboolean;
          Data                    : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A function used by Gtk.Tree_Selection.Set_Select_Function to filter
      --  whether or not a row may be selected. It is called whenever a row's
      --  state might change. A return value of True indicates to Selection
      --  that it is okay to change the selection.
      --  "selection": A Gtk.Tree_Selection.Gtk_Tree_Selection
      --  "model": A Gtk.Tree_Model.Gtk_Tree_Model being viewed
      --  "path": The Gtk.Tree_Model.Gtk_Tree_Path of the row in question
      --  "path_currently_selected": True, if the path is currently selected
      --  "data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Selection               : System.Address;
          Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
          Path                    : System.Address;
          Path_Currently_Selected : Glib.Gboolean;
          Data                    : System.Address) return Glib.Gboolean
      is
         D                       : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Tree_Selection : Gtk.Tree_Selection.Gtk_Tree_Selection_Record;
      begin
         return Boolean'Pos (To_Gtk_Tree_Selection_Func (D.Func) (Gtk.Tree_Selection.Gtk_Tree_Selection (Get_User_Data (Selection, Stub_Gtk_Tree_Selection)), Model, From_Object (Path), Path_Currently_Selected /= 0, D.Data.all));
      end Internal_Cb;

      -------------------------
      -- Set_Select_Function --
      -------------------------

      procedure Set_Select_Function
         (Selection : not null access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
          Func      : Gtk_Tree_Selection_Func;
          Data      : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Tree_Selection_Set_Select_Function (Get_Object (Selection), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Tree_Selection_Set_Select_Function (Get_Object (Selection), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Select_Function;

   end Set_Select_Function_User_Data;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All
      (Selection : not null access Gtk_Tree_Selection_Record)
   is
      procedure Internal (Selection : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_all");
   begin
      Internal (Get_Object (Selection));
   end Unselect_All;

   -------------------
   -- Unselect_Iter --
   -------------------

   procedure Unselect_Iter
      (Selection : not null access Gtk_Tree_Selection_Record;
       Iter      : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      procedure Internal
         (Selection : System.Address;
          Iter      : Gtk.Tree_Model.Gtk_Tree_Iter);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_iter");
   begin
      Internal (Get_Object (Selection), Iter);
   end Unselect_Iter;

   -------------------
   -- Unselect_Path --
   -------------------

   procedure Unselect_Path
      (Selection : not null access Gtk_Tree_Selection_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Selection : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_path");
   begin
      Internal (Get_Object (Selection), Get_Object (Path));
   end Unselect_Path;

   --------------------
   -- Unselect_Range --
   --------------------

   procedure Unselect_Range
      (Selection  : not null access Gtk_Tree_Selection_Record;
       Start_Path : Gtk.Tree_Model.Gtk_Tree_Path;
       End_Path   : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal
         (Selection  : System.Address;
          Start_Path : System.Address;
          End_Path   : System.Address);
      pragma Import (C, Internal, "gtk_tree_selection_unselect_range");
   begin
      Internal (Get_Object (Selection), Get_Object (Start_Path), Get_Object (End_Path));
   end Unselect_Range;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Tree_Selection_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Tree_Selection_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Tree_Selection_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Selection_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Tree_Selection_Record'Class;
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

   procedure Marsh_Gtk_Tree_Selection_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Tree_Selection_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Tree_Selection_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Tree_Selection_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Tree_Selection_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Tree_Selection_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
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
   -- Marsh_Gtk_Tree_Selection_Void --
   -----------------------------------

   procedure Marsh_Gtk_Tree_Selection_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Tree_Selection_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Tree_Selection := Gtk_Tree_Selection (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Tree_Selection_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Tree_Selection_Record;
       Call  : Cb_Gtk_Tree_Selection_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Tree_Selection_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gtk.Tree_Selection;
