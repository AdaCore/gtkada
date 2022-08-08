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

package body Gtk.Flow_Box is

   procedure C_Gtk_Flow_Box_Bind_Model
      (Self                : System.Address;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : System.Address;
       User_Data           : System.Address;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Flow_Box_Bind_Model, "gtk_flow_box_bind_model");
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  Gtk.Flow_Box.Insert or Gtk.Container.Add) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in GtkFlowBox. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  Since: gtk+ 3.18
   --  "model": the Glib.List_Model.Glist_Model to be bound to Box
   --  "create_widget_func": a function that creates widgets for items
   --  "user_data": user data passed to Create_Widget_Func
   --  "user_data_free_func": function for freeing User_Data

   procedure C_Gtk_Flow_Box_Selected_Foreach
      (Self : System.Address;
       Func : System.Address;
       Data : System.Address);
   pragma Import (C, C_Gtk_Flow_Box_Selected_Foreach, "gtk_flow_box_selected_foreach");
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  Since: gtk+ 3.12
   --  "func": the function to call for each selected child
   --  "data": user data to pass to the function

   procedure C_Gtk_Flow_Box_Set_Filter_Func
      (Self        : System.Address;
       Filter_Func : System.Address;
       User_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_Flow_Box_Set_Filter_Func, "gtk_flow_box_set_filter_func");
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the children to show. For instance, to implement a search
   --  function that only shows the children matching the search terms.
   --  The Filter_Func will be called for each child after the call, and it
   --  will continue to be called each time a child changes (via
   --  Gtk.Flow_Box_Child.Changed) or when Gtk.Flow_Box.Invalidate_Filter is
   --  called.
   --  Note that using a filter function is incompatible with using a model
   --  (see Gtk.Flow_Box.Bind_Model).
   --  Since: gtk+ 3.12
   --  "filter_func": callback that lets you filter which children to show
   --  "user_data": user data passed to Filter_Func
   --  "destroy": destroy notifier for User_Data

   procedure C_Gtk_Flow_Box_Set_Sort_Func
      (Self      : System.Address;
       Sort_Func : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Flow_Box_Set_Sort_Func, "gtk_flow_box_set_sort_func");
   --  By setting a sort function on the Box, one can dynamically reorder the
   --  children of the box, based on the contents of the children.
   --  The Sort_Func will be called for each child after the call, and will
   --  continue to be called each time a child changes (via
   --  Gtk.Flow_Box_Child.Changed) and when Gtk.Flow_Box.Invalidate_Sort is
   --  called.
   --  Note that using a sort function is incompatible with using a model (see
   --  Gtk.Flow_Box.Bind_Model).
   --  Since: gtk+ 3.12
   --  "sort_func": the sort function
   --  "user_data": user data passed to Sort_Func
   --  "destroy": destroy notifier for User_Data

   function To_Gtk_Flow_Box_Create_Widget_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Flow_Box_Create_Widget_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Flow_Box_Create_Widget_Func, System.Address);

   function To_Gtk_Flow_Box_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Flow_Box_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Flow_Box_Foreach_Func, System.Address);

   function To_Gtk_Flow_Box_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Flow_Box_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Flow_Box_Filter_Func, System.Address);

   function To_Gtk_Flow_Box_Sort_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Flow_Box_Sort_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Flow_Box_Sort_Func, System.Address);

   function Internal_Gtk_Flow_Box_Create_Widget_Func
      (Item      : System.Address;
       User_Data : System.Address) return System.Address;
   pragma Convention (C, Internal_Gtk_Flow_Box_Create_Widget_Func);
   --  "item": the item from the model for which to create a widget for
   --  "user_data": user data from Gtk.Flow_Box.Bind_Model

   function Internal_Gtk_Flow_Box_Filter_Func
      (Child     : System.Address;
       User_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Flow_Box_Filter_Func);
   --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child that may be filtered
   --  "user_data": user data

   procedure Internal_Gtk_Flow_Box_Foreach_Func
      (Box       : System.Address;
       Child     : System.Address;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Flow_Box_Foreach_Func);
   --  "box": a Gtk.Flow_Box.Gtk_Flow_Box
   --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
   --  "user_data": user data

   function Internal_Gtk_Flow_Box_Sort_Func
      (Child1    : System.Address;
       Child2    : System.Address;
       User_Data : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_Flow_Box_Sort_Func);
   --  "child1": the first child
   --  "child2": the second child
   --  "user_data": user data

   ----------------------------------------------
   -- Internal_Gtk_Flow_Box_Create_Widget_Func --
   ----------------------------------------------

   function Internal_Gtk_Flow_Box_Create_Widget_Func
      (Item      : System.Address;
       User_Data : System.Address) return System.Address
   is
      Func : constant Gtk_Flow_Box_Create_Widget_Func := To_Gtk_Flow_Box_Create_Widget_Func (User_Data);
   begin
      return Get_Object (Func (Item));
   end Internal_Gtk_Flow_Box_Create_Widget_Func;

   ---------------------------------------
   -- Internal_Gtk_Flow_Box_Filter_Func --
   ---------------------------------------

   function Internal_Gtk_Flow_Box_Filter_Func
      (Child     : System.Address;
       User_Data : System.Address) return Glib.Gboolean
   is
      Func                    : constant Gtk_Flow_Box_Filter_Func := To_Gtk_Flow_Box_Filter_Func (User_Data);
      Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
   begin
      return Boolean'Pos (Func (Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child, Stub_Gtk_Flow_Box_Child))));
   end Internal_Gtk_Flow_Box_Filter_Func;

   ----------------------------------------
   -- Internal_Gtk_Flow_Box_Foreach_Func --
   ----------------------------------------

   procedure Internal_Gtk_Flow_Box_Foreach_Func
      (Box       : System.Address;
       Child     : System.Address;
       User_Data : System.Address)
   is
      Func                    : constant Gtk_Flow_Box_Foreach_Func := To_Gtk_Flow_Box_Foreach_Func (User_Data);
      Stub_Gtk_Flow_Box       : Gtk_Flow_Box_Record;
      Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
   begin
      Func (Gtk.Flow_Box.Gtk_Flow_Box (Get_User_Data (Box, Stub_Gtk_Flow_Box)), Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child, Stub_Gtk_Flow_Box_Child)));
   end Internal_Gtk_Flow_Box_Foreach_Func;

   -------------------------------------
   -- Internal_Gtk_Flow_Box_Sort_Func --
   -------------------------------------

   function Internal_Gtk_Flow_Box_Sort_Func
      (Child1    : System.Address;
       Child2    : System.Address;
       User_Data : System.Address) return Glib.Gint
   is
      Func                    : constant Gtk_Flow_Box_Sort_Func := To_Gtk_Flow_Box_Sort_Func (User_Data);
      Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
   begin
      return Func (Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child1, Stub_Gtk_Flow_Box_Child)), Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child2, Stub_Gtk_Flow_Box_Child)));
   end Internal_Gtk_Flow_Box_Sort_Func;

   package Type_Conversion_Gtk_Flow_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Flow_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Flow_Box);

   ----------------------
   -- Gtk_Flow_Box_New --
   ----------------------

   function Gtk_Flow_Box_New return Gtk_Flow_Box is
      Self : constant Gtk_Flow_Box := new Gtk_Flow_Box_Record;
   begin
      Gtk.Flow_Box.Initialize (Self);
      return Self;
   end Gtk_Flow_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Flow_Box) is
   begin
      Self := new Gtk_Flow_Box_Record;
      Gtk.Flow_Box.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Flow_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_flow_box_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Bind_Model --
   ----------------

   procedure Bind_Model
      (Self                : not null access Gtk_Flow_Box_Record;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : Gtk_Flow_Box_Create_Widget_Func;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Create_Widget_Func = null then
         C_Gtk_Flow_Box_Bind_Model (Get_Object (Self), Model, System.Null_Address, System.Null_Address, User_Data_Free_Func);
      else
         C_Gtk_Flow_Box_Bind_Model (Get_Object (Self), Model, Internal_Gtk_Flow_Box_Create_Widget_Func'Address, To_Address (Create_Widget_Func), User_Data_Free_Func);
      end if;
   end Bind_Model;

   package body Bind_Model_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Flow_Box_Create_Widget_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Flow_Box_Create_Widget_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Flow_Box_Create_Widget_Func, System.Address);

      function Internal_Cb
         (Item      : System.Address;
          User_Data : System.Address) return System.Address;
      pragma Convention (C, Internal_Cb);
      --  Called for flow boxes that are bound to a
      --  Glib.List_Model.Glist_Model with Gtk.Flow_Box.Bind_Model for each
      --  item that gets added to the model.
      --  Since: gtk+ 3.18
      --  "item": the item from the model for which to create a widget for
      --  "user_data": user data from Gtk.Flow_Box.Bind_Model

      ----------------
      -- Bind_Model --
      ----------------

      procedure Bind_Model
         (Self                : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Model               : Glib.List_Model.Glist_Model;
          Create_Widget_Func  : Gtk_Flow_Box_Create_Widget_Func;
          User_Data           : User_Data_Type;
          User_Data_Free_Func : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Create_Widget_Func = null then
            C_Gtk_Flow_Box_Bind_Model (Get_Object (Self), Model, System.Null_Address, System.Null_Address, User_Data_Free_Func);
         else
            D := Users.Build (To_Address (Create_Widget_Func), User_Data);
            C_Gtk_Flow_Box_Bind_Model (Get_Object (Self), Model, Internal_Cb'Address, D, User_Data_Free_Func);
         end if;
      end Bind_Model;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Item      : System.Address;
          User_Data : System.Address) return System.Address
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return Get_Object (To_Gtk_Flow_Box_Create_Widget_Func (D.Func) (Item, D.Data.all));
      end Internal_Cb;

   end Bind_Model_User_Data;

   ----------------------------------
   -- Get_Activate_On_Single_Click --
   ----------------------------------

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_Flow_Box_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_flow_box_get_activate_on_single_click");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activate_On_Single_Click;

   ------------------------
   -- Get_Child_At_Index --
   ------------------------

   function Get_Child_At_Index
      (Self : not null access Gtk_Flow_Box_Record;
       Idx  : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
   is
      function Internal
         (Self : System.Address;
          Idx  : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_flow_box_get_child_at_index");
      Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
   begin
      return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Internal (Get_Object (Self), Idx), Stub_Gtk_Flow_Box_Child));
   end Get_Child_At_Index;

   ----------------------
   -- Get_Child_At_Pos --
   ----------------------

   function Get_Child_At_Pos
      (Self : not null access Gtk_Flow_Box_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_flow_box_get_child_at_pos");
      Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
   begin
      return Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Internal (Get_Object (Self), X, Y), Stub_Gtk_Flow_Box_Child));
   end Get_Child_At_Pos;

   ------------------------
   -- Get_Column_Spacing --
   ------------------------

   function Get_Column_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_flow_box_get_column_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Column_Spacing;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
      (Self : not null access Gtk_Flow_Box_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_flow_box_get_homogeneous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Homogeneous;

   -------------------------------
   -- Get_Max_Children_Per_Line --
   -------------------------------

   function Get_Max_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_flow_box_get_max_children_per_line");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Children_Per_Line;

   -------------------------------
   -- Get_Min_Children_Per_Line --
   -------------------------------

   function Get_Min_Children_Per_Line
      (Self : not null access Gtk_Flow_Box_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_flow_box_get_min_children_per_line");
   begin
      return Internal (Get_Object (Self));
   end Get_Min_Children_Per_Line;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
      (Self : not null access Gtk_Flow_Box_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_flow_box_get_row_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Row_Spacing;

   ---------------------------
   -- Get_Selected_Children --
   ---------------------------

   function Get_Selected_Children
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_flow_box_get_selected_children");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Selected_Children;

   ------------------------
   -- Get_Selection_Mode --
   ------------------------

   function Get_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_flow_box_get_selection_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Selection_Mode;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self     : not null access Gtk_Flow_Box_Record;
       Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Widget   : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_flow_box_insert");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Position);
   end Insert;

   -----------------------
   -- Invalidate_Filter --
   -----------------------

   procedure Invalidate_Filter (Self : not null access Gtk_Flow_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_invalidate_filter");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Filter;

   ---------------------
   -- Invalidate_Sort --
   ---------------------

   procedure Invalidate_Sort (Self : not null access Gtk_Flow_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_invalidate_sort");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Sort;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Self : not null access Gtk_Flow_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_select_all");
   begin
      Internal (Get_Object (Self));
   end Select_All;

   ------------------
   -- Select_Child --
   ------------------

   procedure Select_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_select_child");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Select_Child;

   ----------------------
   -- Selected_Foreach --
   ----------------------

   procedure Selected_Foreach
      (Self : not null access Gtk_Flow_Box_Record;
       Func : Gtk_Flow_Box_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_Flow_Box_Selected_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Flow_Box_Selected_Foreach (Get_Object (Self), Internal_Gtk_Flow_Box_Foreach_Func'Address, To_Address (Func));
      end if;
   end Selected_Foreach;

   package body Selected_Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Flow_Box_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Flow_Box_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Flow_Box_Foreach_Func, System.Address);

      procedure Internal_Cb
         (Box       : System.Address;
          Child     : System.Address;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function used by Gtk.Flow_Box.Selected_Foreach. It will be called
      --  on every selected child of the Box.
      --  Since: gtk+ 3.12
      --  "box": a Gtk.Flow_Box.Gtk_Flow_Box
      --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Box       : System.Address;
          Child     : System.Address;
          User_Data : System.Address)
      is
         D                       : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Flow_Box       : Gtk.Flow_Box.Gtk_Flow_Box_Record;
         Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
      begin
         To_Gtk_Flow_Box_Foreach_Func (D.Func) (Gtk.Flow_Box.Gtk_Flow_Box (Get_User_Data (Box, Stub_Gtk_Flow_Box)), Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child, Stub_Gtk_Flow_Box_Child)), D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Selected_Foreach --
      ----------------------

      procedure Selected_Foreach
         (Self : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Func : Gtk_Flow_Box_Foreach_Func;
          Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Flow_Box_Selected_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_Flow_Box_Selected_Foreach (Get_Object (Self), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Selected_Foreach;

   end Selected_Foreach_User_Data;

   ----------------------------------
   -- Set_Activate_On_Single_Click --
   ----------------------------------

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_Flow_Box_Record;
       Single : Boolean)
   is
      procedure Internal (Self : System.Address; Single : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_flow_box_set_activate_on_single_click");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Single));
   end Set_Activate_On_Single_Click;

   ------------------------
   -- Set_Column_Spacing --
   ------------------------

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint)
   is
      procedure Internal (Self : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_flow_box_set_column_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Column_Spacing;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
      (Self        : not null access Gtk_Flow_Box_Record;
       Filter_Func : Gtk_Flow_Box_Filter_Func)
   is
   begin
      if Filter_Func = null then
         C_Gtk_Flow_Box_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Flow_Box_Set_Filter_Func (Get_Object (Self), Internal_Gtk_Flow_Box_Filter_Func'Address, To_Address (Filter_Func), System.Null_Address);
      end if;
   end Set_Filter_Func;

   package body Set_Filter_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Flow_Box_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Flow_Box_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Flow_Box_Filter_Func, System.Address);

      function Internal_Cb
         (Child     : System.Address;
          User_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A function that will be called whenrever a child changes or is
      --  added. It lets you control if the child should be visible or not.
      --  Since: gtk+ 3.12
      --  "child": a Gtk.Flow_Box_Child.Gtk_Flow_Box_Child that may be
      --  filtered
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Child     : System.Address;
          User_Data : System.Address) return Glib.Gboolean
      is
         D                       : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
      begin
         return Boolean'Pos (To_Gtk_Flow_Box_Filter_Func (D.Func) (Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child, Stub_Gtk_Flow_Box_Child)), D.Data.all));
      end Internal_Cb;

      ---------------------
      -- Set_Filter_Func --
      ---------------------

      procedure Set_Filter_Func
         (Self        : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Filter_Func : Gtk_Flow_Box_Filter_Func;
          User_Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Filter_Func = null then
            C_Gtk_Flow_Box_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Filter_Func), User_Data);
            C_Gtk_Flow_Box_Set_Filter_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Filter_Func;

   end Set_Filter_Func_User_Data;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_set_hadjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
      (Self        : not null access Gtk_Flow_Box_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_flow_box_set_homogeneous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -------------------------------
   -- Set_Max_Children_Per_Line --
   -------------------------------

   procedure Set_Max_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint)
   is
      procedure Internal (Self : System.Address; N_Children : Guint);
      pragma Import (C, Internal, "gtk_flow_box_set_max_children_per_line");
   begin
      Internal (Get_Object (Self), N_Children);
   end Set_Max_Children_Per_Line;

   -------------------------------
   -- Set_Min_Children_Per_Line --
   -------------------------------

   procedure Set_Min_Children_Per_Line
      (Self       : not null access Gtk_Flow_Box_Record;
       N_Children : Guint)
   is
      procedure Internal (Self : System.Address; N_Children : Guint);
      pragma Import (C, Internal, "gtk_flow_box_set_min_children_per_line");
   begin
      Internal (Get_Object (Self), N_Children);
   end Set_Min_Children_Per_Line;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Flow_Box_Record;
       Spacing : Guint)
   is
      procedure Internal (Self : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_flow_box_set_row_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Row_Spacing;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (Self : not null access Gtk_Flow_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal
         (Self : System.Address;
          Mode : Gtk.Enums.Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_flow_box_set_selection_mode");
   begin
      Internal (Get_Object (Self), Mode);
   end Set_Selection_Mode;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Self      : not null access Gtk_Flow_Box_Record;
       Sort_Func : Gtk_Flow_Box_Sort_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_Flow_Box_Set_Sort_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Flow_Box_Set_Sort_Func (Get_Object (Self), Internal_Gtk_Flow_Box_Sort_Func'Address, To_Address (Sort_Func), System.Null_Address);
      end if;
   end Set_Sort_Func;

   package body Set_Sort_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Flow_Box_Sort_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Flow_Box_Sort_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Flow_Box_Sort_Func, System.Address);

      function Internal_Cb
         (Child1    : System.Address;
          Child2    : System.Address;
          User_Data : System.Address) return Glib.Gint;
      pragma Convention (C, Internal_Cb);
      --  A function to compare two children to determine which should come
      --  first.
      --  Since: gtk+ 3.12
      --  "child1": the first child
      --  "child2": the second child
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Child1    : System.Address;
          Child2    : System.Address;
          User_Data : System.Address) return Glib.Gint
      is
         D                       : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Flow_Box_Child : Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record;
      begin
         return To_Gtk_Flow_Box_Sort_Func (D.Func) (Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child1, Stub_Gtk_Flow_Box_Child)), Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Get_User_Data (Child2, Stub_Gtk_Flow_Box_Child)), D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Self      : not null access Gtk.Flow_Box.Gtk_Flow_Box_Record'Class;
          Sort_Func : Gtk_Flow_Box_Sort_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_Flow_Box_Set_Sort_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_Flow_Box_Set_Sort_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self       : not null access Gtk_Flow_Box_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_set_vadjustment");
   begin
      Internal (Get_Object (Self), Get_Object (Adjustment));
   end Set_Vadjustment;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Self : not null access Gtk_Flow_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_unselect_all");
   begin
      Internal (Get_Object (Self));
   end Unselect_All;

   --------------------
   -- Unselect_Child --
   --------------------

   procedure Unselect_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Child : not null access Gtk.Flow_Box_Child.Gtk_Flow_Box_Child_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_flow_box_unselect_child");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Unselect_Child;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Flow_Box_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Flow_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Flow_Box_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Flow_Box_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Flow_Box_Child_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Flow_Box_Child_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean);

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Flow_Box_Child_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Flow_Box_Child_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Flow_Box_Child_Void);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void);

   procedure Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean);

   procedure Marsh_Gtk_Flow_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Flow_Box_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Flow_Box_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Flow_Box_Child_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Flow_Box_Child_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Flow_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Movement_Step_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Flow_Box_Child_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Flow_Box_Child_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Flow_Box_Child_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Flow_Box_Child_Void;

   --------------------------------------------------
   -- Marsh_GObject_Gtk_Movement_Step_Gint_Boolean --
   --------------------------------------------------

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Movement_Step_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Movement_Step_Gint_Boolean;

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

   ------------------------------------------------
   -- Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void --
   ------------------------------------------------

   procedure Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Flow_Box := Gtk_Flow_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Flow_Box_Child.Gtk_Flow_Box_Child (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void;

   -------------------------------------------------------
   -- Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean --
   -------------------------------------------------------

   procedure Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Flow_Box := Gtk_Flow_Box (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean;

   -----------------------------
   -- Marsh_Gtk_Flow_Box_Void --
   -----------------------------

   procedure Marsh_Gtk_Flow_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Flow_Box_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Flow_Box := Gtk_Flow_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Flow_Box_Void;

   ------------------------------
   -- On_Activate_Cursor_Child --
   ------------------------------

   procedure On_Activate_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-cursor-child" & ASCII.NUL, Call, After);
   end On_Activate_Cursor_Child;

   ------------------------------
   -- On_Activate_Cursor_Child --
   ------------------------------

   procedure On_Activate_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-cursor-child" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Cursor_Child;

   ------------------------
   -- On_Child_Activated --
   ------------------------

   procedure On_Child_Activated
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Gtk_Flow_Box_Child_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "child-activated" & ASCII.NUL, Call, After);
   end On_Child_Activated;

   ------------------------
   -- On_Child_Activated --
   ------------------------

   procedure On_Child_Activated
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Gtk_Flow_Box_Child_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "child-activated" & ASCII.NUL, Call, After, Slot);
   end On_Child_Activated;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Gtk_Movement_Step_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-all" & ASCII.NUL, Call, After);
   end On_Select_All;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-all" & ASCII.NUL, Call, After, Slot);
   end On_Select_All;

   ----------------------------------
   -- On_Selected_Children_Changed --
   ----------------------------------

   procedure On_Selected_Children_Changed
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selected-children-changed" & ASCII.NUL, Call, After);
   end On_Selected_Children_Changed;

   ----------------------------------
   -- On_Selected_Children_Changed --
   ----------------------------------

   procedure On_Selected_Children_Changed
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selected-children-changed" & ASCII.NUL, Call, After, Slot);
   end On_Selected_Children_Changed;

   ----------------------------
   -- On_Toggle_Cursor_Child --
   ----------------------------

   procedure On_Toggle_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-cursor-child" & ASCII.NUL, Call, After);
   end On_Toggle_Cursor_Child;

   ----------------------------
   -- On_Toggle_Cursor_Child --
   ----------------------------

   procedure On_Toggle_Cursor_Child
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-cursor-child" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Cursor_Child;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_Gtk_Flow_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unselect-all" & ASCII.NUL, Call, After);
   end On_Unselect_All;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_Flow_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unselect-all" & ASCII.NUL, Call, After, Slot);
   end On_Unselect_All;

end Gtk.Flow_Box;
