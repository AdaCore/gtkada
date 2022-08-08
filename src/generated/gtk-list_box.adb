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

package body Gtk.List_Box is

   procedure C_Gtk_List_Box_Bind_Model
      (Self                : System.Address;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : System.Address;
       User_Data           : System.Address;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_List_Box_Bind_Model, "gtk_list_box_bind_model");
   --  Binds Model to Box.
   --  If Box was already bound to a model, that previous binding is
   --  destroyed.
   --  The contents of Box are cleared and then filled with widgets that
   --  represent items from Model. Box is updated whenever Model changes. If
   --  Model is null, Box is left empty.
   --  It is undefined to add or remove widgets directly (for example, with
   --  Gtk.List_Box.Insert or Gtk.Container.Add) while Box is bound to a model.
   --  Note that using a model is incompatible with the filtering and sorting
   --  functionality in GtkListBox. When using a model, filtering and sorting
   --  should be implemented by the model.
   --  Since: gtk+ 3.16
   --  "model": the Glib.List_Model.Glist_Model to be bound to Box
   --  "create_widget_func": a function that creates widgets for items or null
   --  in case you also passed null as Model
   --  "user_data": user data passed to Create_Widget_Func
   --  "user_data_free_func": function for freeing User_Data

   procedure C_Gtk_List_Box_Selected_Foreach
      (Self : System.Address;
       Func : System.Address;
       Data : System.Address);
   pragma Import (C, C_Gtk_List_Box_Selected_Foreach, "gtk_list_box_selected_foreach");
   --  Calls a function for each selected child.
   --  Note that the selection cannot be modified from within this function.
   --  Since: gtk+ 3.14
   --  "func": the function to call for each selected child
   --  "data": user data to pass to the function

   procedure C_Gtk_List_Box_Set_Filter_Func
      (Self        : System.Address;
       Filter_Func : System.Address;
       User_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_List_Box_Set_Filter_Func, "gtk_list_box_set_filter_func");
   --  By setting a filter function on the Box one can decide dynamically
   --  which of the rows to show. For instance, to implement a search function
   --  on a list that filters the original list to only show the matching rows.
   --  The Filter_Func will be called for each row after the call, and it will
   --  continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) or when Gtk.List_Box.Invalidate_Filter is
   --  called.
   --  Note that using a filter function is incompatible with using a model
   --  (see Gtk.List_Box.Bind_Model).
   --  Since: gtk+ 3.10
   --  "filter_func": callback that lets you filter which rows to show
   --  "user_data": user data passed to Filter_Func
   --  "destroy": destroy notifier for User_Data

   procedure C_Gtk_List_Box_Set_Header_Func
      (Self          : System.Address;
       Update_Header : System.Address;
       User_Data     : System.Address;
       Destroy       : System.Address);
   pragma Import (C, C_Gtk_List_Box_Set_Header_Func, "gtk_list_box_set_header_func");
   --  By setting a header function on the Box one can dynamically add headers
   --  in front of rows, depending on the contents of the row and its position
   --  in the list. For instance, one could use it to add headers in front of
   --  the first item of a new kind, in a list sorted by the kind.
   --  The Update_Header can look at the current header widget using
   --  Gtk.List_Box_Row.Get_Header and either update the state of the widget as
   --  needed, or set a new one using Gtk.List_Box_Row.Set_Header. If no header
   --  is needed, set the header to null.
   --  Note that you may get many calls Update_Header to this for a particular
   --  row when e.g. changing things that don't affect the header. In this case
   --  it is important for performance to not blindly replace an existing
   --  header with an identical one.
   --  The Update_Header function will be called for each row after the call,
   --  and it will continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) and when the row before changes (either by
   --  Gtk.List_Box_Row.Changed on the previous row, or when the previous row
   --  becomes a different row). It is also called for all rows when
   --  Gtk.List_Box.Invalidate_Headers is called.
   --  Since: gtk+ 3.10
   --  "update_header": callback that lets you add row headers
   --  "user_data": user data passed to Update_Header
   --  "destroy": destroy notifier for User_Data

   procedure C_Gtk_List_Box_Set_Sort_Func
      (Self      : System.Address;
       Sort_Func : System.Address;
       User_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_List_Box_Set_Sort_Func, "gtk_list_box_set_sort_func");
   --  By setting a sort function on the Box one can dynamically reorder the
   --  rows of the list, based on the contents of the rows.
   --  The Sort_Func will be called for each row after the call, and will
   --  continue to be called each time a row changes (via
   --  Gtk.List_Box_Row.Changed) and when Gtk.List_Box.Invalidate_Sort is
   --  called.
   --  Note that using a sort function is incompatible with using a model (see
   --  Gtk.List_Box.Bind_Model).
   --  Since: gtk+ 3.10
   --  "sort_func": the sort function
   --  "user_data": user data passed to Sort_Func
   --  "destroy": destroy notifier for User_Data

   function To_Gtk_List_Box_Create_Widget_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_List_Box_Create_Widget_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_List_Box_Create_Widget_Func, System.Address);

   function To_Gtk_List_Box_Foreach_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_List_Box_Foreach_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_List_Box_Foreach_Func, System.Address);

   function To_Gtk_List_Box_Filter_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_List_Box_Filter_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_List_Box_Filter_Func, System.Address);

   function To_Gtk_List_Box_Update_Header_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_List_Box_Update_Header_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_List_Box_Update_Header_Func, System.Address);

   function To_Gtk_List_Box_Sort_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_List_Box_Sort_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_List_Box_Sort_Func, System.Address);

   function Internal_Gtk_List_Box_Create_Widget_Func
      (Item      : System.Address;
       User_Data : System.Address) return System.Address;
   pragma Convention (C, Internal_Gtk_List_Box_Create_Widget_Func);
   --  "item": the item from the model for which to create a widget for
   --  "user_data": user data

   function Internal_Gtk_List_Box_Filter_Func
      (Row       : System.Address;
       User_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_List_Box_Filter_Func);
   --  "row": the row that may be filtered
   --  "user_data": user data

   procedure Internal_Gtk_List_Box_Foreach_Func
      (Box       : System.Address;
       Row       : System.Address;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_List_Box_Foreach_Func);
   --  "box": a Gtk.List_Box.Gtk_List_Box
   --  "row": a Gtk.List_Box_Row.Gtk_List_Box_Row
   --  "user_data": user data

   function Internal_Gtk_List_Box_Sort_Func
      (Row1      : System.Address;
       Row2      : System.Address;
       User_Data : System.Address) return Glib.Gint;
   pragma Convention (C, Internal_Gtk_List_Box_Sort_Func);
   --  "row1": the first row
   --  "row2": the second row
   --  "user_data": user data

   procedure Internal_Gtk_List_Box_Update_Header_Func
      (Row       : System.Address;
       Before    : System.Address;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_List_Box_Update_Header_Func);
   --  "row": the row to update
   --  "before": the row before Row, or null if it is first
   --  "user_data": user data

   ----------------------------------------------
   -- Internal_Gtk_List_Box_Create_Widget_Func --
   ----------------------------------------------

   function Internal_Gtk_List_Box_Create_Widget_Func
      (Item      : System.Address;
       User_Data : System.Address) return System.Address
   is
      Func : constant Gtk_List_Box_Create_Widget_Func := To_Gtk_List_Box_Create_Widget_Func (User_Data);
   begin
      return Get_Object (Func (Item));
   end Internal_Gtk_List_Box_Create_Widget_Func;

   ---------------------------------------
   -- Internal_Gtk_List_Box_Filter_Func --
   ---------------------------------------

   function Internal_Gtk_List_Box_Filter_Func
      (Row       : System.Address;
       User_Data : System.Address) return Glib.Gboolean
   is
      Func                  : constant Gtk_List_Box_Filter_Func := To_Gtk_List_Box_Filter_Func (User_Data);
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      return Boolean'Pos (Func (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row))));
   end Internal_Gtk_List_Box_Filter_Func;

   ----------------------------------------
   -- Internal_Gtk_List_Box_Foreach_Func --
   ----------------------------------------

   procedure Internal_Gtk_List_Box_Foreach_Func
      (Box       : System.Address;
       Row       : System.Address;
       User_Data : System.Address)
   is
      Func                  : constant Gtk_List_Box_Foreach_Func := To_Gtk_List_Box_Foreach_Func (User_Data);
      Stub_Gtk_List_Box     : Gtk_List_Box_Record;
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      Func (Gtk.List_Box.Gtk_List_Box (Get_User_Data (Box, Stub_Gtk_List_Box)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row)));
   end Internal_Gtk_List_Box_Foreach_Func;

   -------------------------------------
   -- Internal_Gtk_List_Box_Sort_Func --
   -------------------------------------

   function Internal_Gtk_List_Box_Sort_Func
      (Row1      : System.Address;
       Row2      : System.Address;
       User_Data : System.Address) return Glib.Gint
   is
      Func                  : constant Gtk_List_Box_Sort_Func := To_Gtk_List_Box_Sort_Func (User_Data);
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      return Func (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row1, Stub_Gtk_List_Box_Row)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row2, Stub_Gtk_List_Box_Row)));
   end Internal_Gtk_List_Box_Sort_Func;

   ----------------------------------------------
   -- Internal_Gtk_List_Box_Update_Header_Func --
   ----------------------------------------------

   procedure Internal_Gtk_List_Box_Update_Header_Func
      (Row       : System.Address;
       Before    : System.Address;
       User_Data : System.Address)
   is
      Func                  : constant Gtk_List_Box_Update_Header_Func := To_Gtk_List_Box_Update_Header_Func (User_Data);
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      Func (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Before, Stub_Gtk_List_Box_Row)));
   end Internal_Gtk_List_Box_Update_Header_Func;

   package Type_Conversion_Gtk_List_Box is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_List_Box_Record);
   pragma Unreferenced (Type_Conversion_Gtk_List_Box);

   ----------------------
   -- Gtk_List_Box_New --
   ----------------------

   function Gtk_List_Box_New return Gtk_List_Box is
      Self : constant Gtk_List_Box := new Gtk_List_Box_Record;
   begin
      Gtk.List_Box.Initialize (Self);
      return Self;
   end Gtk_List_Box_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_List_Box) is
   begin
      Self := new Gtk_List_Box_Record;
      Gtk.List_Box.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_List_Box_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_box_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Bind_Model --
   ----------------

   procedure Bind_Model
      (Self                : not null access Gtk_List_Box_Record;
       Model               : Glib.List_Model.Glist_Model;
       Create_Widget_Func  : Gtk_List_Box_Create_Widget_Func;
       User_Data_Free_Func : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Create_Widget_Func = null then
         C_Gtk_List_Box_Bind_Model (Get_Object (Self), Model, System.Null_Address, System.Null_Address, User_Data_Free_Func);
      else
         C_Gtk_List_Box_Bind_Model (Get_Object (Self), Model, Internal_Gtk_List_Box_Create_Widget_Func'Address, To_Address (Create_Widget_Func), User_Data_Free_Func);
      end if;
   end Bind_Model;

   package body Bind_Model_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_List_Box_Create_Widget_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_List_Box_Create_Widget_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_List_Box_Create_Widget_Func, System.Address);

      function Internal_Cb
         (Item      : System.Address;
          User_Data : System.Address) return System.Address;
      pragma Convention (C, Internal_Cb);
      --  Called for list boxes that are bound to a
      --  Glib.List_Model.Glist_Model with Gtk.List_Box.Bind_Model for each
      --  item that gets added to the model.
      --  Versions of GTK+ prior to 3.18 called Gtk.Widget.Show_All on the
      --  rows created by the GtkListBoxCreateWidgetFunc, but this forced all
      --  widgets inside the row to be shown, and is no longer the case.
      --  Applications should be updated to show the desired row widgets.
      --  Since: gtk+ 3.16
      --  "item": the item from the model for which to create a widget for
      --  "user_data": user data

      ----------------
      -- Bind_Model --
      ----------------

      procedure Bind_Model
         (Self                : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Model               : Glib.List_Model.Glist_Model;
          Create_Widget_Func  : Gtk_List_Box_Create_Widget_Func;
          User_Data           : User_Data_Type;
          User_Data_Free_Func : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Create_Widget_Func = null then
            C_Gtk_List_Box_Bind_Model (Get_Object (Self), Model, System.Null_Address, System.Null_Address, User_Data_Free_Func);
         else
            D := Users.Build (To_Address (Create_Widget_Func), User_Data);
            C_Gtk_List_Box_Bind_Model (Get_Object (Self), Model, Internal_Cb'Address, D, User_Data_Free_Func);
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
         return Get_Object (To_Gtk_List_Box_Create_Widget_Func (D.Func) (Item, D.Data.all));
      end Internal_Cb;

   end Bind_Model_User_Data;

   ------------------------
   -- Drag_Highlight_Row --
   ------------------------

   procedure Drag_Highlight_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   is
      procedure Internal (Self : System.Address; Row : System.Address);
      pragma Import (C, Internal, "gtk_list_box_drag_highlight_row");
   begin
      Internal (Get_Object (Self), Get_Object (Row));
   end Drag_Highlight_Row;

   --------------------------
   -- Drag_Unhighlight_Row --
   --------------------------

   procedure Drag_Unhighlight_Row
      (Self : not null access Gtk_List_Box_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_drag_unhighlight_row");
   begin
      Internal (Get_Object (Self));
   end Drag_Unhighlight_Row;

   ----------------------------------
   -- Get_Activate_On_Single_Click --
   ----------------------------------

   function Get_Activate_On_Single_Click
      (Self : not null access Gtk_List_Box_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_box_get_activate_on_single_click");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activate_On_Single_Click;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_get_adjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Adjustment;

   ----------------------
   -- Get_Row_At_Index --
   ----------------------

   function Get_Row_At_Index
      (Self  : not null access Gtk_List_Box_Record;
       Index : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row
   is
      function Internal
         (Self  : System.Address;
          Index : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_get_row_at_index");
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      return Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Internal (Get_Object (Self), Index), Stub_Gtk_List_Box_Row));
   end Get_Row_At_Index;

   ------------------
   -- Get_Row_At_Y --
   ------------------

   function Get_Row_At_Y
      (Self : not null access Gtk_List_Box_Record;
       Y    : Glib.Gint) return Gtk.List_Box_Row.Gtk_List_Box_Row
   is
      function Internal
         (Self : System.Address;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_get_row_at_y");
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      return Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Internal (Get_Object (Self), Y), Stub_Gtk_List_Box_Row));
   end Get_Row_At_Y;

   ----------------------
   -- Get_Selected_Row --
   ----------------------

   function Get_Selected_Row
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.Gtk_List_Box_Row
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_get_selected_row");
      Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
   begin
      return Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_List_Box_Row));
   end Get_Selected_Row;

   -----------------------
   -- Get_Selected_Rows --
   -----------------------

   function Get_Selected_Rows
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.List_Box_Row.List_Box_Row_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_box_get_selected_rows");
      Tmp_Return : Gtk.List_Box_Row.List_Box_Row_List.Glist;
   begin
      Gtk.List_Box_Row.List_Box_Row_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Selected_Rows;

   ------------------------
   -- Get_Selection_Mode --
   ------------------------

   function Get_Selection_Mode
      (Self : not null access Gtk_List_Box_Record)
       return Gtk.Enums.Gtk_Selection_Mode
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Selection_Mode;
      pragma Import (C, Internal, "gtk_list_box_get_selection_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Selection_Mode;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self     : not null access Gtk_List_Box_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Child    : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_list_box_insert");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Position);
   end Insert;

   -----------------------
   -- Invalidate_Filter --
   -----------------------

   procedure Invalidate_Filter (Self : not null access Gtk_List_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_invalidate_filter");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Filter;

   ------------------------
   -- Invalidate_Headers --
   ------------------------

   procedure Invalidate_Headers (Self : not null access Gtk_List_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_invalidate_headers");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Headers;

   ---------------------
   -- Invalidate_Sort --
   ---------------------

   procedure Invalidate_Sort (Self : not null access Gtk_List_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_invalidate_sort");
   begin
      Internal (Get_Object (Self));
   end Invalidate_Sort;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
      (Self  : not null access Gtk_List_Box_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_list_box_prepend");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Prepend;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Self : not null access Gtk_List_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_select_all");
   begin
      Internal (Get_Object (Self));
   end Select_All;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   is
      procedure Internal (Self : System.Address; Row : System.Address);
      pragma Import (C, Internal, "gtk_list_box_select_row");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Row)));
   end Select_Row;

   ----------------------
   -- Selected_Foreach --
   ----------------------

   procedure Selected_Foreach
      (Self : not null access Gtk_List_Box_Record;
       Func : Gtk_List_Box_Foreach_Func)
   is
   begin
      if Func = null then
         C_Gtk_List_Box_Selected_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
      else
         C_Gtk_List_Box_Selected_Foreach (Get_Object (Self), Internal_Gtk_List_Box_Foreach_Func'Address, To_Address (Func));
      end if;
   end Selected_Foreach;

   package body Selected_Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_List_Box_Foreach_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_List_Box_Foreach_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_List_Box_Foreach_Func, System.Address);

      procedure Internal_Cb
         (Box       : System.Address;
          Row       : System.Address;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  A function used by Gtk.List_Box.Selected_Foreach. It will be called
      --  on every selected child of the Box.
      --  Since: gtk+ 3.14
      --  "box": a Gtk.List_Box.Gtk_List_Box
      --  "row": a Gtk.List_Box_Row.Gtk_List_Box_Row
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Box       : System.Address;
          Row       : System.Address;
          User_Data : System.Address)
      is
         D                     : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_List_Box     : Gtk.List_Box.Gtk_List_Box_Record;
         Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
      begin
         To_Gtk_List_Box_Foreach_Func (D.Func) (Gtk.List_Box.Gtk_List_Box (Get_User_Data (Box, Stub_Gtk_List_Box)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row)), D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Selected_Foreach --
      ----------------------

      procedure Selected_Foreach
         (Self : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Func : Gtk_List_Box_Foreach_Func;
          Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_List_Box_Selected_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Func), Data);
            C_Gtk_List_Box_Selected_Foreach (Get_Object (Self), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Selected_Foreach;

   end Selected_Foreach_User_Data;

   ----------------------------------
   -- Set_Activate_On_Single_Click --
   ----------------------------------

   procedure Set_Activate_On_Single_Click
      (Self   : not null access Gtk_List_Box_Record;
       Single : Boolean)
   is
      procedure Internal (Self : System.Address; Single : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_box_set_activate_on_single_click");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Single));
   end Set_Activate_On_Single_Click;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Self       : not null access Gtk_List_Box_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_list_box_set_adjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Adjustment)));
   end Set_Adjustment;

   ---------------------
   -- Set_Filter_Func --
   ---------------------

   procedure Set_Filter_Func
      (Self        : not null access Gtk_List_Box_Record;
       Filter_Func : Gtk_List_Box_Filter_Func)
   is
   begin
      if Filter_Func = null then
         C_Gtk_List_Box_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_List_Box_Set_Filter_Func (Get_Object (Self), Internal_Gtk_List_Box_Filter_Func'Address, To_Address (Filter_Func), System.Null_Address);
      end if;
   end Set_Filter_Func;

   package body Set_Filter_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_List_Box_Filter_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_List_Box_Filter_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_List_Box_Filter_Func, System.Address);

      function Internal_Cb
         (Row       : System.Address;
          User_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  Will be called whenever the row changes or is added and lets you
      --  control if the row should be visible or not.
      --  Since: gtk+ 3.10
      --  "row": the row that may be filtered
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Row       : System.Address;
          User_Data : System.Address) return Glib.Gboolean
      is
         D                     : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
      begin
         return Boolean'Pos (To_Gtk_List_Box_Filter_Func (D.Func) (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row)), D.Data.all));
      end Internal_Cb;

      ---------------------
      -- Set_Filter_Func --
      ---------------------

      procedure Set_Filter_Func
         (Self        : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Filter_Func : Gtk_List_Box_Filter_Func;
          User_Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Filter_Func = null then
            C_Gtk_List_Box_Set_Filter_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Filter_Func), User_Data);
            C_Gtk_List_Box_Set_Filter_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Filter_Func;

   end Set_Filter_Func_User_Data;

   ---------------------
   -- Set_Header_Func --
   ---------------------

   procedure Set_Header_Func
      (Self          : not null access Gtk_List_Box_Record;
       Update_Header : Gtk_List_Box_Update_Header_Func)
   is
   begin
      if Update_Header = null then
         C_Gtk_List_Box_Set_Header_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_List_Box_Set_Header_Func (Get_Object (Self), Internal_Gtk_List_Box_Update_Header_Func'Address, To_Address (Update_Header), System.Null_Address);
      end if;
   end Set_Header_Func;

   package body Set_Header_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_List_Box_Update_Header_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_List_Box_Update_Header_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_List_Box_Update_Header_Func, System.Address);

      procedure Internal_Cb
         (Row       : System.Address;
          Before    : System.Address;
          User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Whenever Row changes or which row is before Row changes this is
      --  called, which lets you update the header on Row. You may remove or
      --  set a new one via Gtk.List_Box_Row.Set_Header or just change the
      --  state of the current header widget.
      --  Since: gtk+ 3.10
      --  "row": the row to update
      --  "before": the row before Row, or null if it is first
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Row       : System.Address;
          Before    : System.Address;
          User_Data : System.Address)
      is
         D                     : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
      begin
         To_Gtk_List_Box_Update_Header_Func (D.Func) (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row, Stub_Gtk_List_Box_Row)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Before, Stub_Gtk_List_Box_Row)), D.Data.all);
      end Internal_Cb;

      ---------------------
      -- Set_Header_Func --
      ---------------------

      procedure Set_Header_Func
         (Self          : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Update_Header : Gtk_List_Box_Update_Header_Func;
          User_Data     : User_Data_Type)
      is
         D : System.Address;
      begin
         if Update_Header = null then
            C_Gtk_List_Box_Set_Header_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Update_Header), User_Data);
            C_Gtk_List_Box_Set_Header_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Header_Func;

   end Set_Header_Func_User_Data;

   ---------------------
   -- Set_Placeholder --
   ---------------------

   procedure Set_Placeholder
      (Self        : not null access Gtk_List_Box_Record;
       Placeholder : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Placeholder : System.Address);
      pragma Import (C, Internal, "gtk_list_box_set_placeholder");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Placeholder)));
   end Set_Placeholder;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (Self : not null access Gtk_List_Box_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal
         (Self : System.Address;
          Mode : Gtk.Enums.Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_list_box_set_selection_mode");
   begin
      Internal (Get_Object (Self), Mode);
   end Set_Selection_Mode;

   -------------------
   -- Set_Sort_Func --
   -------------------

   procedure Set_Sort_Func
      (Self      : not null access Gtk_List_Box_Record;
       Sort_Func : Gtk_List_Box_Sort_Func)
   is
   begin
      if Sort_Func = null then
         C_Gtk_List_Box_Set_Sort_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_List_Box_Set_Sort_Func (Get_Object (Self), Internal_Gtk_List_Box_Sort_Func'Address, To_Address (Sort_Func), System.Null_Address);
      end if;
   end Set_Sort_Func;

   package body Set_Sort_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_List_Box_Sort_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_List_Box_Sort_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_List_Box_Sort_Func, System.Address);

      function Internal_Cb
         (Row1      : System.Address;
          Row2      : System.Address;
          User_Data : System.Address) return Glib.Gint;
      pragma Convention (C, Internal_Cb);
      --  Compare two rows to determine which should be first.
      --  Since: gtk+ 3.10
      --  "row1": the first row
      --  "row2": the second row
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Row1      : System.Address;
          Row2      : System.Address;
          User_Data : System.Address) return Glib.Gint
      is
         D                     : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_List_Box_Row : Gtk.List_Box_Row.Gtk_List_Box_Row_Record;
      begin
         return To_Gtk_List_Box_Sort_Func (D.Func) (Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row1, Stub_Gtk_List_Box_Row)), Gtk.List_Box_Row.Gtk_List_Box_Row (Get_User_Data (Row2, Stub_Gtk_List_Box_Row)), D.Data.all);
      end Internal_Cb;

      -------------------
      -- Set_Sort_Func --
      -------------------

      procedure Set_Sort_Func
         (Self      : not null access Gtk.List_Box.Gtk_List_Box_Record'Class;
          Sort_Func : Gtk_List_Box_Sort_Func;
          User_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Sort_Func = null then
            C_Gtk_List_Box_Set_Sort_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Sort_Func), User_Data);
            C_Gtk_List_Box_Set_Sort_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Sort_Func;

   end Set_Sort_Func_User_Data;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Self : not null access Gtk_List_Box_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_box_unselect_all");
   begin
      Internal (Get_Object (Self));
   end Unselect_All;

   ------------------
   -- Unselect_Row --
   ------------------

   procedure Unselect_Row
      (Self : not null access Gtk_List_Box_Record;
       Row  : not null access Gtk.List_Box_Row.Gtk_List_Box_Row_Record'Class)
   is
      procedure Internal (Self : System.Address; Row : System.Address);
      pragma Import (C, Internal, "gtk_list_box_unselect_row");
   begin
      Internal (Get_Object (Self), Get_Object (Row));
   end Unselect_Row;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_List_Box_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_List_Box_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_List_Box_Gtk_List_Box_Row_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_List_Box_Gtk_List_Box_Row_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_List_Box_Row_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_List_Box_Row_Void);

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_List_Box_Row_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_List_Box_Row_Void);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void);

   procedure Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void);

   procedure Marsh_Gtk_List_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_List_Box_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_List_Box_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Record'Class;
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
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Movement_Step_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_List_Box_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_List_Box_Row_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_List_Box_Row_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------------
   -- Marsh_GObject_Gtk_List_Box_Row_Void --
   -----------------------------------------

   procedure Marsh_GObject_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_List_Box_Row_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.List_Box_Row.Gtk_List_Box_Row (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_List_Box_Row_Void;

   -----------------------------------------------
   -- Marsh_GObject_Gtk_Movement_Step_Gint_Void --
   -----------------------------------------------

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Movement_Step_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Movement_Step_Gint_Void;

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

   ----------------------------------------------
   -- Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_List_Box_Gtk_List_Box_Row_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_List_Box := Gtk_List_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.List_Box_Row.Gtk_List_Box_Row (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_List_Box_Gtk_List_Box_Row_Void;

   ----------------------------------------------------
   -- Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void --
   ----------------------------------------------------

   procedure Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_List_Box := Gtk_List_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_List_Box_Gtk_Movement_Step_Gint_Void;

   -----------------------------
   -- Marsh_Gtk_List_Box_Void --
   -----------------------------

   procedure Marsh_Gtk_List_Box_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_List_Box_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_List_Box := Gtk_List_Box (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_List_Box_Void;

   ----------------------------
   -- On_Activate_Cursor_Row --
   ----------------------------

   procedure On_Activate_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-cursor-row" & ASCII.NUL, Call, After);
   end On_Activate_Cursor_Row;

   ----------------------------
   -- On_Activate_Cursor_Row --
   ----------------------------

   procedure On_Activate_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-cursor-row" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Cursor_Row;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_Movement_Step_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-activated" & ASCII.NUL, Call, After);
   end On_Row_Activated;

   ----------------------
   -- On_Row_Activated --
   ----------------------

   procedure On_Row_Activated
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_List_Box_Row_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-activated" & ASCII.NUL, Call, After, Slot);
   end On_Row_Activated;

   ---------------------
   -- On_Row_Selected --
   ---------------------

   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Gtk_List_Box_Row_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "row-selected" & ASCII.NUL, Call, After);
   end On_Row_Selected;

   ---------------------
   -- On_Row_Selected --
   ---------------------

   procedure On_Row_Selected
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Gtk_List_Box_Row_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "row-selected" & ASCII.NUL, Call, After, Slot);
   end On_Row_Selected;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-all" & ASCII.NUL, Call, After);
   end On_Select_All;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-all" & ASCII.NUL, Call, After, Slot);
   end On_Select_All;

   ------------------------------
   -- On_Selected_Rows_Changed --
   ------------------------------

   procedure On_Selected_Rows_Changed
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selected-rows-changed" & ASCII.NUL, Call, After);
   end On_Selected_Rows_Changed;

   ------------------------------
   -- On_Selected_Rows_Changed --
   ------------------------------

   procedure On_Selected_Rows_Changed
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selected-rows-changed" & ASCII.NUL, Call, After, Slot);
   end On_Selected_Rows_Changed;

   --------------------------
   -- On_Toggle_Cursor_Row --
   --------------------------

   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-cursor-row" & ASCII.NUL, Call, After);
   end On_Toggle_Cursor_Row;

   --------------------------
   -- On_Toggle_Cursor_Row --
   --------------------------

   procedure On_Toggle_Cursor_Row
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-cursor-row" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Cursor_Row;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_Gtk_List_Box_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unselect-all" & ASCII.NUL, Call, After);
   end On_Unselect_All;

   ---------------------
   -- On_Unselect_All --
   ---------------------

   procedure On_Unselect_All
      (Self  : not null access Gtk_List_Box_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unselect-all" & ASCII.NUL, Call, After, Slot);
   end On_Unselect_All;

end Gtk.List_Box;
