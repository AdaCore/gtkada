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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Entry_Completion is

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Self      : System.Address;
       Cell      : System.Address;
       Func      : System.Address;
       Func_Data : System.Address;
       Destroy   : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   pragma Obsolescent (C_Gtk_Cell_Layout_Set_Cell_Data_Func);
   --  Sets the `GtkCellLayout`DataFunc to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Deprecated since 4.10, 1
   --  @param Cell a `GtkCellRenderer`
   --  @param Func the `GtkCellLayout`DataFunc to use
   --  @param Func_Data user data for Func
   --  @param Destroy destroy notify for Func_Data

   procedure C_Gtk_Entry_Completion_Set_Match_Func
      (Self        : System.Address;
       Func        : System.Address;
       Func_Data   : System.Address;
       Func_Notify : System.Address);
   pragma Import (C, C_Gtk_Entry_Completion_Set_Match_Func, "gtk_entry_completion_set_match_func");
   pragma Obsolescent (C_Gtk_Entry_Completion_Set_Match_Func);
   --  Sets the match function for Completion to be Func.
   --  The match function is used to determine if a row should or should not
   --  be in the completion list.
   --  Deprecated since 4.10, 1
   --  @param Func the `GtkEntryCompletion`MatchFunc to use
   --  @param Func_Data user data for Func
   --  @param Func_Notify destroy notify for Func_Data.

   function To_Gtk_Entry_Completion_Match_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Entry_Completion_Match_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Entry_Completion_Match_Func, System.Address);

   function To_Gtk_Cell_Layout_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Layout_Data_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Cell_Layout_Data_Func, System.Address);

   procedure Internal_Gtk_Cell_Layout_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address);
   pragma Obsolescent (Internal_Gtk_Cell_Layout_Data_Func);
   pragma Convention (C, Internal_Gtk_Cell_Layout_Data_Func);
   --  Deprecated since 4.20, 1
   --  @param Cell_Layout a `GtkCellLayout`
   --  @param Cell the cell renderer whose value is to be set
   --  @param Tree_Model the model
   --  @param Iter a `GtkTreeIter` indicating the row to set the value for
   --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

   function Internal_Gtk_Entry_Completion_Match_Func
      (Completion : System.Address;
       Key        : Gtkada.Types.Chars_Ptr;
       Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
       User_Data  : System.Address) return Glib.Gboolean;
   pragma Obsolescent (Internal_Gtk_Entry_Completion_Match_Func);
   pragma Convention (C, Internal_Gtk_Entry_Completion_Match_Func);
   --  Deprecated since 4.20, 1
   --  @param Completion the `GtkEntryCompletion`
   --  @param Key the string to match, normalized and case-folded
   --  @param Iter a `GtkTreeIter` indicating the row to match
   --  @param User_Data user data given to Gtk.Entry_Completion.Set_Match_Func

   ----------------------------------------
   -- Internal_Gtk_Cell_Layout_Data_Func --
   ----------------------------------------

   procedure Internal_Gtk_Cell_Layout_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address)
   is
      Func                   : constant Gtk_Cell_Layout_Data_Func := To_Gtk_Cell_Layout_Data_Func (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      Func (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all);
   end Internal_Gtk_Cell_Layout_Data_Func;

   ----------------------------------------------
   -- Internal_Gtk_Entry_Completion_Match_Func --
   ----------------------------------------------

   function Internal_Gtk_Entry_Completion_Match_Func
      (Completion : System.Address;
       Key        : Gtkada.Types.Chars_Ptr;
       Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
       User_Data  : System.Address) return Glib.Gboolean
   is
      Func                      : constant Gtk_Entry_Completion_Match_Func := To_Gtk_Entry_Completion_Match_Func (User_Data);
      Stub_Gtk_Entry_Completion : Gtk_Entry_Completion_Record;
   begin
      return Boolean'Pos (Func (Gtk.Entry_Completion.Gtk_Entry_Completion (Get_User_Data (Completion, Stub_Gtk_Entry_Completion)), Gtkada.Bindings.Value_Allowing_Null (Key), Iter.all));
   end Internal_Gtk_Entry_Completion_Match_Func;

   package Type_Conversion_Gtk_Entry_Completion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Completion_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Entry_Completion);

   ------------------------------
   -- Gtk_Entry_Completion_New --
   ------------------------------

   function Gtk_Entry_Completion_New return Gtk_Entry_Completion is
      Self : constant Gtk_Entry_Completion := new Gtk_Entry_Completion_Record;
   begin
      Gtk.Entry_Completion.Initialize (Self);
      return Self;
   end Gtk_Entry_Completion_New;

   ----------------------------------------
   -- Gtk_Entry_Completion_New_With_Area --
   ----------------------------------------

   function Gtk_Entry_Completion_New_With_Area
      (Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
       return Gtk_Entry_Completion
   is
      Self : constant Gtk_Entry_Completion := new Gtk_Entry_Completion_Record;
   begin
      Gtk.Entry_Completion.Initialize_With_Area (Self, Area);
      return Self;
   end Gtk_Entry_Completion_New_With_Area;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Entry_Completion) is
   begin
      Self := new Gtk_Entry_Completion_Record;
      Gtk.Entry_Completion.Initialize (Self);
   end Gtk_New;

   -----------------------
   -- Gtk_New_With_Area --
   -----------------------

   procedure Gtk_New_With_Area
      (Self : out Gtk_Entry_Completion;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
   begin
      Self := new Gtk_Entry_Completion_Record;
      Gtk.Entry_Completion.Initialize_With_Area (Self, Area);
   end Gtk_New_With_Area;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Entry_Completion_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_With_Area --
   --------------------------

   procedure Initialize_With_Area
      (Self : not null access Gtk_Entry_Completion_Record'Class;
       Area : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class)
   is
      function Internal (Area : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_new_with_area");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Area)));
      end if;
   end Initialize_With_Area;

   --------------
   -- Complete --
   --------------

   procedure Complete (Self : not null access Gtk_Entry_Completion_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_entry_completion_complete");
   begin
      Internal (Get_Object (Self));
   end Complete;

   --------------------
   -- Compute_Prefix --
   --------------------

   function Compute_Prefix
      (Self : not null access Gtk_Entry_Completion_Record;
       Key  : UTF8_String) return UTF8_String
   is
      function Internal
         (Self : System.Address;
          Key  : Gtkada.Types.Chars_Ptr) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_completion_compute_prefix");
      Tmp_Key    : Gtkada.Types.Chars_Ptr := New_String (Key);
      Tmp_Return : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Key);
      Free (Tmp_Key);
      return Gtkada.Bindings.Value_And_Free (Tmp_Return);
   end Compute_Prefix;

   ---------------------------
   -- Get_Completion_Prefix --
   ---------------------------

   function Get_Completion_Prefix
      (Self : not null access Gtk_Entry_Completion_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_completion_get_completion_prefix");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Completion_Prefix;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_completion_get_entry");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Entry;

   ---------------------------
   -- Get_Inline_Completion --
   ---------------------------

   function Get_Inline_Completion
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_completion_get_inline_completion");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Inline_Completion;

   --------------------------
   -- Get_Inline_Selection --
   --------------------------

   function Get_Inline_Selection
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_completion_get_inline_selection");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Inline_Selection;

   ----------------------------
   -- Get_Minimum_Key_Length --
   ----------------------------

   function Get_Minimum_Key_Length
      (Self : not null access Gtk_Entry_Completion_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_completion_get_minimum_key_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Minimum_Key_Length;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Self : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_entry_completion_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   --------------------------
   -- Get_Popup_Completion --
   --------------------------

   function Get_Popup_Completion
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_completion");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Popup_Completion;

   -------------------------
   -- Get_Popup_Set_Width --
   -------------------------

   function Get_Popup_Set_Width
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_set_width");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Popup_Set_Width;

   ----------------------------
   -- Get_Popup_Single_Match --
   ----------------------------

   function Get_Popup_Single_Match
      (Self : not null access Gtk_Entry_Completion_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_completion_get_popup_single_match");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Popup_Single_Match;

   ---------------------
   -- Get_Text_Column --
   ---------------------

   function Get_Text_Column
      (Self : not null access Gtk_Entry_Completion_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_completion_get_text_column");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Column;

   -------------------
   -- Insert_Prefix --
   -------------------

   procedure Insert_Prefix
      (Self : not null access Gtk_Entry_Completion_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_entry_completion_insert_prefix");
   begin
      Internal (Get_Object (Self));
   end Insert_Prefix;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Entry_Completion_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func : Gtk_Cell_Layout_Data_Func)
   is
   begin
      if Func = null then
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), Internal_Gtk_Cell_Layout_Data_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Cell_Data_Func;

   package body Set_Cell_Data_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Cell_Layout_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Layout_Data_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Cell_Layout_Data_Func, System.Address);

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address);
      pragma Obsolescent (Internal_Cb);
      pragma Convention (C, Internal_Cb);
      --  A function which should set the value of Cell_Layout's cell
      --  renderer(s) as appropriate.
      --  Deprecated since 4.20, 1
      --  @param Cell_Layout a `GtkCellLayout`
      --  @param Cell the cell renderer whose value is to be set
      --  @param Tree_Model the model
      --  @param Iter a `GtkTreeIter` indicating the row to set the value for
      --  @param Data user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : access Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address)
      is
         D                      : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         To_Gtk_Cell_Layout_Data_Func (D.Func) (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Tree_Model, Iter.all, D.Data.all);
      end Internal_Cb;

      ------------------------
      -- Set_Cell_Data_Func --
      ------------------------

      procedure Set_Cell_Data_Func
         (Self      : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
          Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func      : Gtk_Cell_Layout_Data_Func;
          Func_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Func_Data);
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Self), Get_Object (Cell), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

   ---------------------------
   -- Set_Inline_Completion --
   ---------------------------

   procedure Set_Inline_Completion
      (Self              : not null access Gtk_Entry_Completion_Record;
       Inline_Completion : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Inline_Completion : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_completion_set_inline_completion");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Inline_Completion));
   end Set_Inline_Completion;

   --------------------------
   -- Set_Inline_Selection --
   --------------------------

   procedure Set_Inline_Selection
      (Self             : not null access Gtk_Entry_Completion_Record;
       Inline_Selection : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Inline_Selection : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_completion_set_inline_selection");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Inline_Selection));
   end Set_Inline_Selection;

   --------------------
   -- Set_Match_Func --
   --------------------

   procedure Set_Match_Func
      (Self : not null access Gtk_Entry_Completion_Record;
       Func : Gtk_Entry_Completion_Match_Func)
   is
   begin
      if Func = null then
         C_Gtk_Entry_Completion_Set_Match_Func (Get_Object (Self), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Entry_Completion_Set_Match_Func (Get_Object (Self), Internal_Gtk_Entry_Completion_Match_Func'Address, To_Address (Func), System.Null_Address);
      end if;
   end Set_Match_Func;

   package body Set_Match_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Entry_Completion_Match_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Entry_Completion_Match_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Entry_Completion_Match_Func, System.Address);

      function Internal_Cb
         (Completion : System.Address;
          Key        : Gtkada.Types.Chars_Ptr;
          Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data  : System.Address) return Glib.Gboolean;
      pragma Obsolescent (Internal_Cb);
      pragma Convention (C, Internal_Cb);
      --  A function which decides whether the row indicated by Iter matches a
      --  given Key, and should be displayed as a possible completion for Key.
      --  Note that Key is normalized and case-folded (see g_utf8_normalize
      --  and g_utf8_casefold). If this is not appropriate, match functions
      --  have access to the unmodified key via `gtk_editable_get_text
      --  (GTK_EDITABLE (gtk_entry_completion_get_entry ()))`.
      --  Deprecated since 4.20, 1
      --  @param Completion the `GtkEntryCompletion`
      --  @param Key the string to match, normalized and case-folded
      --  @param Iter a `GtkTreeIter` indicating the row to match
      --  @param User_Data user data given to
      --  Gtk.Entry_Completion.Set_Match_Func
      --  @return True if Iter should be displayed as a possible completion
      --  for Key

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Completion : System.Address;
          Key        : Gtkada.Types.Chars_Ptr;
          Iter       : access Gtk.Tree_Model.Gtk_Tree_Iter;
          User_Data  : System.Address) return Glib.Gboolean
      is
         D                         : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Entry_Completion : Gtk.Entry_Completion.Gtk_Entry_Completion_Record;
      begin
         return Boolean'Pos (To_Gtk_Entry_Completion_Match_Func (D.Func) (Gtk.Entry_Completion.Gtk_Entry_Completion (Get_User_Data (Completion, Stub_Gtk_Entry_Completion)), Gtkada.Bindings.Value_Allowing_Null (Key), Iter.all, D.Data.all));
      end Internal_Cb;

      --------------------
      -- Set_Match_Func --
      --------------------

      procedure Set_Match_Func
         (Self      : not null access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class;
          Func      : Gtk_Entry_Completion_Match_Func;
          Func_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Entry_Completion_Set_Match_Func (Get_Object (Self), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Func_Data);
            C_Gtk_Entry_Completion_Set_Match_Func (Get_Object (Self), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Match_Func;

   end Set_Match_Func_User_Data;

   ----------------------------
   -- Set_Minimum_Key_Length --
   ----------------------------

   procedure Set_Minimum_Key_Length
      (Self   : not null access Gtk_Entry_Completion_Record;
       Length : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Length : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_completion_set_minimum_key_length");
   begin
      Internal (Get_Object (Self), Length);
   end Set_Minimum_Key_Length;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Entry_Completion_Record;
       Model : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_entry_completion_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   --------------------------
   -- Set_Popup_Completion --
   --------------------------

   procedure Set_Popup_Completion
      (Self             : not null access Gtk_Entry_Completion_Record;
       Popup_Completion : Boolean)
   is
      procedure Internal
         (Self             : System.Address;
          Popup_Completion : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_completion");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Popup_Completion));
   end Set_Popup_Completion;

   -------------------------
   -- Set_Popup_Set_Width --
   -------------------------

   procedure Set_Popup_Set_Width
      (Self            : not null access Gtk_Entry_Completion_Record;
       Popup_Set_Width : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Popup_Set_Width : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_set_width");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Popup_Set_Width));
   end Set_Popup_Set_Width;

   ----------------------------
   -- Set_Popup_Single_Match --
   ----------------------------

   procedure Set_Popup_Single_Match
      (Self               : not null access Gtk_Entry_Completion_Record;
       Popup_Single_Match : Boolean)
   is
      procedure Internal
         (Self               : System.Address;
          Popup_Single_Match : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_completion_set_popup_single_match");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Popup_Single_Match));
   end Set_Popup_Single_Match;

   ---------------------
   -- Set_Text_Column --
   ---------------------

   procedure Set_Text_Column
      (Self   : not null access Gtk_Entry_Completion_Record;
       Column : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_completion_set_text_column");
   begin
      Internal (Get_Object (Self), Column);
   end Set_Text_Column;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Self      : not null access Gtk_Entry_Completion_Record;
       Cell      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Cell      : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr;
          Column    : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Gtk_Entry_Completion_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Self));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Self : not null access Gtk_Entry_Completion_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Cell : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Cell));
   end Clear_Attributes;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Self : not null access Gtk_Entry_Completion_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self   : not null access Gtk_Entry_Completion_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean)
   is
      procedure Internal
         (Self   : System.Address;
          Cell   : System.Address;
          Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Self   : not null access Gtk_Entry_Completion_Record;
       Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand : Boolean)
   is
      procedure Internal
         (Self   : System.Address;
          Cell   : System.Address;
          Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Self     : not null access Gtk_Entry_Completion_Record;
       Cell     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Cell     : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Get_Object (Self), Get_Object (Cell), Position);
   end Reorder;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Completion_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Completion_UTF8_String_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Completion_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Completion_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_UTF8_String_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean);

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Boolean);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean);

   procedure Marsh_Gtk_Entry_Completion_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Completion_UTF8_String_Boolean);

   procedure Marsh_Gtk_Entry_Completion_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Completion_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_UTF8_String_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Completion_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Completion_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Completion_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Completion_Record'Class;
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

   --------------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean --
   --------------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gtk.Tree_Model.Gtk_Tree_Model (Unchecked_To_Interface (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;

   ---------------------------------------
   -- Marsh_GObject_UTF8_String_Boolean --
   ---------------------------------------

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Boolean;

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

   ---------------------------------------------------------------------
   -- Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean --
   ---------------------------------------------------------------------

   procedure Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry_Completion := Gtk_Entry_Completion (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gtk.Tree_Model.Gtk_Tree_Model (Unchecked_To_Interface (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;

   ----------------------------------------------------
   -- Marsh_Gtk_Entry_Completion_UTF8_String_Boolean --
   ----------------------------------------------------

   procedure Marsh_Gtk_Entry_Completion_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Completion_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry_Completion := Gtk_Entry_Completion (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Completion_UTF8_String_Boolean;

   -------------------------------------
   -- Marsh_Gtk_Entry_Completion_Void --
   -------------------------------------

   procedure Marsh_Gtk_Entry_Completion_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Completion_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry_Completion := Gtk_Entry_Completion (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Completion_Void;

   ------------------------
   -- On_Cursor_On_Match --
   ------------------------

   procedure On_Cursor_On_Match
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cursor-on-match" & ASCII.NUL, Call, After);
   end On_Cursor_On_Match;

   ------------------------
   -- On_Cursor_On_Match --
   ------------------------

   procedure On_Cursor_On_Match
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cursor-on-match" & ASCII.NUL, Call, After, Slot);
   end On_Cursor_On_Match;

   ----------------------
   -- On_Insert_Prefix --
   ----------------------

   procedure On_Insert_Prefix
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_UTF8_String_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-prefix" & ASCII.NUL, Call, After);
   end On_Insert_Prefix;

   ----------------------
   -- On_Insert_Prefix --
   ----------------------

   procedure On_Insert_Prefix
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-prefix" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Prefix;

   -----------------------
   -- On_Match_Selected --
   -----------------------

   procedure On_Match_Selected
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "match-selected" & ASCII.NUL, Call, After);
   end On_Match_Selected;

   -----------------------
   -- On_Match_Selected --
   -----------------------

   procedure On_Match_Selected
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "match-selected" & ASCII.NUL, Call, After, Slot);
   end On_Match_Selected;

   -------------------
   -- On_No_Matches --
   -------------------

   procedure On_No_Matches
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_Gtk_Entry_Completion_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "no-matches" & ASCII.NUL, Call, After);
   end On_No_Matches;

   -------------------
   -- On_No_Matches --
   -------------------

   procedure On_No_Matches
      (Self  : not null access Gtk_Entry_Completion_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "no-matches" & ASCII.NUL, Call, After, Slot);
   end On_No_Matches;

end Gtk.Entry_Completion;
