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
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Cell_View is

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

   package Type_Conversion_Gtk_Cell_View is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_View_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_View);

   -----------------------
   -- Gtk_Cell_View_New --
   -----------------------

   function Gtk_Cell_View_New return Gtk_Cell_View is
      Self : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize (Self);
      return Self;
   end Gtk_Cell_View_New;

   ------------------------------------
   -- Gtk_Cell_View_New_With_Context --
   ------------------------------------

   function Gtk_Cell_View_New_With_Context
      (Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk_Cell_View
   is
      Self : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Context (Self, Area, Context);
      return Self;
   end Gtk_Cell_View_New_With_Context;

   -----------------------------------
   -- Gtk_Cell_View_New_With_Markup --
   -----------------------------------

   function Gtk_Cell_View_New_With_Markup
      (Markup : UTF8_String) return Gtk_Cell_View
   is
      Self : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Markup (Self, Markup);
      return Self;
   end Gtk_Cell_View_New_With_Markup;

   ---------------------------------
   -- Gtk_Cell_View_New_With_Text --
   ---------------------------------

   function Gtk_Cell_View_New_With_Text
      (Text : UTF8_String) return Gtk_Cell_View
   is
      Self : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Text (Self, Text);
      return Self;
   end Gtk_Cell_View_New_With_Text;

   ------------------------------------
   -- Gtk_Cell_View_New_With_Texture --
   ------------------------------------

   function Gtk_Cell_View_New_With_Texture
      (Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class)
       return Gtk_Cell_View
   is
      Self : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Texture (Self, Texture);
      return Self;
   end Gtk_Cell_View_New_With_Texture;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Cell_View) is
   begin
      Self := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_New_With_Context --
   --------------------------

   procedure Gtk_New_With_Context
      (Self    : out Gtk_Cell_View;
       Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
   is
   begin
      Self := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Context (Self, Area, Context);
   end Gtk_New_With_Context;

   -------------------------
   -- Gtk_New_With_Markup --
   -------------------------

   procedure Gtk_New_With_Markup
      (Self   : out Gtk_Cell_View;
       Markup : UTF8_String)
   is
   begin
      Self := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Markup (Self, Markup);
   end Gtk_New_With_Markup;

   -----------------------
   -- Gtk_New_With_Text --
   -----------------------

   procedure Gtk_New_With_Text
      (Self : out Gtk_Cell_View;
       Text : UTF8_String)
   is
   begin
      Self := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Text (Self, Text);
   end Gtk_New_With_Text;

   --------------------------
   -- Gtk_New_With_Texture --
   --------------------------

   procedure Gtk_New_With_Texture
      (Self    : out Gtk_Cell_View;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class)
   is
   begin
      Self := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Texture (Self, Texture);
   end Gtk_New_With_Texture;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Cell_View_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------------
   -- Initialize_With_Context --
   -----------------------------

   procedure Initialize_With_Context
      (Self    : not null access Gtk_Cell_View_Record'Class;
       Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
   is
      function Internal
         (Area    : System.Address;
          Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_context");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Area), Get_Object (Context)));
      end if;
   end Initialize_With_Context;

   ----------------------------
   -- Initialize_With_Markup --
   ----------------------------

   procedure Initialize_With_Markup
      (Self   : not null access Gtk_Cell_View_Record'Class;
       Markup : UTF8_String)
   is
      function Internal
         (Markup : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Markup);
         Free (Tmp_Markup);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_With_Markup;

   --------------------------
   -- Initialize_With_Text --
   --------------------------

   procedure Initialize_With_Text
      (Self : not null access Gtk_Cell_View_Record'Class;
       Text : UTF8_String)
   is
      function Internal
         (Text : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_text");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Text);
         Free (Tmp_Text);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_With_Text;

   -----------------------------
   -- Initialize_With_Texture --
   -----------------------------

   procedure Initialize_With_Texture
      (Self    : not null access Gtk_Cell_View_Record'Class;
       Texture : not null access Gdk.Texture.Gdk_Texture_Record'Class)
   is
      function Internal (Texture : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_texture");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Texture)));
      end if;
   end Initialize_With_Texture;

   -----------------------
   -- Get_Displayed_Row --
   -----------------------

   function Get_Displayed_Row
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_get_displayed_row");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Displayed_Row;

   ------------------------
   -- Get_Draw_Sensitive --
   ------------------------

   function Get_Draw_Sensitive
      (Self : not null access Gtk_Cell_View_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_draw_sensitive");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Draw_Sensitive;

   -------------------
   -- Get_Fit_Model --
   -------------------

   function Get_Fit_Model
      (Self : not null access Gtk_Cell_View_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_fit_model");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Fit_Model;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Self : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_cell_view_get_model");
   begin
      return Internal (Get_Object (Self));
   end Get_Model;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Self : not null access Gtk_Cell_View_Record;
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
         (Self      : not null access Gtk.Cell_View.Gtk_Cell_View_Record'Class;
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

   -----------------------
   -- Set_Displayed_Row --
   -----------------------

   procedure Set_Displayed_Row
      (Self : not null access Gtk_Cell_View_Record;
       Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Self : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_cell_view_set_displayed_row");
   begin
      Internal (Get_Object (Self), Get_Object (Path));
   end Set_Displayed_Row;

   ------------------------
   -- Set_Draw_Sensitive --
   ------------------------

   procedure Set_Draw_Sensitive
      (Self           : not null access Gtk_Cell_View_Record;
       Draw_Sensitive : Boolean)
   is
      procedure Internal
         (Self           : System.Address;
          Draw_Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_view_set_draw_sensitive");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Draw_Sensitive));
   end Set_Draw_Sensitive;

   -------------------
   -- Set_Fit_Model --
   -------------------

   procedure Set_Fit_Model
      (Self      : not null access Gtk_Cell_View_Record;
       Fit_Model : Boolean)
   is
      procedure Internal (Self : System.Address; Fit_Model : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_view_set_fit_model");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Fit_Model));
   end Set_Fit_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Gtk_Cell_View_Record;
       Model : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Self  : System.Address;
          Model : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_cell_view_set_model");
   begin
      Internal (Get_Object (Self), Model);
   end Set_Model;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Self      : not null access Gtk_Cell_View_Record;
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

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Cell_View_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access Gtk_Cell_View_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Self));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Self : not null access Gtk_Cell_View_Record;
       Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Cell : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Cell));
   end Clear_Attributes;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Cell_View_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Cell_View_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      Width.all := Acc_Width;
      Height.all := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Cells;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Cell_View_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self   : not null access Gtk_Cell_View_Record;
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
      (Self   : not null access Gtk_Cell_View_Record;
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
      (Self     : not null access Gtk_Cell_View_Record;
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

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Cell_View_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Cell_View_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Cell_View_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Cell_View_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Cell_View_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Cell_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

end Gtk.Cell_View;
