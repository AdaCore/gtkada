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
with Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Cell_View is

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Cell_Layout : System.Address;
       Cell        : System.Address;
       Func        : System.Address;
       Func_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   --  Sets the Gtk_Cell_Layout_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk_Cell_Layout_Data_Func to use, or null
   --  "func_data": user data for Func
   --  "destroy": destroy notify for Func_Data

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
   pragma Convention (C, Internal_Gtk_Cell_Layout_Data_Func);
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
   --  value for
   --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

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
      Cell_View : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize (Cell_View);
      return Cell_View;
   end Gtk_Cell_View_New;

   ------------------------------------
   -- Gtk_Cell_View_New_With_Context --
   ------------------------------------

   function Gtk_Cell_View_New_With_Context
      (Area    : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk_Cell_View
   is
      Cell_View : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Context (Cell_View, Area, Context);
      return Cell_View;
   end Gtk_Cell_View_New_With_Context;

   -----------------------------------
   -- Gtk_Cell_View_New_With_Markup --
   -----------------------------------

   function Gtk_Cell_View_New_With_Markup
      (Markup : UTF8_String) return Gtk_Cell_View
   is
      Cell_View : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Markup (Cell_View, Markup);
      return Cell_View;
   end Gtk_Cell_View_New_With_Markup;

   -----------------------------------
   -- Gtk_Cell_View_New_With_Pixbuf --
   -----------------------------------

   function Gtk_Cell_View_New_With_Pixbuf
      (Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Cell_View
   is
      Cell_View : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Pixbuf (Cell_View, Pixbuf);
      return Cell_View;
   end Gtk_Cell_View_New_With_Pixbuf;

   ---------------------------------
   -- Gtk_Cell_View_New_With_Text --
   ---------------------------------

   function Gtk_Cell_View_New_With_Text
      (Text : UTF8_String) return Gtk_Cell_View
   is
      Cell_View : constant Gtk_Cell_View := new Gtk_Cell_View_Record;
   begin
      Gtk.Cell_View.Initialize_With_Text (Cell_View, Text);
      return Cell_View;
   end Gtk_Cell_View_New_With_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Cell_View : out Gtk_Cell_View) is
   begin
      Cell_View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize (Cell_View);
   end Gtk_New;

   --------------------------
   -- Gtk_New_With_Context --
   --------------------------

   procedure Gtk_New_With_Context
      (Cell_View : out Gtk_Cell_View;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
   is
   begin
      Cell_View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Context (Cell_View, Area, Context);
   end Gtk_New_With_Context;

   -------------------------
   -- Gtk_New_With_Markup --
   -------------------------

   procedure Gtk_New_With_Markup
      (Cell_View : out Gtk_Cell_View;
       Markup    : UTF8_String)
   is
   begin
      Cell_View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Markup (Cell_View, Markup);
   end Gtk_New_With_Markup;

   -------------------------
   -- Gtk_New_With_Pixbuf --
   -------------------------

   procedure Gtk_New_With_Pixbuf
      (Cell_View : out Gtk_Cell_View;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Cell_View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Pixbuf (Cell_View, Pixbuf);
   end Gtk_New_With_Pixbuf;

   -----------------------
   -- Gtk_New_With_Text --
   -----------------------

   procedure Gtk_New_With_Text
      (Cell_View : out Gtk_Cell_View;
       Text      : UTF8_String)
   is
   begin
      Cell_View := new Gtk_Cell_View_Record;
      Gtk.Cell_View.Initialize_With_Text (Cell_View, Text);
   end Gtk_New_With_Text;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Cell_View : not null access Gtk_Cell_View_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new");
   begin
      if not Cell_View.Is_Created then
         Set_Object (Cell_View, Internal);
      end if;
   end Initialize;

   -----------------------------
   -- Initialize_With_Context --
   -----------------------------

   procedure Initialize_With_Context
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Area      : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
   is
      function Internal
         (Area    : System.Address;
          Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_context");
   begin
      if not Cell_View.Is_Created then
         Set_Object (Cell_View, Internal (Get_Object (Area), Get_Object (Context)));
      end if;
   end Initialize_With_Context;

   ----------------------------
   -- Initialize_With_Markup --
   ----------------------------

   procedure Initialize_With_Markup
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Markup    : UTF8_String)
   is
      function Internal
         (Markup : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
      Tmp_Return : System.Address;
   begin
      if not Cell_View.Is_Created then
         Tmp_Return := Internal (Tmp_Markup);
         Free (Tmp_Markup);
         Set_Object (Cell_View, Tmp_Return);
      end if;
   end Initialize_With_Markup;

   ----------------------------
   -- Initialize_With_Pixbuf --
   ----------------------------

   procedure Initialize_With_Pixbuf
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_pixbuf");
   begin
      if not Cell_View.Is_Created then
         Set_Object (Cell_View, Internal (Get_Object (Pixbuf)));
      end if;
   end Initialize_With_Pixbuf;

   --------------------------
   -- Initialize_With_Text --
   --------------------------

   procedure Initialize_With_Text
      (Cell_View : not null access Gtk_Cell_View_Record'Class;
       Text      : UTF8_String)
   is
      function Internal
         (Text : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_new_with_text");
      Tmp_Text   : Gtkada.Types.Chars_Ptr := New_String (Text);
      Tmp_Return : System.Address;
   begin
      if not Cell_View.Is_Created then
         Tmp_Return := Internal (Tmp_Text);
         Free (Tmp_Text);
         Set_Object (Cell_View, Tmp_Return);
      end if;
   end Initialize_With_Text;

   -----------------------
   -- Get_Displayed_Row --
   -----------------------

   function Get_Displayed_Row
      (Cell_View : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Path
   is
      function Internal (Cell_View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_view_get_displayed_row");
   begin
      return From_Object (Internal (Get_Object (Cell_View)));
   end Get_Displayed_Row;

   ------------------------
   -- Get_Draw_Sensitive --
   ------------------------

   function Get_Draw_Sensitive
      (Cell_View : not null access Gtk_Cell_View_Record) return Boolean
   is
      function Internal (Cell_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_draw_sensitive");
   begin
      return Internal (Get_Object (Cell_View)) /= 0;
   end Get_Draw_Sensitive;

   -------------------
   -- Get_Fit_Model --
   -------------------

   function Get_Fit_Model
      (Cell_View : not null access Gtk_Cell_View_Record) return Boolean
   is
      function Internal (Cell_View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_fit_model");
   begin
      return Internal (Get_Object (Cell_View)) /= 0;
   end Get_Fit_Model;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Cell_View : not null access Gtk_Cell_View_Record)
       return Gtk.Tree_Model.Gtk_Tree_Model
   is
      function Internal
         (Cell_View : System.Address) return Gtk.Tree_Model.Gtk_Tree_Model;
      pragma Import (C, Internal, "gtk_cell_view_get_model");
   begin
      return Internal (Get_Object (Cell_View));
   end Get_Model;

   ---------------------
   -- Get_Size_Of_Row --
   ---------------------

   function Get_Size_Of_Row
      (Cell_View   : not null access Gtk_Cell_View_Record;
       Path        : Gtk.Tree_Model.Gtk_Tree_Path;
       Requisition : access Gtk.Widget.Gtk_Requisition) return Boolean
   is
      function Internal
         (Cell_View       : System.Address;
          Path            : System.Address;
          Acc_Requisition : access Gtk.Widget.Gtk_Requisition)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_view_get_size_of_row");
      Acc_Requisition     : aliased Gtk.Widget.Gtk_Requisition;
      Tmp_Acc_Requisition : aliased Gtk.Widget.Gtk_Requisition;
      Tmp_Return          : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Cell_View), Get_Object (Path), Tmp_Acc_Requisition'Access);
      Acc_Requisition := Tmp_Acc_Requisition;
      Requisition.all := Acc_Requisition;
      return Tmp_Return /= 0;
   end Get_Size_Of_Row;

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
      (Cell_View : not null access Gtk_Cell_View_Record;
       Color     : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Cell_View : System.Address;
          Color     : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_cell_view_set_background_color");
   begin
      Internal (Get_Object (Cell_View), Color);
   end Set_Background_Color;

   -------------------------
   -- Set_Background_Rgba --
   -------------------------

   procedure Set_Background_Rgba
      (Cell_View : not null access Gtk_Cell_View_Record;
       Rgba      : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Cell_View : System.Address;
          Rgba      : Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_cell_view_set_background_rgba");
   begin
      Internal (Get_Object (Cell_View), Rgba);
   end Set_Background_Rgba;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk_Cell_Layout_Data_Func)
   is
   begin
      if Func = null then
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Gtk_Cell_Layout_Data_Func'Address, To_Address (Func), System.Null_Address);
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
      pragma Convention (C, Internal_Cb);
      --  A function which should set the value of Cell_Layout's cell
      --  renderer(s) as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a Gtk.Tree_Model.Gtk_Tree_Iter indicating the row to set the
      --  value for
      --  "data": user data passed to Gtk.Cell_Layout.Set_Cell_Data_Func

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
         (Cell_Layout : not null access Gtk.Cell_View.Gtk_Cell_View_Record'Class;
          Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Gtk_Cell_Layout_Data_Func;
          Func_Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Func = null then
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Func), Func_Data);
            C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

   -----------------------
   -- Set_Displayed_Row --
   -----------------------

   procedure Set_Displayed_Row
      (Cell_View : not null access Gtk_Cell_View_Record;
       Path      : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      procedure Internal (Cell_View : System.Address; Path : System.Address);
      pragma Import (C, Internal, "gtk_cell_view_set_displayed_row");
   begin
      Internal (Get_Object (Cell_View), Get_Object (Path));
   end Set_Displayed_Row;

   ------------------------
   -- Set_Draw_Sensitive --
   ------------------------

   procedure Set_Draw_Sensitive
      (Cell_View      : not null access Gtk_Cell_View_Record;
       Draw_Sensitive : Boolean)
   is
      procedure Internal
         (Cell_View      : System.Address;
          Draw_Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_view_set_draw_sensitive");
   begin
      Internal (Get_Object (Cell_View), Boolean'Pos (Draw_Sensitive));
   end Set_Draw_Sensitive;

   -------------------
   -- Set_Fit_Model --
   -------------------

   procedure Set_Fit_Model
      (Cell_View : not null access Gtk_Cell_View_Record;
       Fit_Model : Boolean)
   is
      procedure Internal
         (Cell_View : System.Address;
          Fit_Model : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_view_set_fit_model");
   begin
      Internal (Get_Object (Cell_View), Boolean'Pos (Fit_Model));
   end Set_Fit_Model;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Cell_View : not null access Gtk_Cell_View_Record;
       Model     : Gtk.Tree_Model.Gtk_Tree_Model)
   is
      procedure Internal
         (Cell_View : System.Address;
          Model     : Gtk.Tree_Model.Gtk_Tree_Model);
      pragma Import (C, Internal, "gtk_cell_view_set_model");
   begin
      Internal (Get_Object (Cell_View), Model);
   end Set_Model;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Glib.Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Attribute   : Gtkada.Types.Chars_Ptr;
          Column      : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Cell_Layout : not null access Gtk_Cell_View_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear_attributes");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell));
   end Clear_Attributes;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells
      (Cell_Layout : not null access Gtk_Cell_View_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Cell_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Cell_Layout)));
      return Tmp_Return;
   end Get_Cells;

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

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Cell_Layout : not null access Gtk_Cell_View_Record;
       Cell        : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Glib.Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Position    : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Position);
   end Reorder;

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

end Gtk.Cell_View;
