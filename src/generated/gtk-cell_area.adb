------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Cell_Area is

   --------------
   -- Get_Area --
   --------------

   function Get_Area
     (Context : access Gtk_Cell_Area_Context_Record)
   return Gtk.Cell_Area.Gtk_Cell_Area
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_context_get_area");
      Stub_Gtk_Cell_Area : Gtk.Cell_Area.Gtk_Cell_Area_Record;
   begin
      return Gtk.Cell_Area.Gtk_Cell_Area
        (Get_User_Data (Internal (Get_Object (Context)), Stub_Gtk_Cell_Area));
   end Get_Area;

   --------------
   -- Get_Area --
   --------------

   function Get_Area
     (Cell_Layout : Gtk_Cell_Layout) return Gtk.Cell_Area.Gtk_Cell_Area
   is
      function Internal
        (Cell_Layout : Gtk_Cell_Layout) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_area");
      Stub_Gtk_Cell_Area : Gtk.Cell_Area.Gtk_Cell_Area_Record;
   begin
      return Gtk.Cell_Area.Gtk_Cell_Area (Get_User_Data (Internal (Cell_Layout), Stub_Gtk_Cell_Area));
   end Get_Area;

   --------------------------
   -- Get_Cell_At_Position --
   --------------------------

   procedure Get_Cell_At_Position
     (Self       : access Gtk_Cell_Area_Record;
      Context    : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
      Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
      X          : Gint;
      Y          : Gint;
      Alloc_Area : out Gdk.Rectangle.Gdk_Rectangle;
      Renderer   : out Gtk.Cell_Renderer.Gtk_Cell_Renderer)
   is
      function Internal
        (Self           : System.Address;
         Context        : System.Address;
         Widget         : System.Address;
         Cell_Area      : Gdk.Rectangle.Gdk_Rectangle;
         X              : Gint;
         Y              : Gint;
         Acc_Alloc_Area : access Gdk.Rectangle.Gdk_Rectangle)
      return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_cell_at_position");
      Acc_Alloc_Area         : aliased Gdk.Rectangle.Gdk_Rectangle;
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      Tmp_Return             : System.Address;
   begin
      Tmp_Return := Internal
        (Get_Object (Self), Get_Object (Context), Get_Object (Widget),
         Cell_Area, X, Y, Acc_Alloc_Area'Access);
      Alloc_Area := Acc_Alloc_Area;
      Renderer := Gtk.Cell_Renderer.Gtk_Cell_Renderer
        (Get_User_Data (Tmp_Return, Stub_Gtk_Cell_Renderer));
   end Get_Cell_At_Position;

   function To_Gtk_Cell_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Callback);

   function To_Gtk_Cell_Alloc_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Alloc_Callback);

   function To_Cell_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Cell_Data_Func);

   procedure C_Gtk_Cell_Area_Foreach
      (Self          : System.Address;
       Callback      : System.Address;
       Callback_Data : System.Address);
   pragma Import (C, C_Gtk_Cell_Area_Foreach, "gtk_cell_area_foreach");
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area.
   --  Since: gtk+ 3.0
   --  "callback": the Gtk.Cell_Area.Gtk_Cell_Callback to call
   --  "callback_data": user provided data pointer

   procedure C_Gtk_Cell_Area_Foreach_Alloc
      (Self            : System.Address;
       Context         : System.Address;
       Widget          : System.Address;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : System.Address;
       Callback_Data   : System.Address);
   pragma Import (C, C_Gtk_Cell_Area_Foreach_Alloc, "gtk_cell_area_foreach_alloc");
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area
   --  with the allocated rectangle inside Cell_Area.
   --  Since: gtk+ 3.0
   --  "context": the Gtk.Cell_Area_Context.Gtk_Cell_Area_Context for this row
   --  of data.
   --  "widget": the Gtk.Widget.Gtk_Widget that Area is rendering to
   --  "cell_area": the Widget relative coordinates and size for Area
   --  "background_area": the Widget relative coordinates of the background
   --  area
   --  "callback": the Gtk.Cell_Area.Gtk_Cell_Alloc_Callback to call
   --  "callback_data": user provided data pointer

   procedure C_Gtk_Cell_Layout_Set_Cell_Data_Func
      (Cell_Layout : System.Address;
       Cell        : System.Address;
       Func        : System.Address;
       Func_Data   : System.Address;
       Destroy     : System.Address);
   pragma Import (C, C_Gtk_Cell_Layout_Set_Cell_Data_Func, "gtk_cell_layout_set_cell_data_func");
   --  Sets the Gtk.Cell_Layout.Cell_Data_Func to use for Cell_Layout.
   --  This function is used instead of the standard attributes mapping for
   --  setting the column value, and should set the value of Cell_Layout's cell
   --  renderer(s) as appropriate.
   --  Func may be null to remove a previously set function.
   --  Since: gtk+ 2.4
   --  "cell": a Gtk.Cell_Renderer.Gtk_Cell_Renderer
   --  "func": the Gtk.Cell_Layout.Cell_Data_Func to use, or null
   --  "func_data": user data for Func
   --  "destroy": destroy notify for Func_Data

   procedure Internal_Cell_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : System.Address;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address);
   pragma Convention (C, Internal_Cell_Data_Func);
   --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
   --  "cell": the cell renderer whose value is to be set
   --  "tree_model": the model
   --  "iter": a GtkTreeIter indicating the row to set the value for
   --  "data": user data passed to Gtk.Entry_Completion.Set_Cell_Data_Func

   function Internal_Gtk_Cell_Alloc_Callback
      (Renderer        : System.Address;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
       Data            : System.Address) return Integer;
   pragma Convention (C, Internal_Gtk_Cell_Alloc_Callback);
   --  "renderer": the cell renderer to operate on
   --  "cell_area": the area allocated to Renderer inside the rectangle
   --  provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "cell_background": the background area for Renderer inside the
   --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "data": user-supplied data

   function Internal_Gtk_Cell_Callback
      (Renderer : System.Address;
       Data     : System.Address) return Integer;
   pragma Convention (C, Internal_Gtk_Cell_Callback);
   --  "renderer": the cell renderer to operate on
   --  "data": user-supplied data

   -----------------------------
   -- Internal_Cell_Data_Func --
   -----------------------------

   procedure Internal_Cell_Data_Func
      (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
       Cell        : System.Address;
       Tree_Model  : System.Address;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Data        : System.Address)
   is
      Func                   : constant Cell_Data_Func := To_Cell_Data_Func (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      Stub_Gtk_Tree_Model    : Gtk.Tree_Model.Gtk_Tree_Model_Record;
   begin
      Func (Cell_Layout, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Gtk_Cell_Renderer)), Gtk.Tree_Model.Gtk_Tree_Model (Get_User_Data (Tree_Model, Stub_Gtk_Tree_Model)), Iter);
   end Internal_Cell_Data_Func;

   --------------------------------------
   -- Internal_Gtk_Cell_Alloc_Callback --
   --------------------------------------

   function Internal_Gtk_Cell_Alloc_Callback
      (Renderer        : System.Address;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
       Data            : System.Address) return Integer
   is
      Func                   : constant Gtk_Cell_Alloc_Callback := To_Gtk_Cell_Alloc_Callback (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Boolean'Pos (Func (Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Renderer, Stub_Gtk_Cell_Renderer)), Cell_Area, Cell_Background));
   end Internal_Gtk_Cell_Alloc_Callback;

   --------------------------------
   -- Internal_Gtk_Cell_Callback --
   --------------------------------

   function Internal_Gtk_Cell_Callback
      (Renderer : System.Address;
       Data     : System.Address) return Integer
   is
      Func                   : constant Gtk_Cell_Callback := To_Gtk_Cell_Callback (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Boolean'Pos (Func (Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Renderer, Stub_Gtk_Cell_Renderer))));
   end Internal_Gtk_Cell_Callback;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Area_Record);
   pragma Unreferenced (Type_Conversion);

   --------------
   -- Activate --
   --------------

   function Activate
      (Self      : access Gtk_Cell_Area_Record;
       Context   : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Edit_Only : Boolean) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Context   : System.Address;
          Widget    : System.Address;
          Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
          Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
          Edit_Only : Integer) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Flags, Boolean'Pos (Edit_Only)));
   end Activate;

   -------------------
   -- Activate_Cell --
   -------------------

   function Activate_Cell
      (Self      : access Gtk_Cell_Area_Record;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Widget    : System.Address;
          Renderer  : System.Address;
          Event     : Gdk.Event.Gdk_Event;
          Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
          Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
          return Integer;
      pragma Import (C, Internal, "gtk_cell_area_activate_cell");
   begin
      return Boolean'Val (Internal (Get_Object (Self), Get_Object (Widget), Get_Object (Renderer), Event, Cell_Area, Flags));
   end Activate_Cell;

   ---------
   -- Add --
   ---------

   procedure Add
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Renderer : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_add");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer));
   end Add;

   -----------------------
   -- Add_Focus_Sibling --
   -----------------------

   procedure Add_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Sibling  : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_add_focus_sibling");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Get_Object (Sibling));
   end Add_Focus_Sibling;

   ----------------------
   -- Apply_Attributes --
   ----------------------

   procedure Apply_Attributes
      (Self        : access Gtk_Cell_Area_Record;
       Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Tree_Model  : System.Address;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Is_Expander : Integer;
          Is_Expanded : Integer);
      pragma Import (C, Internal, "gtk_cell_area_apply_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Tree_Model), Iter, Boolean'Pos (Is_Expander), Boolean'Pos (Is_Expanded));
   end Apply_Attributes;

   -----------------------
   -- Attribute_Connect --
   -----------------------

   procedure Attribute_Connect
      (Self      : access Gtk_Cell_Area_Record;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Attribute : Interfaces.C.Strings.chars_ptr;
          Column    : Gint);
      pragma Import (C, Internal, "gtk_cell_area_attribute_connect");
      Tmp_Attribute : Interfaces.C.Strings.chars_ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Attribute_Connect;

   --------------------------
   -- Attribute_Disconnect --
   --------------------------

   procedure Attribute_Disconnect
      (Self      : access Gtk_Cell_Area_Record;
       Renderer  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Attribute : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_cell_area_attribute_disconnect");
      Tmp_Attribute : Interfaces.C.Strings.chars_ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Attribute);
      Free (Tmp_Attribute);
   end Attribute_Disconnect;

   -----------------------
   -- Cell_Get_Property --
   -----------------------

   procedure Cell_Get_Property
      (Self          : access Gtk_Cell_Area_Record;
       Renderer      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Renderer      : System.Address;
          Property_Name : Interfaces.C.Strings.chars_ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_cell_area_cell_get_property");
      Tmp_Property_Name : Interfaces.C.Strings.chars_ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Cell_Get_Property;

   -----------------------
   -- Cell_Set_Property --
   -----------------------

   procedure Cell_Set_Property
      (Self          : access Gtk_Cell_Area_Record;
       Renderer      : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Renderer      : System.Address;
          Property_Name : Interfaces.C.Strings.chars_ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_cell_area_cell_set_property");
      Tmp_Property_Name : Interfaces.C.Strings.chars_ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Cell_Set_Property;

   ------------------
   -- Copy_Context --
   ------------------

   function Copy_Context
      (Self    : access Gtk_Cell_Area_Record;
       Context : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   is
      function Internal
         (Self    : System.Address;
          Context : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_copy_context");
      Stub_Gtk_Cell_Area_Context : Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record;
   begin
      return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context (Get_User_Data (Internal (Get_Object (Self), Get_Object (Context)), Stub_Gtk_Cell_Area_Context));
   end Copy_Context;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_create_context");
      Stub_Gtk_Cell_Area_Context : Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record;
   begin
      return Gtk.Cell_Area_Context.Gtk_Cell_Area_Context (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Cell_Area_Context));
   end Create_Context;

   -----------
   -- Event --
   -----------

   function Event
      (Self      : access Gtk_Cell_Area_Record;
       Context   : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Gint
   is
      function Internal
         (Self      : System.Address;
          Context   : System.Address;
          Widget    : System.Address;
          Event     : Gdk.Event.Gdk_Event;
          Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
          Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State) return Gint;
      pragma Import (C, Internal, "gtk_cell_area_event");
   begin
      return Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Event, Cell_Area, Flags);
   end Event;

   -----------
   -- Focus --
   -----------

   function Focus
      (Self      : access Gtk_Cell_Area_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Direction : Integer) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Self), Gtk.Enums.Gtk_Direction_Type'Pos (Direction)));
   end Focus;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Self     : access Gtk_Cell_Area_Record;
       Callback : Gtk_Cell_Callback)
   is
   begin
      C_Gtk_Cell_Area_Foreach (Get_Object (Self), Internal_Gtk_Cell_Callback'Address, Callback'Address);
   end Foreach;

   -------------------
   -- Foreach_Alloc --
   -------------------

   procedure Foreach_Alloc
      (Self            : access Gtk_Cell_Area_Record;
       Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : Gtk_Cell_Alloc_Callback)
   is
   begin
      C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, Internal_Gtk_Cell_Alloc_Callback'Address, Callback'Address);
   end Foreach_Alloc;

   package body Foreach_Alloc_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Gtk_Cell_Alloc_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Alloc_Callback);

      function Internal_Cb
         (Renderer        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
          Data            : System.Address) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers and their allocated areas inside a
      --  Gtk.Cell_Area.Gtk_Cell_Area, see Gtk.Cell_Area.Foreach_Alloc.
      --  "renderer": the cell renderer to operate on
      --  "cell_area": the area allocated to Renderer inside the rectangle
      --  provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "cell_background": the background area for Renderer inside the
      --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
      --  "data": user-supplied data

      -------------------
      -- Foreach_Alloc --
      -------------------

      procedure Foreach_Alloc
         (Self            : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
          Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Callback        : Gtk_Cell_Alloc_Callback;
          Callback_Data   : User_Data_Type)
      is
      begin
         C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, Internal_Cb'Address, Users.Build (Callback'Address, Callback_Data));
      end Foreach_Alloc;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Renderer        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
          Data            : System.Address) return Boolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return To_Gtk_Cell_Alloc_Callback (D.Func) (Renderer, Cell_Area, Cell_Background, D.Data.all);
      end Internal_Cb;

   end Foreach_Alloc_User_Data;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Gtk_Cell_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Callback);

      function Internal_Cb
         (Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Data     : System.Address) return Boolean;
      --  The type of the callback functions used for iterating over the cell
      --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see
      --  Gtk.Cell_Area.Foreach.
      --  "renderer": the cell renderer to operate on
      --  "data": user-supplied data

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Self          : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Callback      : Gtk_Cell_Callback;
          Callback_Data : User_Data_Type)
      is
      begin
         C_Gtk_Cell_Area_Foreach (Get_Object (Self), Internal_Cb'Address, Users.Build (Callback'Address, Callback_Data));
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Data     : System.Address) return Boolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         return To_Gtk_Cell_Callback (D.Func) (Renderer, D.Data.all);
      end Internal_Cb;

   end Foreach_User_Data;

   -------------------------
   -- Get_Cell_Allocation --
   -------------------------

   procedure Get_Cell_Allocation
      (Self       : access Gtk_Cell_Area_Record;
       Context    : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer   : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Cell_Area  : access Gdk.Rectangle.Gdk_Rectangle;
       Allocation : access Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self       : System.Address;
          Context    : System.Address;
          Widget     : System.Address;
          Renderer   : System.Address;
          Cell_Area  : access Gdk.Rectangle.Gdk_Rectangle;
          Allocation : access Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_cell_area_get_cell_allocation");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Get_Object (Renderer), Cell_Area, Allocation);
   end Get_Cell_Allocation;

   -----------------------------
   -- Get_Current_Path_String --
   -----------------------------

   function Get_Current_Path_String
      (Self : access Gtk_Cell_Area_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_cell_area_get_current_path_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Self)));
   end Get_Current_Path_String;

   ---------------------
   -- Get_Edit_Widget --
   ---------------------

   function Get_Edit_Widget
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Editable.Gtk_Cell_Editable
   is
      function Internal
         (Self : System.Address) return Gtk.Cell_Editable.Gtk_Cell_Editable;
      pragma Import (C, Internal, "gtk_cell_area_get_edit_widget");
   begin
      return Internal (Get_Object (Self));
   end Get_Edit_Widget;

   ---------------------
   -- Get_Edited_Cell --
   ---------------------

   function Get_Edited_Cell
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_edited_cell");
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Cell_Renderer));
   end Get_Edited_Cell;

   --------------------
   -- Get_Focus_Cell --
   --------------------

   function Get_Focus_Cell
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_focus_cell");
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Cell_Renderer));
   end Get_Focus_Cell;

   ----------------------------
   -- Get_Focus_From_Sibling --
   ----------------------------

   function Get_Focus_From_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Gtk.Cell_Renderer.Gtk_Cell_Renderer
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_focus_from_sibling");
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Internal (Get_Object (Self), Get_Object (Renderer)), Stub_Gtk_Cell_Renderer));
   end Get_Focus_From_Sibling;

   ------------------------
   -- Get_Focus_Siblings --
   ------------------------

   function Get_Focus_Siblings
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Glib.Object.Object_Simple_List.GList
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_focus_siblings");
      Tmp_Return : Glib.Object.Object_Simple_List.GList;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Self), Get_Object (Renderer)));
      return Tmp_Return;
   end Get_Focus_Siblings;

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Self           : access Gtk_Cell_Area_Record;
       Context        : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Height : out Gint;
       Natural_Height : out Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Context        : System.Address;
          Widget         : System.Address;
          Minimum_Height : out Gint;
          Natural_Height : out Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_height");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Minimum_Height, Natural_Height);
   end Get_Preferred_Height;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Self           : access Gtk_Cell_Area_Record;
       Context        : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Gint;
       Minimum_Height : out Gint;
       Natural_Height : out Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Context        : System.Address;
          Widget         : System.Address;
          Width          : Gint;
          Minimum_Height : out Gint;
          Natural_Height : out Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_height_for_width");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Width, Minimum_Height, Natural_Height);
   end Get_Preferred_Height_For_Width;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Self          : access Gtk_Cell_Area_Record;
       Context       : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Width : out Gint;
       Natural_Width : out Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Context       : System.Address;
          Widget        : System.Address;
          Minimum_Width : out Gint;
          Natural_Width : out Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_width");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Minimum_Width, Natural_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Self          : access Gtk_Cell_Area_Record;
       Context       : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Gint;
       Minimum_Width : out Gint;
       Natural_Width : out Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Context       : System.Address;
          Widget        : System.Address;
          Height        : Gint;
          Minimum_Width : out Gint;
          Natural_Width : out Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_width_for_height");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Height, Minimum_Width, Natural_Width);
   end Get_Preferred_Width_For_Height;

   ----------------------
   -- Get_Request_Mode --
   ----------------------

   function Get_Request_Mode
      (Self : access Gtk_Cell_Area_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_get_request_mode");
   begin
      return Gtk.Enums.Gtk_Size_Request_Mode'Val (Internal (Get_Object (Self)));
   end Get_Request_Mode;

   ------------------
   -- Has_Renderer --
   ------------------

   function Has_Renderer
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_has_renderer");
   begin
      return Boolean'Val (Internal (Get_Object (Self), Get_Object (Renderer)));
   end Has_Renderer;

   ---------------------
   -- Inner_Cell_Area --
   ---------------------

   procedure Inner_Cell_Area
      (Self       : access Gtk_Cell_Area_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
       Inner_Area : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self       : System.Address;
          Widget     : System.Address;
          Cell_Area  : Gdk.Rectangle.Gdk_Rectangle;
          Inner_Area : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_cell_area_inner_cell_area");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Cell_Area, Inner_Area);
   end Inner_Cell_Area;

   --------------------
   -- Is_Activatable --
   --------------------

   function Is_Activatable
      (Self : access Gtk_Cell_Area_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_is_activatable");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Is_Activatable;

   ----------------------
   -- Is_Focus_Sibling --
   ----------------------

   function Is_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Sibling  : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_cell_area_is_focus_sibling");
   begin
      return Boolean'Val (Internal (Get_Object (Self), Get_Object (Renderer), Get_Object (Sibling)));
   end Is_Focus_Sibling;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Renderer : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_remove");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer));
   end Remove;

   --------------------------
   -- Remove_Focus_Sibling --
   --------------------------

   procedure Remove_Focus_Sibling
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Sibling  : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_remove_focus_sibling");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Get_Object (Sibling));
   end Remove_Focus_Sibling;

   ------------
   -- Render --
   ------------

   procedure Render
      (Self            : access Gtk_Cell_Area_Record;
       Context         : access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr              : in out Cairo.Cairo_Context;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Paint_Focus     : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Context         : System.Address;
          Widget          : System.Address;
          Cr              : in out Cairo.Cairo_Context;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
          Paint_Focus     : Integer);
      pragma Import (C, Internal, "gtk_cell_area_render");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cr, Background_Area, Cell_Area, Flags, Boolean'Pos (Paint_Focus));
   end Render;

   ----------------------
   -- Request_Renderer --
   ----------------------

   procedure Request_Renderer
      (Self         : access Gtk_Cell_Area_Record;
       Renderer     : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Orientation  : Gtk.Enums.Gtk_Orientation;
       Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
       For_Size     : Gint;
       Minimum_Size : out Gint;
       Natural_Size : out Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Renderer     : System.Address;
          Orientation  : Integer;
          Widget       : System.Address;
          For_Size     : Gint;
          Minimum_Size : out Gint;
          Natural_Size : out Gint);
      pragma Import (C, Internal, "gtk_cell_area_request_renderer");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Gtk.Enums.Gtk_Orientation'Pos (Orientation), Get_Object (Widget), For_Size, Minimum_Size, Natural_Size);
   end Request_Renderer;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Func        : Gtk.Cell_Layout.Cell_Data_Func)
   is
   begin
      C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Cell_Data_Func'Address, Func'Address, System.Null_Address);
   end Set_Cell_Data_Func;

   package body Set_Cell_Data_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);
      function To_Cell_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Cell_Data_Func);

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address);
      --  A function which should set the value of Cell_Layout's cell
      --  renderer(s) as appropriate.
      --  "cell_layout": a Gtk.Cell_Layout.Gtk_Cell_Layout
      --  "cell": the cell renderer whose value is to be set
      --  "tree_model": the model
      --  "iter": a GtkTreeIter indicating the row to set the value for
      --  "data": user data passed to Gtk.Entry_Completion.Set_Cell_Data_Func

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Cell_Layout : Gtk.Cell_Layout.Gtk_Cell_Layout;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Tree_Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Data        : System.Address)
      is
         D : constant Users.Internal_Data_Access := Users.Convert (Data);
      begin
         To_Cell_Data_Func (D.Func) (Cell_Layout, Cell, Tree_Model, Iter, D.Data.all);
      end Internal_Cb;

      ------------------------
      -- Set_Cell_Data_Func --
      ------------------------

      procedure Set_Cell_Data_Func
         (Cell_Layout : access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
          Func        : Cell_Data_Func;
          Func_Data   : User_Data_Type)
      is
      begin
         C_Gtk_Cell_Layout_Set_Cell_Data_Func (Get_Object (Cell_Layout), Get_Object (Cell), Internal_Cb'Address, Users.Build (Func'Address, Func_Data), Users.Free_Data'Address);
      end Set_Cell_Data_Func;

   end Set_Cell_Data_Func_User_Data;

   --------------------
   -- Set_Focus_Cell --
   --------------------

   procedure Set_Focus_Cell
      (Self     : access Gtk_Cell_Area_Record;
       Renderer : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
      procedure Internal (Self : System.Address; Renderer : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_set_focus_cell");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer));
   end Set_Focus_Cell;

   ------------------
   -- Stop_Editing --
   ------------------

   procedure Stop_Editing
      (Self     : access Gtk_Cell_Area_Record;
       Canceled : Boolean)
   is
      procedure Internal (Self : System.Address; Canceled : Integer);
      pragma Import (C, Internal, "gtk_cell_area_stop_editing");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Canceled));
   end Stop_Editing;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute   : UTF8_String;
       Column      : Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Attribute   : Interfaces.C.Strings.chars_ptr;
          Column      : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_add_attribute");
      Tmp_Attribute : Interfaces.C.Strings.chars_ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Add_Attribute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Cell_Layout : access Gtk_Cell_Area_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
      
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
      (Cell_Layout : access Gtk_Cell_Area_Record)
       return Glib.Object.Object_Simple_List.GList
   is
      function Internal (Cell_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Glib.Object.Object_Simple_List.GList;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Cell_Layout)));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Integer);
      pragma Import (C, Internal, "gtk_cell_layout_pack_end");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Expand      : Boolean)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Expand      : Integer);
      pragma Import (C, Internal, "gtk_cell_layout_pack_start");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Boolean'Pos (Expand));
   end Pack_Start;

   -------------
   -- Reorder --
   -------------

   procedure Reorder
      (Cell_Layout : access Gtk_Cell_Area_Record;
       Cell        : access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Position    : Gint)
   is
      procedure Internal
         (Cell_Layout : System.Address;
          Cell        : System.Address;
          Position    : Gint);
      pragma Import (C, Internal, "gtk_cell_layout_reorder");
   begin
      Internal (Get_Object (Cell_Layout), Get_Object (Cell), Position);
   end Reorder;

end Gtk.Cell_Area;
