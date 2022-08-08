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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

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

   procedure C_Gtk_Cell_Area_Foreach
      (Self          : System.Address;
       Callback      : System.Address;
       Callback_Data : System.Address);
   pragma Import (C, C_Gtk_Cell_Area_Foreach, "gtk_cell_area_foreach");
   --  Calls Callback for every Gtk.Cell_Renderer.Gtk_Cell_Renderer in Area.
   --  Since: gtk+ 3.0
   --  "callback": the Gtk_Cell_Callback to call
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
   --  "callback": the Gtk_Cell_Alloc_Callback to call
   --  "callback_data": user provided data pointer

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

   function To_Gtk_Cell_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Cell_Callback, System.Address);

   function To_Gtk_Cell_Alloc_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Alloc_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Cell_Alloc_Callback, System.Address);

   function To_Gtk_Cell_Layout_Data_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Cell_Layout_Data_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Cell_Layout_Data_Func, System.Address);

   function Internal_Gtk_Cell_Alloc_Callback
      (Renderer        : System.Address;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
       Data            : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Cell_Alloc_Callback);
   --  "renderer": the cell renderer to operate on
   --  "cell_area": the area allocated to Renderer inside the rectangle
   --  provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "cell_background": the background area for Renderer inside the
   --  background area provided to Gtk.Cell_Area.Foreach_Alloc.
   --  "data": user-supplied data

   function Internal_Gtk_Cell_Callback
      (Renderer : System.Address;
       Data     : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Cell_Callback);
   --  "renderer": the cell renderer to operate on
   --  "data": user-supplied data

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

   --------------------------------------
   -- Internal_Gtk_Cell_Alloc_Callback --
   --------------------------------------

   function Internal_Gtk_Cell_Alloc_Callback
      (Renderer        : System.Address;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
       Data            : System.Address) return Glib.Gboolean
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
       Data     : System.Address) return Glib.Gboolean
   is
      Func                   : constant Gtk_Cell_Callback := To_Gtk_Cell_Callback (Data);
      Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
   begin
      return Boolean'Pos (Func (Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Renderer, Stub_Gtk_Cell_Renderer))));
   end Internal_Gtk_Cell_Callback;

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

   package Type_Conversion_Gtk_Cell_Area is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Area_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Area);

   --------------
   -- Activate --
   --------------

   function Activate
      (Self      : not null access Gtk_Cell_Area_Record;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
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
          Edit_Only : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_activate");
   begin
      return Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Flags, Boolean'Pos (Edit_Only)) /= 0;
   end Activate;

   -------------------
   -- Activate_Cell --
   -------------------

   function Activate_Cell
      (Self      : not null access Gtk_Cell_Area_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
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
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_activate_cell");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget), Get_Object (Renderer), Event, Cell_Area, Flags) /= 0;
   end Activate_Cell;

   ---------
   -- Add --
   ---------

   procedure Add
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self        : not null access Gtk_Cell_Area_Record;
       Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
       Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
       Is_Expander : Boolean;
       Is_Expanded : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Tree_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
          Iter        : Gtk.Tree_Model.Gtk_Tree_Iter;
          Is_Expander : Glib.Gboolean;
          Is_Expanded : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_area_apply_attributes");
   begin
      Internal (Get_Object (Self), Tree_Model, Iter, Boolean'Pos (Is_Expander), Boolean'Pos (Is_Expanded));
   end Apply_Attributes;

   -----------------------
   -- Attribute_Connect --
   -----------------------

   procedure Attribute_Connect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String;
       Column    : Glib.Gint)
   is
      procedure Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr;
          Column    : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_attribute_connect");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Attribute, Column);
      Free (Tmp_Attribute);
   end Attribute_Connect;

   --------------------------
   -- Attribute_Disconnect --
   --------------------------

   procedure Attribute_Disconnect
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String)
   is
      procedure Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_cell_area_attribute_disconnect");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Attribute);
      Free (Tmp_Attribute);
   end Attribute_Disconnect;

   --------------------------
   -- Attribute_Get_Column --
   --------------------------

   function Attribute_Get_Column
      (Self      : not null access Gtk_Cell_Area_Record;
       Renderer  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Attribute : UTF8_String) return Glib.Gint
   is
      function Internal
         (Self      : System.Address;
          Renderer  : System.Address;
          Attribute : Gtkada.Types.Chars_Ptr) return Glib.Gint;
      pragma Import (C, Internal, "gtk_cell_area_attribute_get_column");
      Tmp_Attribute : Gtkada.Types.Chars_Ptr := New_String (Attribute);
      Tmp_Return    : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Attribute);
      Free (Tmp_Attribute);
      return Tmp_Return;
   end Attribute_Get_Column;

   -----------------------
   -- Cell_Get_Property --
   -----------------------

   procedure Cell_Get_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Renderer      : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_cell_area_cell_get_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Cell_Get_Property;

   -----------------------
   -- Cell_Set_Property --
   -----------------------

   procedure Cell_Set_Property
      (Self          : not null access Gtk_Cell_Area_Record;
       Renderer      : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Renderer      : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_cell_area_cell_set_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Cell_Set_Property;

   ------------------
   -- Copy_Context --
   ------------------

   function Copy_Context
      (Self    : not null access Gtk_Cell_Area_Record;
       Context : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class)
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
      (Self : not null access Gtk_Cell_Area_Record)
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
      (Self      : not null access Gtk_Cell_Area_Record;
       Context   : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Event     : Gdk.Event.Gdk_Event;
       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
       Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
       return Glib.Gint
   is
      function Internal
         (Self      : System.Address;
          Context   : System.Address;
          Widget    : System.Address;
          Event     : Gdk.Event.Gdk_Event;
          Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
          Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
          return Glib.Gint;
      pragma Import (C, Internal, "gtk_cell_area_event");
   begin
      return Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Event, Cell_Area, Flags);
   end Event;

   -----------
   -- Focus --
   -----------

   function Focus
      (Self      : not null access Gtk_Cell_Area_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Direction : Gtk.Enums.Gtk_Direction_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_focus");
   begin
      return Internal (Get_Object (Self), Direction) /= 0;
   end Focus;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Self     : not null access Gtk_Cell_Area_Record;
       Callback : Gtk_Cell_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Cell_Area_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Area_Foreach (Get_Object (Self), Internal_Gtk_Cell_Callback'Address, To_Address (Callback));
      end if;
   end Foreach;

   -------------------
   -- Foreach_Alloc --
   -------------------

   procedure Foreach_Alloc
      (Self            : not null access Gtk_Cell_Area_Record;
       Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Callback        : Gtk_Cell_Alloc_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, System.Null_Address, System.Null_Address);
      else
         C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, Internal_Gtk_Cell_Alloc_Callback'Address, To_Address (Callback));
      end if;
   end Foreach_Alloc;

   package body Foreach_Alloc_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Cell_Alloc_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Alloc_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Cell_Alloc_Callback, System.Address);

      function Internal_Cb
         (Renderer        : System.Address;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
          Data            : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
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
         (Self            : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
          Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Callback        : Gtk_Cell_Alloc_Callback;
          Callback_Data   : User_Data_Type)
      is
         D : System.Address;
      begin
         if Callback = null then
            C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Callback), Callback_Data);
            C_Gtk_Cell_Area_Foreach_Alloc (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cell_Area, Background_Area, Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach_Alloc;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Renderer        : System.Address;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Background : Gdk.Rectangle.Gdk_Rectangle;
          Data            : System.Address) return Glib.Gboolean
      is
         D                      : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         return Boolean'Pos (To_Gtk_Cell_Alloc_Callback (D.Func) (Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Renderer, Stub_Gtk_Cell_Renderer)), Cell_Area, Cell_Background, D.Data.all));
      end Internal_Cb;

   end Foreach_Alloc_User_Data;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Cell_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Cell_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Cell_Callback, System.Address);

      function Internal_Cb
         (Renderer : System.Address;
          Data     : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  The type of the callback functions used for iterating over the cell
      --  renderers of a Gtk.Cell_Area.Gtk_Cell_Area, see
      --  Gtk.Cell_Area.Foreach.
      --  "renderer": the cell renderer to operate on
      --  "data": user-supplied data

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Self          : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
          Callback      : Gtk_Cell_Callback;
          Callback_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Callback = null then
            C_Gtk_Cell_Area_Foreach (Get_Object (Self), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Callback), Callback_Data);
            C_Gtk_Cell_Area_Foreach (Get_Object (Self), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Renderer : System.Address;
          Data     : System.Address) return Glib.Gboolean
      is
         D                      : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Cell_Renderer : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         return Boolean'Pos (To_Gtk_Cell_Callback (D.Func) (Gtk.Cell_Renderer.Gtk_Cell_Renderer (Get_User_Data (Renderer, Stub_Gtk_Cell_Renderer)), D.Data.all));
      end Internal_Cb;

   end Foreach_User_Data;

   -------------------------
   -- Get_Cell_Allocation --
   -------------------------

   procedure Get_Cell_Allocation
      (Self       : not null access Gtk_Cell_Area_Record;
       Context    : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Renderer   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
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
      (Self : not null access Gtk_Cell_Area_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_cell_area_get_current_path_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Current_Path_String;

   ---------------------
   -- Get_Edit_Widget --
   ---------------------

   function Get_Edit_Widget
      (Self : not null access Gtk_Cell_Area_Record)
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
      (Self : not null access Gtk_Cell_Area_Record)
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
      (Self : not null access Gtk_Cell_Area_Record)
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
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Glib.Object.Object_Simple_List.Glist
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_area_get_focus_siblings");
      Tmp_Return : Glib.Object.Object_Simple_List.Glist;
   begin
      Glib.Object.Object_Simple_List.Set_Object (Tmp_Return, Internal (Get_Object (Self), Get_Object (Renderer)));
      return Tmp_Return;
   end Get_Focus_Siblings;

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Context        : System.Address;
          Widget         : System.Address;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_height");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Minimum_Height, Natural_Height);
   end Get_Preferred_Height;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Record;
       Context        : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Context        : System.Address;
          Widget         : System.Address;
          Width          : Glib.Gint;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_height_for_width");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Width, Minimum_Height, Natural_Height);
   end Get_Preferred_Height_For_Width;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Context       : System.Address;
          Widget        : System.Address;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_width");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Minimum_Width, Natural_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Record;
       Context       : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Context       : System.Address;
          Widget        : System.Address;
          Height        : Glib.Gint;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_get_preferred_width_for_height");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Height, Minimum_Width, Natural_Width);
   end Get_Preferred_Width_For_Height;

   ----------------------
   -- Get_Request_Mode --
   ----------------------

   function Get_Request_Mode
      (Self : not null access Gtk_Cell_Area_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Size_Request_Mode;
      pragma Import (C, Internal, "gtk_cell_area_get_request_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Request_Mode;

   ------------------
   -- Has_Renderer --
   ------------------

   function Has_Renderer
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_has_renderer");
   begin
      return Internal (Get_Object (Self), Get_Object (Renderer)) /= 0;
   end Has_Renderer;

   ---------------------
   -- Inner_Cell_Area --
   ---------------------

   procedure Inner_Cell_Area
      (Self       : not null access Gtk_Cell_Area_Record;
       Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
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
      (Self : not null access Gtk_Cell_Area_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_is_activatable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Activatable;

   ----------------------
   -- Is_Focus_Sibling --
   ----------------------

   function Is_Focus_Sibling
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Renderer : System.Address;
          Sibling  : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_area_is_focus_sibling");
   begin
      return Internal (Get_Object (Self), Get_Object (Renderer), Get_Object (Sibling)) /= 0;
   end Is_Focus_Sibling;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Sibling  : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self            : not null access Gtk_Cell_Area_Record;
       Context         : not null access Gtk.Cell_Area_Context.Gtk_Cell_Area_Context_Record'Class;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr              : Cairo.Cairo_Context;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
       Paint_Focus     : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Context         : System.Address;
          Widget          : System.Address;
          Cr              : Cairo.Cairo_Context;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
          Paint_Focus     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_area_render");
   begin
      Internal (Get_Object (Self), Get_Object (Context), Get_Object (Widget), Cr, Background_Area, Cell_Area, Flags, Boolean'Pos (Paint_Focus));
   end Render;

   ----------------------
   -- Request_Renderer --
   ----------------------

   procedure Request_Renderer
      (Self         : not null access Gtk_Cell_Area_Record;
       Renderer     : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
       Orientation  : Gtk.Enums.Gtk_Orientation;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       For_Size     : Glib.Gint;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint)
   is
      procedure Internal
         (Self         : System.Address;
          Renderer     : System.Address;
          Orientation  : Gtk.Enums.Gtk_Orientation;
          Widget       : System.Address;
          For_Size     : Glib.Gint;
          Minimum_Size : out Glib.Gint;
          Natural_Size : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_request_renderer");
   begin
      Internal (Get_Object (Self), Get_Object (Renderer), Orientation, Get_Object (Widget), For_Size, Minimum_Size, Natural_Size);
   end Request_Renderer;

   ------------------------
   -- Set_Cell_Data_Func --
   ------------------------

   procedure Set_Cell_Data_Func
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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
         (Cell_Layout : not null access Gtk.Cell_Area.Gtk_Cell_Area_Record'Class;
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

   --------------------
   -- Set_Focus_Cell --
   --------------------

   procedure Set_Focus_Cell
      (Self     : not null access Gtk_Cell_Area_Record;
       Renderer : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
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
      (Self     : not null access Gtk_Cell_Area_Record;
       Canceled : Boolean)
   is
      procedure Internal (Self : System.Address; Canceled : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_area_stop_editing");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Canceled));
   end Stop_Editing;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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

   procedure Clear (Cell_Layout : not null access Gtk_Cell_Area_Record) is
      procedure Internal (Cell_Layout : System.Address);
      pragma Import (C, Internal, "gtk_cell_layout_clear");
   begin
      Internal (Get_Object (Cell_Layout));
   end Clear;

   ----------------------
   -- Clear_Attributes --
   ----------------------

   procedure Clear_Attributes
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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
      (Cell_Layout : not null access Gtk_Cell_Area_Record)
       return Gtk.Cell_Renderer.Cell_Renderer_List.Glist
   is
      function Internal (Cell_Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_cell_layout_get_cells");
      Tmp_Return : Gtk.Cell_Renderer.Cell_Renderer_List.Glist;
   begin
      Gtk.Cell_Renderer.Cell_Renderer_List.Set_Object (Tmp_Return, Internal (Get_Object (Cell_Layout)));
      return Tmp_Return;
   end Get_Cells;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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
      (Cell_Layout : not null access Gtk_Cell_Area_Record;
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

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void);

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void);

   procedure Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void);

   procedure Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void);

   procedure Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void);

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void);

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void);

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void);

   procedure Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Area_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------------------------------------------------
   -- Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void --
   --------------------------------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 2)), Unchecked_To_Gdk_Rectangle (Params, 3), Unchecked_To_UTF8_String (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;

   ------------------------------------------------------------
   -- Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void --
   ------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;

   ------------------------------------------------------
   -- Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void --
   ------------------------------------------------------

   procedure Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Cell_Renderer_UTF8_String_Void;

   ---------------------------------------------------------------------
   -- Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void --
   ---------------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Tree_Model.Gtk_Tree_Model (Unchecked_To_Interface (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Boolean (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;

   --------------------------------------------------------------------------------------------
   -- Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void --
   --------------------------------------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Area := Gtk_Cell_Area (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 2)), Unchecked_To_Gdk_Rectangle (Params, 3), Unchecked_To_UTF8_String (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;

   ------------------------------------------------------------------
   -- Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void --
   ------------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Area := Gtk_Cell_Area (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 2)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;

   ------------------------------------------------------------
   -- Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void --
   ------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Area := Gtk_Cell_Area (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Cell_Renderer.Gtk_Cell_Renderer (Unchecked_To_Object (Params, 1)), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void;

   ---------------------------------------------------------------------------
   -- Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void --
   ---------------------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Area := Gtk_Cell_Area (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Tree_Model.Gtk_Tree_Model (Unchecked_To_Interface (Params, 1)), Unchecked_To_Gtk_Tree_Iter (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Boolean (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;

   ---------------------
   -- On_Add_Editable --
   ---------------------

   procedure On_Add_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "add-editable" & ASCII.NUL, Call, After);
   end On_Add_Editable;

   ---------------------
   -- On_Add_Editable --
   ---------------------

   procedure On_Add_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Gdk_Rectangle_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "add-editable" & ASCII.NUL, Call, After, Slot);
   end On_Add_Editable;

   -------------------------
   -- On_Apply_Attributes --
   -------------------------

   procedure On_Apply_Attributes
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "apply-attributes" & ASCII.NUL, Call, After);
   end On_Apply_Attributes;

   -------------------------
   -- On_Apply_Attributes --
   -------------------------

   procedure On_Apply_Attributes
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Tree_Model_Gtk_Tree_Iter_Boolean_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "apply-attributes" & ASCII.NUL, Call, After, Slot);
   end On_Apply_Attributes;

   ----------------------
   -- On_Focus_Changed --
   ----------------------

   procedure On_Focus_Changed
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus-changed" & ASCII.NUL, Call, After);
   end On_Focus_Changed;

   ----------------------
   -- On_Focus_Changed --
   ----------------------

   procedure On_Focus_Changed
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus-changed" & ASCII.NUL, Call, After, Slot);
   end On_Focus_Changed;

   ------------------------
   -- On_Remove_Editable --
   ------------------------

   procedure On_Remove_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_Gtk_Cell_Area_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "remove-editable" & ASCII.NUL, Call, After);
   end On_Remove_Editable;

   ------------------------
   -- On_Remove_Editable --
   ------------------------

   procedure On_Remove_Editable
      (Self  : not null access Gtk_Cell_Area_Record;
       Call  : Cb_GObject_Gtk_Cell_Renderer_Gtk_Cell_Editable_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "remove-editable" & ASCII.NUL, Call, After, Slot);
   end On_Remove_Editable;

end Gtk.Cell_Area;
