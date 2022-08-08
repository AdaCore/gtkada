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

package body Gtk.Container is

   procedure C_Gtk_Container_Forall
      (Container     : System.Address;
       Callback      : System.Address;
       Callback_Data : System.Address);
   pragma Import (C, C_Gtk_Container_Forall, "gtk_container_forall");
   --  Invokes Callback on each direct child of Container, including children
   --  that are considered "internal" (implementation details of the
   --  container). "Internal" children generally weren't added by the user of
   --  the container, but were added by the container implementation itself.
   --  Most applications should use Gtk.Container.Foreach, rather than
   --  Gtk.Container.Forall.
   --  "callback": a callback
   --  "callback_data": callback user data

   procedure C_Gtk_Container_Foreach
      (Container     : System.Address;
       Callback      : System.Address;
       Callback_Data : System.Address);
   pragma Import (C, C_Gtk_Container_Foreach, "gtk_container_foreach");
   --  Invokes Callback on each non-internal child of Container. See
   --  Gtk.Container.Forall for details on what constitutes an "internal"
   --  child. For all practical purposes, this function should iterate over
   --  precisely those child widgets that were added to the container by the
   --  application with explicit add calls.
   --  It is permissible to remove the child from the Callback handler.
   --  Most applications should use Gtk.Container.Foreach, rather than
   --  Gtk.Container.Forall.
   --  "callback": a callback
   --  "callback_data": callback user data

   function To_Gtk_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Callback, System.Address);

   procedure Internal_Gtk_Callback
      (Widget : System.Address;
       Data   : System.Address);
   pragma Convention (C, Internal_Gtk_Callback);
   --  "widget": the widget to operate on
   --  "data": user-supplied data

   ---------------------------
   -- Internal_Gtk_Callback --
   ---------------------------

   procedure Internal_Gtk_Callback
      (Widget : System.Address;
       Data   : System.Address)
   is
      Func            : constant Gtk_Callback := To_Gtk_Callback (Data);
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      Func (Gtk.Widget.Gtk_Widget (Get_User_Data (Widget, Stub_Gtk_Widget)));
   end Internal_Gtk_Callback;

   package Type_Conversion_Gtk_Container is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Container_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Container);

   ---------
   -- Add --
   ---------

   procedure Add
      (Container : not null access Gtk_Container_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Container : System.Address;
          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Add;

   ------------------
   -- Check_Resize --
   ------------------

   procedure Check_Resize (Container : not null access Gtk_Container_Record) is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_check_resize");
   begin
      Internal (Get_Object (Container));
   end Check_Resize;

   ------------------------
   -- Child_Get_Property --
   ------------------------

   procedure Child_Get_Property
      (Container     : not null access Gtk_Container_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Container     : System.Address;
          Child         : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_container_child_get_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Container), Get_Object (Child), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Child_Get_Property;

   ------------------
   -- Child_Notify --
   ------------------

   procedure Child_Notify
      (Container      : not null access Gtk_Container_Record;
       Child          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Child_Property : UTF8_String)
   is
      procedure Internal
         (Container      : System.Address;
          Child          : System.Address;
          Child_Property : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_container_child_notify");
      Tmp_Child_Property : Gtkada.Types.Chars_Ptr := New_String (Child_Property);
   begin
      Internal (Get_Object (Container), Get_Object (Child), Tmp_Child_Property);
      Free (Tmp_Child_Property);
   end Child_Notify;

   ---------------------------
   -- Child_Notify_By_Pspec --
   ---------------------------

   procedure Child_Notify_By_Pspec
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pspec     : in out Glib.Param_Spec)
   is
      procedure Internal
         (Container : System.Address;
          Child     : System.Address;
          Pspec     : in out Glib.Param_Spec);
      pragma Import (C, Internal, "gtk_container_child_notify_by_pspec");
   begin
      Internal (Get_Object (Container), Get_Object (Child), Pspec);
   end Child_Notify_By_Pspec;

   ------------------------
   -- Child_Set_Property --
   ------------------------

   procedure Child_Set_Property
      (Container     : not null access Gtk_Container_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Property_Name : UTF8_String;
       Value         : Glib.Values.GValue)
   is
      procedure Internal
         (Container     : System.Address;
          Child         : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_container_child_set_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Container), Get_Object (Child), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Child_Set_Property;

   ----------------
   -- Child_Type --
   ----------------

   function Child_Type
      (Container : not null access Gtk_Container_Record) return GType
   is
      function Internal (Container : System.Address) return GType;
      pragma Import (C, Internal, "gtk_container_child_type");
   begin
      return Internal (Get_Object (Container));
   end Child_Type;

   ------------
   -- Forall --
   ------------

   procedure Forall
      (Container : not null access Gtk_Container_Record;
       Callback  : Gtk_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Container_Forall (Get_Object (Container), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Container_Forall (Get_Object (Container), Internal_Gtk_Callback'Address, To_Address (Callback));
      end if;
   end Forall;

   package body Forall_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Callback, System.Address);

      procedure Internal_Cb (Widget : System.Address; Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  The type of the callback functions used for e.g. iterating over the
      --  children of a container, see Gtk.Container.Foreach.
      --  "widget": the widget to operate on
      --  "data": user-supplied data

      ------------
      -- Forall --
      ------------

      procedure Forall
         (Container     : not null access Gtk.Container.Gtk_Container_Record'Class;
          Callback      : Gtk_Callback;
          Callback_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Callback = null then
            C_Gtk_Container_Forall (Get_Object (Container), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Callback), Callback_Data);
            C_Gtk_Container_Forall (Get_Object (Container), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Forall;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb (Widget : System.Address; Data : System.Address) is
         D               : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      begin
         To_Gtk_Callback (D.Func) (Gtk.Widget.Gtk_Widget (Get_User_Data (Widget, Stub_Gtk_Widget)), D.Data.all);
      end Internal_Cb;

   end Forall_User_Data;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
      (Container : not null access Gtk_Container_Record;
       Callback  : Gtk_Callback)
   is
   begin
      if Callback = null then
         C_Gtk_Container_Foreach (Get_Object (Container), System.Null_Address, System.Null_Address);
      else
         C_Gtk_Container_Foreach (Get_Object (Container), Internal_Gtk_Callback'Address, To_Address (Callback));
      end if;
   end Foreach;

   package body Foreach_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Callback, System.Address);

      procedure Internal_Cb (Widget : System.Address; Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  The type of the callback functions used for e.g. iterating over the
      --  children of a container, see Gtk.Container.Foreach.
      --  "widget": the widget to operate on
      --  "data": user-supplied data

      -------------
      -- Foreach --
      -------------

      procedure Foreach
         (Container     : not null access Gtk.Container.Gtk_Container_Record'Class;
          Callback      : Gtk_Callback;
          Callback_Data : User_Data_Type)
      is
         D : System.Address;
      begin
         if Callback = null then
            C_Gtk_Container_Foreach (Get_Object (Container), System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Callback), Callback_Data);
            C_Gtk_Container_Foreach (Get_Object (Container), Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Foreach;

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb (Widget : System.Address; Data : System.Address) is
         D               : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
      begin
         To_Gtk_Callback (D.Func) (Gtk.Widget.Gtk_Widget (Get_User_Data (Widget, Stub_Gtk_Widget)), D.Data.all);
      end Internal_Cb;

   end Foreach_User_Data;

   ----------------------
   -- Get_Border_Width --
   ----------------------

   function Get_Border_Width
      (Container : not null access Gtk_Container_Record) return Guint
   is
      function Internal (Container : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_container_get_border_width");
   begin
      return Internal (Get_Object (Container));
   end Get_Border_Width;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
      (Container : not null access Gtk_Container_Record)
       return Gtk.Widget.Widget_List.Glist
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_children");
      Tmp_Return : Gtk.Widget.Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Container)));
      return Tmp_Return;
   end Get_Children;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
      (Container : not null access Gtk_Container_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Container)), Stub_Gtk_Widget));
   end Get_Focus_Child;

   ---------------------------
   -- Get_Focus_Hadjustment --
   ---------------------------

   function Get_Focus_Hadjustment
      (Container : not null access Gtk_Container_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Container)), Stub_Gtk_Adjustment));
   end Get_Focus_Hadjustment;

   ---------------------------
   -- Get_Focus_Vadjustment --
   ---------------------------

   function Get_Focus_Vadjustment
      (Container : not null access Gtk_Container_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Container : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_container_get_focus_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Container)), Stub_Gtk_Adjustment));
   end Get_Focus_Vadjustment;

   ------------------------
   -- Get_Path_For_Child --
   ------------------------

   function Get_Path_For_Child
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget_Path
   is
      function Internal
         (Container : System.Address;
          Child     : System.Address)
          return access Gtk.Widget.Gtk_Widget_Path;
      pragma Import (C, Internal, "gtk_container_get_path_for_child");
   begin
      return From_Object_Free (Internal (Get_Object (Container), Get_Object (Child)));
   end Get_Path_For_Child;

   ---------------------
   -- Get_Resize_Mode --
   ---------------------

   function Get_Resize_Mode
      (Container : not null access Gtk_Container_Record)
       return Gtk.Enums.Gtk_Resize_Mode
   is
      function Internal
         (Container : System.Address) return Gtk.Enums.Gtk_Resize_Mode;
      pragma Import (C, Internal, "gtk_container_get_resize_mode");
   begin
      return Internal (Get_Object (Container));
   end Get_Resize_Mode;

   --------------------
   -- Propagate_Draw --
   --------------------

   procedure Propagate_Draw
      (Container : not null access Gtk_Container_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cr        : Cairo.Cairo_Context)
   is
      procedure Internal
         (Container : System.Address;
          Child     : System.Address;
          Cr        : Cairo.Cairo_Context);
      pragma Import (C, Internal, "gtk_container_propagate_draw");
   begin
      Internal (Get_Object (Container), Get_Object (Child), Cr);
   end Propagate_Draw;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Container : not null access Gtk_Container_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Container : System.Address;
          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_remove");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Remove;

   ---------------------
   -- Resize_Children --
   ---------------------

   procedure Resize_Children
      (Container : not null access Gtk_Container_Record)
   is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_resize_children");
   begin
      Internal (Get_Object (Container));
   end Resize_Children;

   ----------------------
   -- Set_Border_Width --
   ----------------------

   procedure Set_Border_Width
      (Container    : not null access Gtk_Container_Record;
       Border_Width : Guint)
   is
      procedure Internal (Container : System.Address; Border_Width : Guint);
      pragma Import (C, Internal, "gtk_container_set_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Set_Border_Width;

   ---------------------
   -- Set_Focus_Chain --
   ---------------------

   procedure Set_Focus_Chain
      (Container         : not null access Gtk_Container_Record;
       Focusable_Widgets : Gtk.Widget.Widget_List.Glist)
   is
      procedure Internal
         (Container         : System.Address;
          Focusable_Widgets : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_chain");
   begin
      Internal (Get_Object (Container), Gtk.Widget.Widget_List.Get_Object (Focusable_Widgets));
   end Set_Focus_Chain;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
      (Container : not null access Gtk_Container_Record;
       Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Container : System.Address;
          Child     : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_child");
   begin
      Internal (Get_Object (Container), Get_Object_Or_Null (GObject (Child)));
   end Set_Focus_Child;

   ---------------------------
   -- Set_Focus_Hadjustment --
   ---------------------------

   procedure Set_Focus_Hadjustment
      (Container  : not null access Gtk_Container_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Container  : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_hadjustment");
   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Hadjustment;

   ---------------------------
   -- Set_Focus_Vadjustment --
   ---------------------------

   procedure Set_Focus_Vadjustment
      (Container  : not null access Gtk_Container_Record;
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Container  : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_vadjustment");
   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Vadjustment;

   ----------------------------
   -- Set_Reallocate_Redraws --
   ----------------------------

   procedure Set_Reallocate_Redraws
      (Container     : not null access Gtk_Container_Record;
       Needs_Redraws : Boolean)
   is
      procedure Internal
         (Container     : System.Address;
          Needs_Redraws : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_container_set_reallocate_redraws");
   begin
      Internal (Get_Object (Container), Boolean'Pos (Needs_Redraws));
   end Set_Reallocate_Redraws;

   ---------------------
   -- Set_Resize_Mode --
   ---------------------

   procedure Set_Resize_Mode
      (Container   : not null access Gtk_Container_Record;
       Resize_Mode : Gtk.Enums.Gtk_Resize_Mode)
   is
      procedure Internal
         (Container   : System.Address;
          Resize_Mode : Gtk.Enums.Gtk_Resize_Mode);
      pragma Import (C, Internal, "gtk_container_set_resize_mode");
   begin
      Internal (Get_Object (Container), Resize_Mode);
   end Set_Resize_Mode;

   -----------------------
   -- Unset_Focus_Chain --
   -----------------------

   procedure Unset_Focus_Chain
      (Container : not null access Gtk_Container_Record)
   is
      procedure Internal (Container : System.Address);
      pragma Import (C, Internal, "gtk_container_unset_focus_chain");
   begin
      Internal (Get_Object (Container));
   end Unset_Focus_Chain;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Container_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Container_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Container_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Container_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Container_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Container_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Container_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Container_Gtk_Widget_Void);

   procedure Marsh_Gtk_Container_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Container_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Container_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Container_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Container_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Container_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Container_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Container_Record'Class;
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

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

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

   -----------------------------------------
   -- Marsh_Gtk_Container_Gtk_Widget_Void --
   -----------------------------------------

   procedure Marsh_Gtk_Container_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Container_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Container := Gtk_Container (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Container_Gtk_Widget_Void;

   ------------------------------
   -- Marsh_Gtk_Container_Void --
   ------------------------------

   procedure Marsh_Gtk_Container_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Container_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Container := Gtk_Container (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Container_Void;

   ------------
   -- On_Add --
   ------------

   procedure On_Add
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "add" & ASCII.NUL, Call, After);
   end On_Add;

   ------------
   -- On_Add --
   ------------

   procedure On_Add
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "add" & ASCII.NUL, Call, After, Slot);
   end On_Add;

   ---------------------
   -- On_Check_Resize --
   ---------------------

   procedure On_Check_Resize
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "check-resize" & ASCII.NUL, Call, After);
   end On_Check_Resize;

   ---------------------
   -- On_Check_Resize --
   ---------------------

   procedure On_Check_Resize
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "check-resize" & ASCII.NUL, Call, After, Slot);
   end On_Check_Resize;

   ---------------
   -- On_Remove --
   ---------------

   procedure On_Remove
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "remove" & ASCII.NUL, Call, After);
   end On_Remove;

   ---------------
   -- On_Remove --
   ---------------

   procedure On_Remove
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "remove" & ASCII.NUL, Call, After, Slot);
   end On_Remove;

   ------------------------
   -- On_Set_Focus_Child --
   ------------------------

   procedure On_Set_Focus_Child
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_Gtk_Container_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "set-focus-child" & ASCII.NUL, Call, After);
   end On_Set_Focus_Child;

   ------------------------
   -- On_Set_Focus_Child --
   ------------------------

   procedure On_Set_Focus_Child
      (Self  : not null access Gtk_Container_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "set-focus-child" & ASCII.NUL, Call, After, Slot);
   end On_Set_Focus_Child;

end Gtk.Container;
