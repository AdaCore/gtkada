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

package body Gtk.Toolbar is

   package Type_Conversion_Gtk_Toolbar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toolbar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Toolbar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Toolbar : out Gtk_Toolbar) is
   begin
      Toolbar := new Gtk_Toolbar_Record;
      Gtk.Toolbar.Initialize (Toolbar);
   end Gtk_New;

   ---------------------
   -- Gtk_Toolbar_New --
   ---------------------

   function Gtk_Toolbar_New return Gtk_Toolbar is
      Toolbar : constant Gtk_Toolbar := new Gtk_Toolbar_Record;
   begin
      Gtk.Toolbar.Initialize (Toolbar);
      return Toolbar;
   end Gtk_Toolbar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Toolbar : not null access Gtk_Toolbar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_new");
   begin
      if not Toolbar.Is_Created then
         Set_Object (Toolbar, Internal);
      end if;
   end Initialize;

   --------------------
   -- Get_Drop_Index --
   --------------------

   function Get_Drop_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       X       : Glib.Gint;
       Y       : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Toolbar : System.Address;
          X       : Glib.Gint;
          Y       : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_drop_index");
   begin
      return Internal (Get_Object (Toolbar), X, Y);
   end Get_Drop_Index;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_toolbar_get_icon_size");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Icon_Size;

   --------------------
   -- Get_Item_Index --
   --------------------

   function Get_Item_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Toolbar : System.Address;
          Item    : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_item_index");
   begin
      return Internal (Get_Object (Toolbar), Get_Object (Item));
   end Get_Item_Index;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Toolbar : not null access Gtk_Toolbar_Record) return Glib.Gint
   is
      function Internal (Toolbar : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_n_items");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_N_Items;

   ------------------
   -- Get_Nth_Item --
   ------------------

   function Get_Nth_Item
      (Toolbar : not null access Gtk_Toolbar_Record;
       N       : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item
   is
      function Internal
         (Toolbar : System.Address;
          N       : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_get_nth_item");
      Stub_Gtk_Tool_Item : Gtk.Tool_Item.Gtk_Tool_Item_Record;
   begin
      return Gtk.Tool_Item.Gtk_Tool_Item (Get_User_Data (Internal (Get_Object (Toolbar), N), Stub_Gtk_Tool_Item));
   end Get_Nth_Item;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_relief_style");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Relief_Style;

   --------------------
   -- Get_Show_Arrow --
   --------------------

   function Get_Show_Arrow
      (Toolbar : not null access Gtk_Toolbar_Record) return Boolean
   is
      function Internal (Toolbar : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_toolbar_get_show_arrow");
   begin
      return Internal (Get_Object (Toolbar)) /= 0;
   end Get_Show_Arrow;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Toolbar_Style
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_style");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Style;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Pos     : Glib.Gint := -1)
   is
      procedure Internal
         (Toolbar : System.Address;
          Item    : System.Address;
          Pos     : Glib.Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert");
   begin
      Internal (Get_Object (Toolbar), Get_Object (Item), Pos);
   end Insert;

   -----------------------------
   -- Set_Drop_Highlight_Item --
   -----------------------------

   procedure Set_Drop_Highlight_Item
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Tool_Item : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Index     : Glib.Gint)
   is
      procedure Internal
         (Toolbar   : System.Address;
          Tool_Item : System.Address;
          Index     : Glib.Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_drop_highlight_item");
   begin
      Internal (Get_Object (Toolbar), Get_Object_Or_Null (GObject (Tool_Item)), Index);
   end Set_Drop_Highlight_Item;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Toolbar   : System.Address;
          Icon_Size : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_toolbar_set_icon_size");
   begin
      Internal (Get_Object (Toolbar), Icon_Size);
   end Set_Icon_Size;

   --------------------
   -- Set_Show_Arrow --
   --------------------

   procedure Set_Show_Arrow
      (Toolbar    : not null access Gtk_Toolbar_Record;
       Show_Arrow : Boolean := True)
   is
      procedure Internal
         (Toolbar    : System.Address;
          Show_Arrow : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_toolbar_set_show_arrow");
   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Show_Arrow));
   end Set_Show_Arrow;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
      (Toolbar : not null access Gtk_Toolbar_Record;
       Style   : Gtk.Enums.Gtk_Toolbar_Style)
   is
      procedure Internal
         (Toolbar : System.Address;
          Style   : Gtk.Enums.Gtk_Toolbar_Style);
      pragma Import (C, Internal, "gtk_toolbar_set_style");
   begin
      Internal (Get_Object (Toolbar), Style);
   end Set_Style;

   ---------------------
   -- Unset_Icon_Size --
   ---------------------

   procedure Unset_Icon_Size (Toolbar : not null access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_icon_size");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Icon_Size;

   -----------------
   -- Unset_Style --
   -----------------

   procedure Unset_Style (Toolbar : not null access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_style");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Style;

   ------------------------
   -- Get_Ellipsize_Mode --
   ------------------------

   function Get_Ellipsize_Mode
      (Self : not null access Gtk_Toolbar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Self : System.Address) return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_tool_shell_get_ellipsize_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Ellipsize_Mode;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Text_Alignment --
   ------------------------

   function Get_Text_Alignment
      (Self : not null access Gtk_Toolbar_Record) return Gfloat
   is
      function Internal (Self : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Alignment;

   --------------------------
   -- Get_Text_Orientation --
   --------------------------

   function Get_Text_Orientation
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Orientation;

   -------------------------
   -- Get_Text_Size_Group --
   -------------------------

   function Get_Text_Size_Group
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Size_Group.Gtk_Size_Group
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_size_group");
      Stub_Gtk_Size_Group : Gtk.Size_Group.Gtk_Size_Group_Record;
   begin
      return Gtk.Size_Group.Gtk_Size_Group (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Size_Group));
   end Get_Text_Size_Group;

   ------------------
   -- Rebuild_Menu --
   ------------------

   procedure Rebuild_Menu (Self : not null access Gtk_Toolbar_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tool_shell_rebuild_menu");
   begin
      Internal (Get_Object (Self));
   end Rebuild_Menu;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Toolbar_Record;
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
     (Cb_Gtk_Toolbar_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Toolbar_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Toolbar_Gtk_Orientation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Toolbar_Gtk_Orientation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Orientation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Orientation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Toolbar_Style_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Toolbar_Style_Void);

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gtk_Orientation_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Orientation_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Toolbar_Style_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gint_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Gint_Boolean);

   procedure Marsh_GObject_Gtk_Orientation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Orientation_Void);

   procedure Marsh_GObject_Gtk_Toolbar_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Toolbar_Style_Void);

   procedure Marsh_Gtk_Toolbar_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Toolbar_Boolean_Boolean);

   procedure Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean);

   procedure Marsh_Gtk_Toolbar_Gtk_Orientation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Toolbar_Gtk_Orientation_Void);

   procedure Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Toolbar_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gtk_Orientation_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Toolbar_Gtk_Orientation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Orientation_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Orientation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Toolbar_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Toolbar_Style_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Toolbar_Style_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   ------------------------------------------
   -- Marsh_GObject_Gint_Gint_Gint_Boolean --
   ------------------------------------------

   procedure Marsh_GObject_Gint_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Gint_Boolean;

   ----------------------------------------
   -- Marsh_GObject_Gtk_Orientation_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_Orientation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Orientation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Orientation (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Orientation_Void;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Toolbar_Style_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Toolbar_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Toolbar_Style_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Toolbar_Style (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Toolbar_Style_Void;

   ---------------------------------------
   -- Marsh_Gtk_Toolbar_Boolean_Boolean --
   ---------------------------------------

   procedure Marsh_Gtk_Toolbar_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Toolbar_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Toolbar := Gtk_Toolbar (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Toolbar_Boolean_Boolean;

   ----------------------------------------------
   -- Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean --
   ----------------------------------------------

   procedure Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Toolbar := Gtk_Toolbar (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Toolbar_Gint_Gint_Gint_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_Toolbar_Gtk_Orientation_Void --
   --------------------------------------------

   procedure Marsh_Gtk_Toolbar_Gtk_Orientation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Toolbar_Gtk_Orientation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Toolbar := Gtk_Toolbar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Orientation (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Toolbar_Gtk_Orientation_Void;

   ----------------------------------------------
   -- Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Toolbar := Gtk_Toolbar (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Toolbar_Style (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Toolbar_Gtk_Toolbar_Style_Void;

   --------------------------
   -- On_Focus_Home_Or_End --
   --------------------------

   procedure On_Focus_Home_Or_End
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus-home-or-end" & ASCII.NUL, Call, After);
   end On_Focus_Home_Or_End;

   --------------------------
   -- On_Focus_Home_Or_End --
   --------------------------

   procedure On_Focus_Home_Or_End
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus-home-or-end" & ASCII.NUL, Call, After, Slot);
   end On_Focus_Home_Or_End;

   ----------------------------
   -- On_Orientation_Changed --
   ----------------------------

   procedure On_Orientation_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gtk_Orientation_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "orientation-changed" & ASCII.NUL, Call, After);
   end On_Orientation_Changed;

   ----------------------------
   -- On_Orientation_Changed --
   ----------------------------

   procedure On_Orientation_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gtk_Orientation_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "orientation-changed" & ASCII.NUL, Call, After, Slot);
   end On_Orientation_Changed;

   ---------------------------
   -- On_Popup_Context_Menu --
   ---------------------------

   procedure On_Popup_Context_Menu
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gint_Gint_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popup-context-menu" & ASCII.NUL, Call, After);
   end On_Popup_Context_Menu;

   ---------------------------
   -- On_Popup_Context_Menu --
   ---------------------------

   procedure On_Popup_Context_Menu
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gint_Gint_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popup-context-menu" & ASCII.NUL, Call, After, Slot);
   end On_Popup_Context_Menu;

   ----------------------
   -- On_Style_Changed --
   ----------------------

   procedure On_Style_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_Gtk_Toolbar_Gtk_Toolbar_Style_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "style-changed" & ASCII.NUL, Call, After);
   end On_Style_Changed;

   ----------------------
   -- On_Style_Changed --
   ----------------------

   procedure On_Style_Changed
      (Self  : not null access Gtk_Toolbar_Record;
       Call  : Cb_GObject_Gtk_Toolbar_Style_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "style-changed" & ASCII.NUL, Call, After, Slot);
   end On_Style_Changed;

end Gtk.Toolbar;
