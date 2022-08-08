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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Popover is

   package Type_Conversion_Gtk_Popover is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Popover_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Popover);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self        : out Gtk_Popover;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Self := new Gtk_Popover_Record;
      Gtk.Popover.Initialize (Self, Relative_To);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Model --
   ------------------------

   procedure Gtk_New_From_Model
      (Self        : out Gtk_Popover;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Self := new Gtk_Popover_Record;
      Gtk.Popover.Initialize_From_Model (Self, Relative_To, Model);
   end Gtk_New_From_Model;

   ---------------------
   -- Gtk_Popover_New --
   ---------------------

   function Gtk_Popover_New
      (Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Popover
   is
      Self : constant Gtk_Popover := new Gtk_Popover_Record;
   begin
      Gtk.Popover.Initialize (Self, Relative_To);
      return Self;
   end Gtk_Popover_New;

   --------------------------------
   -- Gtk_Popover_New_From_Model --
   --------------------------------

   function Gtk_Popover_New_From_Model
      (Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Popover
   is
      Self : constant Gtk_Popover := new Gtk_Popover_Record;
   begin
      Gtk.Popover.Initialize_From_Model (Self, Relative_To, Model);
      return Self;
   end Gtk_Popover_New_From_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self        : not null access Gtk_Popover_Record'Class;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      function Internal (Relative_To : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Relative_To))));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Model --
   ---------------------------

   procedure Initialize_From_Model
      (Self        : not null access Gtk_Popover_Record'Class;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal
         (Relative_To : System.Address;
          Model       : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_new_from_model");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Relative_To)), Get_Object (Model)));
      end if;
   end Initialize_From_Model;

   ----------------
   -- Bind_Model --
   ----------------

   procedure Bind_Model
      (Self             : not null access Gtk_Popover_Record;
       Model            : access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Action_Namespace : UTF8_String := "")
   is
      procedure Internal
         (Self             : System.Address;
          Model            : System.Address;
          Action_Namespace : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_popover_bind_model");
      Tmp_Action_Namespace : Gtkada.Types.Chars_Ptr;
   begin
      if Action_Namespace = "" then
         Tmp_Action_Namespace := Gtkada.Types.Null_Ptr;
      else
         Tmp_Action_Namespace := New_String (Action_Namespace);
      end if;
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Model)), Tmp_Action_Namespace);
      Free (Tmp_Action_Namespace);
   end Bind_Model;

   ----------------------
   -- Get_Constrain_To --
   ----------------------

   function Get_Constrain_To
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Enums.Gtk_Popover_Constraint
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Popover_Constraint;
      pragma Import (C, Internal, "gtk_popover_get_constrain_to");
   begin
      return Internal (Get_Object (Self));
   end Get_Constrain_To;

   ------------------------
   -- Get_Default_Widget --
   ------------------------

   function Get_Default_Widget
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_get_default_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Default_Widget;

   ---------------
   -- Get_Modal --
   ---------------

   function Get_Modal
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_modal");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Modal;

   ---------------------
   -- Get_Pointing_To --
   ---------------------

   function Get_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Acc_Rect : access Gdk.Rectangle.Gdk_Rectangle)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_pointing_to");
      Acc_Rect   : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Rect'Access);
      Rect.all := Acc_Rect;
      return Tmp_Return /= 0;
   end Get_Pointing_To;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_popover_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   ---------------------
   -- Get_Relative_To --
   ---------------------

   function Get_Relative_To
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_get_relative_to");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Relative_To;

   -----------------------------
   -- Get_Transitions_Enabled --
   -----------------------------

   function Get_Transitions_Enabled
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_transitions_enabled");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Transitions_Enabled;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Self : not null access Gtk_Popover_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_popover_popdown");
   begin
      Internal (Get_Object (Self));
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup (Self : not null access Gtk_Popover_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_popover_popup");
   begin
      Internal (Get_Object (Self));
   end Popup;

   ----------------------
   -- Set_Constrain_To --
   ----------------------

   procedure Set_Constrain_To
      (Self       : not null access Gtk_Popover_Record;
       Constraint : Gtk.Enums.Gtk_Popover_Constraint)
   is
      procedure Internal
         (Self       : System.Address;
          Constraint : Gtk.Enums.Gtk_Popover_Constraint);
      pragma Import (C, Internal, "gtk_popover_set_constrain_to");
   begin
      Internal (Get_Object (Self), Constraint);
   end Set_Constrain_To;

   ------------------------
   -- Set_Default_Widget --
   ------------------------

   procedure Set_Default_Widget
      (Self   : not null access Gtk_Popover_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_popover_set_default_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Widget)));
   end Set_Default_Widget;

   ---------------
   -- Set_Modal --
   ---------------

   procedure Set_Modal
      (Self  : not null access Gtk_Popover_Record;
       Modal : Boolean)
   is
      procedure Internal (Self : System.Address; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_modal");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Modal));
   end Set_Modal;

   ---------------------
   -- Set_Pointing_To --
   ---------------------

   procedure Set_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self : System.Address;
          Rect : Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_popover_set_pointing_to");
   begin
      Internal (Get_Object (Self), Rect);
   end Set_Pointing_To;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Self     : not null access Gtk_Popover_Record;
       Position : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_popover_set_position");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Position;

   ---------------------
   -- Set_Relative_To --
   ---------------------

   procedure Set_Relative_To
      (Self        : not null access Gtk_Popover_Record;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Relative_To : System.Address);
      pragma Import (C, Internal, "gtk_popover_set_relative_to");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Relative_To)));
   end Set_Relative_To;

   -----------------------------
   -- Set_Transitions_Enabled --
   -----------------------------

   procedure Set_Transitions_Enabled
      (Self                : not null access Gtk_Popover_Record;
       Transitions_Enabled : Boolean)
   is
      procedure Internal
         (Self                : System.Address;
          Transitions_Enabled : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_transitions_enabled");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Transitions_Enabled));
   end Set_Transitions_Enabled;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Popover_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Popover_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Popover_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Popover_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Popover_Record'Class;
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

   procedure Marsh_Gtk_Popover_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Popover_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Popover_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Popover_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Popover_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Popover_Record'Class;
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

   ----------------------------
   -- Marsh_Gtk_Popover_Void --
   ----------------------------

   procedure Marsh_Gtk_Popover_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Popover_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Popover := Gtk_Popover (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Popover_Void;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_Gtk_Popover_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "closed" & ASCII.NUL, Call, After);
   end On_Closed;

   ---------------
   -- On_Closed --
   ---------------

   procedure On_Closed
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "closed" & ASCII.NUL, Call, After, Slot);
   end On_Closed;

end Gtk.Popover;
