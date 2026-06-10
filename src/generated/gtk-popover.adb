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
with Gdk.Surface;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Popover is

   package Type_Conversion_Gtk_Popover is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Popover_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Popover);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Popover) is
   begin
      Self := new Gtk_Popover_Record;
      Gtk.Popover.Initialize (Self);
   end Gtk_New;

   ---------------------
   -- Gtk_Popover_New --
   ---------------------

   function Gtk_Popover_New return Gtk_Popover is
      Self : constant Gtk_Popover := new Gtk_Popover_Record;
   begin
      Gtk.Popover.Initialize (Self);
      return Self;
   end Gtk_Popover_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Popover_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_popover_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------
   -- Get_Autohide --
   ------------------

   function Get_Autohide
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_autohide");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Autohide;

   -------------------------
   -- Get_Cascade_Popdown --
   -------------------------

   function Get_Cascade_Popdown
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_cascade_popdown");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Cascade_Popdown;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_popover_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -------------------
   -- Get_Has_Arrow --
   -------------------

   function Get_Has_Arrow
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_has_arrow");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Arrow;

   ---------------------------
   -- Get_Mnemonics_Visible --
   ---------------------------

   function Get_Mnemonics_Visible
      (Self : not null access Gtk_Popover_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_popover_get_mnemonics_visible");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Mnemonics_Visible;

   ----------------
   -- Get_Offset --
   ----------------

   procedure Get_Offset
      (Self     : not null access Gtk_Popover_Record;
       X_Offset : out Glib.Gint;
       Y_Offset : out Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          X_Offset : out Glib.Gint;
          Y_Offset : out Glib.Gint);
      pragma Import (C, Internal, "gtk_popover_get_offset");
   begin
      Internal (Get_Object (Self), X_Offset, Y_Offset);
   end Get_Offset;

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

   -------------
   -- Present --
   -------------

   procedure Present (Self : not null access Gtk_Popover_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_popover_present");
   begin
      Internal (Get_Object (Self));
   end Present;

   ------------------
   -- Set_Autohide --
   ------------------

   procedure Set_Autohide
      (Self     : not null access Gtk_Popover_Record;
       Autohide : Boolean)
   is
      procedure Internal (Self : System.Address; Autohide : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_autohide");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Autohide));
   end Set_Autohide;

   -------------------------
   -- Set_Cascade_Popdown --
   -------------------------

   procedure Set_Cascade_Popdown
      (Self            : not null access Gtk_Popover_Record;
       Cascade_Popdown : Boolean)
   is
      procedure Internal
         (Self            : System.Address;
          Cascade_Popdown : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_cascade_popdown");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Cascade_Popdown));
   end Set_Cascade_Popdown;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Popover_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_popover_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

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

   -------------------
   -- Set_Has_Arrow --
   -------------------

   procedure Set_Has_Arrow
      (Self      : not null access Gtk_Popover_Record;
       Has_Arrow : Boolean)
   is
      procedure Internal (Self : System.Address; Has_Arrow : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_has_arrow");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Has_Arrow));
   end Set_Has_Arrow;

   ---------------------------
   -- Set_Mnemonics_Visible --
   ---------------------------

   procedure Set_Mnemonics_Visible
      (Self              : not null access Gtk_Popover_Record;
       Mnemonics_Visible : Boolean)
   is
      procedure Internal
         (Self              : System.Address;
          Mnemonics_Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_popover_set_mnemonics_visible");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Mnemonics_Visible));
   end Set_Mnemonics_Visible;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
      (Self     : not null access Gtk_Popover_Record;
       X_Offset : Glib.Gint;
       Y_Offset : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          X_Offset : Glib.Gint;
          Y_Offset : Glib.Gint);
      pragma Import (C, Internal, "gtk_popover_set_offset");
   begin
      Internal (Get_Object (Self), X_Offset, Y_Offset);
   end Set_Offset;

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

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Popover_Record;
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

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Popover_Record) return UTF8_String
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
      (Self : not null access Gtk_Popover_Record)
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
      (Self : not null access Gtk_Popover_Record)
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
      (Self : not null access Gtk_Popover_Record)
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
      (Self   : not null access Gtk_Popover_Record;
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

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Popover_Record)
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
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Popover_Record;
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

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Popover_Record) return Gdk.Gdk_Surface
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_native_get_surface");
      Stub_Gdk_Surface : Gdk.Surface.Gdk_Surface_Record;
   begin
      return Gdk.Gdk_Surface (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Surface));
   end Get_Surface;

   ---------------------------
   -- Get_Surface_Transform --
   ---------------------------

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Popover_Record;
       X    : out Gdouble;
       Y    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          X    : out Gdouble;
          Y    : out Gdouble);
      pragma Import (C, Internal, "gtk_native_get_surface_transform");
   begin
      Internal (Get_Object (Self), X, Y);
   end Get_Surface_Transform;

   -------------
   -- Realize --
   -------------

   procedure Realize (Self : not null access Gtk_Popover_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_realize");
   begin
      Internal (Get_Object (Self));
   end Realize;

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Popover_Record;
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
      (Self     : not null access Gtk_Popover_Record;
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
      (Self  : not null access Gtk_Popover_Record;
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
      (Self         : not null access Gtk_Popover_Record;
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

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Self : not null access Gtk_Popover_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_native_unrealize");
   begin
      Internal (Get_Object (Self));
   end Unrealize;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Popover_Record;
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
      (Self  : not null access Gtk_Popover_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

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

   -------------------------
   -- On_Activate_Default --
   -------------------------

   procedure On_Activate_Default
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_Gtk_Popover_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-default" & ASCII.NUL, Call, After);
   end On_Activate_Default;

   -------------------------
   -- On_Activate_Default --
   -------------------------

   procedure On_Activate_Default
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-default" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Default;

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
