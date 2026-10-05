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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Overlay is

   package Type_Conversion_Gtk_Overlay is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Overlay_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Overlay);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Overlay) is
   begin
      Self := new Gtk_Overlay_Record;
      Gtk.Overlay.Initialize (Self);
   end Gtk_New;

   ---------------------
   -- Gtk_Overlay_New --
   ---------------------

   function Gtk_Overlay_New return Gtk_Overlay is
      Self : constant Gtk_Overlay := new Gtk_Overlay_Record;
   begin
      Gtk.Overlay.Initialize (Self);
      return Self;
   end Gtk_Overlay_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Overlay_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_overlay_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Add_Overlay --
   -----------------

   procedure Add_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_overlay_add_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Widget));
   end Add_Overlay;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_Overlay_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_overlay_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   ----------------------
   -- Get_Clip_Overlay --
   ----------------------

   function Get_Clip_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_overlay_get_clip_overlay");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget)) /= 0;
   end Get_Clip_Overlay;

   -------------------------
   -- Get_Measure_Overlay --
   -------------------------

   function Get_Measure_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_overlay_get_measure_overlay");
   begin
      return Internal (Get_Object (Self), Get_Object (Widget)) /= 0;
   end Get_Measure_Overlay;

   --------------------
   -- Remove_Overlay --
   --------------------

   procedure Remove_Overlay
      (Self   : not null access Gtk_Overlay_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_overlay_remove_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Widget));
   end Remove_Overlay;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_Overlay_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_overlay_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   ----------------------
   -- Set_Clip_Overlay --
   ----------------------

   procedure Set_Clip_Overlay
      (Self         : not null access Gtk_Overlay_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Clip_Overlay : Boolean)
   is
      procedure Internal
         (Self         : System.Address;
          Widget       : System.Address;
          Clip_Overlay : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_overlay_set_clip_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Boolean'Pos (Clip_Overlay));
   end Set_Clip_Overlay;

   -------------------------
   -- Set_Measure_Overlay --
   -------------------------

   procedure Set_Measure_Overlay
      (Self    : not null access Gtk_Overlay_Record;
       Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Measure : Boolean)
   is
      procedure Internal
         (Self    : System.Address;
          Widget  : System.Address;
          Measure : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_overlay_set_measure_overlay");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Boolean'Pos (Measure));
   end Set_Measure_Overlay;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Overlay_Record;
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
      (Self : not null access Gtk_Overlay_Record) return UTF8_String
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
      (Self : not null access Gtk_Overlay_Record)
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
      (Self : not null access Gtk_Overlay_Record)
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
      (Self : not null access Gtk_Overlay_Record)
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
      (Self   : not null access Gtk_Overlay_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean
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
      X := Acc_X;
      Y := Acc_Y;
      Width := Acc_Width;
      Height := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Overlay_Record)
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
      (Self : not null access Gtk_Overlay_Record)
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
      (Self  : not null access Gtk_Overlay_Record;
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

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Overlay_Record;
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
      (Self     : not null access Gtk_Overlay_Record;
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
      (Self  : not null access Gtk_Overlay_Record;
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
      (Self         : not null access Gtk_Overlay_Record;
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

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Overlay_Record;
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
      (Self  : not null access Gtk_Overlay_Record;
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
     (Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean);

   procedure Connect
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean);

   procedure Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Overlay_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ----------------------------------------------------
   -- Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean --
   ----------------------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gdk_Rectangle_Access (Params, 2).all);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;

   --------------------------------------------------------
   -- Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean --
   --------------------------------------------------------

   procedure Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Overlay := Gtk_Overlay (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gdk_Rectangle_Access (Params, 2).all);
   begin
      Set_Value (Return_Value, V'Address);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;

   ---------------------------
   -- On_Get_Child_Position --
   ---------------------------

   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_Gtk_Overlay_Gtk_Widget_Gdk_Rectangle_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "get-child-position" & ASCII.NUL, Call, After);
   end On_Get_Child_Position;

   ---------------------------
   -- On_Get_Child_Position --
   ---------------------------

   procedure On_Get_Child_Position
      (Self  : not null access Gtk_Overlay_Record;
       Call  : Cb_GObject_Gtk_Widget_Gdk_Rectangle_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "get-child-position" & ASCII.NUL, Call, After, Slot);
   end On_Get_Child_Position;

end Gtk.Overlay;
