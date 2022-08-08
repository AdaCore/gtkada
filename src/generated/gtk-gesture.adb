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

package body Gtk.Gesture is

   function Convert (R : Gtk.Gesture.Gtk_Gesture) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Gesture.Gtk_Gesture is
      Stub : Gtk.Gesture.Gtk_Gesture_Record;begin
         return Gtk.Gesture.Gtk_Gesture (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   function Convert (R : Gdk.Event.Gdk_Event_Sequence) return System.Address is
   begin
      return Glib.To_Address (Glib.C_Proxy (R));
   end Convert;

   function Convert (R : System.Address) return Gdk.Event.Gdk_Event_Sequence is
   begin
      return Gdk.Event.Gdk_Event_Sequence(Glib.C_Proxy'(Glib.To_Proxy (R)));
   end Convert;

   procedure Set_State
     (Self : not null access Gtk_Gesture_Record'Class;
      State : Gtk.Enums.Gtk_Event_Sequence_State := Event_Sequence_Denied)
   is
      Dummy : Boolean;
   begin
      Dummy := Set_State (Self, State);
   end Set_State;

   procedure On_Widget_Destroyed (Gesture, Object : System.Address);
   pragma Convention (C, On_Widget_Destroyed);
   procedure On_Widget_Destroyed (Gesture, Object : System.Address) is
      pragma Unreferenced (Object);
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_unref");
   begin
      Internal (Gesture);
   end On_Widget_Destroyed;

   procedure Watch
     (Self   : not null access Gtk_Gesture_Record'Class;
      Object : not null access GObject_Record'Class)
   is
   begin
      Weak_Ref (Object, Notify => On_Widget_Destroyed'Access,
         Data => Get_Object (Self));
   end Watch;

   package Type_Conversion_Gtk_Gesture is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Gesture_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Gesture);

   ----------------------
   -- Get_Bounding_Box --
   ----------------------

   function Get_Bounding_Box
      (Self : not null access Gtk_Gesture_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Acc_Rect : access Gdk.Rectangle.Gdk_Rectangle)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_get_bounding_box");
      Acc_Rect   : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Rect'Access);
      Rect.all := Acc_Rect;
      return Tmp_Return /= 0;
   end Get_Bounding_Box;

   -----------------------------
   -- Get_Bounding_Box_Center --
   -----------------------------

   function Get_Bounding_Box_Center
      (Self : not null access Gtk_Gesture_Record;
       X    : access Gdouble;
       Y    : access Gdouble) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Acc_X : access Gdouble;
          Acc_Y : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_get_bounding_box_center");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      return Tmp_Return /= 0;
   end Get_Bounding_Box_Center;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device
      (Self : not null access Gtk_Gesture_Record)
       return Gdk.Device.Gdk_Device
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gesture_get_device");
      Stub_Gdk_Device : Gdk.Device.Gdk_Device_Record;
   begin
      return Gdk.Device.Gdk_Device (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Device));
   end Get_Device;

   ---------------
   -- Get_Group --
   ---------------

   function Get_Group
      (Self : not null access Gtk_Gesture_Record) return Gesture_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gesture_get_group");
      Tmp_Return : Gesture_List.Glist;
   begin
      Gtk.Gesture.Gesture_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Group;

   --------------------
   -- Get_Last_Event --
   --------------------

   function Get_Last_Event
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Gdk.Event.Gdk_Event
   is
      function Internal
         (Self     : System.Address;
          Sequence : Gdk.Event.Gdk_Event_Sequence)
          return Gdk.Event.Gdk_Event;
      pragma Import (C, Internal, "gtk_gesture_get_last_event");
   begin
      return Internal (Get_Object (Self), Sequence);
   end Get_Last_Event;

   -------------------------------
   -- Get_Last_Updated_Sequence --
   -------------------------------

   function Get_Last_Updated_Sequence
      (Self : not null access Gtk_Gesture_Record)
       return Gdk.Event.Gdk_Event_Sequence
   is
      function Internal
         (Self : System.Address) return access Gdk.Event.Gdk_Event_Sequence;
      pragma Import (C, Internal, "gtk_gesture_get_last_updated_sequence");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Last_Updated_Sequence;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       X        : access Gdouble;
       Y        : access Gdouble) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Sequence : Gdk.Event.Gdk_Event_Sequence;
          Acc_X    : access Gdouble;
          Acc_Y    : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_get_point");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Sequence, Acc_X'Access, Acc_Y'Access);
      if X /= null then
         X.all := Acc_X;
      end if;
      if Y /= null then
         Y.all := Acc_Y;
      end if;
      return Tmp_Return /= 0;
   end Get_Point;

   ------------------------
   -- Get_Sequence_State --
   ------------------------

   function Get_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence)
       return Gtk.Enums.Gtk_Event_Sequence_State
   is
      function Internal
         (Self     : System.Address;
          Sequence : Gdk.Event.Gdk_Event_Sequence)
          return Gtk.Enums.Gtk_Event_Sequence_State;
      pragma Import (C, Internal, "gtk_gesture_get_sequence_state");
   begin
      return Internal (Get_Object (Self), Sequence);
   end Get_Sequence_State;

   -------------------
   -- Get_Sequences --
   -------------------

   function Get_Sequences
      (Self : not null access Gtk_Gesture_Record)
       return Gdk_Event_Sequence_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_gesture_get_sequences");
      Tmp_Return : Gdk_Event_Sequence_List.Glist;
   begin
      Gtk.Gesture.Gdk_Event_Sequence_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end Get_Sequences;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
      (Self : not null access Gtk_Gesture_Record) return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_gesture_get_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Window;

   -----------
   -- Group --
   -----------

   procedure Group
      (Self    : not null access Gtk_Gesture_Record;
       Gesture : not null access Gtk_Gesture_Record'Class)
   is
      procedure Internal (Self : System.Address; Gesture : System.Address);
      pragma Import (C, Internal, "gtk_gesture_group");
   begin
      Internal (Get_Object (Self), Get_Object (Gesture));
   end Group;

   ----------------------
   -- Handles_Sequence --
   ----------------------

   function Handles_Sequence
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Sequence : Gdk.Event.Gdk_Event_Sequence) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_handles_sequence");
   begin
      return Internal (Get_Object (Self), Sequence) /= 0;
   end Handles_Sequence;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
      (Self : not null access Gtk_Gesture_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_is_active");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Active;

   ---------------------
   -- Is_Grouped_With --
   ---------------------

   function Is_Grouped_With
      (Self  : not null access Gtk_Gesture_Record;
       Other : not null access Gtk_Gesture_Record'Class) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_is_grouped_with");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Is_Grouped_With;

   -------------------
   -- Is_Recognized --
   -------------------

   function Is_Recognized
      (Self : not null access Gtk_Gesture_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_is_recognized");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Recognized;

   ------------------------
   -- Set_Sequence_State --
   ------------------------

   function Set_Sequence_State
      (Self     : not null access Gtk_Gesture_Record;
       Sequence : Gdk.Event.Gdk_Event_Sequence;
       State    : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Sequence : Gdk.Event.Gdk_Event_Sequence;
          State    : Gtk.Enums.Gtk_Event_Sequence_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_set_sequence_state");
   begin
      return Internal (Get_Object (Self), Sequence, State) /= 0;
   end Set_Sequence_State;

   ---------------
   -- Set_State --
   ---------------

   function Set_State
      (Self  : not null access Gtk_Gesture_Record;
       State : Gtk.Enums.Gtk_Event_Sequence_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_Event_Sequence_State) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_set_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Set_State;

   ----------------
   -- Set_Window --
   ----------------

   procedure Set_Window
      (Self   : not null access Gtk_Gesture_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Self : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_gesture_set_window");
   begin
      Internal (Get_Object (Self), Window);
   end Set_Window;

   -------------
   -- Ungroup --
   -------------

   procedure Ungroup (Self : not null access Gtk_Gesture_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_gesture_ungroup");
   begin
      Internal (Get_Object (Self));
   end Ungroup;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Gesture_Gdk_Event_Sequence_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Gesture_Gdk_Event_Sequence_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Sequence_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Sequence_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void);

   procedure Connect
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Sequence_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void);

   procedure Marsh_GObject_Gdk_Event_Sequence_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Sequence_Void);

   procedure Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void);

   procedure Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Sequence_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Sequence_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Gesture_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------------------------------
   -- Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void --
   --------------------------------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdk_Event_Sequence (Params, 1), Unchecked_To_Gtk_Event_Sequence_State (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;

   -------------------------------------------
   -- Marsh_GObject_Gdk_Event_Sequence_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Sequence_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Sequence_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdk_Event_Sequence (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Sequence_Void;

   ------------------------------------------------------------------------
   -- Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void --
   ------------------------------------------------------------------------

   procedure Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Gesture := Gtk_Gesture (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdk_Event_Sequence (Params, 1), Unchecked_To_Gtk_Event_Sequence_State (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;

   -----------------------------------------------
   -- Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void --
   -----------------------------------------------

   procedure Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Gesture_Gdk_Event_Sequence_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Gesture := Gtk_Gesture (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdk_Event_Sequence (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Gesture_Gdk_Event_Sequence_Void;

   --------------
   -- On_Begin --
   --------------

   procedure On_Begin
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "begin" & ASCII.NUL, Call, After);
   end On_Begin;

   --------------
   -- On_Begin --
   --------------

   procedure On_Begin
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "begin" & ASCII.NUL, Call, After, Slot);
   end On_Begin;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cancel" & ASCII.NUL, Call, After);
   end On_Cancel;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cancel" & ASCII.NUL, Call, After, Slot);
   end On_Cancel;

   ------------
   -- On_End --
   ------------

   procedure On_End
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "end" & ASCII.NUL, Call, After);
   end On_End;

   ------------
   -- On_End --
   ------------

   procedure On_End
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "end" & ASCII.NUL, Call, After, Slot);
   end On_End;

   -------------------------------
   -- On_Sequence_State_Changed --
   -------------------------------

   procedure On_Sequence_State_Changed
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "sequence-state-changed" & ASCII.NUL, Call, After);
   end On_Sequence_State_Changed;

   -------------------------------
   -- On_Sequence_State_Changed --
   -------------------------------

   procedure On_Sequence_State_Changed
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Gtk_Event_Sequence_State_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "sequence-state-changed" & ASCII.NUL, Call, After, Slot);
   end On_Sequence_State_Changed;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_Gtk_Gesture_Gdk_Event_Sequence_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "update" & ASCII.NUL, Call, After);
   end On_Update;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update
      (Self  : not null access Gtk_Gesture_Record;
       Call  : Cb_GObject_Gdk_Event_Sequence_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "update" & ASCII.NUL, Call, After, Slot);
   end On_Update;

end Gtk.Gesture;
