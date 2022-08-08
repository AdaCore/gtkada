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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;             use Gtkada.Types;
pragma Warnings(On);

package body Gdk.Window is

   function From_Object_Free (B : access Gdk_Geometry) return Gdk_Geometry is
      Result : constant Gdk_Geometry := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function Convert (R : Gdk.Gdk_Window) return System.Address is
   begin
      return Glib.To_Address (Glib.C_Proxy (R));
   end Convert;

   function Convert (R : System.Address) return Gdk.Gdk_Window is
   begin
      return Gdk.Gdk_Window(Glib.C_Proxy'(Glib.To_Proxy (R)));
   end Convert;

   function Get_User_Data (Window : Gdk_Window) return Glib.Object.GObject is
      procedure Internal (Window : Gdk_Window; Widget : System.Address);
      pragma Import (C, Internal, "gdk_window_get_user_data");
      Data : aliased System.Address;
      Stub : GObject_Record;
   begin
      Internal (Window, Data'Address);
      return Get_User_Data (Data, Stub);
   end Get_User_Data;

   procedure Set_User_Data
     (Window : Gdk_Window;
      Widget : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal (Window : Gdk_Window; Widget : System.Address);
      pragma Import (C, Internal, "gdk_window_set_user_data");
   begin
      Internal (Window, Get_Object (Widget));
   end Set_User_Data;

   procedure C_Gdk_Window_Invalidate_Maybe_Recurse
      (Self       : Gdk.Gdk_Window;
       Region     : Cairo.Region.Cairo_Region;
       Child_Func : System.Address;
       User_Data  : System.Address);
   pragma Import (C, C_Gdk_Window_Invalidate_Maybe_Recurse, "gdk_window_invalidate_maybe_recurse");
   --  Adds Region to the update area for Window. The update area is the
   --  region that needs to be redrawn, or "dirty region." The call
   --  Gdk.Window.Process_Updates sends one or more expose events to the
   --  window, which together cover the entire update area. An application
   --  would normally redraw the contents of Window in response to those expose
   --  events.
   --  GDK will call Gdk.Window.Process_All_Updates on your behalf whenever
   --  your program returns to the main loop and becomes idle, so normally
   --  there's no need to do that manually, you just need to invalidate regions
   --  that you know should be redrawn.
   --  The Child_Func parameter controls whether the region of each child
   --  window that intersects Region will also be invalidated. Only children
   --  for which Child_Func returns TRUE will have the area invalidated.
   --  "region": a cairo_region_t
   --  "child_func": function to use to decide if to recurse to a child, null
   --  means never recurse.
   --  "user_data": data passed to Child_Func

   procedure C_Gdk_Window_Set_Invalidate_Handler
      (Self    : Gdk.Gdk_Window;
       Handler : System.Address);
   pragma Import (C, C_Gdk_Window_Set_Invalidate_Handler, "gdk_window_set_invalidate_handler");
   --  Registers an invalidate handler for a specific window. This will get
   --  called whenever a region in the window or its children is invalidated.
   --  This can be used to record the invalidated region, which is useful if
   --  you are keeping an offscreen copy of some region and want to keep it up
   --  to date. You can also modify the invalidated region in case you're doing
   --  some effect where e.g. a child widget appears in multiple places.
   --  Since: gtk+ 3.10
   --  "handler": a Gdk_Window_Invalidate_Handler_Func callback function

   function To_Gdk_Window_Child_Func is new Ada.Unchecked_Conversion
     (System.Address, Gdk_Window_Child_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gdk_Window_Child_Func, System.Address);

   function To_Address is new Ada.Unchecked_Conversion
     (Gdk_Window_Invalidate_Handler_Func, System.Address);

   function Internal_Gdk_Window_Child_Func
      (Window    : Gdk.Gdk_Window;
       User_Data : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gdk_Window_Child_Func);
   --  "window": a Gdk.Gdk_Window
   --  "user_data": user data

   ------------------------------------
   -- Internal_Gdk_Window_Child_Func --
   ------------------------------------

   function Internal_Gdk_Window_Child_Func
      (Window    : Gdk.Gdk_Window;
       User_Data : System.Address) return Glib.Gboolean
   is
      Func : constant Gdk_Window_Child_Func := To_Gdk_Window_Child_Func (User_Data);
   begin
      return Boolean'Pos (Func (Window));
   end Internal_Gdk_Window_Child_Func;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Self            : out Gdk_Window;
       Parent          : Gdk.Gdk_Window;
       Attributes      : Gdk.Gdk_Window_Attr;
       Attributes_Mask : Gdk_Window_Attributes_Type)
   is
      function Internal
         (Parent          : Gdk.Gdk_Window;
          Attributes      : Gdk.Gdk_Window_Attr;
          Attributes_Mask : Gdk_Window_Attributes_Type) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_new");
   begin
      Self := Internal (Parent, Attributes, Attributes_Mask);
   end Gdk_New;

   --------------------
   -- Gdk_Window_New --
   --------------------

   function Gdk_Window_New
      (Parent          : Gdk.Gdk_Window;
       Attributes      : Gdk.Gdk_Window_Attr;
       Attributes_Mask : Gdk_Window_Attributes_Type) return Gdk_Window
   is
      function Internal
         (Parent          : Gdk.Gdk_Window;
          Attributes      : Gdk.Gdk_Window_Attr;
          Attributes_Mask : Gdk_Window_Attributes_Type) return Gdk_Window;
      pragma Import (C, Internal, "gdk_window_new");
      Self : Gdk_Window;
   begin
      Self := Internal (Parent, Attributes, Attributes_Mask);
      return Self;
   end Gdk_Window_New;

   ----------------------
   -- Begin_Draw_Frame --
   ----------------------

   function Begin_Draw_Frame
      (Self   : Gdk.Gdk_Window;
       Region : Cairo.Region.Cairo_Region)
       return Gdk.Drawing_Context.Gdk_Drawing_Context
   is
      function Internal
         (Self   : Gdk.Gdk_Window;
          Region : Cairo.Region.Cairo_Region) return System.Address;
      pragma Import (C, Internal, "gdk_window_begin_draw_frame");
      Stub_Gdk_Drawing_Context : Gdk.Drawing_Context.Gdk_Drawing_Context_Record;
   begin
      return Gdk.Drawing_Context.Gdk_Drawing_Context (Get_User_Data (Internal (Self, Region), Stub_Gdk_Drawing_Context));
   end Begin_Draw_Frame;

   --------------------------------
   -- Begin_Move_Drag_For_Device --
   --------------------------------

   procedure Begin_Move_Drag_For_Device
      (Self      : Gdk.Gdk_Window;
       Device    : not null access Gdk.Device.Gdk_Device_Record'Class;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32)
   is
      procedure Internal
         (Self      : Gdk.Gdk_Window;
          Device    : System.Address;
          Button    : Glib.Gint;
          Root_X    : Glib.Gint;
          Root_Y    : Glib.Gint;
          Timestamp : Guint32);
      pragma Import (C, Internal, "gdk_window_begin_move_drag_for_device");
   begin
      Internal (Self, Get_Object (Device), Button, Root_X, Root_Y, Timestamp);
   end Begin_Move_Drag_For_Device;

   ----------------------------------
   -- Begin_Resize_Drag_For_Device --
   ----------------------------------

   procedure Begin_Resize_Drag_For_Device
      (Self      : Gdk.Gdk_Window;
       Edge      : Gdk_Window_Edge;
       Device    : not null access Gdk.Device.Gdk_Device_Record'Class;
       Button    : Glib.Gint;
       Root_X    : Glib.Gint;
       Root_Y    : Glib.Gint;
       Timestamp : Guint32)
   is
      procedure Internal
         (Self      : Gdk.Gdk_Window;
          Edge      : Gdk_Window_Edge;
          Device    : System.Address;
          Button    : Glib.Gint;
          Root_X    : Glib.Gint;
          Root_Y    : Glib.Gint;
          Timestamp : Guint32);
      pragma Import (C, Internal, "gdk_window_begin_resize_drag_for_device");
   begin
      Internal (Self, Edge, Get_Object (Device), Button, Root_X, Root_Y, Timestamp);
   end Begin_Resize_Drag_For_Device;

   -----------------------
   -- Create_Gl_Context --
   -----------------------

   function Create_Gl_Context
      (Self : Gdk.Gdk_Window) return Gdk.GLContext.Gdk_GLContext
   is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_create_gl_context");
      Stub_Gdk_GLContext : Gdk.GLContext.Gdk_GLContext_Record;
   begin
      return Gdk.GLContext.Gdk_GLContext (Get_User_Data (Internal (Self), Stub_Gdk_GLContext));
   end Create_Gl_Context;

   --------------------
   -- End_Draw_Frame --
   --------------------

   procedure End_Draw_Frame
      (Self    : Gdk.Gdk_Window;
       Context : not null access Gdk.Drawing_Context.Gdk_Drawing_Context_Record'Class)
   is
      procedure Internal (Self : Gdk.Gdk_Window; Context : System.Address);
      pragma Import (C, Internal, "gdk_window_end_draw_frame");
   begin
      Internal (Self, Get_Object (Context));
   end End_Draw_Frame;

   -------------------
   -- Ensure_Native --
   -------------------

   function Ensure_Native (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_ensure_native");
   begin
      return Internal (Self) /= 0;
   end Ensure_Native;

   ----------------------
   -- Get_Accept_Focus --
   ----------------------

   function Get_Accept_Focus (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_accept_focus");
   begin
      return Internal (Self) /= 0;
   end Get_Accept_Focus;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
      (Self : Gdk.Gdk_Window) return Gdk_Window_List.Glist
   is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_children");
      Tmp_Return : Gdk_Window_List.Glist;
   begin
      Gdk.Window.Gdk_Window_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end Get_Children;

   --------------------
   -- Get_Composited --
   --------------------

   function Get_Composited (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_composited");
   begin
      return Internal (Self) /= 0;
   end Get_Composited;

   ---------------------
   -- Get_Decorations --
   ---------------------

   procedure Get_Decorations
      (Self            : Gdk.Gdk_Window;
       Decorations     : out Gdk_WMDecoration;
       Has_Decorations : out Boolean)
   is
      function Internal
         (Self            : Gdk.Gdk_Window;
          Acc_Decorations : access Gdk_WMDecoration) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_decorations");
      Acc_Decorations : aliased Gdk_WMDecoration;
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Acc_Decorations'Access);
      Decorations := Acc_Decorations;
      Has_Decorations := Tmp_Return /= 0;
   end Get_Decorations;

   -----------------------
   -- Get_Device_Cursor --
   -----------------------

   function Get_Device_Cursor
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Gdk_Cursor
   is
      function Internal
         (Self   : Gdk.Gdk_Window;
          Device : System.Address) return Gdk.Gdk_Cursor;
      pragma Import (C, Internal, "gdk_window_get_device_cursor");
   begin
      return Internal (Self, Get_Object (Device));
   end Get_Device_Cursor;

   -----------------------
   -- Get_Device_Events --
   -----------------------

   function Get_Device_Events
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Event.Gdk_Event_Mask
   is
      function Internal
         (Self   : Gdk.Gdk_Window;
          Device : System.Address) return Gdk.Event.Gdk_Event_Mask;
      pragma Import (C, Internal, "gdk_window_get_device_events");
   begin
      return Internal (Self, Get_Object (Device));
   end Get_Device_Events;

   -------------------------
   -- Get_Device_Position --
   -------------------------

   procedure Get_Device_Position
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Mask   : out Gdk.Types.Gdk_Modifier_Type;
       Window : out Gdk.Gdk_Window)
   is
      function Internal
         (Self     : Gdk.Gdk_Window;
          Device   : System.Address;
          Acc_X    : access Glib.Gint;
          Acc_Y    : access Glib.Gint;
          Acc_Mask : access Gdk.Types.Gdk_Modifier_Type)
          return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_window_get_device_position");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Mask   : aliased Gdk.Types.Gdk_Modifier_Type;
      Tmp_Return : Gdk.Gdk_Window;
   begin
      Tmp_Return := Internal (Self, Get_Object (Device), Acc_X'Access, Acc_Y'Access, Acc_Mask'Access);
      X := Acc_X;
      Y := Acc_Y;
      Mask := Acc_Mask;
      Window := Tmp_Return;
   end Get_Device_Position;

   --------------------------------
   -- Get_Device_Position_Double --
   --------------------------------

   function Get_Device_Position_Double
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       X      : access Gdouble;
       Y      : access Gdouble;
       Mask   : access Gdk.Types.Gdk_Modifier_Type) return Gdk.Gdk_Window
   is
      function Internal
         (Self     : Gdk.Gdk_Window;
          Device   : System.Address;
          Acc_X    : access Gdouble;
          Acc_Y    : access Gdouble;
          Acc_Mask : access Gdk.Types.Gdk_Modifier_Type)
          return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_window_get_device_position_double");
      Acc_X      : aliased Gdouble;
      Acc_Y      : aliased Gdouble;
      Acc_Mask   : aliased Gdk.Types.Gdk_Modifier_Type;
      Tmp_Return : Gdk.Gdk_Window;
   begin
      Tmp_Return := Internal (Self, Get_Object (Device), Acc_X'Access, Acc_Y'Access, Acc_Mask'Access);
      if X /= null then
         X.all := Acc_X;
      end if;
      if Y /= null then
         Y.all := Acc_Y;
      end if;
      if Mask /= null then
         Mask.all := Acc_Mask;
      end if;
      return Tmp_Return;
   end Get_Device_Position_Double;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : Gdk.Gdk_Window) return Gdk.Display.Gdk_Display
   is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Self), Stub_Gdk_Display));
   end Get_Display;

   ---------------------------
   -- Get_Event_Compression --
   ---------------------------

   function Get_Event_Compression (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_event_compression");
   begin
      return Internal (Self) /= 0;
   end Get_Event_Compression;

   ----------------------
   -- Get_Focus_On_Map --
   ----------------------

   function Get_Focus_On_Map (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_focus_on_map");
   begin
      return Internal (Self) /= 0;
   end Get_Focus_On_Map;

   ---------------------
   -- Get_Frame_Clock --
   ---------------------

   function Get_Frame_Clock
      (Self : Gdk.Gdk_Window) return Gdk.Frame_Clock.Gdk_Frame_Clock
   is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_frame_clock");
      Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
   begin
      return Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Internal (Self), Stub_Gdk_Frame_Clock));
   end Get_Frame_Clock;

   --------------------
   -- Get_Modal_Hint --
   --------------------

   function Get_Modal_Hint (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_modal_hint");
   begin
      return Internal (Self) /= 0;
   end Get_Modal_Hint;

   ----------------------
   -- Get_Pass_Through --
   ----------------------

   function Get_Pass_Through (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_pass_through");
   begin
      return Internal (Self) /= 0;
   end Get_Pass_Through;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
      (Self   : Gdk.Gdk_Window;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Mask   : out Gdk.Types.Gdk_Modifier_Type;
       Window : out Gdk.Gdk_Window)
   is
      function Internal
         (Self     : Gdk.Gdk_Window;
          Acc_X    : access Glib.Gint;
          Acc_Y    : access Glib.Gint;
          Acc_Mask : access Gdk.Types.Gdk_Modifier_Type)
          return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_window_get_pointer");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Mask   : aliased Gdk.Types.Gdk_Modifier_Type;
      Tmp_Return : Gdk.Gdk_Window;
   begin
      Tmp_Return := Internal (Self, Acc_X'Access, Acc_Y'Access, Acc_Mask'Access);
      X := Acc_X;
      Y := Acc_Y;
      Mask := Acc_Mask;
      Window := Tmp_Return;
   end Get_Pointer;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen (Self : Gdk.Gdk_Window) return Gdk.Screen.Gdk_Screen is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Self), Stub_Gdk_Screen));
   end Get_Screen;

   -----------------------------
   -- Get_Support_Multidevice --
   -----------------------------

   function Get_Support_Multidevice (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_get_support_multidevice");
   begin
      return Internal (Self) /= 0;
   end Get_Support_Multidevice;

   ----------------
   -- Has_Native --
   ----------------

   function Has_Native (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_has_native");
   begin
      return Internal (Self) /= 0;
   end Has_Native;

   ------------------------------
   -- Invalidate_Maybe_Recurse --
   ------------------------------

   procedure Invalidate_Maybe_Recurse
      (Self       : Gdk.Gdk_Window;
       Region     : Cairo.Region.Cairo_Region;
       Child_Func : Gdk_Window_Child_Func)
   is
   begin
      if Child_Func = null then
         C_Gdk_Window_Invalidate_Maybe_Recurse (Self, Region, System.Null_Address, System.Null_Address);
      else
         C_Gdk_Window_Invalidate_Maybe_Recurse (Self, Region, Internal_Gdk_Window_Child_Func'Address, To_Address (Child_Func));
      end if;
   end Invalidate_Maybe_Recurse;

   package body Invalidate_Maybe_Recurse_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gdk_Window_Child_Func is new Ada.Unchecked_Conversion
        (System.Address, Gdk_Window_Child_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gdk_Window_Child_Func, System.Address);

      function Internal_Cb
         (Window    : Gdk.Gdk_Window;
          User_Data : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  A function of this type is passed to
      --  Gdk.Window.Invalidate_Maybe_Recurse. It gets called for each child of
      --  the window to determine whether to recursively invalidate it or now.
      --  "window": a Gdk.Gdk_Window
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Window    : Gdk.Gdk_Window;
          User_Data : System.Address) return Glib.Gboolean
      is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         return Boolean'Pos (To_Gdk_Window_Child_Func (D.Func) (Window, D.Data.all));
      end Internal_Cb;

      ------------------------------
      -- Invalidate_Maybe_Recurse --
      ------------------------------

      procedure Invalidate_Maybe_Recurse
         (Self       : Gdk.Gdk_Window;
          Region     : Cairo.Region.Cairo_Region;
          Child_Func : Gdk_Window_Child_Func;
          User_Data  : User_Data_Type)
      is
         D : System.Address;
      begin
         if Child_Func = null then
            C_Gdk_Window_Invalidate_Maybe_Recurse (Self, Region, System.Null_Address, System.Null_Address);
         else
            D := Users.Build (To_Address (Child_Func), User_Data);
            C_Gdk_Window_Invalidate_Maybe_Recurse (Self, Region, Internal_Cb'Address, D);
            Users.Free_Data (D);
         end if;
      end Invalidate_Maybe_Recurse;

   end Invalidate_Maybe_Recurse_User_Data;

   ---------------------
   -- Invalidate_Rect --
   ---------------------

   procedure Invalidate_Rect
      (Self                : Gdk.Gdk_Window;
       Rect                : Gdk.Rectangle.Gdk_Rectangle;
       Invalidate_Children : Boolean)
   is
      procedure Internal
         (Self                : Gdk.Gdk_Window;
          Rect                : Gdk.Rectangle.Gdk_Rectangle;
          Invalidate_Children : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_invalidate_rect");
   begin
      Internal (Self, Rect, Boolean'Pos (Invalidate_Children));
   end Invalidate_Rect;

   -----------------------
   -- Invalidate_Region --
   -----------------------

   procedure Invalidate_Region
      (Self                : Gdk.Gdk_Window;
       Region              : Cairo.Region.Cairo_Region;
       Invalidate_Children : Boolean)
   is
      procedure Internal
         (Self                : Gdk.Gdk_Window;
          Region              : Cairo.Region.Cairo_Region;
          Invalidate_Children : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_invalidate_region");
   begin
      Internal (Self, Region, Boolean'Pos (Invalidate_Children));
   end Invalidate_Region;

   ------------------
   -- Is_Destroyed --
   ------------------

   function Is_Destroyed (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_is_destroyed");
   begin
      return Internal (Self) /= 0;
   end Is_Destroyed;

   -------------------
   -- Is_Input_Only --
   -------------------

   function Is_Input_Only (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_is_input_only");
   begin
      return Internal (Self) /= 0;
   end Is_Input_Only;

   ---------------
   -- Is_Shaped --
   ---------------

   function Is_Shaped (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_is_shaped");
   begin
      return Internal (Self) /= 0;
   end Is_Shaped;

   -----------------
   -- Is_Viewable --
   -----------------

   function Is_Viewable (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_is_viewable");
   begin
      return Internal (Self) /= 0;
   end Is_Viewable;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Self : Gdk.Gdk_Window) return Boolean is
      function Internal (Self : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_is_visible");
   begin
      return Internal (Self) /= 0;
   end Is_Visible;

   -------------------
   -- Peek_Children --
   -------------------

   function Peek_Children
      (Self : Gdk.Gdk_Window) return Gdk_Window_List.Glist
   is
      function Internal (Self : Gdk.Gdk_Window) return System.Address;
      pragma Import (C, Internal, "gdk_window_peek_children");
      Tmp_Return : Gdk_Window_List.Glist;
   begin
      Gdk.Window.Gdk_Window_List.Set_Object (Tmp_Return, Internal (Self));
      return Tmp_Return;
   end Peek_Children;

   ---------------------
   -- Process_Updates --
   ---------------------

   procedure Process_Updates
      (Self            : Gdk.Gdk_Window;
       Update_Children : Boolean)
   is
      procedure Internal
         (Self            : Gdk.Gdk_Window;
          Update_Children : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_process_updates");
   begin
      Internal (Self, Boolean'Pos (Update_Children));
   end Process_Updates;

   -------------
   -- Restack --
   -------------

   procedure Restack
      (Self    : Gdk.Gdk_Window;
       Sibling : Gdk.Gdk_Window;
       Above   : Boolean)
   is
      procedure Internal
         (Self    : Gdk.Gdk_Window;
          Sibling : Gdk.Gdk_Window;
          Above   : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_restack");
   begin
      Internal (Self, Sibling, Boolean'Pos (Above));
   end Restack;

   ----------------------
   -- Set_Accept_Focus --
   ----------------------

   procedure Set_Accept_Focus
      (Self         : Gdk.Gdk_Window;
       Accept_Focus : Boolean)
   is
      procedure Internal
         (Self         : Gdk.Gdk_Window;
          Accept_Focus : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_accept_focus");
   begin
      Internal (Self, Boolean'Pos (Accept_Focus));
   end Set_Accept_Focus;

   --------------------
   -- Set_Composited --
   --------------------

   procedure Set_Composited (Self : Gdk.Gdk_Window; Composited : Boolean) is
      procedure Internal (Self : Gdk.Gdk_Window; Composited : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_composited");
   begin
      Internal (Self, Boolean'Pos (Composited));
   end Set_Composited;

   -----------------------
   -- Set_Device_Cursor --
   -----------------------

   procedure Set_Device_Cursor
      (Self   : Gdk.Gdk_Window;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Cursor : Gdk.Gdk_Cursor)
   is
      procedure Internal
         (Self   : Gdk.Gdk_Window;
          Device : System.Address;
          Cursor : Gdk.Gdk_Cursor);
      pragma Import (C, Internal, "gdk_window_set_device_cursor");
   begin
      Internal (Self, Get_Object (Device), Cursor);
   end Set_Device_Cursor;

   -----------------------
   -- Set_Device_Events --
   -----------------------

   procedure Set_Device_Events
      (Self       : Gdk.Gdk_Window;
       Device     : not null access Gdk.Device.Gdk_Device_Record'Class;
       Event_Mask : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
         (Self       : Gdk.Gdk_Window;
          Device     : System.Address;
          Event_Mask : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gdk_window_set_device_events");
   begin
      Internal (Self, Get_Object (Device), Event_Mask);
   end Set_Device_Events;

   ---------------------------
   -- Set_Event_Compression --
   ---------------------------

   procedure Set_Event_Compression
      (Self              : Gdk.Gdk_Window;
       Event_Compression : Boolean)
   is
      procedure Internal
         (Self              : Gdk.Gdk_Window;
          Event_Compression : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_event_compression");
   begin
      Internal (Self, Boolean'Pos (Event_Compression));
   end Set_Event_Compression;

   ----------------------
   -- Set_Focus_On_Map --
   ----------------------

   procedure Set_Focus_On_Map
      (Self         : Gdk.Gdk_Window;
       Focus_On_Map : Boolean)
   is
      procedure Internal
         (Self         : Gdk.Gdk_Window;
          Focus_On_Map : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_focus_on_map");
   begin
      Internal (Self, Boolean'Pos (Focus_On_Map));
   end Set_Focus_On_Map;

   -------------------
   -- Set_Icon_Name --
   -------------------

   procedure Set_Icon_Name (Self : Gdk.Gdk_Window; Name : UTF8_String := "") is
      procedure Internal
         (Self : Gdk.Gdk_Window;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_window_set_icon_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Self, Tmp_Name);
      Free (Tmp_Name);
   end Set_Icon_Name;

   ----------------------------
   -- Set_Invalidate_Handler --
   ----------------------------

   procedure Set_Invalidate_Handler
      (Self    : Gdk.Gdk_Window;
       Handler : Gdk_Window_Invalidate_Handler_Func)
   is
   begin
      if Handler = null then
         C_Gdk_Window_Set_Invalidate_Handler (Self, System.Null_Address);
      else
         C_Gdk_Window_Set_Invalidate_Handler (Self, To_Address (Handler));
      end if;
   end Set_Invalidate_Handler;

   --------------------
   -- Set_Keep_Above --
   --------------------

   procedure Set_Keep_Above (Self : Gdk.Gdk_Window; Setting : Boolean) is
      procedure Internal (Self : Gdk.Gdk_Window; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_keep_above");
   begin
      Internal (Self, Boolean'Pos (Setting));
   end Set_Keep_Above;

   --------------------
   -- Set_Keep_Below --
   --------------------

   procedure Set_Keep_Below (Self : Gdk.Gdk_Window; Setting : Boolean) is
      procedure Internal (Self : Gdk.Gdk_Window; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_keep_below");
   begin
      Internal (Self, Boolean'Pos (Setting));
   end Set_Keep_Below;

   --------------------
   -- Set_Modal_Hint --
   --------------------

   procedure Set_Modal_Hint (Self : Gdk.Gdk_Window; Modal : Boolean) is
      procedure Internal (Self : Gdk.Gdk_Window; Modal : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_modal_hint");
   begin
      Internal (Self, Boolean'Pos (Modal));
   end Set_Modal_Hint;

   ---------------------------
   -- Set_Override_Redirect --
   ---------------------------

   procedure Set_Override_Redirect
      (Self              : Gdk.Gdk_Window;
       Override_Redirect : Boolean)
   is
      procedure Internal
         (Self              : Gdk.Gdk_Window;
          Override_Redirect : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_override_redirect");
   begin
      Internal (Self, Boolean'Pos (Override_Redirect));
   end Set_Override_Redirect;

   ----------------------
   -- Set_Pass_Through --
   ----------------------

   procedure Set_Pass_Through
      (Self         : Gdk.Gdk_Window;
       Pass_Through : Boolean)
   is
      procedure Internal
         (Self         : Gdk.Gdk_Window;
          Pass_Through : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_pass_through");
   begin
      Internal (Self, Boolean'Pos (Pass_Through));
   end Set_Pass_Through;

   --------------
   -- Set_Role --
   --------------

   procedure Set_Role (Self : Gdk.Gdk_Window; Role : UTF8_String) is
      procedure Internal
         (Self : Gdk.Gdk_Window;
          Role : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_window_set_role");
      Tmp_Role : Gtkada.Types.Chars_Ptr := New_String (Role);
   begin
      Internal (Self, Tmp_Role);
      Free (Tmp_Role);
   end Set_Role;

   -------------------------
   -- Set_Skip_Pager_Hint --
   -------------------------

   procedure Set_Skip_Pager_Hint
      (Self        : Gdk.Gdk_Window;
       Skips_Pager : Boolean)
   is
      procedure Internal
         (Self        : Gdk.Gdk_Window;
          Skips_Pager : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_skip_pager_hint");
   begin
      Internal (Self, Boolean'Pos (Skips_Pager));
   end Set_Skip_Pager_Hint;

   ---------------------------
   -- Set_Skip_Taskbar_Hint --
   ---------------------------

   procedure Set_Skip_Taskbar_Hint
      (Self          : Gdk.Gdk_Window;
       Skips_Taskbar : Boolean)
   is
      procedure Internal
         (Self          : Gdk.Gdk_Window;
          Skips_Taskbar : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_skip_taskbar_hint");
   begin
      Internal (Self, Boolean'Pos (Skips_Taskbar));
   end Set_Skip_Taskbar_Hint;

   --------------------
   -- Set_Startup_Id --
   --------------------

   procedure Set_Startup_Id
      (Self       : Gdk.Gdk_Window;
       Startup_Id : UTF8_String)
   is
      procedure Internal
         (Self       : Gdk.Gdk_Window;
          Startup_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_window_set_startup_id");
      Tmp_Startup_Id : Gtkada.Types.Chars_Ptr := New_String (Startup_Id);
   begin
      Internal (Self, Tmp_Startup_Id);
      Free (Tmp_Startup_Id);
   end Set_Startup_Id;

   --------------------------
   -- Set_Static_Gravities --
   --------------------------

   function Set_Static_Gravities
      (Self       : Gdk.Gdk_Window;
       Use_Static : Boolean) return Boolean
   is
      function Internal
         (Self       : Gdk.Gdk_Window;
          Use_Static : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_set_static_gravities");
   begin
      return Internal (Self, Boolean'Pos (Use_Static)) /= 0;
   end Set_Static_Gravities;

   -----------------------------
   -- Set_Support_Multidevice --
   -----------------------------

   procedure Set_Support_Multidevice
      (Self                : Gdk.Gdk_Window;
       Support_Multidevice : Boolean)
   is
      procedure Internal
         (Self                : Gdk.Gdk_Window;
          Support_Multidevice : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_support_multidevice");
   begin
      Internal (Self, Boolean'Pos (Support_Multidevice));
   end Set_Support_Multidevice;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Self : Gdk.Gdk_Window; Title : UTF8_String) is
      procedure Internal
         (Self  : Gdk.Gdk_Window;
          Title : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gdk_window_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
   begin
      Internal (Self, Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ----------------------
   -- Set_Urgency_Hint --
   ----------------------

   procedure Set_Urgency_Hint (Self : Gdk.Gdk_Window; Urgent : Boolean) is
      procedure Internal (Self : Gdk.Gdk_Window; Urgent : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_urgency_hint");
   begin
      Internal (Self, Boolean'Pos (Urgent));
   end Set_Urgency_Hint;

   ----------------------
   -- Show_Window_Menu --
   ----------------------

   function Show_Window_Menu
      (Self  : Gdk.Gdk_Window;
       Event : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Self  : Gdk.Gdk_Window;
          Event : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_window_show_window_menu");
   begin
      return Internal (Self, Event) /= 0;
   end Show_Window_Menu;

   ----------------
   -- At_Pointer --
   ----------------

   procedure At_Pointer
      (Win_X  : out Glib.Gint;
       Win_Y  : out Glib.Gint;
       Window : out Gdk.Gdk_Window)
   is
      function Internal
         (Acc_Win_X : access Glib.Gint;
          Acc_Win_Y : access Glib.Gint) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_window_at_pointer");
      Acc_Win_X  : aliased Glib.Gint;
      Acc_Win_Y  : aliased Glib.Gint;
      Tmp_Return : Gdk.Gdk_Window;
   begin
      Tmp_Return := Internal (Acc_Win_X'Access, Acc_Win_Y'Access);
      Win_X := Acc_Win_X;
      Win_Y := Acc_Win_Y;
      Window := Tmp_Return;
   end At_Pointer;

   -----------------------
   -- Set_Debug_Updates --
   -----------------------

   procedure Set_Debug_Updates (Setting : Boolean) is
      procedure Internal (Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gdk_window_set_debug_updates");
   begin
      Internal (Boolean'Pos (Setting));
   end Set_Debug_Updates;

end Gdk.Window;
