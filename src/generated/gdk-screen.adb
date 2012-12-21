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
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Gdk.Screen is

   function Get_Screen
     (Display    : access Gdk_Display_Record'Class;
      Screen_Num : Gint)
   return Gdk_Screen
   is
      function Internal
        (Display    : System.Address;
         Screen_Num : Gint)
      return System.Address;
      pragma Import (C, Internal, "gdk_display_get_screen");
      --  External binding: gdk_display_get_screen
      Stub : Gdk_Screen_Record;
   begin
      return Gdk_Screen
        (Get_User_Data
           (Internal (Get_Object (Display), Screen_Num), Stub));
   end Get_Screen;

   function Get_Default_Screen
     (Display : access Gdk_Display_Record'Class)
   return Gdk_Screen
   is
      function Internal
        (Display : System.Address)
      return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default_screen");
      --  External binding: gdk_display_get_default_screen
      Stub : Gdk_Screen_Record;
   begin
      return Gdk_Screen
        (Get_User_Data
           (Internal (Get_Object (Display)), Stub));
   end Get_Default_Screen;

   procedure Get_Pointer
     (Display : access Gdk_Display_Record'Class;
      Screen  : out Gdk_Screen;
      X       : out Gint;
      Y       : out Gint;
      Mask    : out Gdk_Modifier_Type)
   is
      procedure Internal
        (Display : System.Address;
         Screen  : out System.Address;
         X       : out Gint;
         Y       : out Gint;
         Mask    : out Gdk_Modifier_Type);
      pragma Import (C, Internal, "gdk_display_get_pointer");
      --  External binding: gdk_display_get_pointer

      S    : System.Address;
      Stub : Gdk_Screen_Record;

   begin
      Internal (Get_Object (Display), S, X, Y, Mask);
      Screen := Gdk_Screen (Get_User_Data (S, Stub));
   end Get_Pointer;

   procedure Warp_Pointer
     (Display : access Gdk.Display.Gdk_Display_Record'Class;
      Screen  : access Gdk_Screen_Record;
      X       : Glib.Gint;
      Y       : Glib.Gint)
   is
      procedure Internal (D, S : System.Address; X, Y : Gint);
      pragma Import (C, Internal, "gdk_display_warp_pointer");
      --  External binding: gdk_display_warp_pointer
   begin
      Internal (Get_Object (Display), Get_Object (Screen), X, Y);
   end Warp_Pointer;

   package Type_Conversion_Gdk_Screen is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Screen_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Screen);

   -----------------------
   -- Get_Active_Window --
   -----------------------

   function Get_Active_Window
      (Screen : not null access Gdk_Screen_Record) return Gdk.Gdk_Window
   is
      function Internal (Screen : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_screen_get_active_window");
   begin
      return Internal (Get_Object (Screen));
   end Get_Active_Window;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_screen_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Screen)), Stub_Gdk_Display));
   end Get_Display;

   ----------------------
   -- Get_Font_Options --
   ----------------------

   function Get_Font_Options
      (Screen : not null access Gdk_Screen_Record)
       return Cairo.Cairo_Font_Options
   is
      function Internal
         (Screen : System.Address) return Cairo.Cairo_Font_Options;
      pragma Import (C, Internal, "gdk_screen_get_font_options");
   begin
      return Internal (Get_Object (Screen));
   end Get_Font_Options;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_height");
   begin
      return Internal (Get_Object (Screen));
   end Get_Height;

   -------------------
   -- Get_Height_Mm --
   -------------------

   function Get_Height_Mm
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_height_mm");
   begin
      return Internal (Get_Object (Screen));
   end Get_Height_Mm;

   --------------------------
   -- Get_Monitor_At_Point --
   --------------------------

   function Get_Monitor_At_Point
      (Screen : not null access Gdk_Screen_Record;
       X      : Gint;
       Y      : Gint) return Gint
   is
      function Internal
         (Screen : System.Address;
          X      : Gint;
          Y      : Gint) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_at_point");
   begin
      return Internal (Get_Object (Screen), X, Y);
   end Get_Monitor_At_Point;

   ---------------------------
   -- Get_Monitor_At_Window --
   ---------------------------

   function Get_Monitor_At_Window
      (Screen : not null access Gdk_Screen_Record;
       Window : Gdk.Gdk_Window) return Gint
   is
      function Internal
         (Screen : System.Address;
          Window : Gdk.Gdk_Window) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_at_window");
   begin
      return Internal (Get_Object (Screen), Window);
   end Get_Monitor_At_Window;

   --------------------------
   -- Get_Monitor_Geometry --
   --------------------------

   procedure Get_Monitor_Geometry
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Gint;
       Dest        : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Screen      : System.Address;
          Monitor_Num : Gint;
          Dest        : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_screen_get_monitor_geometry");
   begin
      Internal (Get_Object (Screen), Monitor_Num, Dest);
   end Get_Monitor_Geometry;

   ---------------------------
   -- Get_Monitor_Height_Mm --
   ---------------------------

   function Get_Monitor_Height_Mm
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Gint) return Gint
   is
      function Internal
         (Screen      : System.Address;
          Monitor_Num : Gint) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_height_mm");
   begin
      return Internal (Get_Object (Screen), Monitor_Num);
   end Get_Monitor_Height_Mm;

   ---------------------------
   -- Get_Monitor_Plug_Name --
   ---------------------------

   function Get_Monitor_Plug_Name
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Gint) return UTF8_String
   is
      function Internal
         (Screen      : System.Address;
          Monitor_Num : Gint) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_screen_get_monitor_plug_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Screen), Monitor_Num));
   end Get_Monitor_Plug_Name;

   --------------------------
   -- Get_Monitor_Width_Mm --
   --------------------------

   function Get_Monitor_Width_Mm
      (Screen      : not null access Gdk_Screen_Record;
       Monitor_Num : Gint) return Gint
   is
      function Internal
         (Screen      : System.Address;
          Monitor_Num : Gint) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_monitor_width_mm");
   begin
      return Internal (Get_Object (Screen), Monitor_Num);
   end Get_Monitor_Width_Mm;

   --------------------
   -- Get_N_Monitors --
   --------------------

   function Get_N_Monitors
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_n_monitors");
   begin
      return Internal (Get_Object (Screen));
   end Get_N_Monitors;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_number");
   begin
      return Internal (Get_Object (Screen));
   end Get_Number;

   -------------------------
   -- Get_Primary_Monitor --
   -------------------------

   function Get_Primary_Monitor
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_primary_monitor");
   begin
      return Internal (Get_Object (Screen));
   end Get_Primary_Monitor;

   --------------------
   -- Get_Resolution --
   --------------------

   function Get_Resolution
      (Screen : not null access Gdk_Screen_Record) return Gdouble
   is
      function Internal (Screen : System.Address) return Gdouble;
      pragma Import (C, Internal, "gdk_screen_get_resolution");
   begin
      return Internal (Get_Object (Screen));
   end Get_Resolution;

   ---------------------
   -- Get_Rgba_Visual --
   ---------------------

   function Get_Rgba_Visual
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Visual.Gdk_Visual
   is
      function Internal
         (Screen : System.Address) return Gdk.Visual.Gdk_Visual;
      pragma Import (C, Internal, "gdk_screen_get_rgba_visual");
   begin
      return Internal (Get_Object (Screen));
   end Get_Rgba_Visual;

   ---------------------
   -- Get_Root_Window --
   ---------------------

   function Get_Root_Window
      (Screen : not null access Gdk_Screen_Record) return Gdk.Gdk_Window
   is
      function Internal (Screen : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_screen_get_root_window");
   begin
      return Internal (Get_Object (Screen));
   end Get_Root_Window;

   -----------------------
   -- Get_System_Visual --
   -----------------------

   function Get_System_Visual
      (Screen : not null access Gdk_Screen_Record)
       return Gdk.Visual.Gdk_Visual
   is
      function Internal
         (Screen : System.Address) return Gdk.Visual.Gdk_Visual;
      pragma Import (C, Internal, "gdk_screen_get_system_visual");
   begin
      return Internal (Get_Object (Screen));
   end Get_System_Visual;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_width");
   begin
      return Internal (Get_Object (Screen));
   end Get_Width;

   ------------------
   -- Get_Width_Mm --
   ------------------

   function Get_Width_Mm
      (Screen : not null access Gdk_Screen_Record) return Gint
   is
      function Internal (Screen : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_screen_get_width_mm");
   begin
      return Internal (Get_Object (Screen));
   end Get_Width_Mm;

   -------------------
   -- Is_Composited --
   -------------------

   function Is_Composited
      (Screen : not null access Gdk_Screen_Record) return Boolean
   is
      function Internal (Screen : System.Address) return Integer;
      pragma Import (C, Internal, "gdk_screen_is_composited");
   begin
      return Boolean'Val (Internal (Get_Object (Screen)));
   end Is_Composited;

   -----------------------
   -- Make_Display_Name --
   -----------------------

   function Make_Display_Name
      (Screen : not null access Gdk_Screen_Record) return UTF8_String
   is
      function Internal
         (Screen : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_screen_make_display_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Screen)));
   end Make_Display_Name;

   ----------------------
   -- Set_Font_Options --
   ----------------------

   procedure Set_Font_Options
      (Screen  : not null access Gdk_Screen_Record;
       Options : in out Cairo.Cairo_Font_Options)
   is
      procedure Internal
         (Screen  : System.Address;
          Options : in out Cairo.Cairo_Font_Options);
      pragma Import (C, Internal, "gdk_screen_set_font_options");
   begin
      Internal (Get_Object (Screen), Options);
   end Set_Font_Options;

   --------------------
   -- Set_Resolution --
   --------------------

   procedure Set_Resolution
      (Screen : not null access Gdk_Screen_Record;
       Dpi    : Gdouble)
   is
      procedure Internal (Screen : System.Address; Dpi : Gdouble);
      pragma Import (C, Internal, "gdk_screen_set_resolution");
   begin
      Internal (Get_Object (Screen), Dpi);
   end Set_Resolution;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gdk_Screen is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_screen_get_default");
      Stub_Gdk_Screen : Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal, Stub_Gdk_Screen));
   end Get_Default;

   ------------
   -- Height --
   ------------

   function Height return Gint is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_screen_height");
   begin
      return Internal;
   end Height;

   ---------------
   -- Height_Mm --
   ---------------

   function Height_Mm return Gint is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_screen_height_mm");
   begin
      return Internal;
   end Height_Mm;

   -----------
   -- Width --
   -----------

   function Width return Gint is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_screen_width");
   begin
      return Internal;
   end Width;

   --------------
   -- Width_Mm --
   --------------

   function Width_Mm return Gint is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_screen_width_mm");
   begin
      return Internal;
   end Width_Mm;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Screen_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Screen_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Screen_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Screen_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Screen_Record'Class;
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

   procedure Marsh_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Screen_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Screen_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Screen_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Screen_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Screen_Record'Class;
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
         Func_Data   => Get_Object (Slot),
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
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant access Glib.Object.GObject_Record'Class := Glib.Object.Convert (User_Data);
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------
   -- Marsh_Gdk_Screen_Void --
   ---------------------------

   procedure Marsh_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Screen_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant access Gdk_Screen_Record'Class := Gdk_Screen (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Screen_Void;

   ---------------------------
   -- On_Composited_Changed --
   ---------------------------

   procedure On_Composited_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_Gdk_Screen_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "composited-changed" & ASCII.NUL, Call, After);
   end On_Composited_Changed;

   ---------------------------
   -- On_Composited_Changed --
   ---------------------------

   procedure On_Composited_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "composited-changed" & ASCII.NUL, Call, After, Slot);
   end On_Composited_Changed;

   -------------------------
   -- On_Monitors_Changed --
   -------------------------

   procedure On_Monitors_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_Gdk_Screen_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "monitors-changed" & ASCII.NUL, Call, After);
   end On_Monitors_Changed;

   -------------------------
   -- On_Monitors_Changed --
   -------------------------

   procedure On_Monitors_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "monitors-changed" & ASCII.NUL, Call, After, Slot);
   end On_Monitors_Changed;

   ---------------------
   -- On_Size_Changed --
   ---------------------

   procedure On_Size_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_Gdk_Screen_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "size-changed" & ASCII.NUL, Call, After);
   end On_Size_Changed;

   ---------------------
   -- On_Size_Changed --
   ---------------------

   procedure On_Size_Changed
      (Self  : not null access Gdk_Screen_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "size-changed" & ASCII.NUL, Call, After, Slot);
   end On_Size_Changed;

end Gdk.Screen;
