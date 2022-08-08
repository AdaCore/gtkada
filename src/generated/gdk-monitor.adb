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
with Gtkada.Types;               use Gtkada.Types;

package body Gdk.Monitor is

   package Type_Conversion_Gdk_Monitor is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Monitor_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Monitor);

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Monitor_Record) return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_monitor_get_display");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Display;

   ------------------
   -- Get_Geometry --
   ------------------

   procedure Get_Geometry
      (Self     : not null access Gdk_Monitor_Record;
       Geometry : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self     : System.Address;
          Geometry : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_monitor_get_geometry");
   begin
      Internal (Get_Object (Self), Geometry);
   end Get_Geometry;

   -------------------
   -- Get_Height_Mm --
   -------------------

   function Get_Height_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_monitor_get_height_mm");
   begin
      return Internal (Get_Object (Self));
   end Get_Height_Mm;

   ----------------------
   -- Get_Manufacturer --
   ----------------------

   function Get_Manufacturer
      (Self : not null access Gdk_Monitor_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_monitor_get_manufacturer");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Manufacturer;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
      (Self : not null access Gdk_Monitor_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_monitor_get_model");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Model;

   ----------------------
   -- Get_Refresh_Rate --
   ----------------------

   function Get_Refresh_Rate
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_monitor_get_refresh_rate");
   begin
      return Internal (Get_Object (Self));
   end Get_Refresh_Rate;

   ----------------------
   -- Get_Scale_Factor --
   ----------------------

   function Get_Scale_Factor
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_monitor_get_scale_factor");
   begin
      return Internal (Get_Object (Self));
   end Get_Scale_Factor;

   -------------------------
   -- Get_Subpixel_Layout --
   -------------------------

   function Get_Subpixel_Layout
      (Self : not null access Gdk_Monitor_Record) return Gdk_Subpixel_Layout
   is
      function Internal (Self : System.Address) return Gdk_Subpixel_Layout;
      pragma Import (C, Internal, "gdk_monitor_get_subpixel_layout");
   begin
      return Internal (Get_Object (Self));
   end Get_Subpixel_Layout;

   ------------------
   -- Get_Width_Mm --
   ------------------

   function Get_Width_Mm
      (Self : not null access Gdk_Monitor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_monitor_get_width_mm");
   begin
      return Internal (Get_Object (Self));
   end Get_Width_Mm;

   ------------------
   -- Get_Workarea --
   ------------------

   procedure Get_Workarea
      (Self     : not null access Gdk_Monitor_Record;
       Workarea : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self     : System.Address;
          Workarea : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gdk_monitor_get_workarea");
   begin
      Internal (Get_Object (Self), Workarea);
   end Get_Workarea;

   ----------------
   -- Is_Primary --
   ----------------

   function Is_Primary
      (Self : not null access Gdk_Monitor_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_monitor_is_primary");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Primary;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gdk_Monitor_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gdk_Monitor_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gdk_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Monitor_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gdk_Monitor_Record'Class;
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

   procedure Marsh_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gdk_Monitor_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gdk_Monitor_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gdk_Monitor_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gdk_Monitor_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gdk_Monitor_Record'Class;
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
   -- Marsh_Gdk_Monitor_Void --
   ----------------------------

   procedure Marsh_Gdk_Monitor_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gdk_Monitor_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gdk_Monitor := Gdk_Monitor (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gdk_Monitor_Void;

   -------------------
   -- On_Invalidate --
   -------------------

   procedure On_Invalidate
      (Self  : not null access Gdk_Monitor_Record;
       Call  : Cb_Gdk_Monitor_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "invalidate" & ASCII.NUL, Call, After);
   end On_Invalidate;

   -------------------
   -- On_Invalidate --
   -------------------

   procedure On_Invalidate
      (Self  : not null access Gdk_Monitor_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "invalidate" & ASCII.NUL, Call, After, Slot);
   end On_Invalidate;

end Gdk.Monitor;
