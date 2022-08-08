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

package body Gtk.Cell_Renderer is

   function Convert (R : Gtk.Cell_Renderer.Gtk_Cell_Renderer) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Cell_Renderer.Gtk_Cell_Renderer is
      Stub : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;begin
         return Gtk.Cell_Renderer.Gtk_Cell_Renderer (Glib.Object.Get_User_Data (R, Stub));
      end Convert;

   package Type_Conversion_Gtk_Cell_Renderer is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Renderer_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Renderer);

   --------------
   -- Activate --
   --------------

   function Activate
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Event           : Gdk.Event.Gdk_Event;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Path            : UTF8_String;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State) return Boolean
   is
      function Internal
         (Cell            : System.Address;
          Event           : Gdk.Event.Gdk_Event;
          Widget          : System.Address;
          Path            : Gtkada.Types.Chars_Ptr;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Flags           : Gtk_Cell_Renderer_State) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_activate");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Cell), Event, Get_Object (Widget), Tmp_Path, Background_Area, Cell_Area, Flags);
      Free (Tmp_Path);
      return Tmp_Return /= 0;
   end Activate;

   ----------------------
   -- Get_Aligned_Area --
   ----------------------

   procedure Get_Aligned_Area
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Flags        : Gtk_Cell_Renderer_State;
       Cell_Area    : Gdk.Rectangle.Gdk_Rectangle;
       Aligned_Area : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Cell         : System.Address;
          Widget       : System.Address;
          Flags        : Gtk_Cell_Renderer_State;
          Cell_Area    : Gdk.Rectangle.Gdk_Rectangle;
          Aligned_Area : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_cell_renderer_get_aligned_area");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Flags, Cell_Area, Aligned_Area);
   end Get_Aligned_Area;

   -------------------
   -- Get_Alignment --
   -------------------

   procedure Get_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat)
   is
      procedure Internal
         (Cell   : System.Address;
          Xalign : out Gfloat;
          Yalign : out Gfloat);
      pragma Import (C, Internal, "gtk_cell_renderer_get_alignment");
   begin
      Internal (Get_Object (Cell), Xalign, Yalign);
   end Get_Alignment;

   --------------------
   -- Get_Fixed_Size --
   --------------------

   procedure Get_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Cell   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_fixed_size");
   begin
      Internal (Get_Object (Cell), Width, Height);
   end Get_Fixed_Size;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : out Glib.Gint;
       Ypad : out Glib.Gint)
   is
      procedure Internal
         (Cell : System.Address;
          Xpad : out Glib.Gint;
          Ypad : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_padding");
   begin
      Internal (Get_Object (Cell), Xpad, Ypad);
   end Get_Padding;

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint)
   is
      procedure Internal
         (Cell         : System.Address;
          Widget       : System.Address;
          Minimum_Size : out Glib.Gint;
          Natural_Size : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_preferred_height");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Minimum_Size, Natural_Size);
   end Get_Preferred_Height;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Cell           : not null access Gtk_Cell_Renderer_Record;
       Widget         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Cell           : System.Address;
          Widget         : System.Address;
          Width          : Glib.Gint;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_preferred_height_for_width");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Width, Minimum_Height, Natural_Height);
   end Get_Preferred_Height_For_Width;

   ------------------------
   -- Get_Preferred_Size --
   ------------------------

   procedure Get_Preferred_Size
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Gtk.Widget.Gtk_Requisition;
       Natural_Size : out Gtk.Widget.Gtk_Requisition)
   is
      procedure Internal
         (Cell         : System.Address;
          Widget       : System.Address;
          Minimum_Size : out Gtk.Widget.Gtk_Requisition;
          Natural_Size : out Gtk.Widget.Gtk_Requisition);
      pragma Import (C, Internal, "gtk_cell_renderer_get_preferred_size");
      Tmp_Minimum_Size : aliased Gtk.Widget.Gtk_Requisition;
      Tmp_Natural_Size : aliased Gtk.Widget.Gtk_Requisition;
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Tmp_Minimum_Size, Tmp_Natural_Size);
      Natural_Size := Tmp_Natural_Size;
      Minimum_Size := Tmp_Minimum_Size;
   end Get_Preferred_Size;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Cell         : not null access Gtk_Cell_Renderer_Record;
       Widget       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Minimum_Size : out Glib.Gint;
       Natural_Size : out Glib.Gint)
   is
      procedure Internal
         (Cell         : System.Address;
          Widget       : System.Address;
          Minimum_Size : out Glib.Gint;
          Natural_Size : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_preferred_width");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Minimum_Size, Natural_Size);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Cell          : not null access Gtk_Cell_Renderer_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Cell          : System.Address;
          Widget        : System.Address;
          Height        : Glib.Gint;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_preferred_width_for_height");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Height, Minimum_Width, Natural_Width);
   end Get_Preferred_Width_For_Height;

   ----------------------
   -- Get_Request_Mode --
   ----------------------

   function Get_Request_Mode
      (Cell : not null access Gtk_Cell_Renderer_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode
   is
      function Internal
         (Cell : System.Address) return Gtk.Enums.Gtk_Size_Request_Mode;
      pragma Import (C, Internal, "gtk_cell_renderer_get_request_mode");
   begin
      return Internal (Get_Object (Cell));
   end Get_Request_Mode;

   -------------------
   -- Get_Sensitive --
   -------------------

   function Get_Sensitive
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean
   is
      function Internal (Cell : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_get_sensitive");
   begin
      return Internal (Get_Object (Cell)) /= 0;
   end Get_Sensitive;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
      (Cell      : not null access Gtk_Cell_Renderer_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_Area : in out Gdk.Rectangle.Gdk_Rectangle;
       X_Offset  : out Glib.Gint;
       Y_Offset  : out Glib.Gint;
       Width     : out Glib.Gint;
       Height    : out Glib.Gint)
   is
      procedure Internal
         (Cell      : System.Address;
          Widget    : System.Address;
          Cell_Area : in out Gdk.Rectangle.Gdk_Rectangle;
          X_Offset  : out Glib.Gint;
          Y_Offset  : out Glib.Gint;
          Width     : out Glib.Gint;
          Height    : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_get_size");
   begin
      Internal (Get_Object (Cell), Get_Object (Widget), Cell_Area, X_Offset, Y_Offset, Width, Height);
   end Get_Size;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Cell       : not null access Gtk_Cell_Renderer_Record;
       Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
       Cell_State : Gtk_Cell_Renderer_State)
       return Gtk.Enums.Gtk_State_Flags
   is
      function Internal
         (Cell       : System.Address;
          Widget     : System.Address;
          Cell_State : Gtk_Cell_Renderer_State)
          return Gtk.Enums.Gtk_State_Flags;
      pragma Import (C, Internal, "gtk_cell_renderer_get_state");
   begin
      return Internal (Get_Object (Cell), Get_Object_Or_Null (GObject (Widget)), Cell_State);
   end Get_State;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean
   is
      function Internal (Cell : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_get_visible");
   begin
      return Internal (Get_Object (Cell)) /= 0;
   end Get_Visible;

   --------------------
   -- Is_Activatable --
   --------------------

   function Is_Activatable
      (Cell : not null access Gtk_Cell_Renderer_Record) return Boolean
   is
      function Internal (Cell : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cell_renderer_is_activatable");
   begin
      return Internal (Get_Object (Cell)) /= 0;
   end Is_Activatable;

   ------------
   -- Render --
   ------------

   procedure Render
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Cr              : Cairo.Cairo_Context;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State)
   is
      procedure Internal
         (Cell            : System.Address;
          Cr              : Cairo.Cairo_Context;
          Widget          : System.Address;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Flags           : Gtk_Cell_Renderer_State);
      pragma Import (C, Internal, "gtk_cell_renderer_render");
   begin
      Internal (Get_Object (Cell), Cr, Get_Object (Widget), Background_Area, Cell_Area, Flags);
   end Render;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Xalign : Gfloat;
       Yalign : Gfloat)
   is
      procedure Internal
         (Cell   : System.Address;
          Xalign : Gfloat;
          Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_cell_renderer_set_alignment");
   begin
      Internal (Get_Object (Cell), Xalign, Yalign);
   end Set_Alignment;

   --------------------
   -- Set_Fixed_Size --
   --------------------

   procedure Set_Fixed_Size
      (Cell   : not null access Gtk_Cell_Renderer_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Cell   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_set_fixed_size");
   begin
      Internal (Get_Object (Cell), Width, Height);
   end Set_Fixed_Size;

   -----------------
   -- Set_Padding --
   -----------------

   procedure Set_Padding
      (Cell : not null access Gtk_Cell_Renderer_Record;
       Xpad : Glib.Gint;
       Ypad : Glib.Gint)
   is
      procedure Internal
         (Cell : System.Address;
          Xpad : Glib.Gint;
          Ypad : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_renderer_set_padding");
   begin
      Internal (Get_Object (Cell), Xpad, Ypad);
   end Set_Padding;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
      (Cell      : not null access Gtk_Cell_Renderer_Record;
       Sensitive : Boolean)
   is
      procedure Internal (Cell : System.Address; Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_set_sensitive");
   begin
      Internal (Get_Object (Cell), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Cell    : not null access Gtk_Cell_Renderer_Record;
       Visible : Boolean)
   is
      procedure Internal (Cell : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_set_visible");
   begin
      Internal (Get_Object (Cell), Boolean'Pos (Visible));
   end Set_Visible;

   -------------------
   -- Start_Editing --
   -------------------

   function Start_Editing
      (Cell            : not null access Gtk_Cell_Renderer_Record;
       Event           : Gdk.Event.Gdk_Event;
       Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Path            : UTF8_String;
       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
       Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
       Flags           : Gtk_Cell_Renderer_State)
       return Gtk.Cell_Editable.Gtk_Cell_Editable
   is
      function Internal
         (Cell            : System.Address;
          Event           : Gdk.Event.Gdk_Event;
          Widget          : System.Address;
          Path            : Gtkada.Types.Chars_Ptr;
          Background_Area : Gdk.Rectangle.Gdk_Rectangle;
          Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
          Flags           : Gtk_Cell_Renderer_State)
          return Gtk.Cell_Editable.Gtk_Cell_Editable;
      pragma Import (C, Internal, "gtk_cell_renderer_start_editing");
      Tmp_Path   : Gtkada.Types.Chars_Ptr := New_String (Path);
      Tmp_Return : Gtk.Cell_Editable.Gtk_Cell_Editable;
   begin
      Tmp_Return := Internal (Get_Object (Cell), Event, Get_Object (Widget), Tmp_Path, Background_Area, Cell_Area, Flags);
      Free (Tmp_Path);
      return Tmp_Return;
   end Start_Editing;

   ------------------
   -- Stop_Editing --
   ------------------

   procedure Stop_Editing
      (Cell     : not null access Gtk_Cell_Renderer_Record;
       Canceled : Boolean)
   is
      procedure Internal (Cell : System.Address; Canceled : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_stop_editing");
   begin
      Internal (Get_Object (Cell), Boolean'Pos (Canceled));
   end Stop_Editing;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Renderer_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Renderer_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void);

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void);

   procedure Marsh_Gtk_Cell_Renderer_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Cell_Renderer_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Renderer_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Cell_Renderer_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------------------------------------
   -- Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void --
   ------------------------------------------------------

   procedure Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 1)), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Cell_Editable_UTF8_String_Void;

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

   ----------------------------------------------------------------
   -- Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void --
   ----------------------------------------------------------------

   procedure Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Renderer := Gtk_Cell_Renderer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Cell_Editable.Gtk_Cell_Editable (Unchecked_To_Interface (Params, 1)), Unchecked_To_UTF8_String (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void;

   ----------------------------------
   -- Marsh_Gtk_Cell_Renderer_Void --
   ----------------------------------

   procedure Marsh_Gtk_Cell_Renderer_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Cell_Renderer_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Cell_Renderer := Gtk_Cell_Renderer (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Cell_Renderer_Void;

   -------------------------
   -- On_Editing_Canceled --
   -------------------------

   procedure On_Editing_Canceled
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_Gtk_Cell_Renderer_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "editing-canceled" & ASCII.NUL, Call, After);
   end On_Editing_Canceled;

   -------------------------
   -- On_Editing_Canceled --
   -------------------------

   procedure On_Editing_Canceled
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "editing-canceled" & ASCII.NUL, Call, After, Slot);
   end On_Editing_Canceled;

   ------------------------
   -- On_Editing_Started --
   ------------------------

   procedure On_Editing_Started
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_Gtk_Cell_Renderer_Gtk_Cell_Editable_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "editing-started" & ASCII.NUL, Call, After);
   end On_Editing_Started;

   ------------------------
   -- On_Editing_Started --
   ------------------------

   procedure On_Editing_Started
      (Self  : not null access Gtk_Cell_Renderer_Record;
       Call  : Cb_GObject_Gtk_Cell_Editable_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "editing-started" & ASCII.NUL, Call, After, Slot);
   end On_Editing_Started;

end Gtk.Cell_Renderer;
