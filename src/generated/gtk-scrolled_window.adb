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

package body Gtk.Scrolled_Window is

   package Type_Conversion_Gtk_Scrolled_Window is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scrolled_Window_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Scrolled_Window);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Scrolled_Window : out Gtk_Scrolled_Window;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null)
   is
   begin
      Scrolled_Window := new Gtk_Scrolled_Window_Record;
      Gtk.Scrolled_Window.Initialize (Scrolled_Window, Hadjustment, Vadjustment);
   end Gtk_New;

   -----------------------------
   -- Gtk_Scrolled_Window_New --
   -----------------------------

   function Gtk_Scrolled_Window_New
      (Hadjustment : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment : Gtk.Adjustment.Gtk_Adjustment := null)
       return Gtk_Scrolled_Window
   is
      Scrolled_Window : constant Gtk_Scrolled_Window := new Gtk_Scrolled_Window_Record;
   begin
      Gtk.Scrolled_Window.Initialize (Scrolled_Window, Hadjustment, Vadjustment);
      return Scrolled_Window;
   end Gtk_Scrolled_Window_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record'Class;
       Hadjustment     : Gtk.Adjustment.Gtk_Adjustment := null;
       Vadjustment     : Gtk.Adjustment.Gtk_Adjustment := null)
   is
      function Internal
         (Hadjustment : System.Address;
          Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");
   begin
      if not Scrolled_Window.Is_Created then
         Set_Object (Scrolled_Window, Internal (Get_Object_Or_Null (GObject (Hadjustment)), Get_Object_Or_Null (GObject (Vadjustment))));
      end if;
   end Initialize;

   -----------------------
   -- Add_With_Viewport --
   -----------------------

   procedure Add_With_Viewport
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Child           : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Child           : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_add_with_viewport");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Child));
   end Add_With_Viewport;

   ------------------------------
   -- Get_Capture_Button_Press --
   ------------------------------

   function Get_Capture_Button_Press
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean
   is
      function Internal
         (Scrolled_Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrolled_window_get_capture_button_press");
   begin
      return Internal (Get_Object (Scrolled_Window)) /= 0;
   end Get_Capture_Button_Press;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   --------------------
   -- Get_Hscrollbar --
   --------------------

   function Get_Hscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hscrollbar");
      Stub_Gtk_Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar_Record;
   begin
      return Gtk.Scrollbar.Gtk_Scrollbar (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Scrollbar));
   end Get_Hscrollbar;

   ---------------------------
   -- Get_Kinetic_Scrolling --
   ---------------------------

   function Get_Kinetic_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean
   is
      function Internal
         (Scrolled_Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrolled_window_get_kinetic_scrolling");
   begin
      return Internal (Get_Object (Scrolled_Window)) /= 0;
   end Get_Kinetic_Scrolling;

   ----------------------------
   -- Get_Max_Content_Height --
   ----------------------------

   function Get_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint
   is
      function Internal (Scrolled_Window : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_max_content_height");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Max_Content_Height;

   ---------------------------
   -- Get_Max_Content_Width --
   ---------------------------

   function Get_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint
   is
      function Internal (Scrolled_Window : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_max_content_width");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Max_Content_Width;

   ----------------------------
   -- Get_Min_Content_Height --
   ----------------------------

   function Get_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint
   is
      function Internal (Scrolled_Window : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_min_content_height");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Min_Content_Height;

   ---------------------------
   -- Get_Min_Content_Width --
   ---------------------------

   function Get_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Glib.Gint
   is
      function Internal (Scrolled_Window : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_scrolled_window_get_min_content_width");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Min_Content_Width;

   ---------------------------
   -- Get_Overlay_Scrolling --
   ---------------------------

   function Get_Overlay_Scrolling
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean
   is
      function Internal
         (Scrolled_Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrolled_window_get_overlay_scrolling");
   begin
      return Internal (Get_Object (Scrolled_Window)) /= 0;
   end Get_Overlay_Scrolling;

   -------------------
   -- Get_Placement --
   -------------------

   function Get_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Corner_Type
   is
      function Internal
         (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Corner_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_placement");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Placement;

   ----------------
   -- Get_Policy --
   ----------------

   procedure Get_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Hscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type;
          Vscrollbar_Policy : out Gtk.Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_get_policy");
   begin
      Internal (Get_Object (Scrolled_Window), Hscrollbar_Policy, Vscrollbar_Policy);
   end Get_Policy;

   ----------------------------------
   -- Get_Propagate_Natural_Height --
   ----------------------------------

   function Get_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean
   is
      function Internal
         (Scrolled_Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrolled_window_get_propagate_natural_height");
   begin
      return Internal (Get_Object (Scrolled_Window)) /= 0;
   end Get_Propagate_Natural_Height;

   ---------------------------------
   -- Get_Propagate_Natural_Width --
   ---------------------------------

   function Get_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Boolean
   is
      function Internal
         (Scrolled_Window : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrolled_window_get_propagate_natural_width");
   begin
      return Internal (Get_Object (Scrolled_Window)) /= 0;
   end Get_Propagate_Natural_Width;

   ---------------------
   -- Get_Shadow_Type --
   ---------------------

   function Get_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Enums.Gtk_Shadow_Type
   is
      function Internal
         (Scrolled_Window : System.Address) return Gtk.Enums.Gtk_Shadow_Type;
      pragma Import (C, Internal, "gtk_scrolled_window_get_shadow_type");
   begin
      return Internal (Get_Object (Scrolled_Window));
   end Get_Shadow_Type;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   --------------------
   -- Get_Vscrollbar --
   --------------------

   function Get_Vscrollbar
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
       return Gtk.Scrollbar.Gtk_Scrollbar
   is
      function Internal
         (Scrolled_Window : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vscrollbar");
      Stub_Gtk_Scrollbar : Gtk.Scrollbar.Gtk_Scrollbar_Record;
   begin
      return Gtk.Scrollbar.Gtk_Scrollbar (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub_Gtk_Scrollbar));
   end Get_Vscrollbar;

   ------------------------------
   -- Set_Capture_Button_Press --
   ------------------------------

   procedure Set_Capture_Button_Press
      (Scrolled_Window      : not null access Gtk_Scrolled_Window_Record;
       Capture_Button_Press : Boolean)
   is
      procedure Internal
         (Scrolled_Window      : System.Address;
          Capture_Button_Press : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scrolled_window_set_capture_button_press");
   begin
      Internal (Get_Object (Scrolled_Window), Boolean'Pos (Capture_Button_Press));
   end Set_Capture_Button_Press;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Hadjustment     : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Hadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_hadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object_Or_Null (GObject (Hadjustment)));
   end Set_Hadjustment;

   ---------------------------
   -- Set_Kinetic_Scrolling --
   ---------------------------

   procedure Set_Kinetic_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Kinetic_Scrolling : Boolean)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Kinetic_Scrolling : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scrolled_window_set_kinetic_scrolling");
   begin
      Internal (Get_Object (Scrolled_Window), Boolean'Pos (Kinetic_Scrolling));
   end Set_Kinetic_Scrolling;

   ----------------------------
   -- Set_Max_Content_Height --
   ----------------------------

   procedure Set_Max_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Height          : Glib.Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_max_content_height");
   begin
      Internal (Get_Object (Scrolled_Window), Height);
   end Set_Max_Content_Height;

   ---------------------------
   -- Set_Max_Content_Width --
   ---------------------------

   procedure Set_Max_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Width           : Glib.Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_max_content_width");
   begin
      Internal (Get_Object (Scrolled_Window), Width);
   end Set_Max_Content_Width;

   ----------------------------
   -- Set_Min_Content_Height --
   ----------------------------

   procedure Set_Min_Content_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Height          : Glib.Gint)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Height          : Glib.Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_min_content_height");
   begin
      Internal (Get_Object (Scrolled_Window), Height);
   end Set_Min_Content_Height;

   ---------------------------
   -- Set_Min_Content_Width --
   ---------------------------

   procedure Set_Min_Content_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Width           : Glib.Gint)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Width           : Glib.Gint);
      pragma Import (C, Internal, "gtk_scrolled_window_set_min_content_width");
   begin
      Internal (Get_Object (Scrolled_Window), Width);
   end Set_Min_Content_Width;

   ---------------------------
   -- Set_Overlay_Scrolling --
   ---------------------------

   procedure Set_Overlay_Scrolling
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Overlay_Scrolling : Boolean)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Overlay_Scrolling : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scrolled_window_set_overlay_scrolling");
   begin
      Internal (Get_Object (Scrolled_Window), Boolean'Pos (Overlay_Scrolling));
   end Set_Overlay_Scrolling;

   -------------------
   -- Set_Placement --
   -------------------

   procedure Set_Placement
      (Scrolled_Window  : not null access Gtk_Scrolled_Window_Record;
       Window_Placement : Gtk.Enums.Gtk_Corner_Type)
   is
      procedure Internal
         (Scrolled_Window  : System.Address;
          Window_Placement : Gtk.Enums.Gtk_Corner_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_placement");
   begin
      Internal (Get_Object (Scrolled_Window), Window_Placement);
   end Set_Placement;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
      (Scrolled_Window   : not null access Gtk_Scrolled_Window_Record;
       Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
       Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type)
   is
      procedure Internal
         (Scrolled_Window   : System.Address;
          Hscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type;
          Vscrollbar_Policy : Gtk.Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window), Hscrollbar_Policy, Vscrollbar_Policy);
   end Set_Policy;

   ----------------------------------
   -- Set_Propagate_Natural_Height --
   ----------------------------------

   procedure Set_Propagate_Natural_Height
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Propagate       : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scrolled_window_set_propagate_natural_height");
   begin
      Internal (Get_Object (Scrolled_Window), Boolean'Pos (Propagate));
   end Set_Propagate_Natural_Height;

   ---------------------------------
   -- Set_Propagate_Natural_Width --
   ---------------------------------

   procedure Set_Propagate_Natural_Width
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Propagate       : Boolean)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Propagate       : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_scrolled_window_set_propagate_natural_width");
   begin
      Internal (Get_Object (Scrolled_Window), Boolean'Pos (Propagate));
   end Set_Propagate_Natural_Width;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       The_Type        : Gtk.Enums.Gtk_Shadow_Type)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          The_Type        : Gtk.Enums.Gtk_Shadow_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_shadow_type");
   begin
      Internal (Get_Object (Scrolled_Window), The_Type);
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record;
       Vadjustment     : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Scrolled_Window : System.Address;
          Vadjustment     : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_vadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object_Or_Null (GObject (Vadjustment)));
   end Set_Vadjustment;

   ---------------------
   -- Unset_Placement --
   ---------------------

   procedure Unset_Placement
      (Scrolled_Window : not null access Gtk_Scrolled_Window_Record)
   is
      procedure Internal (Scrolled_Window : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_unset_placement");
   begin
      Internal (Get_Object (Scrolled_Window));
   end Unset_Placement;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Position_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Position_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean);

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Position_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Void);

   procedure Marsh_GObject_Gtk_Position_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Position_Type_Void);

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean);

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void);

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void);

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Position_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Position_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Scrolled_Window_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Void;

   ------------------------------------------
   -- Marsh_GObject_Gtk_Position_Type_Void --
   ------------------------------------------

   procedure Marsh_GObject_Gtk_Position_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Position_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Position_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Position_Type_Void;

   ---------------------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean --
   ---------------------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1), Unchecked_To_Boolean (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Type_Boolean_Boolean;

   -------------------------------------------------------
   -- Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void --
   -------------------------------------------------------

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scrolled_Window_Gtk_Direction_Type_Void;

   ------------------------------------------------------
   -- Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void --
   ------------------------------------------------------

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Position_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scrolled_Window_Gtk_Position_Type_Void;

   ---------------------------------------------------------------
   -- Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean --
   ---------------------------------------------------------------

   procedure Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Scrolled_Window := Gtk_Scrolled_Window (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Scroll_Type (Params, 1), Unchecked_To_Boolean (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean;

   ----------------------
   -- On_Edge_Overshot --
   ----------------------

   procedure On_Edge_Overshot
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "edge-overshot" & ASCII.NUL, Call, After);
   end On_Edge_Overshot;

   ----------------------
   -- On_Edge_Overshot --
   ----------------------

   procedure On_Edge_Overshot
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Position_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "edge-overshot" & ASCII.NUL, Call, After, Slot);
   end On_Edge_Overshot;

   ---------------------
   -- On_Edge_Reached --
   ---------------------

   procedure On_Edge_Reached
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Position_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "edge-reached" & ASCII.NUL, Call, After);
   end On_Edge_Reached;

   ---------------------
   -- On_Edge_Reached --
   ---------------------

   procedure On_Edge_Reached
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Position_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "edge-reached" & ASCII.NUL, Call, After, Slot);
   end On_Edge_Reached;

   -----------------------
   -- On_Move_Focus_Out --
   -----------------------

   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-focus-out" & ASCII.NUL, Call, After);
   end On_Move_Focus_Out;

   -----------------------
   -- On_Move_Focus_Out --
   -----------------------

   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-focus-out" & ASCII.NUL, Call, After, Slot);
   end On_Move_Focus_Out;

   ---------------------
   -- On_Scroll_Child --
   ---------------------

   procedure On_Scroll_Child
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_Gtk_Scrolled_Window_Gtk_Scroll_Type_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll-child" & ASCII.NUL, Call, After);
   end On_Scroll_Child;

   ---------------------
   -- On_Scroll_Child --
   ---------------------

   procedure On_Scroll_Child
      (Self  : not null access Gtk_Scrolled_Window_Record;
       Call  : Cb_GObject_Gtk_Scroll_Type_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll-child" & ASCII.NUL, Call, After, Slot);
   end On_Scroll_Child;

end Gtk.Scrolled_Window;
