-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2005 AdaCore                    --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings;        use Interfaces.C.Strings;
with Gdk.Visual; use Gdk.Visual;
with Gdk.Color;  use Gdk.Color;
with Ada.Unchecked_Conversion;
with Pango.Context; use Pango.Context;
with Pango.Layout;  use Pango.Layout;

package body Gtk.Widget is

   --------------
   -- Activate --
   --------------

   procedure Activate (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");

   begin
      Internal (Get_Object (Widget));
   end Activate;

   ---------------------
   -- Add_Accelerator --
   ---------------------

   procedure Add_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : String;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags)
   is
      procedure Internal
        (Widget       : System.Address;
         Accel_Signal : String;
         Accel_Group  : System.Address;
         Accel_Key    : Gdk.Types.Gdk_Key_Type;
         Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
         Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags);
      pragma Import (C, Internal, "gtk_widget_add_accelerator");

   begin
      Internal
        (Get_Object (Widget),
         Accel_Signal & ASCII.NUL,
         Get_Object (Accel_Group),
         Accel_Key,
         Accel_Mods,
         Accel_Flags);
   end Add_Accelerator;

   ----------------
   -- Add_Events --
   ----------------

   procedure Add_Events
     (Widget : access Gtk_Widget_Record;
      Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
        (Widget : System.Address; Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_add_events");

   begin
      Internal (Get_Object (Widget), Events);
   end Add_Events;

   ----------------------
   -- Can_Focus_Is_Set --
   ----------------------

   function Can_Focus_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Can_Focus);
   end Can_Focus_Is_Set;

   -------------
   -- Convert --
   -------------

   function Convert (W : Gtk_Widget) return System.Address is
   begin
      if W = null then
         return System.Null_Address;
      else
         return Get_Object (W);
      end if;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (W : System.Address) return Gtk_Widget is
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget (Get_User_Data (W, Stub));
   end Convert;

   --------------------------
   -- Create_Pango_Context --
   --------------------------

   function Create_Pango_Context (Widget : access Gtk_Widget_Record)
      return Pango.Context.Pango_Context
   is
      function Internal (Widget : System.Address) return Pango_Context;
      pragma Import (C, Internal, "gtk_widget_create_pango_context");
   begin
      return Internal (Get_Object (Widget));
   end Create_Pango_Context;

   -------------------------
   -- Create_Pango_Layout --
   -------------------------

   function Create_Pango_Layout
     (Widget : access Gtk_Widget_Record; Text : UTF8_String := "")
      return Pango.Layout.Pango_Layout
   is
      function Internal (Widget : System.Address; Text : UTF8_String)
         return System.Address;
      pragma Import (C, Internal, "gtk_widget_create_pango_layout");

      function Internal2 (Widget : System.Address; Text : System.Address)
         return System.Address;
      pragma Import (C, Internal2, "gtk_widget_create_pango_layout");
      Stub : Pango_Layout_Record;
   begin
      if Text = "" then
         return Pango_Layout (Get_User_Data_Fast
           (Internal2 (Get_Object (Widget), System.Null_Address), Stub));
      else
         return Pango_Layout (Get_User_Data_Fast
           (Internal (Get_Object (Widget), Text & ASCII.NUL), Stub));
      end if;
   end Create_Pango_Layout;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Widget);
   end Destroy_Cb;

   ----------------------------
   -- Double_Buffered_Is_Set --
   ----------------------------

   function Double_Buffered_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Double_Buffered);
   end Double_Buffered_Is_Set;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Widget : access Gtk_Widget_Record;
      Area   : Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area)
   is
      procedure Internal
        (Widget : System.Address; Area : Gdk.Rectangle.Gdk_Rectangle);
      procedure Internal (Widget : System.Address; Area : System.Address);
      pragma Import (C, Internal, "gtk_widget_draw");

      use type Gdk.Rectangle.Gdk_Rectangle;

   begin
      if Area = Gdk.Rectangle.Full_Area then
         --  Redraw the whole widget
         Internal (Get_Object (Widget), System.Null_Address);
      else
         Internal (Get_Object (Widget), Area);
      end if;
   end Draw;

   ---------------------
   -- Drawable_Is_Set --
   ---------------------

   function Drawable_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal (Widget : System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_drawable");

   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Drawable_Is_Set;

   -----------
   -- Event --
   -----------

   function Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
        (Widget : System.Address; Event : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_event");

   begin
      return
        Internal (Get_Object (Widget), Gdk.Event.To_Address (Event)) /= 0;
   end Event;

   ---------------------------
   -- Get_Allocation_Height --
   ---------------------------

   function Get_Allocation_Height
     (Widget : access Gtk_Widget_Record) return Allocation_Int
   is
      function Internal (Widget : System.Address) return Allocation_Int;
      pragma Import (C, Internal, "ada_widget_allocation_height");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Height;

   --------------------------
   -- Get_Allocation_Width --
   --------------------------

   function Get_Allocation_Width
     (Widget : access Gtk_Widget_Record) return Allocation_Int
   is
      function Internal (Widget : System.Address) return Allocation_Int;
      pragma Import (C, Internal, "ada_widget_allocation_width");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Width;

   ----------------------
   -- Get_Allocation_X --
   ----------------------

   function Get_Allocation_X
     (Widget : access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_x");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_X;

   ----------------------
   -- Get_Allocation_Y --
   ----------------------

   function Get_Allocation_Y
     (Widget : access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_y");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Y;

   ----------------
   -- Get_Events --
   ----------------

   function Get_Events
     (Widget : access Gtk_Widget_Record) return Gdk.Event.Gdk_Event_Mask
   is
      function Internal
        (Widget : System.Address) return Gdk.Event.Gdk_Event_Mask;
      pragma Import (C, Internal, "gtk_widget_get_events");

   begin
      return Internal (Get_Object (Widget));
   end Get_Events;

   -----------------------
   -- Get_Pango_Context --
   -----------------------

   function Get_Pango_Context (Widget : access Gtk_Widget_Record)
      return Pango.Context.Pango_Context
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_pango_context");
      Stub : Pango_Context_Record;
   begin
      return Pango_Context
        (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Pango_Context;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Widget : access Gtk_Widget_Record) return UTF8_String is
      function Internal
        (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_name");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Name;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Widget : access Gtk_Widget_Record) return Gtk_Widget is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_parent");

      S : System.Address;
      use type System.Address;

   begin
      S := Internal (Get_Object (Widget));

      if S = System.Null_Address then
         return null;
      else
         return Convert (S);
      end if;
   end Get_Parent;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
     (Widget : access Gtk_Widget_Record;
      X      : out Gint;
      Y      : out Gint)
   is
      procedure Internal
        (Widget : System.Address;
         X      : out Gint;
         Y      : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_pointer");

   begin
      Internal (Get_Object (Widget), X, Y);
   end Get_Pointer;

   ------------------
   -- Get_Toplevel --
   ------------------

   function Get_Toplevel
     (Widget : access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_toplevel");

   begin
      return Convert (Internal (Get_Object (Widget)));
   end Get_Toplevel;

   ---------------------
   -- Get_Requisition --
   ---------------------

   function Get_Requisition
     (Value : Glib.Values.GValue) return Gtk_Requisition_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function To_Requisition is new
        Ada.Unchecked_Conversion (System.Address, Gtk_Requisition_Access);
      pragma Warnings (On);

   begin
      return To_Requisition (Glib.Values.Get_Address (Value));
   end Get_Requisition;

   --------------------
   -- Get_Allocation --
   --------------------

   function Get_Allocation
     (Value : Glib.Values.GValue) return Gtk_Allocation_Access
   is
      pragma Warnings (Off);
      --  This UC is safe aliasing-wise, so kill warning
      function To_Allocation is new
        Ada.Unchecked_Conversion (System.Address, Gtk_Allocation_Access);
      pragma Warnings (On);

   begin
      return To_Allocation (Glib.Values.Get_Address (Value));
   end Get_Allocation;

   ----------------
   -- Set_Window --
   ----------------

   procedure Set_Window
     (Widget : access Gtk_Widget_Record;
      Window : Gdk.Window.Gdk_Window)
   is
      procedure Internal
        (Widget : System.Address; Window : Gdk.Window.Gdk_Window);
      pragma Import (C, Internal, "ada_widget_set_window");

   begin
      Internal (Get_Object (Widget), Window);
   end Set_Window;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
     (Widget : access Gtk_Widget_Record) return Gdk.Window.Gdk_Window
   is
      function Internal (Widget : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "ada_widget_get_window");

   begin
      return Internal (Get_Object (Widget));
   end Get_Window;

   ------------------
   -- Grab_Default --
   ------------------

   procedure Grab_Default (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_default");

   begin
      Internal (Get_Object (Widget));
   end Grab_Default;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_focus");

   begin
      Internal (Get_Object (Widget));
   end Grab_Focus;

   ------------------
   -- Get_Colormap --
   ------------------

   function Get_Colormap
     (Widget : access Gtk_Widget_Record) return Gdk.Color.Gdk_Colormap
   is
      function Internal
        (Widget : System.Address) return Gdk.Color.Gdk_Colormap;
      pragma Import (C, Internal, "gtk_widget_get_colormap");

   begin
      return Internal (Get_Object (Widget));
   end Get_Colormap;

   ---------------------------------
   -- Default_Motion_Notify_Event --
   ---------------------------------

   function Default_Motion_Notify_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Gint
   is
      function Internal
        (Widget : System.Address;
         Event  : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_get_motion_notify");

   begin
      return Internal (Get_Object (Widget), Gdk.Event.To_Address (Event));
   end Default_Motion_Notify_Event;

   ---------------------------
   -- Get_Child_Requisition --
   ---------------------------

   function Get_Child_Requisition
     (Widget : access Gtk_Widget_Record) return Gtk_Requisition
   is
      procedure Internal
        (Widget : System.Address;
         Req    : out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_get_child_requisition");

      Req : Gtk_Requisition;

   begin
      Internal (Get_Object (Widget), Req);
      return Req;
   end Get_Child_Requisition;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual
     (Widget : access Gtk_Widget_Record) return Gdk.Gdk_Visual
   is
      function Internal (Widget : System.Address) return Gdk.Gdk_Visual;
      pragma Import (C, Internal, "gtk_widget_get_visual");

   begin
      return Internal (Get_Object (Widget));
   end Get_Visual;

   ---------------------------------------
   -- Has_Default_Motion_Notify_Handler --
   ---------------------------------------

   function Has_Default_Motion_Notify_Handler
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_has_default_motion_notify");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Default_Motion_Notify_Handler;

   ------------------------
   -- Has_Default_Is_Set --
   ------------------------

   function Has_Default_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Default);
   end Has_Default_Is_Set;

   ----------------------
   -- Has_Focus_Is_Set --
   ----------------------

   function Has_Focus_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Focus);
   end Has_Focus_Is_Set;

   ---------------------
   -- Has_Grab_Is_Set --
   ---------------------

   function Has_Grab_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Grab);
   end Has_Grab_Is_Set;

   ----------
   -- Hide --
   ----------

   procedure Hide (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");

   begin
      Internal (Get_Object (Widget));
   end Hide;

   --------------------
   -- Hide_On_Delete --
   --------------------

   function Hide_On_Delete
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal (Widget : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_widget_hide_on_delete");

   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Hide_On_Delete;

   ---------------
   -- Intersect --
   ---------------

   function Intersect
     (Widget       : access Gtk_Widget_Record;
      Area         : Gdk.Rectangle.Gdk_Rectangle;
      Intersection : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
        (Widget : System.Address;
         Area   : Gdk.Rectangle.Gdk_Rectangle;
         Inter  : access Gdk.Rectangle.Gdk_Rectangle) return Gint;
      pragma Import (C, Internal, "gtk_widget_intersect");

      Result : Gint;

   begin
      Result := Internal (Get_Object (Widget), Area, Intersection);
      return Boolean'Val (Result);
   end Intersect;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (Widget   : access Gtk_Widget_Record;
      Ancestor : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal
        (Widget : System.Address; Ancestor : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_is_ancestor");

   begin
      return Boolean'Val
        (Internal (Get_Object (Widget), Get_Object (Ancestor)));
   end Is_Ancestor;

   ---------
   -- Map --
   ---------

   procedure Map (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_map");

   begin
      Internal (Get_Object (Widget));
   end Map;

   -------------------
   -- Mapped_Is_Set --
   -------------------

   function Mapped_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Mapped);
   end Mapped_Is_Set;

   ----------------------
   -- No_Window_Is_Set --
   ----------------------

   function No_Window_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, No_Window);
   end No_Window_Is_Set;

   ----------------
   -- Queue_Draw --
   ----------------

   procedure Queue_Draw (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_draw");

   begin
      Internal (Get_Object (Widget));
   end Queue_Draw;

   ---------------------
   -- Queue_Draw_Area --
   ---------------------

   procedure Queue_Draw_Area
     (Widget : access Gtk_Widget_Record;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint)
   is
      procedure Internal
        (Widget : System.Address;
         X      : Gint;
         Y      : Gint;
         Width  : Gint;
         Height : Gint);
      pragma Import (C, Internal, "gtk_widget_queue_draw_area");

   begin
      Internal (Get_Object (Widget), X, Y, Width, Height);
   end Queue_Draw_Area;

   -----------------
   -- Queue_Clear --
   -----------------

   procedure Queue_Clear (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_clear");

   begin
      Internal (Get_Object (Widget));
   end Queue_Clear;

   ----------------------
   -- Queue_Clear_Area --
   ----------------------

   procedure Queue_Clear_Area
     (Widget : access Gtk_Widget_Record;
      X      : Gint;
      Y      : Gint;
      Width  : Gint;
      Height : Gint)
   is
      procedure Internal
        (Widget : System.Address;
         X      : Gint;
         Y      : Gint;
         Width  : Gint;
         Height : Gint);
      pragma Import (C, Internal, "gtk_widget_queue_clear_area");

   begin
      Internal (Get_Object (Widget), X, Y, Width, Height);
   end Queue_Clear_Area;

   ------------------
   -- Queue_Resize --
   ------------------

   procedure Queue_Resize (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_resize");

   begin
      Internal (Get_Object (Widget));
   end Queue_Resize;

   ---------------------
   -- Rc_Style_Is_Set --
   ---------------------

   function Rc_Style_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Rc_Style);
   end Rc_Style_Is_Set;

   -------------
   -- Realize --
   -------------

   procedure Realize (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_realize");

   begin
      Internal (Get_Object (Widget));
   end Realize;

   ---------------------
   -- Realized_Is_Set --
   ---------------------

   function Realized_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Realized);
   end Realized_Is_Set;

   ------------------------
   -- Remove_Accelerator --
   ------------------------

   procedure Remove_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal
        (Widget      : System.Address;
         Accel_Group : System.Address;
         Accel_Key   : Gdk.Types.Gdk_Key_Type;
         Accel_Mods  : Gdk.Types.Gdk_Modifier_Type);
      pragma Import (C, Internal, "gtk_widget_remove_accelerator");

   begin
      Internal (Get_Object (Widget),
                Get_Object (Accel_Group),
                Accel_Key,
                Accel_Mods);
   end Remove_Accelerator;

   --------------
   -- Reparent --
   --------------

   procedure Reparent
     (Widget : access Gtk_Widget_Record;
      New_Parent : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget, New_Parent : System.Address);
      pragma Import (C, Internal, "gtk_widget_reparent");

   begin
      Internal (Get_Object (Widget), Get_Object (New_Parent));
   end Reparent;

   ---------------------------
   -- Translate_Coordinates --
   ---------------------------

   procedure Translate_Coordinates
     (Src_Widget  : Gtk_Widget;
      Dest_Widget : Gtk_Widget;
      Src_X       : Gint;
      Src_Y       : Gint;
      Dest_X      : out Gint;
      Dest_Y      : out Gint;
      Result      : out Boolean)
   is
      function Internal
        (Src_Widget  : System.Address;
         Dest_Widget : System.Address;
         Src_X       : Gint;
         Src_Y       : Gint;
         Dest_X      : access Gint;
         Dest_Y      : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_widget_translate_coordinates");

      X, Y : aliased Gint;

   begin
      Result := Boolean'Val (Internal
        (Get_Object (Src_Widget),
         Get_Object (Dest_Widget),
         Src_X, Src_Y, X'Access, Y'Access));

      if Result then
         Dest_X := X;
         Dest_Y := Y;
      end if;
   end Translate_Coordinates;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal (Widget : System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_is_sensitive");

   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Sensitive;

   -----------------
   -- Send_Expose --
   -----------------

   procedure Send_Expose
     (Widget : access Gtk_Widget_Record;
      Event  : Gdk.Event.Gdk_Event_Expose)
   is
      procedure Internal
        (Widget : System.Address; Event : Gdk.Event.Gdk_Event_Expose);
      pragma Import (C, Internal, "gtk_widget_send_expose");

   begin
      Internal (Get_Object (Widget), Event);
   end Send_Expose;

   -----------------------
   -- Set_App_Paintable --
   -----------------------

   procedure Set_App_Paintable
     (Widget        : access Gtk_Widget_Record;
      App_Paintable : Boolean)
   is
      procedure Internal (Widget : System.Address; App_Paintable : Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_app_paintable");

   begin
      Internal (Get_Object (Widget), Boolean'Pos (App_Paintable));
   end Set_App_Paintable;

   ------------------
   -- Set_Colormap --
   ------------------

   procedure Set_Colormap
     (Widget : access Gtk_Widget_Record;
      Cmap   : Gdk.Gdk_Colormap)
   is
      procedure Internal (Widget : System.Address; Cmap : Gdk.Gdk_Colormap);
      pragma Import (C, Internal, "gtk_widget_set_colormap");

   begin
      Internal (Get_Object (Widget), Cmap);
   end Set_Colormap;

   ----------------
   -- Set_Events --
   ----------------

   procedure Set_Events
     (Widget : access Gtk_Widget_Record;
      Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
        (Widget : System.Address; Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_set_events");

   begin
      Internal (Get_Object (Widget), Events);
   end Set_Events;

   --------------------------
   -- Set_Extension_Events --
   --------------------------

   procedure Set_Extension_Events
     (Widget : access Gtk_Widget_Record;
      Mode   : Gdk.Types.Gdk_Extension_Mode)
   is
      procedure Internal
        (Widget : System.Address; Mode : Gdk.Types.Gdk_Extension_Mode);
      pragma Import (C, Internal, "gtk_widget_set_extension_events");

   begin
      Internal (Get_Object (Widget), Mode);
   end Set_Extension_Events;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Widget : access Gtk_Widget_Record;
      Style  : Gtk.Style.Gtk_Style)
   is
      procedure Internal
        (Widget : System.Address; Style : Gtk.Style.Gtk_Style);
      pragma Import (C, Internal, "gtk_widget_set_style");

   begin
      Internal (Get_Object (Widget), Style);
   end Set_Style;

   -----------------------
   -- Get_Parent_Window --
   -----------------------

   function Get_Parent_Window
     (Widget : access Gtk_Widget_Record) return Gdk.Window.Gdk_Window
   is
      function Internal (Widget : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_widget_get_parent_window");

   begin
      return Internal (Get_Object (Widget));
   end Get_Parent_Window;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Widget : access Gtk_Widget_Record) return Gtk.Style.Gtk_Style
   is
      function Internal (Widget : System.Address) return Gtk.Style.Gtk_Style;
      pragma Import (C, Internal, "gtk_widget_get_style");

   begin
      return Internal (Get_Object (Widget));
   end Get_Style;

   ------------------------
   -- Get_Modifier_Style --
   ------------------------

   function Get_Modifier_Style
     (Widget : access Gtk_Widget_Record) return Gtk.Style.Gtk_Style
   is
      function Internal (Widget : System.Address) return Gtk.Style.Gtk_Style;
      pragma Import (C, Internal, "gtk_widget_get_modifier_style");

   begin
      return Internal (Get_Object (Widget));
   end Get_Modifier_Style;

   ---------------
   -- Modify_Fg --
   ---------------

   procedure Modify_Fg
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Widget : System.Address;
         State  : Enums.Gtk_State_Type;
         Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_fg");

      use type Gdk.Color.Gdk_Color;
      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Widget), State_Type, Color_A);
   end Modify_Fg;

   ---------------
   -- Modify_Bg --
   ---------------

   procedure Modify_Bg
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Widget : System.Address;
         State  : Enums.Gtk_State_Type;
         Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_bg");

      use type Gdk.Color.Gdk_Color;
      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Widget), State_Type, Color_A);
   end Modify_Bg;

   -----------------
   -- Modify_Text --
   -----------------

   procedure Modify_Text
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Widget : System.Address;
         State  : Enums.Gtk_State_Type;
         Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_text");

      use type Gdk.Color.Gdk_Color;
      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Widget), State_Type, Color_A);
   end Modify_Text;

   -----------------
   -- Modify_Base --
   -----------------

   procedure Modify_Base
     (Widget     : access Gtk_Widget_Record;
      State_Type : Enums.Gtk_State_Type;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Widget : System.Address;
         State  : Enums.Gtk_State_Type;
         Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_base");

      use type Gdk.Color.Gdk_Color;
      Col     : aliased Gdk.Color.Gdk_Color := Color;
      Color_A : System.Address := Col'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Color_A := System.Null_Address;
      end if;

      Internal (Get_Object (Widget), State_Type, Color_A);
   end Modify_Base;

   -----------------
   -- Modify_Font --
   -----------------

   procedure Modify_Font
     (Widget : access Gtk_Widget_Record;
      Desc   : Pango.Font.Pango_Font_Description)
   is
      procedure Internal (Widget : System.Address;
                          Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "gtk_widget_modify_font");
   begin
      Internal (Get_Object (Widget), Desc);
   end Modify_Font;

   --------------------------
   -- Get_Extension_Events --
   --------------------------

   function Get_Extension_Events
     (Widget : access Gtk_Widget_Record) return Gdk.Types.Gdk_Extension_Mode
   is
      function Internal
        (Widget : System.Address) return Gdk.Types.Gdk_Extension_Mode;
      pragma Import (C, Internal, "gtk_widget_get_extension_events");

   begin
      return Internal (Get_Object (Widget));
   end Get_Extension_Events;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor
     (Widget        : access Gtk_Widget_Record;
      Ancestor_Type : Gtk_Type) return Gtk_Widget
   is
      function Internal
        (Widget        : System.Address;
         Ancestor_Type : Gtk_Type) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_ancestor");

      S : System.Address;
      use type System.Address;

   begin
      S := Internal (Get_Object (Widget), Ancestor_Type);

      if S = System.Null_Address then
         return null;
      else
         return Convert (S);
      end if;
   end Get_Ancestor;

   -------------------------
   -- Set_Double_Buffered --
   -------------------------

   procedure Set_Double_Buffered
     (Widget          : access Gtk_Widget_Record;
      Double_Buffered : Boolean := True)
   is
      procedure Internal (Widget : System.Address; Double_Buffered : Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_double_buffered");

   begin
      Internal (Get_Object (Widget), Boolean'Pos (Double_Buffered));
   end Set_Double_Buffered;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Widget : access Gtk_Widget_Record; Name : UTF8_String)
   is
      procedure Internal (Widget : System.Address; Name : UTF8_String);
      pragma Import (C, Internal, "gtk_widget_set_name");

   begin
      Internal (Get_Object (Widget), Name & ASCII.NUL);
   end Set_Name;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Widget : access Gtk_Widget_Record;
      Parent : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget, Parent : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_parent");

   begin
      Internal (Get_Object (Widget), Get_Object (Parent));
   end Set_Parent;

   -----------------------
   -- Set_Parent_Window --
   -----------------------

   procedure Set_Parent_Window
     (Widget : access Gtk_Widget_Record;
      Window : Gdk.Window.Gdk_Window)
   is
      procedure Internal (Widget : System.Address; Parent : Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_set_parent_window");

   begin
      Internal (Get_Object (Widget), Window);
   end Set_Parent_Window;

   ------------------
   -- Ensure_Style --
   ------------------

   procedure Ensure_Style (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_ensure_style");

   begin
      Internal (Get_Object (Widget));
   end Ensure_Style;

   ---------------------------
   -- Restore_Default_Style --
   ---------------------------

   procedure Restore_Default_Style (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address; Style : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_style");

   begin
      Internal (Get_Object (Widget), System.Null_Address);
   end Restore_Default_Style;

   ---------------------
   -- Reset_Rc_Styles --
   ---------------------

   procedure Reset_Rc_Styles (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_reset_rc_styles");

   begin
      Internal (Get_Object (Widget));
   end Reset_Rc_Styles;

   ----------------------------
   -- Set_Scroll_Adjustments --
   ----------------------------

   procedure Set_Scroll_Adjustments
     (Widget : access Gtk_Widget_Record;
      Hadj   : Gtk.Adjustment.Gtk_Adjustment;
      Vadj   : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Widget : System.Address;
         Hadj   : System.Address;
         Vadj   : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_scroll_adjustments");

      use type Gtk.Adjustment.Gtk_Adjustment;

      H : System.Address := System.Null_Address;
      V : System.Address := System.Null_Address;

   begin
      if Hadj /= null then
         H := Get_Object (Hadj);
      end if;

      if Vadj /= null then
         V := Get_Object (Vadj);
      end if;

      Internal (Get_Object (Widget), H, V);
   end Set_Scroll_Adjustments;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Widget    : access Gtk_Widget_Record;
      Sensitive : Boolean := True)
   is
      procedure Internal (Widget : System.Address; Sensitive : Gint);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");

   begin
      Internal (Get_Object (Widget), To_Gint (Sensitive));
   end Set_Sensitive;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Widget : access Gtk_Widget_Record;
      State  : Enums.Gtk_State_Type)
   is
      procedure Internal
        (Widget : System.Address; State : Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_widget_set_state");

   begin
      Internal (Get_Object (Widget), State);
   end Set_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Widget : access Gtk_Widget_Record) return Enums.Gtk_State_Type
   is
      function Internal (Widget : System.Address) return Enums.Gtk_State_Type;
      pragma Import (C, Internal, "ada_widget_get_state");

   begin
      return Internal (Get_Object (Widget));
   end Get_State;

   -------------------
   -- Set_UPosition --
   -------------------

   procedure Set_UPosition (Widget : access Gtk_Widget_Record; X, Y : Gint) is
      procedure Internal (Widget : System.Address; X, Y : Gint);
      pragma Import (C, Internal, "gtk_widget_set_uposition");

   begin
      Internal (Get_Object (Widget), X, Y);
   end Set_UPosition;

   ---------------
   -- Set_USize --
   ---------------

   procedure Set_USize
     (Widget        : access Gtk_Widget_Record;
      Width, Height : Gint)
   is
      procedure Internal (Widget : System.Address; Width, Height : Gint);
      pragma Import (C, Internal, "gtk_widget_set_usize");

   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_USize;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
     (Widget      : access Gtk_Widget_Record;
      Width, Height : Gint := -1)
   is
      procedure Internal (Widget : System.Address; Width, Height : Gint);
      pragma Import (C, Internal, "gtk_widget_set_size_request");

   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_Size_Request;

   ----------
   -- Show --
   ----------

   procedure Show (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");

   begin
      Internal (Get_Object (Widget));
   end Show;

   --------------
   -- Show_Now --
   --------------

   procedure Show_Now (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_now");

   begin
      Internal (Get_Object (Widget));
   end Show_Now;

   --------------
   -- Show_All --
   --------------

   procedure Show_All (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_all");

   begin
      Internal (Get_Object (Widget));
   end Show_All;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Widget      : access Gtk_Widget_Record;
      Requisition : in out Gtk_Requisition)
   is
      procedure Internal
        (Widget : System.Address; Requisition : in out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_size_request");

   begin
      Internal (Get_Object (Widget), Requisition);
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Widget     : access Gtk_Widget_Record;
      Allocation : Gtk_Allocation)
   is
      procedure Internal
        (Widget : System.Address; Allocation : Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_size_allocate");

   begin
      Internal (Get_Object (Widget), Allocation);
   end Size_Allocate;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_hide_all");
   begin
      Internal (Get_Object (Widget));
   end Hide_All;

   -----------------
   -- Render_Icon --
   -----------------

   function Render_Icon
     (Widget   : access Gtk_Widget_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size;
      Detail   : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Widget : System.Address;
         Stock_Id : String;
         Size : Gtk.Enums.Gtk_Icon_Size;
         Detail : System.Address) return Gdk.Pixbuf.Gdk_Pixbuf;
      pragma Import (C, Internal, "gtk_widget_render_icon");
      D : System.Address := System.Null_Address;
   begin
      if Detail /= "" then
         D := Detail'Address;
      end if;

      return Internal (Get_Object (Widget), Stock_Id & ASCII.NUL, Size, D);
   end Render_Icon;

   --------------------
   -- Set_Allocation --
   --------------------

   procedure Set_Allocation
     (Widget : access Gtk_Widget_Record'Class; Alloc : Gtk_Allocation)
   is
      procedure Internal (Widget : System.Address; Alloc : Gtk_Allocation);
      pragma Import (C, Internal, "ada_gtk_widget_set_allocation");
   begin
      Internal (Get_Object (Widget), Alloc);
   end Set_Allocation;

   -----------------
   -- Set_Realize --
   -----------------

   package body Realize_Handling is

      procedure Internal_Realize (Widget : System.Address);
      --  The wrapper passed to Gtk+.
      pragma Convention (C, Internal_Realize);

      procedure Internal_Realize (Widget : System.Address) is
         Dummy : Widget_Type;
         pragma Warnings (Off, Dummy);
      begin
         Realize_Proc (Widget_Type (Get_User_Data (Widget, Dummy).all)'Access);
      end Internal_Realize;

      procedure Set_Realize (Widget : access Gtk_Widget_Record'Class) is
         procedure Internal
           (Widget : System.Address; Realize : System.Address);
         pragma Import (C, Internal, "ada_widget_set_realize");

      begin
         Internal (Get_Object (Widget), Internal_Realize'Address);
      end Set_Realize;

   end Realize_Handling;

   -----------------------------------
   -- Set_Scroll_Adjustments_Signal --
   -----------------------------------

   procedure Set_Scroll_Adjustments_Signal
     (Widget : GObject_Class; Signal : String)
   is
      procedure Internal (Widget : GObject_Class; Signal : String);
      pragma Import (C, Internal, "ada_widget_set_scroll_adjustments_signal");

   begin
      Internal (Widget, Signal & ASCII.NUL);
   end Set_Scroll_Adjustments_Signal;

   ------------------------
   -- Shape_Combine_Mask --
   ------------------------

   procedure Shape_Combine_Mask
     (Widget     : access Gtk_Widget_Record;
      Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
      Offset_X   : Gint;
      Offset_Y   : Gint)
   is
      procedure Internal
        (Widget     : System.Address;
         Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
         Offset_X   : Gint;
         Offset_Y   : Gint);
      pragma Import (C, Internal, "gtk_widget_shape_combine_mask");

   begin
      Internal (Get_Object (Widget), Shape_Mask, Offset_X, Offset_Y);
   end Shape_Combine_Mask;

   ---------------------
   -- Toplevel_Is_Set --
   ---------------------

   function Toplevel_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Toplevel);
   end Toplevel_Is_Set;

   -----------
   -- Unmap --
   -----------

   procedure Unmap (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unmap");

   begin
      Internal (Get_Object (Widget));
   end Unmap;

   --------------
   -- Unparent --
   --------------

   procedure Unparent (Widget : access Gtk_Widget_Record'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_unparent");
   begin
      Internal (Get_Object (Widget));
   end Unparent;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unrealize");

   begin
      Internal (Get_Object (Widget));
   end Unrealize;

   --------------------
   -- Visible_Is_Set --
   --------------------

   function Visible_Is_Set
     (Widget : access Gtk_Widget_Record'Class) return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Visible);
   end Visible_Is_Set;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
     (Widget     : access Gtk_Widget_Record;
      Accel_Path : UTF8_String;
      Group      : Gtk.Accel_Group.Gtk_Accel_Group)
   is
      procedure Internal
        (Widget     : System.Address;
         Accel_Path : Interfaces.C.Strings.chars_ptr;
         Group      : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_accel_path");

      S : Interfaces.C.Strings.chars_ptr;
   begin
      if Accel_Path = "" then
         Internal (Get_Object (Widget), Null_Ptr, Get_Object (Group));
      else
         S := New_String (Accel_Path);
         Internal (Get_Object (Widget), S, Get_Object (Group));
         Free (S);
      end if;
   end Set_Accel_Path;

   -----------------------
   -- Set_Child_Visible --
   -----------------------

   procedure Set_Child_Visible
     (Widget : access Gtk_Widget_Record; Is_Visible : Boolean)
   is
      procedure Internal (Widget : System.Address; Is_Visible : Integer);
      pragma Import (C, Internal, "gtk_widget_set_child_visible");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Is_Visible));
   end Set_Child_Visible;

   -----------------
   -- Child_Focus --
   -----------------

   function Child_Focus
     (Child     : access Gtk_Widget_Record'Class;
      Direction : Gtk.Enums.Gtk_Direction_Type := Gtk.Enums.Dir_Tab_Forward)
      return Boolean
   is
      function Internal
        (Child : System.Address;
         Dir   : Gtk.Enums.Gtk_Direction_Type) return Integer;
      pragma Import (C, Internal, "gtk_widget_child_focus");

   begin
      return Boolean'Val (Internal (Get_Object (Child), Direction));
   end Child_Focus;

   -----------------------
   -- Get_Child_Visible --
   -----------------------

   function Get_Child_Visible
     (Widget : access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_child_visible");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Child_Visible;

end Gtk.Widget;
