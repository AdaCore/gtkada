-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Interfaces.C.Strings;
with Gdk.Visual; use Gdk.Visual;
with Gdk.Color;  use Gdk.Color;

package body Gtk.Widget is

   ------------------------
   -- Accelerator_Signal --
   ------------------------

   function Accelerator_Signal
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type)
     return Guint
   is
      function Internal (Widget : System.Address;
                         Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group;
                         Accel_Key   : Gint;
                         Accel_Mods  : Gint)
        return Guint;
      pragma Import (C, Internal, "gtk_widget_accelerator_signal");

   begin
      return Internal (Get_Object (Widget),
                       Accel_Group,
                       Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                       Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Accelerator_Signal;

   --------------
   -- Activate --
   --------------

   procedure Activate (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");

   begin
      Internal (Get_Object (Widget));
   end Activate;

   ---------------------
   -- Add_Accelerator --
   ---------------------

   procedure Add_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : in String;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type;
      Accel_Flags  : in Gtk.Accel_Group.Gtk_Accel_Flags)
   is
      procedure Internal (Widget : System.Address;
                          Accel_Signal : String;
                          Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group;
                          Accel_Key   : Gint;
                          Accel_Mods  : Gint;
                          Accel_Flags : Gint);
      pragma Import (C, Internal, "gtk_widget_add_accelerator");

   begin
      Internal (Get_Object (Widget), Accel_Signal & ASCII.NUL,
                Accel_Group,
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods),
                Gtk.Accel_Group.Gtk_Accel_Flags'Pos (Accel_Flags));
   end Add_Accelerator;

   ----------------
   -- Add_Events --
   ----------------

   procedure Add_Events (Widget : access Gtk_Widget_Record;
                         Events : in     Gdk.Types.Gdk_Event_Mask)
   is
      procedure Internal (Widget : in System.Address;
                          Events : in Gint);
      pragma Import (C, Internal, "gtk_widget_add_events");

   begin
      Internal (Get_Object (Widget),
                Gdk.Types.Gdk_Event_Mask'Pos (Events));
   end Add_Events;

   ----------------------
   -- Can_Focus_Is_Set --
   ----------------------

   function Can_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Can_Focus);
   end Can_Focus_Is_Set;

   -------------
   -- Convert --
   -------------

   function Convert (W : in Gtk_Widget) return System.Address is
   begin
      return Get_Object (W);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (W : System.Address) return Gtk_Widget is
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget (Get_User_Data (W, Stub));
   end Convert;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Widget);
   end Destroy_Cb;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Widget : access Gtk_Widget_Record;
      Area   : in Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area)
   is
      procedure Internal
        (Widget : in System.Address;
         Area   : in System.Address);
      pragma Import (C, Internal, "gtk_widget_draw");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
      use type Gdk.Rectangle.Gdk_Rectangle;
   begin
      if Rec = Gdk.Rectangle.Full_Area then
         --  Redraw the whole widget
         Internal (Get_Object (Widget), System.Null_Address);
      else
         Internal (Get_Object (Widget), Rec'Address);
      end if;
   end Draw;

   ---------------------
   -- Drawable_Is_Set --
   ---------------------

   function Drawable_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_drawable");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Drawable_Is_Set;

   -----------
   -- Event --
   -----------

   function Event (Widget : access Gtk_Widget_Record'Class;
                   Event  : Gdk.Event.Gdk_Event)
                  return Gint
   is
      function Internal (Widget : System.Address; Event : System.Address)
                        return Gint;
      pragma Import (C, Internal, "gtk_widget_event");

   begin
      return Internal (Get_Object (Widget), Gdk.Event.To_Address (Event));
   end Event;

   ---------------------------
   -- Get_Allocation_Height --
   ---------------------------

   function Get_Allocation_Height (Widget : access Gtk_Widget_Record)
     return Guint
   is
      function Internal (Widget : System.Address) return Guint;
      pragma Import (C, Internal, "ada_widget_allocation_height");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Height;

   --------------------------
   -- Get_Allocation_Width --
   --------------------------

   function Get_Allocation_Width (Widget : access Gtk_Widget_Record)
     return Guint
   is
      function Internal (Widget : System.Address) return Guint;
      pragma Import (C, Internal, "ada_widget_allocation_width");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Width;

   ----------------------
   -- Get_Allocation_X --
   ----------------------

   function Get_Allocation_X (Widget : access Gtk_Widget_Record)
     return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_x");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_X;

   ----------------------
   -- Get_Allocation_Y --
   ----------------------

   function Get_Allocation_Y (Widget : access Gtk_Widget_Record)
     return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_y");

   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Y;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Events (Widget : access Gtk_Widget_Record)
         return Gdk.Types.Gdk_Event_Mask
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_events");
   begin
      return Gdk.Types.Gdk_Event_Mask'Val (Internal (Get_Object (Widget)));
   end Get_Events;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Widget : access Gtk_Widget_Record) return String is
      function Internal (Widget : System.Address)
        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Name;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Widget : access Gtk_Widget_Record) return Gtk_Widget is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_widget_get_parent");
      Stub : Gtk_Widget_Record;
      S    : System.Address;
      use type System.Address;
   begin
      S := Internal (Get_Object (Widget));
      if S = System.Null_Address then
         return null;
      else
         return Gtk_Widget (Get_User_Data (S, Stub));
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

   function Get_Toplevel (Widget : access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_toplevel");
      Stub : Gtk_Widget_Record;

   begin
      return Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Toplevel;

   ----------------
   -- Set_Window --
   ----------------

   procedure Set_Window
     (Widget : access Gtk_Widget_Record;
      Window : in Gdk.Window.Gdk_Window)
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

   function Get_Window (Widget : access Gtk_Widget_Record)
                       return Gdk.Window.Gdk_Window
   is
      function Internal (Widget : System.Address)
                        return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "ada_widget_get_window");
   begin
      return Internal (Get_Object (Widget));
   end Get_Window;

   ------------------
   -- Grab_Default --
   ------------------

   procedure Grab_Default (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_default");

   begin
      Internal (Get_Object (Widget));
   end Grab_Default;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_focus");

   begin
      Internal (Get_Object (Widget));
   end Grab_Focus;

   ------------------
   -- Get_Colormap --
   ------------------

   function Get_Colormap (Widget : access Gtk_Widget_Record)
                         return Gdk.Color.Gdk_Colormap
   is
      function Internal (Widget : in System.Address)
                        return Gdk.Color.Gdk_Colormap;
      pragma Import (C, Internal, "gtk_widget_get_colormap");
   begin
      return Internal (Get_Object (Widget));
   end Get_Colormap;

   ---------------------------------
   -- Default_Motion_Notify_Event --
   ---------------------------------

   function Default_Motion_Notify_Event
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event)
     return Gint
   is
      function Internal (Widget : in System.Address;
                         Event  : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_get_motion_notify");
   begin
      return Internal (Get_Object (Widget), Gdk.Event.To_Address (Event));
   end Default_Motion_Notify_Event;

   ---------------------------------------
   -- Has_Default_Motion_Notify_Handler --
   ---------------------------------------

   function Has_Default_Motion_Notify_Handler
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_has_default_motion_notify");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Default_Motion_Notify_Handler;

   ---------------------------
   -- Get_Child_Requisition --
   ---------------------------

   function Get_Child_Requisition
     (Widget : access Gtk_Widget_Record) return Gtk_Requisition
   is
      procedure Internal (Widget : System.Address;
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

   function Get_Visual (Widget : access Gtk_Widget_Record) return Gdk_Visual is
      function Internal (Widget : System.Address) return Gdk_Visual;
      pragma Import (C, Internal, "gtk_widget_get_visual");
   begin
      return Internal (Get_Object (Widget));
   end Get_Visual;

   ------------------------
   -- Has_Default_Is_Set --
   ------------------------

   function Has_Default_Is_Set (Widget : access Gtk_Widget_Record'Class)
                               return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Default);
   end Has_Default_Is_Set;

   ----------------------
   -- Has_Focus_Is_Set --
   ----------------------

   function Has_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Focus);
   end Has_Focus_Is_Set;

   ---------------------
   -- Has_Grab_Is_Set --
   ---------------------

   function Has_Grab_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Has_Grab);
   end Has_Grab_Is_Set;

   ----------
   -- Hide --
   ----------

   procedure Hide (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");

   begin
      Internal (Get_Object (Widget));
   end Hide;

   -----------------------
   -- Initialize_Widget --
   -----------------------

   procedure Initialize_Widget (Widget : access Gtk_Widget_Record'Class) is
      --  XXX function Internal
      --    (The_Type : in Gtk_Type;
      --     Nargs    : in Guint;
      --     Args     : System.Address) return System.Address;
      --  pragma Import (C, Internal, "gtk_widget_newv");
   begin
      raise Program_Error;
      --  Set_Object (Widget, Internal (Get_Type, 0, System.Null_Address));
      --  Initialize_User_Data (Widget);
   end Initialize_Widget;

   ---------------
   -- Intersect --
   ---------------

   function Intersect (Widget       : access Gtk_Widget_Record;
                       Area         : Gdk.Rectangle.Gdk_Rectangle;
                       Intersection : access Gdk.Rectangle.Gdk_Rectangle)
                      return Boolean
   is
      function Internal
        (Widget : System.Address;
         Area   : access Gdk.Rectangle.Gdk_Rectangle;
         Inter  : access Gdk.Rectangle.Gdk_Rectangle) return Gint;
      pragma Import (C, Internal, "gtk_widget_intersect");

      Area_Local : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
      Result     : Gint;

   begin
      Result := Internal
        (Get_Object (Widget), Area_Local'Access, Intersection);
      return Boolean'Val (Result);
   end Intersect;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (Widget   : access Gtk_Widget_Record;
                         Ancestor : access Gtk_Widget_Record'Class)
                         return Boolean
   is
      function Internal (Widget : System.Address; Ancestor : System.Address)
                        return Gint;
      pragma Import (C, Internal, "gtk_widget_is_ancestor");
   begin
      return Boolean'Val (Internal (Get_Object (Widget),
                                    Get_Object (Ancestor)));
   end Is_Ancestor;

   -----------------------
   -- Lock_Accelerators --
   -----------------------

   procedure Lock_Accelerators (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_lock_accelerators");

   begin
      Internal (Get_Object (Widget));
   end Lock_Accelerators;

   ---------
   -- Map --
   ---------

   procedure Map (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_map");

   begin
      Internal (Get_Object (Widget));
   end Map;

   -------------------
   -- Mapped_Is_Set --
   -------------------

   function Mapped_Is_Set (Widget : access Gtk_Widget_Record'Class)
                          return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Mapped);
   end Mapped_Is_Set;

   ----------------------
   -- No_Window_Is_Set --
   ----------------------

   function No_Window_Is_Set (Widget : access Gtk_Widget_Record'Class)
                             return Boolean is
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

   procedure Queue_Draw_Area (Widget : access Gtk_Widget_Record;
                              X      : Gint;
                              Y      : Gint;
                              Width  : Gint;
                              Height : Gint)
   is
      procedure Internal (Widget : System.Address;
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

   procedure Queue_Clear_Area (Widget : access Gtk_Widget_Record;
                               X      : Gint;
                               Y      : Gint;
                               Width  : Gint;
                               Height : Gint)
   is
      procedure Internal (Widget : System.Address;
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
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_realize");

   begin
      Internal (Get_Object (Widget));
   end Realize;

   ---------------------
   -- Realized_Is_Set --
   ---------------------

   function Realized_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Realized);
   end Realized_Is_Set;

   ------------------------
   -- Remove_Accelerator --
   ------------------------

   procedure Remove_Accelerator
     (Widget       : access Gtk_Widget_Record;
      Accel_Group  : in Gtk.Accel_Group.Gtk_Accel_Group;
      Accel_Key    : in Gdk.Types.Gdk_Key_Type;
      Accel_Mods   : in Gdk.Types.Gdk_Modifier_Type)
   is
      procedure Internal (Widget : System.Address;
                          Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group;
                          Accel_Key   : Gint;
                          Accel_Mods  : Gint);
      pragma Import (C, Internal, "gtk_widget_remove_accelerator");

   begin
      Internal (Get_Object (Widget),
                Accel_Group,
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Remove_Accelerator;

   -------------------------
   -- Remove_Accelerators --
   -------------------------

   procedure Remove_Accelerators
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : in String;
      Visible_Only : in Boolean := True)
   is
      procedure Internal (Widget : System.Address;
                          Accel_Signal : String;
                          Visible_Only : Gint);
      pragma Import (C, Internal, "gtk_widget_remove_accelerators");

   begin
      Internal (Get_Object (Widget),
                Accel_Signal & ASCII.NUL,
                Boolean'Pos (Visible_Only));
   end Remove_Accelerators;

   --------------
   -- Reparent --
   --------------

   procedure Reparent (Widget : access Gtk_Widget_Record;
                       New_Parent : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget, New_Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_reparent");

   begin
      Internal (Get_Object (Widget), Get_Object (New_Parent));
   end Reparent;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive (Widget : access Gtk_Widget_Record'Class)
                         return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_is_sensitive");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Is_Sensitive;

   -----------------------
   -- Set_App_Paintable --
   -----------------------

   procedure Set_App_Paintable (Widget        : access Gtk_Widget_Record;
                                App_Paintable : Boolean)
   is
      procedure Internal (Widget        : System.Address;
                          App_Paintable : Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_app_paintable");
   begin
      Internal (Get_Object (Widget),
                Gboolean'Val (Boolean'Pos (App_Paintable)));
   end Set_App_Paintable;

   ------------------
   -- Set_Colormap --
   ------------------

   procedure Set_Colormap (Widget : access Gtk_Widget_Record;
                           Cmap : Gdk_Colormap)
   is
      procedure Internal (Widget : System.Address; Cmap : Gdk_Colormap);
      pragma Import (C, Internal, "gtk_widget_set_colormap");

   begin
      Internal (Get_Object (Widget), Cmap);
   end Set_Colormap;

   ----------------
   -- Set_Events --
   ----------------

   procedure Set_Events (Widget : access Gtk_Widget_Record;
                         Events : in     Gdk.Types.Gdk_Event_Mask)
   is
      procedure Internal (Widget : in System.Address;
                          Events : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_events");

   begin
      Internal (Get_Object (Widget),
                Gdk.Types.Gdk_Event_Mask'Pos (Events));
   end Set_Events;

   --------------------------
   -- Set_Extension_Events --
   --------------------------

   procedure Set_Extension_Events
     (Widget : access Gtk_Widget_Record;
      Mode   : in     Gdk.Types.Gdk_Extension_Mode)
   is
      procedure Internal (Widget : in System.Address;
                          Mode   : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_extension_events");

   begin
      Internal (Get_Object (Widget),
                Gdk.Types.Gdk_Extension_Mode'Pos (Mode));
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

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Widget : access Gtk_Widget_Record) return Gtk.Style.Gtk_Style
   is
      function Internal
        (Widget : System.Address) return Gtk.Style.Gtk_Style;
      pragma Import (C, Internal, "gtk_widget_get_style");

   begin
      return Internal (Get_Object (Widget));
   end Get_Style;

   ------------------
   -- Modify_Style --
   ------------------

   procedure Modify_Style
     (Widget : access Gtk_Widget_Record;
      Style  : Gtk_Rc_Style)
   is
      procedure Internal (Widget : System.Address; Style : Gtk_Rc_Style);
      pragma Import (C, Internal, "gtk_widget_modify_style");

   begin
      Internal (Get_Object (Widget), Style);
   end Modify_Style;

   --------------------------
   -- Get_Extension_Events --
   --------------------------

   function Get_Extension_Events (Widget : access Gtk_Widget_Record)
                                 return Gdk.Types.Gdk_Extension_Mode
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_extension_events");
   begin
      return Gdk.Types.Gdk_Extension_Mode'Val (Internal (Get_Object (Widget)));
   end Get_Extension_Events;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor (Widget        : access Gtk_Widget_Record;
                          Ancestor_Type : in Gtk_Type)
                         return Gtk_Widget
   is
      function Internal (Widget        : System.Address;
                         Ancestor_Type : Gtk_Type)
                        return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_ancestor");
      S : System.Address;
      use type System.Address;
      Stub : Gtk_Widget_Record;
   begin
      S := Internal (Get_Object (Widget), Ancestor_Type);
      if S = System.Null_Address then
         return null;
      else
         return Gtk_Widget (Get_User_Data (S, Stub));
      end if;
   end Get_Ancestor;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Widget : access Gtk_Widget_Record; Name : in String) is
      procedure Internal (Widget : in System.Address;
                          Name : in String);
      pragma Import (C, Internal, "gtk_widget_set_name");

   begin
      Internal (Get_Object (Widget), Name & ASCII.NUL);
   end Set_Name;

   ------------------
   --  Set_Parent  --
   ------------------

   procedure Set_Parent (Widget : access Gtk_Widget_Record;
                         Parent : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget, Parent : in System.Address);
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
      procedure Internal (Widget : in System.Address; Parent : Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_set_parent_window");
   begin
      Internal (Get_Object (Widget), Window);
   end Set_Parent_Window;

   ------------------
   -- Set_Rc_Style --
   ------------------

   procedure Set_Rc_Style (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_rc_style");
   begin
      Internal (Get_Object (Widget));
   end Set_Rc_Style;

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
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_restore_default_style");
   begin
      Internal (Get_Object (Widget));
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
      procedure Internal (Widget : System.Address;
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

   ---------------------
   --  Set_Sensitive  --
   ---------------------

   procedure Set_Sensitive (Widget    : access Gtk_Widget_Record;
                            Sensitive : in Boolean := True)
   is
      procedure Internal (Widget : in System.Address; Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");

   begin
      Internal (Get_Object (Widget), To_Gint (Sensitive));
   end Set_Sensitive;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State (Widget : access Gtk_Widget_Record;
                        State : in Enums.Gtk_State_Type)
   is
      procedure Internal (Widget : System.Address; State : Gint);
      pragma Import (C, Internal, "gtk_widget_set_state");

   begin
      Internal (Get_Object (Widget), Enums.Gtk_State_Type'Pos (State));
   end Set_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Widget : access Gtk_Widget_Record)
     return Enums.Gtk_State_Type
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_get_state");

   begin
      return Enums.Gtk_State_Type'Val (Internal (Get_Object (Widget)));
   end Get_State;

   -------------------
   -- Set_UPosition --
   -------------------

   procedure Set_UPosition (Widget : access Gtk_Widget_Record;
                            X, Y : in Gint)
   is
      procedure Internal (Widget : System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_uposition");

   begin
      Internal (Get_Object (Widget), X, Y);
   end Set_UPosition;

   ---------------
   -- Set_USize --
   ---------------

   procedure Set_USize (Widget : access Gtk_Widget_Record;
                        Width, Height : Gint)
   is
      procedure Internal (Widget : System.Address; Width, Height : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_usize");

   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_USize;

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

   procedure Size_Request (Widget      : access Gtk_Widget_Record;
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
      Allocation : in out Gtk_Allocation)
   is
      procedure Internal
        (Widget : System.Address; Allocation : in out Gtk_Allocation);
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
   -- Set_Realize --
   -----------------

   package body Realize_Handling is

      procedure Internal_Realize (Widget : System.Address);
      --  The wrapper passed to Gtk+.
      pragma Convention (C, Internal_Realize);

      procedure Internal_Realize (Widget : System.Address) is
         Dummy : Widget_Type;
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

   ------------------------
   -- Shape_Combine_Mask --
   ------------------------

   procedure Shape_Combine_Mask
     (Widget     : access Gtk_Widget_Record;
      Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
      Offset_X   : Gint;
      Offset_Y   : Gint)
   is
      procedure Internal (Widget     : System.Address;
                          Shape_Mask : Gdk.Bitmap.Gdk_Bitmap;
                          Offset_X   : Gint;
                          Offset_Y   : Gint);
      pragma Import (C, Internal, "gtk_widget_shape_combine_mask");
   begin
      Internal (Get_Object (Widget),
                Shape_Mask,
                Offset_X,
                Offset_Y);
   end Shape_Combine_Mask;

   ---------------------
   -- Toplevel_Is_Set --
   ---------------------

   function Toplevel_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Toplevel);
   end Toplevel_Is_Set;

   -------------------------
   -- Unlock_Accelerators --
   -------------------------

   procedure Unlock_Accelerators (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_unlock_accelerators");

   begin
      Internal (Get_Object (Widget));
   end Unlock_Accelerators;

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

end Gtk.Widget;
