-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Gtk.Box;              use Gtk.Box;
with Gtk.Util;             use Gtk.Util;
with Gtk.Fixed;            use Gtk.Fixed;
with Gtk.Layout;           use Gtk.Layout;
with Gtk.Packer;           use Gtk.Packer;
with Gtk.Container;        use Gtk.Container;
with Gtk.Toolbar;          use Gtk.Toolbar;
with Interfaces.C.Strings;
with Gtk.Table;            use Gtk.Table;
with Ada.Strings.Fixed;    use Ada.Strings.Fixed;
with Ada.Strings.Maps;     use Ada.Strings.Maps;
with Gdk.Visual;           use Gdk.Visual;
with Gdk.Color;            use Gdk.Color;

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
      Internal (Get_Object (Widget), Accel_Signal & ASCII.Nul,
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
      procedure Internal (Widget : in System.Address;
                          Area   : in System.Address);
      pragma Import (C, Internal, "gtk_widget_draw");

      Rec : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
      use type Gdk.Rectangle.Gdk_Rectangle;
   begin
      if Rec = Gdk.Rectangle.Full_Area then
         --  Redraw the whole widget
         Internal (Get_Object (Widget), System.Null_Address);
      else
         Internal (Get_Object (Widget), Rec'Address);
      end if;
   end Draw;

   ----------------
   -- Draw_Focus --
   ----------------

   procedure Draw_Focus (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_draw_focus");
   begin
      Internal (Get_Object (Widget));
   end Draw_Focus;

   ------------------
   -- Draw_Default --
   ------------------

   procedure Draw_Default (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_draw_default");
   begin
      Internal (Get_Object (Widget));
   end Draw_Default;

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

   procedure Get_Pointer (Widget : access Gtk_Widget_Record;
                          X      : out Gint;
                          Y      : out Gint)
   is
      procedure Internal (Widget : System.Address;
                          X      : System.Address;
                          Y      : System.Address);
      pragma Import (C, Internal, "gtk_widget_get_pointer");
      X1 : aliased Gint;
      Y1 : aliased Gint;
   begin
      Internal (Get_Object (Widget), X1'Address, Y1'Address);
      X := X1;
      Y := Y1;
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

   ---------------------------
   -- Get_Child_Requisition --
   ---------------------------

   function Get_Child_Requisition (Widget : access Gtk_Widget_Record)
                            return Gtk_Requisition
   is
      procedure Internal (Widget : System.Address;
                          Req    : System.Address);
      pragma Import (C, Internal, "gtk_widget_get_child_requisition");
      Req : aliased Gtk_Requisition;
   begin
      Internal (Get_Object (Widget), Req'Address);
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
      function Internal
        (The_Type : in Gtk_Type;
         Nargs    : in Guint;
         Args     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_newv");
   begin
      Set_Object (Widget, Internal (Get_Type, 0, System.Null_Address));
      Initialize_User_Data (Widget);
   end Initialize_Widget;

   ---------------
   -- Intersect --
   ---------------

   function Intersect (Widget       : access Gtk_Widget_Record;
                       Area         : Gdk.Rectangle.Gdk_Rectangle;
                       Intersection : access Gdk.Rectangle.Gdk_Rectangle)
                      return Boolean
   is
      function Internal (Widget : System.Address;
                         Area   : System.Address;
                         Inter  : System.Address)
                        return Gint;
      pragma Import (C, Internal, "gtk_widget_intersect");
      Area_Local : aliased Gdk.Rectangle.Gdk_Rectangle := Area;
      Area_Out   : aliased Gdk.Rectangle.Gdk_Rectangle;
      Result     : Gint;
   begin
      Result := Internal (Get_Object (Widget), Area_Local'Address,
                          Area_Out'Address);
      Intersection.all := Area_Out;
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

   -----------
   -- Popup --
   -----------

   procedure Popup (Widget : access Gtk_Widget_Record; X, Y : in Gint) is
      procedure Internal (Widget : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_popup");

   begin
      Internal (Get_Object (Widget), X, Y);
   end Popup;

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

   function Rc_Style_Is_Set (Widget : access Gtk_Widget_Record'Class)
                            return Boolean is
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
                Accel_Signal & ASCII.Nul,
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
      Internal (Get_Object (Widget), Name & ASCII.Nul);
   end Set_Name;

   ------------------
   --  Set_Parent  --
   ------------------

   procedure Set_Parent (Widget : access Gtk_Widget_Record;
                         Parent : in     Gtk_Widget)
   is
      procedure Internal (Widget, Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_set_parent");

   begin
      Internal (Get_Object (Widget), Get_Object (Parent));
   end Set_Parent;

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

   ----------------
   -- Set_Visual --
   ----------------

   procedure Set_Visual (Widget : access Gtk_Widget_Record;
                         Visual : Gdk_Visual)
   is
      procedure Internal (Widget : System.Address; Visual : Gdk_Visual);
      pragma Import (C, Internal, "gtk_widget_set_visual");

   begin
      Internal (Get_Object (Widget), Visual);
   end Set_Visual;

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
      procedure Internal (Widget      : System.Address;
                          Requisition : System.Address);
      pragma Import (C, Internal, "gtk_widget_size_request");
      Req : aliased Gtk_Requisition := Requisition;
   begin
      Internal (Get_Object (Widget), Req'Address);
      Requisition := Req;
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate (Widget     : access Gtk_Widget_Record;
                            Allocation : in out Gtk_Allocation)
   is
      procedure Internal (Widget      : System.Address;
                          Allocation : System.Address);
      pragma Import (C, Internal, "gtk_widget_size_allocate");
      Alloc : aliased Gtk_Allocation := Allocation;
   begin
      Internal (Get_Object (Widget), Alloc'Address);
      Allocation := Alloc;
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

   function Visible_Is_Set (Widget : access Gtk_Widget_Record'Class)
                           return Boolean is
   begin
      return Gtk.Widget.Flag_Is_Set (Widget, Visible);
   end Visible_Is_Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
      Child       : Node_Ptr := Find_Tag (N.Child, "child");
      Q           : Node_Ptr;
      Top         : constant Node_Ptr   := Find_Top_Widget (N);
      Top_Name    : constant String_Ptr := Get_Field (Top, "name");
      Cur         : constant String_Ptr := Get_Field (N, "name");
      S           : String_Ptr;
      Flag_Set    : Boolean;
      Use_Default : Boolean;
      First       : Natural;
      Last        : Natural;
      The_First   : Natural;

   begin
      Object.Generate (N, File);

      S := Get_Field (Find_Child (Top.Parent, "project"), "use_widget_names");

      if S /= null and then Boolean'Value (S.all) then
         Gen_Set (N, "Widget", "name", File, Delim => '"');
      end if;

      Gen_Set (N, "Widget", "sensitive", File);
      Gen_Set (N, "Widget", "UPosition", "x", "y", "", "", File);
      Gen_Set (N, "Widget", "USize", "width", "height", "", "", File);
      Gen_Set (N, "Widget", "state", File);
      Gen_Set (N, "Widget", "extension_events", File);

      S := Get_Field (N, "can_default");

      if S /= null and then Boolean'Value (S.all) then
         Add_Package ("Object");
         Put (File, "   Set_Flags (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ", Can_Default);");
      end if;

      S := Get_Field (N, "has_focus");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Focus (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ");");
      end if;

      S := Get_Field (N, "has_default");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Default (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ");");
      end if;

      S := Get_Field (N, "events");

      if S /= null then
         Put (File, "   Set_Events (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put (File, To_Ada (Cur.all) & ", ");

         Flag_Set := False;
         The_First := S'First;

         loop
            Find_Token (S (The_First .. S'Last), To_Set (" |"),
              Ada.Strings.Inside, First, Last);

            exit when Last = 0;

            if Flag_Set then
               Put_Line (File, " or");
            else
               New_Line (File);
               Flag_Set := True;
            end if;

            Put (File, "     " & To_Ada (S (The_First + 4 .. First - 1)));
            The_First := Last + 1;
         end loop;

         if The_First /= S'Last then
            if Flag_Set then
               Put_Line (File, " or");
            else
               New_Line (File);
            end if;

            Put (File, "     " & To_Ada (S (The_First + 4 .. S'Last)));

         elsif not Flag_Set then
            Put (File, "0");
         end if;

         Put_Line (File, ");");
      end if;

      --  ??? Need to find a better way to call Pack_Start

      if Child /= null then
         Q := Find_Tag (Child.Child, "pack");

         if Q = null or else Q.Value.all = "GTK_PACK_START" then
            if Get_Field (Child, "fill") /= null then

               --  This widget is part of a Gtk_Box

               Gen_Call_Child (N, Child, "Box", "Pack_Start",
                 "expand", "fill", "padding", File);
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "left_attach") /= null then

               --  This widget is part of a Gtk_Table

               Add_Package ("Table");
               Put_Line (File, "   Attach (" &
                 To_Ada (Top_Name.all) & "." &
                 To_Ada (Find_Tag
                   (Find_Parent (N.Parent, "Table"), "name").Value.all) &
                 ", " & To_Ada (Top_Name.all) & "." &
                 To_Ada (Cur.all) &
                 ", " & Get_Field (Child, "left_attach").all &
                 ", " & Get_Field (Child, "right_attach").all &
                 ", " & Get_Field (Child, "top_attach").all &
                 ", " & Get_Field (Child, "bottom_attach").all & ",");

               Put (File, "     ");

               Flag_Set := False;

               if Boolean'Value (Get_Field (Child, "xexpand").all) then
                  Put (File, "Expand");
                  Flag_Set := True;
               end if;

               if Boolean'Value (Get_Field (Child, "xshrink").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Shrink");
               end if;

               if Boolean'Value (Get_Field (Child, "xfill").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Fill");
               end if;

               if not Flag_Set then
                  Put (File, "0");
               end if;

               Put (File, ", ");

               Flag_Set := False;

               if Boolean'Value (Get_Field (Child, "yexpand").all) then
                  Put (File, "Expand");
                  Flag_Set := True;
               end if;

               if Boolean'Value (Get_Field (Child, "yshrink").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Shrink");
               end if;

               if Boolean'Value (Get_Field (Child, "yfill").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Fill");
               end if;

               if not Flag_Set then
                  Put_Line (File, "0,");
               else
                  Put_Line (File, ",");
               end if;

               Put_Line (File, "     " & Get_Field (Child, "xpad").all & ", " &
                 Get_Field (Child, "ypad").all & ");");
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "side") /= null then

               --  This widget is part of a packer

               Add_Package ("Packer");
               S := Get_Field (Child, "use_default");
               Use_Default := S /= null and then Boolean'Value (S.all);

               if Use_Default then
                  Put (File, "   Add_Defaults (");
               else
                  Put (File, "   Add (");
               end if;

               Put_Line (File,
                 To_Ada (Top_Name.all) & "." &
                 To_Ada (Find_Tag
                   (Find_Parent (N.Parent, "Packer"), "name").Value.all) &
                 ", " & To_Ada (Top_Name.all) & "." &
                 To_Ada (Cur.all) &
                 ", " & To_Ada (Get_Field (Child, "side").all) &
                 ", " & To_Ada (Get_Field (Child, "anchor").all) & ",");

               Put (File, "     ");

               Flag_Set := False;
               S := Get_Field (Child, "expand");

               if S /= null and then S.all = "True" then
                  Flag_Set := True;
                  Put (File, "Gtk_Pack_Expand");
               end if;

               S := Get_Field (Child, "xfill");

               if S /= null and then S.all = "True" then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Gtk_Fill_X");
               end if;

               S := Get_Field (Child, "yfill");

               if S /= null and then S.all = "True" then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Gtk_Fill_Y");
               end if;

               if not Flag_Set then
                  Put (File, "0");
               end if;

               if not Use_Default then
                  Put_Line (File, ",");
                  Put (File,
                    "     " & Get_Field (Child, "border_width").all &
                    ", " & Get_Field (Child, "xpad").all &
                    ", " & Get_Field (Child, "ypad").all &
                    ", " & Get_Field (Child, "xipad").all &
                    ", " & Get_Field (Child, "yipad").all);
               end if;

               Put_Line (File, ");");
               N.Specific_Data.Has_Container := True;
            end if;
         end if;
      end if;

      Q := Find_Tag (N.Child, "accelerator");

      if Q /= null then
         if not Top.Specific_Data.Has_Accel_Group then
            Add_Package ("Accel_Group");
            Put_Line (File, "   Gtk_New (The_Accel_Group);");
            Put_Line (File, "   Add_Accel_Group (" &
              To_Ada (Top_Name.all) & ", The_Accel_Group);");
            Top.Specific_Data.Has_Accel_Group := True;
         end if;

         Put_Line (File, "   Add_Accelerator (" &
            To_Ada (Top_Name.all) & "." &
            To_Ada (Cur.all) & ", """ &
            Get_Field (Q, "signal").all & """,");
         Add_Package ("Gdk.Types.Keysyms");
         S := Get_Field (Q, "modifiers");
         Put (File, "     The_Accel_Group, " & Get_Field (Q, "key").all);

         if S'Length > 4 and then S (S'First .. S'First + 3) = "GDK_" then
            Put_Line (File, ", Gdk.Types." & To_Ada (S.all) &
              ", Accel_Visible);");
         else
            Put_Line (File, ", " & S.all & ", Accel_Visible);");
         end if;
      end if;

      Gen_Signal (N, File);

      if Find_Tag (N.Child, "child_name") = null then
         if not N.Specific_Data.Has_Container then
            S := Get_Field (N.Parent, "class");

            if S /= null then
               if S.all = "GtkFixed" then
                  Gen_Call_Child (N, N, "Fixed",
                    "Put", "x", "y", File => File);

               elsif S.all = "GtkLayout" then
                  Gen_Call_Child (N, N, "Layout",
                    "Put", "x", "y", File => File);

               elsif S.all = "GtkToolbar" then
                  --  ??? Need to handle tooltip
                  Gen_Call_Child (N, null, "Toolbar",
                    "Append_Widget", File => File);

               else
                  Gen_Call_Child (N, null, "Container", "Add", File => File);
               end if;
            end if;

            N.Specific_Data.Has_Container := True;
         end if;
      end if;
   end Generate;

   procedure Generate (Widget : in out Object.Gtk_Object;
                       N      : in Node_Ptr) is
      S, S2, S3   : String_Ptr;
      Child       : Node_Ptr := Find_Tag (N.Child, "child");
      Q           : Node_Ptr;
      Func        : Callback;
      Data        : System.Address;
      Events      : Gdk.Types.Gdk_Event_Mask;
      Options     : Gtk_Packer_Options;
      Use_Default : Boolean;

      procedure Signal_Connect
        (Object        : System.Address;
         Name          : String;
         Func          : Callback;
         Func_Data     : System.Address);
      pragma Import (C, Signal_Connect, "gtk_signal_connect");

      function Decode_Events (S : String) return Gdk.Types.Gdk_Event_Mask;

      function Decode_Events (S : String) return Gdk.Types.Gdk_Event_Mask is
         use Gdk.Types;

         Events    : Gdk_Event_Mask := Null_Event_Mask;
         First     : Natural;
         Last      : Natural;
         The_First : Natural;

         type Gdk_Event_Mask_Enum is
           (Gdk_Null_Event_Mask,
            Gdk_Exposure_Mask,
            Gdk_Pointer_Motion_Mask,
            Gdk_Pointer_Motion_Hint_Mask,
            Gdk_Button_Motion_Mask,
            Gdk_Button1_Motion_Mask,
            Gdk_Button2_Motion_Mask,
            Gdk_Button3_Motion_Mask,
            Gdk_Button_Press_Mask,
            Gdk_Button_Release_Mask,
            Gdk_Key_Press_Mask,
            Gdk_Key_Release_Mask,
            Gdk_Enter_Notify_Mask,
            Gdk_Leave_Notify_Mask,
            Gdk_Focus_Change_Mask,
            Gdk_Structure_Mask,
            Gdk_Property_Change_Mask,
            Gdk_Visibility_Notify_Mask,
            Gdk_Proximity_In_Mask,
            Gdk_Proximity_Out_Mask);
         pragma Warnings (Off, Gdk_Event_Mask_Enum);

      begin
         The_First := S'First;

         loop
            Find_Token (S (The_First .. S'Last), To_Set (" |"),
              Ada.Strings.Inside, First, Last);

            exit when Last = 0;

            Events := Events or
              2 ** Gdk_Event_Mask_Enum'Pos
                (Gdk_Event_Mask_Enum'Value (S (The_First .. First - 1)));
            The_First := Last + 1;
         end loop;

         if The_First /= S'Last then
            Events := Events or
              2 ** Gdk_Event_Mask_Enum'Pos
                (Gdk_Event_Mask_Enum'Value (S (The_First .. S'Last)));
         end if;

         return Events;
      end Decode_Events;

      use Object;
      use Enums;

   begin
      Object.Generate (Widget, N);

      S := Get_Field (N, "sensitive");

      if S /= null then
         Set_Sensitive (Gtk_Widget (Widget), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "x");
      S2 := Get_Field (N, "y");

      if S /= null and then S2 /= null then
         Set_UPosition
           (Gtk_Widget (Widget), Gint'Value (S.all), Gint'Value (S2.all));
      end if;

      S := Get_Field (N, "width");
      S2 := Get_Field (N, "height");

      if S /= null and then S2 /= null then
         Set_USize
           (Gtk_Widget (Widget), Gint'Value (S.all), Gint'Value (S2.all));
      end if;

      S := Get_Field (N, "state");

      if S /= null then
         Set_State (Gtk_Widget (Widget), Enums.Gtk_State_Type'Value (S.all));
      end if;

      S := Get_Field (N, "events");

      if S /= null then
         Events := Decode_Events (S.all);
         Set_Events (Gtk_Widget (Widget), Events);
      end if;

      S := Get_Field (N, "extension_events");

      if S /= null then
         Set_Extension_Events (Gtk_Widget (Widget),
           Gdk.Types.Gdk_Extension_Mode'Value (S (S'First + 4 .. S'Last)));
      end if;

      S := Get_Field (N, "can_default");

      if S /= null and then Boolean'Value (S.all) then
         Set_Flags (Widget, Can_Default);
      end if;

      S := Get_Field (N, "has_focus");

      if S /= null and then Boolean'Value (S.all) then
         Grab_Focus (Gtk_Widget (Widget));
      end if;

      S := Get_Field (N, "has_default");

      if S /= null and then Boolean'Value (S.all) then
         Grab_Default (Gtk_Widget (Widget));
      end if;

      --  ??? Need to find a better way to call Pack_Start

      if Child /= null then
         Q := Find_Tag (Child.Child, "pack");
         S := Get_Field (Child, "expand");
         S2 := Get_Field (Child, "fill");
         S3 := Get_Field (Child, "padding");

         if Q = null or else Q.Value.all = "GTK_PACK_START" then
            if S /= null and then S2 /= null and then S3 /= null then

               --  This widget is part of a Gtk_Box

               Gtk.Box.Pack_Start
                 (Gtk_Box (Get_Object (Find_Tag
                   (Find_Parent (N.Parent, "Box"), "name").Value)),
                  Gtk_Widget (Get_Object (Get_Field (N, "name"))),
                  Boolean'Value (S.all), Boolean'Value (S2.all),
                  Gint'Value (S3.all));
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "left_attach") /= null then

               --  This widget is part of a Gtk_Table

               declare
                  Xoptions, Yoptions : Gtk_Attach_Options := 0;
               begin
                  if Boolean'Value (Get_Field (Child, "xexpand").all) then
                     Xoptions := Expand;
                  end if;

                  if Boolean'Value (Get_Field (Child, "xshrink").all) then
                     Xoptions := Xoptions or Shrink;
                  end if;

                  if Boolean'Value (Get_Field (Child, "xfill").all) then
                     Xoptions := Xoptions or Fill;
                  end if;

                  if Boolean'Value (Get_Field (Child, "yexpand").all) then
                     Yoptions := Expand;
                  end if;

                  if Boolean'Value (Get_Field (Child, "yshrink").all) then
                     Yoptions := Yoptions or Shrink;
                  end if;

                  if Boolean'Value (Get_Field (Child, "yfill").all) then
                     Yoptions := Yoptions or Fill;
                  end if;

                  Gtk.Table.Attach (Gtk_Table (Get_Object (Find_Tag
                    (Find_Parent (N.Parent, "Table"), "name").Value)),
                     Gtk_Widget (Get_Object (Get_Field (N, "name"))),
                     Guint'Value (Get_Field (Child, "left_attach").all),
                     Guint'Value (Get_Field (Child, "right_attach").all),
                     Guint'Value (Get_Field (Child, "top_attach").all),
                     Guint'Value (Get_Field (Child, "bottom_attach").all),
                     Xoptions, Yoptions,
                     Guint'Value (Get_Field (Child, "xpad").all),
                     Guint'Value (Get_Field (Child, "ypad").all));
                  N.Specific_Data.Has_Container := True;
               exception
                  when Constraint_Error =>
                     null;
               end;

            elsif Get_Field (Child, "side") /= null then

               --  This widget is part of a packer

               S := Get_Field (Child, "use_default");
               Use_Default := S /= null and then Boolean'Value (S.all);
               Options := 0;

               if Boolean'Value (Get_Field (Child, "expand").all) then
                  Options := Options or Gtk_Pack_Expand;
               end if;

               if Boolean'Value (Get_Field (Child, "xfill").all) then
                  Options := Options or Gtk_Fill_X;
               end if;

               if Boolean'Value (Get_Field (Child, "yfill").all) then
                  Options := Options or Gtk_Fill_Y;
               end if;

               S := Get_Field (Child, "side");
               S2 := Get_Field (Child, "anchor");

               if Use_Default then
                  Gtk.Packer.Add_Defaults (Gtk_Packer (Get_Object (Find_Tag
                    (Find_Parent (N.Parent, "Packer"), "name").Value)),
                     Gtk_Widget (Get_Object (Get_Field (N, "name"))),
                     Gtk_Side_Type'Value (S (S'First + 4 .. S'Last)),
                     Gtk_Anchor_Type'Value (S2 (S2'First + 4 .. S2'Last)),
                     Options);

               else
                  Gtk.Packer.Add (Gtk_Packer (Get_Object (Find_Tag
                    (Find_Parent (N.Parent, "Packer"), "name").Value)),
                     Gtk_Widget (Get_Object (Get_Field (N, "name"))),
                     Gtk_Side_Type'Value (S (S'First + 4 .. S'Last)),
                     Gtk_Anchor_Type'Value (S2 (S2'First + 4 .. S2'Last)),
                     Options,
                     Guint'Value (Get_Field (Child, "border_width").all),
                     Guint'Value (Get_Field (Child, "xpad").all),
                     Guint'Value (Get_Field (Child, "ypad").all),
                     Guint'Value (Get_Field (Child, "xipad").all),
                     Guint'Value (Get_Field (Child, "yipad").all));
               end if;

               N.Specific_Data.Has_Container := True;
            end if;
         end if;
      end if;

      --  ??? Need to handle accelerators

      Q := Find_Tag (N.Child, "signal");

      while Q /= null loop
         S := Get_Field (Q, "handler");

         if S /= null then
            Get_Signal (S.all, Func, Data);
            Signal_Connect
              (Get_Object (Widget),
               Get_Field (Q, "name").all & ASCII.Nul,
               Func, Data);
         end if;

         Q := Find_Tag (Q.Next, "signal");
      end loop;

      if Find_Tag (N.Child, "child_name") = null then
         if not N.Specific_Data.Has_Container then
            S := Get_Field (N.Parent, "class");

            if S /= null then
               if S.all = "GtkFixed" then
                  Fixed.Put
                    (Gtk_Fixed (Get_Object (Get_Field (N.Parent, "name"))),
                     Gtk_Widget (Widget),
                     Gint16'Value (Get_Field (N, "x").all),
                     Gint16'Value (Get_Field (N, "y").all));

               elsif S.all = "GtkLayout" then
                  Layout.Put
                    (Gtk_Layout (Get_Object (Get_Field (N.Parent, "name"))),
                     Gtk_Widget (Widget),
                     Gint16'Value (Get_Field (N, "x").all),
                     Gint16'Value (Get_Field (N, "y").all));

               elsif S.all = "GtkToolbar" then
                  --  ??? Need to handle tooltip
                  Toolbar.Append_Widget (Gtk_Toolbar
                    (Get_Object (Get_Field (N.Parent, "name"))),
                     Gtk_Widget (Widget));

               else
                  Container.Add
                    (Gtk_Container (Get_Object (Get_Field (N.Parent, "name"))),
                     Gtk_Widget (Widget));
               end if;
            end if;

            N.Specific_Data.Has_Container := True;
         end if;
      end if;
   end Generate;

end Gtk.Widget;
