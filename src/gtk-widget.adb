-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gdk; use Gdk;
with Gtk.Box; use Gtk.Box;
with Gtk.Util; use Gtk.Util;
with Interfaces.C.Strings;
with Gtk.Table; use Gtk.Table;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

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
                         Accel_Group : System.Address;
                         Accel_Key   : Gint;
                         Accel_Mods  : Gint)
        return Guint;
      pragma Import (C, Internal, "gtk_widget_accelerator_signal");

   begin
      return Internal (Get_Object (Widget),
                       Get_Object (Accel_Group),
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
                          Accel_Group : System.Address;
                          Accel_Key   : Gint;
                          Accel_Mods  : Gint;
                          Accel_Flags : Gint);
      pragma Import (C, Internal, "gtk_widget_add_accelerator");

   begin
      Internal (Get_Object (Widget), Accel_Signal & Ascii.Nul,
                Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods),
                Gtk.Accel_Group.Gtk_Accel_Flags'Pos (Accel_Flags));
   end Add_Accelerator;

   ----------------------
   -- Can_Focus_Is_Set --
   ----------------------

   function Can_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_can_focus");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
      Tmp : System.Address := Get_Object (Widget);

   begin
      --  We use a Tmp variable, because when the C widget is destroyed,
      --  the Ada structure might be freed (or not), and we always want
      --  to set the Object back to Null_Address;
      Set_Object (Widget, System.Null_Address);
      Internal (Tmp);
   end Destroy;

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

   begin
      Internal (Get_Object (Widget), Area'Address);
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

   procedure Event (Widget : access Gtk_Widget_Record'Class;
                    Event  : Gdk.Event.Gdk_Event)
   is
      procedure Internal (Widget : System.Address; Event : System.Address);
      pragma Import (C, Internal, "gtk_widget_event");

   begin
      Internal (Get_Object (Widget), Get_Object (Event));
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

   -------------------------------------
   -- Get_Default_Motion_Notify_Event --
   -------------------------------------

   function Get_Default_Motion_Notify_Event (Widget : access Gtk_Widget_Record)
     return System.Address
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_widget_get_motion_notify");

   begin
      return Internal (Get_Object (Widget));
   end Get_Default_Motion_Notify_Event;

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

   begin
      return Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Parent;

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
   -- Get_Window --
   ----------------

   function Get_Window (Widget : access Gtk_Widget_Record)
     return Gdk.Window.Gdk_Window
   is
      function Internal (Widget : System.Address)
        return System.Address;
      pragma Import (C, Internal, "ada_widget_get_window");

      Win : Gdk.Window.Gdk_Window;

   begin
      Set_Object (Win, Internal (Get_Object (Widget)));
      return Win;
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
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_colormap");

      Result : Gdk.Color.Gdk_Colormap;

   begin
      Set_Object (Result, Internal (Get_Object (Widget)));
      return Result;
   end Get_Colormap;

   --------------------------
   -- Get_Default_Colormap --
   --------------------------

   function Get_Default_Colormap return Gdk.Color.Gdk_Colormap is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_default_colormap");

      Result : Gdk.Color.Gdk_Colormap;

   begin
      Set_Object (Result, Internal);
      return Result;
   end Get_Default_Colormap;

   ------------------------
   -- Get_Default_Visual --
   ------------------------

   function Get_Default_Visual return Gdk_Visual is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_default_visual");

      Visual : Gdk_Visual;

   begin
      Set_Object (Visual, Internal);
      return Visual;
   end Get_Default_Visual;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual (Widget : access Gtk_Widget_Record) return Gdk_Visual is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_visual");

      Visual : Gdk_Visual;

   begin
      Set_Object (Visual, Internal (Get_Object (Widget)));
      return Visual;
   end Get_Visual;

   ------------------------
   -- Has_Default_Is_Set --
   ------------------------

   function Has_Default_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_default");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Has_Default_Is_Set;

   ----------------------
   -- Has_Focus_Is_Set --
   ----------------------

   function Has_Focus_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_focus");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Has_Focus_Is_Set;

   ---------------------
   -- Has_Grab_Is_Set --
   ---------------------

   function Has_Grab_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_grab");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
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

   ---------------------------
   --  Is_Sensitive_Is_Set  --
   ---------------------------

   function Is_Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_is_sensitive");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Is_Sensitive_Is_Set;

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
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_mapped");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Mapped_Is_Set;

   ----------------------
   -- No_Window_Is_Set --
   ----------------------

   function No_Window_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_no_window");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end No_Window_Is_Set;

   -----------------------------
   -- Parent_Sensitive_Is_Set --
   -----------------------------

   function Parent_Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_parent_sensitive");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Parent_Sensitive_Is_Set;

   -------------------
   -- Push_Colormap --
   -------------------

   procedure Push_Colormap (Cmap   : Gdk_Colormap) is
      procedure Internal (Cmap : System.Address);
      pragma Import (C, Internal, "gtk_widget_push_colormap");
   begin
      Internal (Get_Object (Cmap));
   end Push_Colormap;

   -----------------
   -- Push_Visual --
   -----------------

   procedure Push_Visual (Visual : Gdk_Visual) is
      procedure Internal (Visual : System.Address);
      pragma Import (C, Internal, "gtk_widget_push_visual");
   begin
      Internal (Get_Object (Visual));
   end Push_Visual;

   -----------
   -- Popup --
   -----------

   procedure Popup (Widget : access Gtk_Widget_Record; X, Y : in Gint) is
      procedure Internal (Widget : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_popup");

   begin
      Internal (Get_Object (Widget), X, Y);
   end Popup;

   ---------------------
   -- Rc_Style_Is_Set --
   ---------------------

   function Rc_Style_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_rc_style");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
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
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_realized");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
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
                          Accel_Group : System.Address;
                          Accel_Key   : Gint;
                          Accel_Mods  : Gint);
      pragma Import (C, Internal, "gtk_widget_remove_accelerator");

   begin
      Internal (Get_Object (Widget),
                Get_Object (Accel_Group),
                Gdk.Types.Gdk_Key_Type'Pos (Accel_Key),
                Gdk.Types.Gdk_Modifier_Type'Pos (Accel_Mods));
   end Remove_Accelerator;

   -------------------------
   -- Remove_Accelerators --
   -------------------------

   procedure Remove_Accelerators
     (Widget       : access Gtk_Widget_Record;
      Accel_Signal : in String;
      Visible_Only : in Boolean)
   is
      procedure Internal (Widget : System.Address;
                          Accel_Signal : String;
                          Visible_Only : Gint);
      pragma Import (C, Internal, "gtk_widget_remove_accelerators");

   begin
      Internal (Get_Object (Widget),
                Accel_Signal & Ascii.NUL,
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

   ----------------------
   -- Sensitive_Is_Set --
   ----------------------

   function Sensitive_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_sensitive");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Sensitive_Is_Set;

   ------------------
   -- Set_Colormap --
   ------------------

   procedure Set_Colormap (Widget : access Gtk_Widget_Record;
                           Cmap : Gdk_Colormap)
   is
      procedure Internal (Widget : System.Address; Cmap : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_colormap");

   begin
      Internal (Get_Object (Widget), Get_Object (Cmap));
   end Set_Colormap;

   --------------------------
   -- Set_Default_Colormap --
   --------------------------

   procedure Set_Default_Colormap (Cmap : Gdk_Colormap) is
      procedure Internal (Cmap : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_default_colormap");

   begin
      Internal (Get_Object (Cmap));
   end Set_Default_Colormap;

   ------------------------
   -- Set_Default_Visual --
   ------------------------

   procedure Set_Default_Visual (Visual : Gdk_Visual) is
      procedure Internal (Visual : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_default_visual");

   begin
      Internal (Get_Object (Visual));
   end Set_Default_Visual;

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

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Widget : access Gtk_Widget_Record; Name : in String) is
      procedure Internal (Widget : in System.Address;
                          Name : in String);
      pragma Import (C, Internal, "gtk_widget_set_name");

   begin
      Internal (Get_Object (Widget), Name & Ascii.NUL);
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
      procedure Internal (Widget : System.Address; Visual : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_visual");

   begin
      Internal (Get_Object (Widget), Get_Object (Visual));
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
   -- Show_All --
   --------------

   procedure Show_All (Widget : access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_all");

   begin
      Internal (Get_Object (Widget));
   end Show_All;

   ---------------------
   -- Toplevel_Is_Set --
   ---------------------

   function Toplevel_Is_Set (Widget : access Gtk_Widget_Record'Class)
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_toplevel");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
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
     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_visible");

   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Visible_Is_Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
      Child     : Node_Ptr := Find_Tag (N.Child, "child");
      Q         : Node_Ptr;
      Top       : constant Node_Ptr := Find_Top_Widget (N);
      Top_Name  : constant String_Ptr := Get_Field (Top, "name");
      Cur       : constant String_Ptr := Get_Field (N, "name");
      S         : String_Ptr;
      Flag_Set  : Boolean;
      First     : Natural;
      Last      : Natural;
      The_First : Natural;

   begin
      Object.Generate (N, File);
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
            if Get_Field (Child, "expand") /= null then
               Gen_Call_Child (N, Child, "Box", "Pack_Start",
                 "expand", "fill", "padding", File);
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "left_attach") /= null then
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
            end if;
         end if;
      end if;

      Q := Find_Tag (N.Child, "accelerator");

      if Q /= null then
         if not Top.Specific_Data.Has_Accel_Group then
            Put_Line (File, "   Gtk_New (The_Accel_Group);");
            Put_Line (File, "   Add_Accel_Group (" &
              To_Ada (Top_Name.all) & ", The_Accel_Group);");
            Top.Specific_Data.Has_Accel_Group := True;
         end if;

         Put_Line (File, "   Add_Accelerator (" &
            To_Ada (Top_Name.all) & "." &
            To_Ada (Cur.all) & ", """ &
            Get_Field (Q, "signal").all & """,");
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
   end Generate;

   procedure Generate (Widget : in out Object.Gtk_Object;
                       N      : in Node_Ptr) is
      S, S2, S3  : String_Ptr;
      Child      : Node_Ptr := Find_Tag (N.Child, "child");
      Q          : Node_Ptr;
      Func       : Callback;
      Data       : System.Address;
      Events     : Gdk.Types.Gdk_Event_Mask;

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
               Gtk.Box.Pack_Start
                 (Gtk_Box (Get_Object (Find_Tag
                   (Find_Parent (N.Parent, "Box"), "name").Value)),
                  Gtk_Widget (Get_Object (Get_Field (N, "name"))),
                  Boolean'Value (S.all), Boolean'Value (S2.all),
                  Gint'Value (S3.all));
               N.Specific_Data.Has_Container := True;

            else
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
                     Gint'Value (Get_Field (Child, "left_attach").all),
                     Gint'Value (Get_Field (Child, "right_attach").all),
                     Gint'Value (Get_Field (Child, "top_attach").all),
                     Gint'Value (Get_Field (Child, "bottom_attach").all),
                     Xoptions, Yoptions,
                     Gint'Value (Get_Field (Child, "xpad").all),
                     Gint'Value (Get_Field (Child, "ypad").all));
                  N.Specific_Data.Has_Container := True;
               exception
                  when Constraint_Error =>
                     null;
               end;
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
              (Gdk.Get_Object (Widget.all),
               Get_Field (Q, "name").all & ASCII.Nul,
               Func, Data);
         end if;

         Q := Find_Tag (Q.Next, "signal");
      end loop;
   end Generate;

end Gtk.Widget;
