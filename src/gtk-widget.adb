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

package body Gtk.Widget is

   ----------------
   --  Activate  --
   ----------------

   procedure Activate (Widget : in out Gtk_Widget)
   is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      Internal (Get_Object (Widget));
   end Activate;

   ------------------------
   --  Can_Focus_Is_Set  --
   ------------------------

   function Can_Focus_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_can_focus");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Can_Focus_Is_Set;

   -------------
   -- Convert --
   -------------

   function Convert (W : Gtk_Widget'Class) return System.Address is
   begin
      return Get_Object (W);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (W : System.Address) return Gtk_Widget'Class is
      Widget : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Widget, W);
      return Widget;
   end Convert;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widget));
      Set_Object (Widget, System.Null_Address);
   end Destroy;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Dummy  : in out Gtk_Widget;
                        Widget : in out Gtk_Widget_Access)
   is
      pragma Warnings (Off, Dummy);
   begin
      Set_Object (Widget.all, System.Null_Address);
   end Destroyed;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Widget : in Gtk_Widget;
      Area   : in Gdk.Rectangle.Gdk_Rectangle := Gdk.Rectangle.Full_Area)
   is
      procedure Internal (Widget : in System.Address;
                          Area   : in System.Address);
      pragma Import (C, Internal, "gtk_widget_draw");
   begin
      Internal (Get_Object (Widget), Area'Address);
   end Draw;

   -----------------------
   --  Drawable_Is_Set  --
   -----------------------

   function Drawable_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_drawable");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Drawable_Is_Set;

   -----------
   -- Event --
   -----------

   procedure Event (Widget : Gtk.Widget.Gtk_Widget'Class;
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

   function Get_Allocation_Height (Widget : Gtk_Widget) return Guint is
      function Internal (Widget : System.Address) return Guint;
      pragma Import (C, Internal, "ada_widget_allocation_height");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Height;

   --------------------------
   -- Get_Allocation_Width --
   --------------------------

   function Get_Allocation_Width (Widget : Gtk_Widget) return Guint is
      function Internal (Widget : System.Address) return Guint;
      pragma Import (C, Internal, "ada_widget_allocation_width");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Width;

   ----------------------
   -- Get_Allocation_X --
   ----------------------

   function Get_Allocation_X (Widget : Gtk_Widget) return Gint is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_x");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_X;

   ----------------------
   -- Get_Allocation_Y --
   ----------------------

   function Get_Allocation_Y (Widget : Gtk_Widget) return Gint is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "ada_widget_allocation_y");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocation_Y;

   -------------------------------------
   -- Get_Default_Motion_Notify_Event --
   -------------------------------------

   function Get_Default_Motion_Notify_Event (Widget : in Gtk_Widget)
                                             return System.Address
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_widget_get_motion_notify");
   begin
      return Internal (Get_Object (Widget));
   end Get_Default_Motion_Notify_Event;

   ------------------
   --  Get_Object  --
   ------------------

   function Get_Events (Widget : in Gtk_Widget) return Gint is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_events");
   begin
      return Internal (Get_Object (Widget));
   end Get_Events;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Widget : in Gtk_Widget) return Gtk_Widget'Class is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_widget_get_parent");
      Parent : Gtk_Widget;
   begin
      Set_Object (Parent, Internal (Get_Object (Widget)));
      return Parent;
   end Get_Parent;

   --------------------
   --  Get_Toplevel  --
   --------------------

   function Get_Toplevel (Widget : in Gtk_Widget) return Gtk_Widget'Class is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_toplevel");
      Result : Gtk_Widget;
   begin
      Set_Object (Result, Internal (Get_Object (Widget)));
      return Result;
   end Get_Toplevel;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window (Widget : in Gtk_Widget)
                        return Gdk.Window.Gdk_Window
   is
      function Internal (Widget : System.Address)
                        return    System.Address;
      pragma Import (C, Internal, "ada_widget_get_window");
      Window : Gdk.Window.Gdk_Window;
   begin
      Set_Object (Window, Internal (Get_Object (Widget)));
      return Window;
   end Get_Window;

   --------------------
   --  Grab_Default  --
   --------------------

   procedure Grab_Default (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_default");
   begin
      Internal (Get_Object (Widget));
   end Grab_Default;


   ------------------
   --  Grab_Focus  --
   ------------------

   procedure Grab_Focus (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_focus");
   begin
      Internal (Get_Object (Widget));
   end Grab_Focus;

   --------------------
   --  Get_Colormap  --
   --------------------

   function Get_Colormap (Widget : in Gtk_Widget)
                          return Gdk.Color.Gdk_Colormap is
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

   function Get_Visual (Widget : Gtk_Widget) return Gdk_Visual is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_visual");
      Visual : Gdk_Visual;
   begin
      Set_Object (Visual, Internal (Get_Object (Widget)));
      return Visual;
   end Get_Visual;

   --------------------------
   --  Has_Default_Is_Set  --
   --------------------------

   function Has_Default_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_default");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Has_Default_Is_Set;


   ------------------------
   --  Has_Focus_Is_Set  --
   ------------------------

   function Has_Focus_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_focus");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Has_Focus_Is_Set;

   -----------------------
   --  Has_Grab_Is_Set  --
   -----------------------

   function Has_Grab_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_has_grab");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Has_Grab_Is_Set;

   ------------
   --  Hide  --
   ------------

   procedure Hide (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");
   begin
      Internal (Get_Object (Widget));
   end Hide;

   ---------------------------
   --  Is_Sensitive_Is_Set  --
   ---------------------------

   function Is_Sensitive_Is_Set (Widget : Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_is_sensitive");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Is_Sensitive_Is_Set;

   -----------
   --  Map  --
   -----------

   procedure Map (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_map");
   begin
      Internal (Get_Object (Widget));
   end Map;

   ---------------------
   --  Mapped_Is_Set  --
   ---------------------

   function Mapped_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_mapped");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Mapped_Is_Set;

   ------------------------
   --  No_Window_Is_Set  --
   ------------------------

   function No_Window_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_no_window");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end No_Window_Is_Set;

   -------------------------------
   --  Parent_Sensitive_Is_Set  --
   -------------------------------

   function Parent_Sensitive_Is_Set (Widget : Gtk_Widget'Class)
                                     return Boolean
   is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_parent_sensitive");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Parent_Sensitive_Is_Set;

   -------------
   --  Popup  --
   -------------

   procedure Popup (Widget : in out Gtk_Widget; X, Y : in Gint) is
      procedure Internal (Widget : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_popup");
   begin
      Internal (Get_Object (Widget), X, Y);
   end Popup;

   -----------------------
   --  Rc_Style_Is_Set  --
   -----------------------

   function Rc_Style_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_rc_style");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Rc_Style_Is_Set;

   --------------
   -- Realize  --
   -------------

   procedure Realize (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_realize");
   begin
      Internal (Get_Object (Widget));
   end Realize;

   -----------------------
   --  Realized_Is_Set  --
   -----------------------

   function Realized_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_realized");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Realized_Is_Set;

   ----------------
   --  Reparent  --
   ----------------

   procedure Reparent (Widget : in out Gtk_Widget;
                       New_Parent : in Gtk_Widget'Class)
   is
      procedure Internal (Widget, New_Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_reparent");
   begin
      Internal (Get_Object (Widget), Get_Object (New_Parent));
   end Reparent;

   ------------------------
   --  Sensitive_Is_Set  --
   ------------------------

   function Sensitive_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_sensitive");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Sensitive_Is_Set;

   --------------------------
   -- Set_Default_Colormap --
   --------------------------

   procedure Set_Default_Colormap (Widget : Gtk_Widget; Cmap : Gdk_Colormap) is
      procedure Internal (Widget : System.Address; Cmap : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_default_colormap");
   begin
      Internal (Get_Object (Widget), Get_Object (Cmap));
   end Set_Default_Colormap;

   ------------------------
   -- Set_Default_Visual --
   ------------------------

   procedure Set_Default_Visual (Widget : Gtk_Widget; Visual : Gdk_Visual) is
      procedure Internal (Widget : System.Address; Visual : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_default_visual");
   begin
      Internal (Get_Object (Widget), Get_Object (Visual));
   end Set_Default_Visual;

   ------------------
   --  Set_Events  --
   ------------------

   procedure Set_Events (Widget : in out Gtk_Widget;
                         Events : in     Gdk.Types.Gdk_Event_Mask)
   is
      procedure Internal (Widget : in System.Address;
                          Events : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_events");
   begin
      Internal (Get_Object (Widget), Gdk.Types.Gdk_Event_Mask'Pos (Events));
   end Set_Events;

   ----------------
   --  Set_Name  --
   ----------------

   procedure Set_Name (Widget : in out Gtk_Widget; Name : in String) is
      procedure Internal (Widget : in System.Address;
                          Name : in String);
      pragma Import (C, Internal, "gtk_widget_set_name");
   begin
      Internal (Get_Object (Widget), Name & Ascii.NUL);
   end Set_Name;

   ------------------
   --  Set_Parent  --
   ------------------

   procedure Set_Parent (Widget : in out Gtk_Widget;
                         Parent : in     Gtk_Widget'Class)
   is
      procedure Internal (Widget, Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_set_parent");
   begin
      Internal (Get_Object (Widget), Get_Object (Parent));
   end Set_Parent;

   ---------------------
   --  Set_Sensitive  --
   ---------------------

   procedure Set_Sensitive (Widget    : in out Gtk_Widget;
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

   procedure Set_State (Widget : in out Gtk_Widget;
                        State : in Enums.Gtk_State_Type)
   is
      procedure Internal (Widget : System.Address; State : Gint);
      pragma Import (C, Internal, "gtk_widget_set_state");
   begin
      Internal (Get_Object (Widget), Enums.Gtk_State_Type'Pos (State));
   end Set_State;

   ---------------------
   --  Set_UPosition  --
   ---------------------

   procedure Set_UPosition (Widget : in out Gtk_Widget; X, Y : in Gint) is
      procedure Internal (Widget : System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_uposition");
   begin
      Internal (Get_Object (Widget), X, Y);
   end Set_UPosition;

   -----------------
   --  Set_USize  --
   -----------------

   procedure Set_USize (Widget : in out Gtk_Widget; Width, Height : Gint) is
      procedure Internal (Widget : System.Address; Width, Height : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_usize");
   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_USize;

   ------------
   --  Show  --
   ------------

   procedure Show (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Show;

   ----------------
   --  Show_All  --
   ----------------

   procedure Show_All (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_all");
   begin
      Internal (Get_Object (Widget));
   end Show_All;

   -----------------------
   --  Toplevel_Is_Set  --
   -----------------------

   function Toplevel_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_toplevel");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Toplevel_Is_Set;

   -------------
   --  Unmap  --
   -------------

   procedure Unmap (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unmap");
   begin
      Internal (Get_Object (Widget));
   end Unmap;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Widget : in out Gtk_Widget) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unrealize");
   begin
      Internal (Get_Object (Widget));
   end Unrealize;

   --------------------
   -- Visible_Is_Set --
   --------------------

   function Visible_Is_Set (Widget : in Gtk_Widget'Class) return Boolean is
      function Internal (Widget : in System.Address) return Guint32;
      pragma Import (C, Internal, "ada_widget_visible");
   begin
      return To_Boolean (Internal (Get_Object (Widget)));
   end Visible_Is_Set;

   --------------
   -- Generate --
   --------------

   procedure Generate (Widget : in Gtk_Widget;
                       N      : in Node_Ptr;
                       File   : in File_Type) is
      use Object;

      Child : Node_Ptr := Find_Tag (N.Child, "child");
      Q     : Node_Ptr;

   begin
      Generate (Gtk_Object (Widget), N, File);
      Gen_Set (N, "Widget", "name", File, '"');
      Gen_Set (N, "Widget", "sensitive", File);
      Gen_Set (N, "Widget", "UPosition", "x", "y", "", File);
      Gen_Set (N, "Widget", "USize", "width", "height", "", File);
      --  ??? Missing Set_Parent
      --  ??? Missing Set_Events
      --  ??? Missing Set_Default_Visual
      --  ??? Missing Set_Default_Colormap
      Gen_Set (N, "Widget", "state", File);

      --  ??? Need to find a better way to call Pack_Start

      if Child /= null then
         Q := Find_Tag (Child.Child, "pack");

         if Q = null or else Q.Value.all /= "GTK_PACK_END" then
            Gen_Call_Child (N, Child, "Box", "Pack_Start",
              "expand", "fill", "padding", File);
         end if;
      end if;

      Gen_Signal (N, File);
   end Generate;

   procedure Generate (Widget : in out Gtk_Widget;
                       N      : in Node_Ptr) is
      use Object;

      S, S2, S3  : String_Ptr;
      Child      : Node_Ptr := Find_Tag (N.Child, "child");
      Q          : Node_Ptr;

      procedure Signal_Connect
        (Object        : System.Address;
         Name          : String;
         Func          : Void_Signal;
         Func_Data     : System.Address := System.Null_Address);
      pragma Import (C, Signal_Connect, "gtk_signal_connect");

   begin
      Generate (Gtk_Object (Widget), N);
      S := Get_Field (N, "name");

      if S /= null then
         Set_Name (Widget, S.all);
      end if;

      S := Get_Field (N, "sensitive");

      if S /= null then
         Set_Sensitive (Widget, Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "x");
      S2 := Get_Field (N, "y");

      if S /= null and then S2 /= null then
         Set_UPosition (Widget, Gint'Value (S.all), Gint'Value (S2.all));
      end if;

      S := Get_Field (N, "width");
      S2 := Get_Field (N, "height");

      if S /= null and then S2 /= null then
         Set_USize (Widget, Gint'Value (S.all), Gint'Value (S2.all));
      end if;

      S := Get_Field (N, "state");

      if S /= null then
         Set_State (Widget, Enums.Gtk_State_Type'Value (S.all));
      end if;

      --  ??? Need to find a better way to call Pack_Start

      if Child /= null then
         Q := Find_Tag (Child.Child, "pack");

         if Q = null or else Q.Value.all /= "GTK_PACK_END" then
            S := Get_Field (Child, "expand");
            S2 := Get_Field (Child, "fill");
            S3 := Get_Field (Child, "padding");

            if S /= null and then S2 /= null and then S3 /= null then
               Gtk.Box.Pack_Start
                 (Gtk_Box (Get_Object (Find_Tag
                   (Find_Parent (N.Parent, "Box"), "name").Value).all),
                  Get_Object (Get_Field (N, "name")).all,
                  Boolean'Value (S.all), Boolean'Value (S2.all),
                  Gint'Value (S3.all));
            end if;
         end if;
      end if;

      Q := Find_Tag (N.Child, "signal");

      while Q /= null loop
         Signal_Connect
           (Gdk.Get_Object (Widget),
            Get_Field (Q, "name").all & ASCII.Nul,
            Get_Signal (Get_Field (Q, "handler").all));
         Q := Find_Tag (Q.Next, "signal");
      end loop;
   end Generate;

end Gtk.Widget;
