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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Widget is

   function From_Object_Free (B : access Gtk_Requisition) return Gtk_Requisition is
      Result : constant Gtk_Requisition := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object_Free (B : access Gtk_Widget_Path) return Gtk_Widget_Path is
      Result : constant Gtk_Widget_Path := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function Convert (R : Gtk.Widget.Gtk_Widget) return System.Address is
   begin
      return Get_Object (R);
   end Convert;

   function Convert (R : System.Address) return Gtk.Widget.Gtk_Widget is
      Stub : Gtk.Widget.Gtk_Widget_Record;begin
         return Gtk.Widget.Gtk_Widget (Glib.Object.Get_User_Data (R, Stub));
      end Convert;


   function Convert is new Ada.Unchecked_Conversion
     (Draw_Handler, System.Address);

   function Proxy_Draw
     (W  : System.Address;
      Cr : Cairo.Cairo_Context) return Gboolean
   is
      Stub : Gtk_Widget_Record;
      W2 : constant Gtk_Widget := Gtk_Widget (Get_User_Data (W, Stub));
   begin
      return Boolean'Pos (Draw (W2, Cr));
   end Proxy_Draw;

   procedure Set_Default_Draw_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Draw_Handler)
   is
      procedure Internal (K : GObject_Class; H : System.Address);
      pragma Import (C, Internal, "ada_gtk_set_draw_handler");
   begin
      Internal (Klass, Convert (Handler));
   end Set_Default_Draw_Handler;

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

   package Type_Conversion_Gtk_Widget is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Widget_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Widget);

   --------------
   -- Activate --
   --------------

   function Activate
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Activate;

   ---------------------
   -- Add_Accelerator --
   ---------------------

   procedure Add_Accelerator
      (Widget       : not null access Gtk_Widget_Record;
       Accel_Signal : UTF8_String;
       Accel_Group  : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key    : Gtk.Accel_Group.Gtk_Accel_Key;
       Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags)
   is
      procedure Internal
         (Widget       : System.Address;
          Accel_Signal : Interfaces.C.Strings.chars_ptr;
          Accel_Group  : System.Address;
          Accel_Key    : Gtk.Accel_Group.Gtk_Accel_Key;
          Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
          Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags);
      pragma Import (C, Internal, "gtk_widget_add_accelerator");
      Tmp_Accel_Signal : Interfaces.C.Strings.chars_ptr := New_String (Accel_Signal);
   begin
      Internal (Get_Object (Widget), Tmp_Accel_Signal, Get_Object (Accel_Group), Accel_Key, Accel_Mods, Accel_Flags);
      Free (Tmp_Accel_Signal);
   end Add_Accelerator;

   -----------------------
   -- Add_Device_Events --
   -----------------------

   procedure Add_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
         (Widget : System.Address;
          Device : System.Address;
          Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_add_device_events");
   begin
      Internal (Get_Object (Widget), Get_Object (Device), Events);
   end Add_Device_Events;

   ----------------
   -- Add_Events --
   ----------------

   procedure Add_Events
      (Widget : not null access Gtk_Widget_Record;
       Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
         (Widget : System.Address;
          Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_add_events");
   begin
      Internal (Get_Object (Widget), Events);
   end Add_Events;

   ------------------------
   -- Add_Mnemonic_Label --
   ------------------------

   procedure Add_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address; Label : System.Address);
      pragma Import (C, Internal, "gtk_widget_add_mnemonic_label");
   begin
      Internal (Get_Object (Widget), Get_Object (Label));
   end Add_Mnemonic_Label;

   ------------------------
   -- Can_Activate_Accel --
   ------------------------

   function Can_Activate_Accel
      (Widget    : not null access Gtk_Widget_Record;
       Signal_Id : Guint) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Signal_Id : Guint) return Integer;
      pragma Import (C, Internal, "gtk_widget_can_activate_accel");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Signal_Id));
   end Can_Activate_Accel;

   -----------------
   -- Child_Focus --
   -----------------

   function Child_Focus
      (Widget    : not null access Gtk_Widget_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Direction : Gtk.Enums.Gtk_Direction_Type) return Integer;
      pragma Import (C, Internal, "gtk_widget_child_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Direction));
   end Child_Focus;

   ------------------
   -- Child_Notify --
   ------------------

   procedure Child_Notify
      (Widget         : not null access Gtk_Widget_Record;
       Child_Property : UTF8_String)
   is
      procedure Internal
         (Widget         : System.Address;
          Child_Property : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_child_notify");
      Tmp_Child_Property : Interfaces.C.Strings.chars_ptr := New_String (Child_Property);
   begin
      Internal (Get_Object (Widget), Tmp_Child_Property);
      Free (Tmp_Child_Property);
   end Child_Notify;

   --------------------
   -- Compute_Expand --
   --------------------

   function Compute_Expand
      (Widget      : not null access Gtk_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation) return Boolean
   is
      function Internal
         (Widget      : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation) return Integer;
      pragma Import (C, Internal, "gtk_widget_compute_expand");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Orientation));
   end Compute_Expand;

   --------------------------
   -- Create_Pango_Context --
   --------------------------

   function Create_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_create_pango_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (Widget)), Stub_Pango_Context));
   end Create_Pango_Context;

   -------------------------
   -- Create_Pango_Layout --
   -------------------------

   function Create_Pango_Layout
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String := "") return Pango.Layout.Pango_Layout
   is
      function Internal
         (Widget : System.Address;
          Text   : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_widget_create_pango_layout");
      Tmp_Text          : Interfaces.C.Strings.chars_ptr;
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
      Tmp_Return        : System.Address;
   begin
      if Text = "" then
         Tmp_Text := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Text);
      Free (Tmp_Text);
      return Pango.Layout.Pango_Layout (Get_User_Data (Tmp_Return, Stub_Pango_Layout));
   end Create_Pango_Layout;

   procedure Destroy (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");

      procedure Unref_Internal (Widget : System.Address);
      pragma Import (C, Unref_Internal, "g_object_unref");
      --  External binding: g_object_unref

      Ptr : constant System.Address := Get_Object (Widget);

      use type System.Address;
   begin
      --  Keep a reference on the object, so that the Ada structure is
      --  never automatically deleted when the C object is.
      --  We can't reset the content of Widget to System.Null_Address before
      --  calling the C function, because we want the user's destroy
      --  callbacks to be called with the appropriate object.
      Ref (Widget);
      Internal (Ptr);

      --  We then can make sure that the object won't be referenced any
      --  more, (The Ada structure won't be free before the ref count goes
      --  down to 0, and we don't want the user to use a deleted object...).
      Set_Object (Widget, System.Null_Address);

      --  Free the reference we had. In most cases, this results in the
      --  object being freed. We can't use directly Unref, since the Ptr
      --  field for Object is Null_Address.
      Unref_Internal (Ptr);
   end Destroy;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed
      (Widget         : not null access Gtk_Widget_Record;
       Widget_Pointer : in out Gtk_Widget)
   is
      procedure Internal
         (Widget         : System.Address;
          Widget_Pointer : in out System.Address);
      pragma Import (C, Internal, "gtk_widget_destroyed");
      Tmp_Widget_Pointer : aliased System.Address := Get_Object (Widget_Pointer);
      Stub_Gtk_Widget    : Gtk_Widget_Record;
   begin
      Internal (Get_Object (Widget), Tmp_Widget_Pointer);
      Widget_Pointer := Gtk.Widget.Gtk_Widget (Get_User_Data (Tmp_Widget_Pointer, Stub_Gtk_Widget));
   end Destroyed;

   ------------------------
   -- Device_Is_Shadowed --
   ------------------------

   function Device_Is_Shadowed
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean
   is
      function Internal
         (Widget : System.Address;
          Device : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_device_is_shadowed");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Get_Object (Device)));
   end Device_Is_Shadowed;

   --------------------------
   -- Drag_Check_Threshold --
   --------------------------

   function Drag_Check_Threshold
      (Widget    : not null access Gtk_Widget_Record;
       Start_X   : Gint;
       Start_Y   : Gint;
       Current_X : Gint;
       Current_Y : Gint) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Start_X   : Gint;
          Start_Y   : Gint;
          Current_X : Gint;
          Current_Y : Gint) return Integer;
      pragma Import (C, Internal, "gtk_drag_check_threshold");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Start_X, Start_Y, Current_X, Current_Y));
   end Drag_Check_Threshold;

   ---------------------------------
   -- Drag_Dest_Add_Image_Targets --
   ---------------------------------

   procedure Drag_Dest_Add_Image_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_image_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Dest_Add_Image_Targets;

   --------------------------------
   -- Drag_Dest_Add_Text_Targets --
   --------------------------------

   procedure Drag_Dest_Add_Text_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_text_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Dest_Add_Text_Targets;

   -------------------------------
   -- Drag_Dest_Add_Uri_Targets --
   -------------------------------

   procedure Drag_Dest_Add_Uri_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_add_uri_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Dest_Add_Uri_Targets;

   --------------------------------
   -- Drag_Dest_Get_Track_Motion --
   --------------------------------

   function Drag_Dest_Get_Track_Motion
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_drag_dest_get_track_motion");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Drag_Dest_Get_Track_Motion;

   -------------------------
   -- Drag_Dest_Set_Proxy --
   -------------------------

   procedure Drag_Dest_Set_Proxy
      (Widget          : not null access Gtk_Widget_Record;
       Proxy_Window    : Gdk.Gdk_Window;
       Protocol        : Gdk.Drag_Contexts.Gdk_Drag_Protocol;
       Use_Coordinates : Boolean)
   is
      procedure Internal
         (Widget          : System.Address;
          Proxy_Window    : Gdk.Gdk_Window;
          Protocol        : Gdk.Drag_Contexts.Gdk_Drag_Protocol;
          Use_Coordinates : Integer);
      pragma Import (C, Internal, "gtk_drag_dest_set_proxy");
   begin
      Internal (Get_Object (Widget), Proxy_Window, Protocol, Boolean'Pos (Use_Coordinates));
   end Drag_Dest_Set_Proxy;

   --------------------------------
   -- Drag_Dest_Set_Track_Motion --
   --------------------------------

   procedure Drag_Dest_Set_Track_Motion
      (Widget       : not null access Gtk_Widget_Record;
       Track_Motion : Boolean)
   is
      procedure Internal (Widget : System.Address; Track_Motion : Integer);
      pragma Import (C, Internal, "gtk_drag_dest_set_track_motion");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Track_Motion));
   end Drag_Dest_Set_Track_Motion;

   ---------------------
   -- Drag_Dest_Unset --
   ---------------------

   procedure Drag_Dest_Unset (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_dest_unset");
   begin
      Internal (Get_Object (Widget));
   end Drag_Dest_Unset;

   -------------------
   -- Drag_Get_Data --
   -------------------

   procedure Drag_Get_Data
      (Widget  : not null access Gtk_Widget_Record;
       Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
       Target  : Gdk.Types.Gdk_Atom;
       Time    : guint32)
   is
      procedure Internal
         (Widget  : System.Address;
          Context : System.Address;
          Target  : Gdk.Types.Gdk_Atom;
          Time    : guint32);
      pragma Import (C, Internal, "gtk_drag_get_data");
   begin
      Internal (Get_Object (Widget), Get_Object (Context), Target, Time);
   end Drag_Get_Data;

   --------------------
   -- Drag_Highlight --
   --------------------

   procedure Drag_Highlight (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_highlight");
   begin
      Internal (Get_Object (Widget));
   end Drag_Highlight;

   -----------------------------------
   -- Drag_Source_Add_Image_Targets --
   -----------------------------------

   procedure Drag_Source_Add_Image_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_image_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Source_Add_Image_Targets;

   ----------------------------------
   -- Drag_Source_Add_Text_Targets --
   ----------------------------------

   procedure Drag_Source_Add_Text_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_text_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Source_Add_Text_Targets;

   ---------------------------------
   -- Drag_Source_Add_Uri_Targets --
   ---------------------------------

   procedure Drag_Source_Add_Uri_Targets
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_add_uri_targets");
   begin
      Internal (Get_Object (Widget));
   end Drag_Source_Add_Uri_Targets;

   -------------------------------
   -- Drag_Source_Set_Icon_Name --
   -------------------------------

   procedure Drag_Source_Set_Icon_Name
      (Widget    : not null access Gtk_Widget_Record;
       Icon_Name : UTF8_String)
   is
      procedure Internal
         (Widget    : System.Address;
          Icon_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (Widget), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Drag_Source_Set_Icon_Name;

   ---------------------------------
   -- Drag_Source_Set_Icon_Pixbuf --
   ---------------------------------

   procedure Drag_Source_Set_Icon_Pixbuf
      (Widget : not null access Gtk_Widget_Record;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Widget : System.Address; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_pixbuf");
   begin
      Internal (Get_Object (Widget), Get_Object (Pixbuf));
   end Drag_Source_Set_Icon_Pixbuf;

   --------------------------------
   -- Drag_Source_Set_Icon_Stock --
   --------------------------------

   procedure Drag_Source_Set_Icon_Stock
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String)
   is
      procedure Internal
         (Widget   : System.Address;
          Stock_Id : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Widget), Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
   end Drag_Source_Set_Icon_Stock;

   -----------------------
   -- Drag_Source_Unset --
   -----------------------

   procedure Drag_Source_Unset (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_source_unset");
   begin
      Internal (Get_Object (Widget));
   end Drag_Source_Unset;

   ----------------------
   -- Drag_Unhighlight --
   ----------------------

   procedure Drag_Unhighlight (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_drag_unhighlight");
   begin
      Internal (Get_Object (Widget));
   end Drag_Unhighlight;

   ----------
   -- Draw --
   ----------

   procedure Draw
      (Widget : not null access Gtk_Widget_Record;
       Cr     : Cairo.Cairo_Context)
   is
      procedure Internal (Widget : System.Address; Cr : Cairo.Cairo_Context);
      pragma Import (C, Internal, "gtk_widget_draw");
   begin
      Internal (Get_Object (Widget), Cr);
   end Draw;

   ------------------
   -- Ensure_Style --
   ------------------

   procedure Ensure_Style (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_ensure_style");
   begin
      Internal (Get_Object (Widget));
   end Ensure_Style;

   ----------------
   -- Error_Bell --
   ----------------

   procedure Error_Bell (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_error_bell");
   begin
      Internal (Get_Object (Widget));
   end Error_Bell;

   -----------
   -- Event --
   -----------

   function Event
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Widget : System.Address;
          Event  : Gdk.Event.Gdk_Event) return Integer;
      pragma Import (C, Internal, "gtk_widget_event");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Event));
   end Event;

   -------------------------
   -- Freeze_Child_Notify --
   -------------------------

   procedure Freeze_Child_Notify
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_freeze_child_notify");
   begin
      Internal (Get_Object (Widget));
   end Freeze_Child_Notify;

   --------------------------
   -- Get_Allocated_Height --
   --------------------------

   function Get_Allocated_Height
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_allocated_height");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocated_Height;

   -------------------------
   -- Get_Allocated_Width --
   -------------------------

   function Get_Allocated_Width
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_allocated_width");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocated_Width;

   --------------------
   -- Get_Allocation --
   --------------------

   procedure Get_Allocation
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : out Gtk_Allocation)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : out Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_get_allocation");
   begin
      Internal (Get_Object (Widget), Allocation);
   end Get_Allocation;

   ------------------
   -- Get_Ancestor --
   ------------------

   function Get_Ancestor
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType) return Gtk_Widget
   is
      function Internal
         (Widget      : System.Address;
          Widget_Type : GType) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_ancestor");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget), Widget_Type), Stub_Gtk_Widget));
   end Get_Ancestor;

   -----------------------
   -- Get_App_Paintable --
   -----------------------

   function Get_App_Paintable
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_app_paintable");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_App_Paintable;

   ---------------------
   -- Get_Can_Default --
   ---------------------

   function Get_Can_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_can_default");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Can_Default;

   -------------------
   -- Get_Can_Focus --
   -------------------

   function Get_Can_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_can_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Can_Focus;

   ---------------------------
   -- Get_Child_Requisition --
   ---------------------------

   procedure Get_Child_Requisition
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition)
   is
      procedure Internal
         (Widget      : System.Address;
          Requisition : out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_get_child_requisition");
      Tmp_Requisition : aliased Gtk_Requisition;
   begin
      Internal (Get_Object (Widget), Tmp_Requisition);
      Requisition := Tmp_Requisition;
   end Get_Child_Requisition;

   -----------------------
   -- Get_Child_Visible --
   -----------------------

   function Get_Child_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_child_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Child_Visible;

   ------------------------
   -- Get_Composite_Name --
   ------------------------

   function Get_Composite_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_composite_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Widget)));
   end Get_Composite_Name;

   ------------------------
   -- Get_Device_Enabled --
   ------------------------

   function Get_Device_Enabled
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Boolean
   is
      function Internal
         (Widget : System.Address;
          Device : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_device_enabled");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Get_Object (Device)));
   end Get_Device_Enabled;

   -----------------------
   -- Get_Device_Events --
   -----------------------

   function Get_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class)
       return Gdk.Event.Gdk_Event_Mask
   is
      function Internal
         (Widget : System.Address;
          Device : System.Address) return Gdk.Event.Gdk_Event_Mask;
      pragma Import (C, Internal, "gtk_widget_get_device_events");
   begin
      return Internal (Get_Object (Widget), Get_Object (Device));
   end Get_Device_Events;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_Text_Direction
   is
      function Internal
         (Widget : System.Address) return Gtk.Enums.Gtk_Text_Direction;
      pragma Import (C, Internal, "gtk_widget_get_direction");
   begin
      return Internal (Get_Object (Widget));
   end Get_Direction;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Display.Gdk_Display
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Display.Gdk_Display (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gdk_Display));
   end Get_Display;

   -------------------------
   -- Get_Double_Buffered --
   -------------------------

   function Get_Double_Buffered
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_double_buffered");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Double_Buffered;

   ----------------
   -- Get_Events --
   ----------------

   function Get_Events
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Event.Gdk_Event_Mask
   is
      function Internal
         (Widget : System.Address) return Gdk.Event.Gdk_Event_Mask;
      pragma Import (C, Internal, "gtk_widget_get_events");
   begin
      return Internal (Get_Object (Widget));
   end Get_Events;

   ----------------
   -- Get_Halign --
   ----------------

   function Get_Halign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align
   is
      function Internal (Widget : System.Address) return Gtk_Align;
      pragma Import (C, Internal, "gtk_widget_get_halign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Halign;

   ---------------------
   -- Get_Has_Tooltip --
   ---------------------

   function Get_Has_Tooltip
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_has_tooltip");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Has_Tooltip;

   --------------------
   -- Get_Has_Window --
   --------------------

   function Get_Has_Window
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_has_window");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Has_Window;

   -----------------
   -- Get_Hexpand --
   -----------------

   function Get_Hexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_hexpand");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Hexpand;

   ---------------------
   -- Get_Hexpand_Set --
   ---------------------

   function Get_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_hexpand_set");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Hexpand_Set;

   ----------------
   -- Get_Mapped --
   ----------------

   function Get_Mapped
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_mapped");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Mapped;

   -----------------------
   -- Get_Margin_Bottom --
   -----------------------

   function Get_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_bottom");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Bottom;

   ---------------------
   -- Get_Margin_Left --
   ---------------------

   function Get_Margin_Left
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_left");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Left;

   ----------------------
   -- Get_Margin_Right --
   ----------------------

   function Get_Margin_Right
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_right");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Right;

   --------------------
   -- Get_Margin_Top --
   --------------------

   function Get_Margin_Top
      (Widget : not null access Gtk_Widget_Record) return Gint
   is
      function Internal (Widget : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_top");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Top;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Name;

   ---------------------
   -- Get_No_Show_All --
   ---------------------

   function Get_No_Show_All
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_no_show_all");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_No_Show_All;

   -----------------------
   -- Get_Pango_Context --
   -----------------------

   function Get_Pango_Context
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_pango_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (Widget)), Stub_Pango_Context));
   end Get_Pango_Context;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_parent");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Parent;

   -----------------------
   -- Get_Parent_Window --
   -----------------------

   function Get_Parent_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window
   is
      function Internal (Widget : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_widget_get_parent_window");
   begin
      return Internal (Get_Object (Widget));
   end Get_Parent_Window;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget_Path
   is
      function Internal
         (Widget : System.Address) return access Gtk_Widget_Path;
      pragma Import (C, Internal, "gtk_widget_get_path");
   begin
      return Internal (Get_Object (Widget)).all;
   end Get_Path;

   -----------------
   -- Get_Pointer --
   -----------------

   procedure Get_Pointer
      (Widget : not null access Gtk_Widget_Record;
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

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Widget         : not null access Gtk_Widget_Record;
       Minimum_Height : out Gint;
       Natural_Height : out Gint)
   is
      procedure Internal
         (Widget         : System.Address;
          Minimum_Height : out Gint;
          Natural_Height : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_height");
   begin
      Internal (Get_Object (Widget), Minimum_Height, Natural_Height);
   end Get_Preferred_Height;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Widget         : not null access Gtk_Widget_Record;
       Width          : Gint;
       Minimum_Height : out Gint;
       Natural_Height : out Gint)
   is
      procedure Internal
         (Widget         : System.Address;
          Width          : Gint;
          Minimum_Height : out Gint;
          Natural_Height : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_height_for_width");
   begin
      Internal (Get_Object (Widget), Width, Minimum_Height, Natural_Height);
   end Get_Preferred_Height_For_Width;

   ------------------------
   -- Get_Preferred_Size --
   ------------------------

   procedure Get_Preferred_Size
      (Widget       : not null access Gtk_Widget_Record;
       Minimum_Size : out Gtk_Requisition;
       Natural_Size : out Gtk_Requisition)
   is
      procedure Internal
         (Widget       : System.Address;
          Minimum_Size : out Gtk_Requisition;
          Natural_Size : out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_get_preferred_size");
      Tmp_Minimum_Size : aliased Gtk_Requisition;
      Tmp_Natural_Size : aliased Gtk_Requisition;
   begin
      Internal (Get_Object (Widget), Tmp_Minimum_Size, Tmp_Natural_Size);
      Natural_Size := Tmp_Natural_Size;
      Minimum_Size := Tmp_Minimum_Size;
   end Get_Preferred_Size;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Widget        : not null access Gtk_Widget_Record;
       Minimum_Width : out Gint;
       Natural_Width : out Gint)
   is
      procedure Internal
         (Widget        : System.Address;
          Minimum_Width : out Gint;
          Natural_Width : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_width");
   begin
      Internal (Get_Object (Widget), Minimum_Width, Natural_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Widget        : not null access Gtk_Widget_Record;
       Height        : Gint;
       Minimum_Width : out Gint;
       Natural_Width : out Gint)
   is
      procedure Internal
         (Widget        : System.Address;
          Height        : Gint;
          Minimum_Width : out Gint;
          Natural_Width : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_width_for_height");
   begin
      Internal (Get_Object (Widget), Height, Minimum_Width, Natural_Width);
   end Get_Preferred_Width_For_Height;

   ------------------
   -- Get_Realized --
   ------------------

   function Get_Realized
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_realized");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Realized;

   --------------------------
   -- Get_Receives_Default --
   --------------------------

   function Get_Receives_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_receives_default");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Receives_Default;

   ----------------------
   -- Get_Request_Mode --
   ----------------------

   function Get_Request_Mode
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode
   is
      function Internal
         (Widget : System.Address) return Gtk.Enums.Gtk_Size_Request_Mode;
      pragma Import (C, Internal, "gtk_widget_get_request_mode");
   begin
      return Internal (Get_Object (Widget));
   end Get_Request_Mode;

   ---------------------
   -- Get_Requisition --
   ---------------------

   procedure Get_Requisition
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition)
   is
      procedure Internal
         (Widget      : System.Address;
          Requisition : out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_get_requisition");
      Tmp_Requisition : aliased Gtk_Requisition;
   begin
      Internal (Get_Object (Widget), Tmp_Requisition);
      Requisition := Tmp_Requisition;
   end Get_Requisition;

   ---------------------
   -- Get_Root_Window --
   ---------------------

   function Get_Root_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window
   is
      function Internal (Widget : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_widget_get_root_window");
   begin
      return Internal (Get_Object (Widget));
   end Get_Root_Window;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gdk_Screen));
   end Get_Screen;

   -------------------
   -- Get_Sensitive --
   -------------------

   function Get_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Sensitive;

   ----------------------
   -- Get_Size_Request --
   ----------------------

   procedure Get_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : out Gint;
       Height : out Gint)
   is
      procedure Internal
         (Widget : System.Address;
          Width  : out Gint;
          Height : out Gint);
      pragma Import (C, Internal, "gtk_widget_get_size_request");
   begin
      Internal (Get_Object (Widget), Width, Height);
   end Get_Size_Request;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_State_Type
   is
      function Internal
         (Widget : System.Address) return Gtk.Enums.Gtk_State_Type;
      pragma Import (C, Internal, "gtk_widget_get_state");
   begin
      return Internal (Get_Object (Widget));
   end Get_State;

   ---------------------
   -- Get_State_Flags --
   ---------------------

   function Get_State_Flags
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Enums.Gtk_State_Flags
   is
      function Internal
         (Widget : System.Address) return Gtk.Enums.Gtk_State_Flags;
      pragma Import (C, Internal, "gtk_widget_get_state_flags");
   begin
      return Internal (Get_Object (Widget));
   end Get_State_Flags;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Style.Gtk_Style
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_style");
      Stub_Gtk_Style : Gtk.Style.Gtk_Style_Record;
   begin
      return Gtk.Style.Gtk_Style (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Style));
   end Get_Style;

   -----------------------------
   -- Get_Support_Multidevice --
   -----------------------------

   function Get_Support_Multidevice
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_support_multidevice");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Support_Multidevice;

   ------------------------
   -- Get_Tooltip_Markup --
   ------------------------

   function Get_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_tooltip_markup");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Widget)));
   end Get_Tooltip_Markup;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   function Get_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_widget_get_tooltip_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Widget)));
   end Get_Tooltip_Text;

   ------------------------
   -- Get_Tooltip_Window --
   ------------------------

   function Get_Tooltip_Window
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_tooltip_window");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Tooltip_Window;

   ------------------
   -- Get_Toplevel --
   ------------------

   function Get_Toplevel
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_toplevel");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Toplevel;

   ----------------
   -- Get_Valign --
   ----------------

   function Get_Valign
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align
   is
      function Internal (Widget : System.Address) return Gtk_Align;
      pragma Import (C, Internal, "gtk_widget_get_valign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Valign;

   -----------------
   -- Get_Vexpand --
   -----------------

   function Get_Vexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_vexpand");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Vexpand;

   ---------------------
   -- Get_Vexpand_Set --
   ---------------------

   function Get_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_vexpand_set");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Vexpand_Set;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Get_Visible;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Visual.Gdk_Visual
   is
      function Internal
         (Widget : System.Address) return Gdk.Visual.Gdk_Visual;
      pragma Import (C, Internal, "gtk_widget_get_visual");
   begin
      return Internal (Get_Object (Widget));
   end Get_Visual;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
      (Widget : not null access Gtk_Widget_Record) return Gdk.Gdk_Window
   is
      function Internal (Widget : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gtk_widget_get_window");
   begin
      return Internal (Get_Object (Widget));
   end Get_Window;

   --------------
   -- Grab_Add --
   --------------

   procedure Grab_Add (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_add");
   begin
      Internal (Get_Object (Widget));
   end Grab_Add;

   ------------------
   -- Grab_Default --
   ------------------

   procedure Grab_Default (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_default");
   begin
      Internal (Get_Object (Widget));
   end Grab_Default;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_focus");
   begin
      Internal (Get_Object (Widget));
   end Grab_Focus;

   -----------------
   -- Grab_Remove --
   -----------------

   procedure Grab_Remove (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_grab_remove");
   begin
      Internal (Get_Object (Widget));
   end Grab_Remove;

   -----------------
   -- Has_Default --
   -----------------

   function Has_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_default");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Default;

   ---------------
   -- Has_Focus --
   ---------------

   function Has_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Focus;

   --------------
   -- Has_Grab --
   --------------

   function Has_Grab
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_grab");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Grab;

   ------------------
   -- Has_Rc_Style --
   ------------------

   function Has_Rc_Style
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_rc_style");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Rc_Style;

   ----------------
   -- Has_Screen --
   ----------------

   function Has_Screen
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_screen");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Screen;

   -----------------------
   -- Has_Visible_Focus --
   -----------------------

   function Has_Visible_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_has_visible_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Has_Visible_Focus;

   ----------
   -- Hide --
   ----------

   procedure Hide (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");
   begin
      Internal (Get_Object (Widget));
   end Hide;

   --------------------
   -- Hide_On_Delete --
   --------------------

   function Hide_On_Delete
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_hide_on_delete");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Hide_On_Delete;

   --------------------
   -- In_Destruction --
   --------------------

   function In_Destruction
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_in_destruction");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end In_Destruction;

   --------------------------------
   -- Input_Shape_Combine_Region --
   --------------------------------

   procedure Input_Shape_Combine_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Widget : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gtk_widget_input_shape_combine_region");
   begin
      Internal (Get_Object (Widget), Region);
   end Input_Shape_Combine_Region;

   ---------------
   -- Intersect --
   ---------------

   function Intersect
      (Widget       : not null access Gtk_Widget_Record;
       Area         : Gdk.Rectangle.Gdk_Rectangle;
       Intersection : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
         (Widget           : System.Address;
          Area             : Gdk.Rectangle.Gdk_Rectangle;
          Acc_Intersection : access Gdk.Rectangle.Gdk_Rectangle)
          return Integer;
      pragma Import (C, Internal, "gtk_widget_intersect");
      Acc_Intersection : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return       : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Area, Acc_Intersection'Access);
      Intersection.all := Acc_Intersection;
      return Boolean'Val (Tmp_Return);
   end Intersect;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
      (Widget   : not null access Gtk_Widget_Record;
       Ancestor : not null access Gtk_Widget_Record'Class) return Boolean
   is
      function Internal
         (Widget   : System.Address;
          Ancestor : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_ancestor");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Get_Object (Ancestor)));
   end Is_Ancestor;

   -------------------
   -- Is_Composited --
   -------------------

   function Is_Composited
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_composited");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Composited;

   -----------------
   -- Is_Drawable --
   -----------------

   function Is_Drawable
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_drawable");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Drawable;

   --------------
   -- Is_Focus --
   --------------

   function Is_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Focus;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Sensitive;

   -----------------
   -- Is_Toplevel --
   -----------------

   function Is_Toplevel
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_widget_is_toplevel");
   begin
      return Boolean'Val (Internal (Get_Object (Widget)));
   end Is_Toplevel;

   -------------------
   -- Keynav_Failed --
   -------------------

   function Keynav_Failed
      (Widget    : not null access Gtk_Widget_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Direction : Gtk.Enums.Gtk_Direction_Type) return Integer;
      pragma Import (C, Internal, "gtk_widget_keynav_failed");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Direction));
   end Keynav_Failed;

   --------------------------
   -- List_Mnemonic_Labels --
   --------------------------

   function List_Mnemonic_Labels
      (Widget : not null access Gtk_Widget_Record) return Widget_List.Glist
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_list_mnemonic_labels");
      Tmp_Return : Widget_List.Glist;
   begin
      Gtk.Widget.Widget_List.Set_Object (Tmp_Return, Internal (Get_Object (Widget)));
      return Tmp_Return;
   end List_Mnemonic_Labels;

   ---------
   -- Map --
   ---------

   procedure Map (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_map");
   begin
      Internal (Get_Object (Widget));
   end Map;

   -----------------------
   -- Mnemonic_Activate --
   -----------------------

   function Mnemonic_Activate
      (Widget        : not null access Gtk_Widget_Record;
       Group_Cycling : Boolean) return Boolean
   is
      function Internal
         (Widget        : System.Address;
          Group_Cycling : Integer) return Integer;
      pragma Import (C, Internal, "gtk_widget_mnemonic_activate");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Boolean'Pos (Group_Cycling)));
   end Mnemonic_Activate;

   -----------------
   -- Modify_Base --
   -----------------

   procedure Modify_Base
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Type;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_base");
   begin
      Internal (Get_Object (Widget), State, Gdk.Color.Gdk_Color_Or_Null (Color'Address));
   end Modify_Base;

   ---------------
   -- Modify_Bg --
   ---------------

   procedure Modify_Bg
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Type;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_bg");
   begin
      Internal (Get_Object (Widget), State, Gdk.Color.Gdk_Color_Or_Null (Color'Address));
   end Modify_Bg;

   -------------------
   -- Modify_Cursor --
   -------------------

   procedure Modify_Cursor
      (Widget    : not null access Gtk_Widget_Record;
       Primary   : Gdk.Color.Gdk_Color;
       Secondary : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Widget    : System.Address;
          Primary   : Gdk.Color.Gdk_Color;
          Secondary : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_widget_modify_cursor");
   begin
      Internal (Get_Object (Widget), Primary, Secondary);
   end Modify_Cursor;

   ---------------
   -- Modify_Fg --
   ---------------

   procedure Modify_Fg
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Type;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_fg");
   begin
      Internal (Get_Object (Widget), State, Gdk.Color.Gdk_Color_Or_Null (Color'Address));
   end Modify_Fg;

   -----------------
   -- Modify_Font --
   -----------------

   procedure Modify_Font
      (Widget    : not null access Gtk_Widget_Record;
       Font_Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Widget    : System.Address;
          Font_Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "gtk_widget_modify_font");
   begin
      Internal (Get_Object (Widget), Font_Desc);
   end Modify_Font;

   -----------------
   -- Modify_Text --
   -----------------

   procedure Modify_Text
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type;
       Color  : Gdk.Color.Gdk_Color)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Type;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_text");
   begin
      Internal (Get_Object (Widget), State, Gdk.Color.Gdk_Color_Or_Null (Color'Address));
   end Modify_Text;

   -------------------------------
   -- Override_Background_Color --
   -------------------------------

   procedure Override_Background_Color
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Color  : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_override_background_color");
   begin
      Internal (Get_Object (Widget), State, Gdk.RGBA.Gdk_RGBA_Or_Null (Color'Address));
   end Override_Background_Color;

   --------------------
   -- Override_Color --
   --------------------

   procedure Override_Color
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Color  : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_override_color");
   begin
      Internal (Get_Object (Widget), State, Gdk.RGBA.Gdk_RGBA_Or_Null (Color'Address));
   end Override_Color;

   ---------------------
   -- Override_Cursor --
   ---------------------

   procedure Override_Cursor
      (Widget           : not null access Gtk_Widget_Record;
       Cursor           : Gdk.RGBA.Gdk_RGBA;
       Secondary_Cursor : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Widget           : System.Address;
          Cursor           : System.Address;
          Secondary_Cursor : System.Address);
      pragma Import (C, Internal, "gtk_widget_override_cursor");
   begin
      Internal (Get_Object (Widget), Gdk.RGBA.Gdk_RGBA_Or_Null (Cursor'Address), Gdk.RGBA.Gdk_RGBA_Or_Null (Secondary_Cursor'Address));
   end Override_Cursor;

   -------------------
   -- Override_Font --
   -------------------

   procedure Override_Font
      (Widget    : not null access Gtk_Widget_Record;
       Font_Desc : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Widget    : System.Address;
          Font_Desc : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "gtk_widget_override_font");
   begin
      Internal (Get_Object (Widget), Font_Desc);
   end Override_Font;

   -----------------------------
   -- Override_Symbolic_Color --
   -----------------------------

   procedure Override_Symbolic_Color
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Color  : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Interfaces.C.Strings.chars_ptr;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_override_symbolic_color");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name, Gdk.RGBA.Gdk_RGBA_Or_Null (Color'Address));
      Free (Tmp_Name);
   end Override_Symbolic_Color;

   --------------------------
   -- Queue_Compute_Expand --
   --------------------------

   procedure Queue_Compute_Expand
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_compute_expand");
   begin
      Internal (Get_Object (Widget));
   end Queue_Compute_Expand;

   ----------------
   -- Queue_Draw --
   ----------------

   procedure Queue_Draw (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_draw");
   begin
      Internal (Get_Object (Widget));
   end Queue_Draw;

   ---------------------
   -- Queue_Draw_Area --
   ---------------------

   procedure Queue_Draw_Area
      (Widget : not null access Gtk_Widget_Record;
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

   -----------------------
   -- Queue_Draw_Region --
   -----------------------

   procedure Queue_Draw_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Widget : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gtk_widget_queue_draw_region");
   begin
      Internal (Get_Object (Widget), Region);
   end Queue_Draw_Region;

   ------------------
   -- Queue_Resize --
   ------------------

   procedure Queue_Resize (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_resize");
   begin
      Internal (Get_Object (Widget));
   end Queue_Resize;

   ----------------------------
   -- Queue_Resize_No_Redraw --
   ----------------------------

   procedure Queue_Resize_No_Redraw
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_resize_no_redraw");
   begin
      Internal (Get_Object (Widget));
   end Queue_Resize_No_Redraw;

   -------------
   -- Realize --
   -------------

   procedure Realize (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_realize");
   begin
      Internal (Get_Object (Widget));
   end Realize;

   ----------------------
   -- Region_Intersect --
   ----------------------

   function Region_Intersect
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region) return Cairo.Region.Cairo_Region
   is
      function Internal
         (Widget : System.Address;
          Region : Cairo.Region.Cairo_Region)
          return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gtk_widget_region_intersect");
   begin
      return Internal (Get_Object (Widget), Region);
   end Region_Intersect;

   ------------------------
   -- Remove_Accelerator --
   ------------------------

   function Remove_Accelerator
      (Widget      : not null access Gtk_Widget_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key   : Gtk.Accel_Group.Gtk_Accel_Key;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Widget      : System.Address;
          Accel_Group : System.Address;
          Accel_Key   : Gtk.Accel_Group.Gtk_Accel_Key;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Integer;
      pragma Import (C, Internal, "gtk_widget_remove_accelerator");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Get_Object (Accel_Group), Accel_Key, Accel_Mods));
   end Remove_Accelerator;

   ---------------------------
   -- Remove_Mnemonic_Label --
   ---------------------------

   procedure Remove_Mnemonic_Label
      (Widget : not null access Gtk_Widget_Record;
       Label  : not null access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address; Label : System.Address);
      pragma Import (C, Internal, "gtk_widget_remove_mnemonic_label");
   begin
      Internal (Get_Object (Widget), Get_Object (Label));
   end Remove_Mnemonic_Label;

   -----------------
   -- Render_Icon --
   -----------------

   function Render_Icon
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size;
       Detail   : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Widget   : System.Address;
          Stock_Id : Interfaces.C.Strings.chars_ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size;
          Detail   : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_widget_render_icon");
      Tmp_Stock_Id    : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Detail      : Interfaces.C.Strings.chars_ptr;
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      if Detail = "" then
         Tmp_Detail := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Detail := New_String (Detail);
      end if;
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Stock_Id, Size, Tmp_Detail);
      Free (Tmp_Detail);
      Free (Tmp_Stock_Id);
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Render_Icon;

   ------------------------
   -- Render_Icon_Pixbuf --
   ------------------------

   function Render_Icon_Pixbuf
      (Widget   : not null access Gtk_Widget_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Widget   : System.Address;
          Stock_Id : Interfaces.C.Strings.chars_ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_widget_render_icon_pixbuf");
      Tmp_Stock_Id    : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Stock_Id, Size);
      Free (Tmp_Stock_Id);
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Render_Icon_Pixbuf;

   --------------
   -- Reparent --
   --------------

   procedure Reparent
      (Widget     : not null access Gtk_Widget_Record;
       New_Parent : not null access Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Widget     : System.Address;
          New_Parent : System.Address);
      pragma Import (C, Internal, "gtk_widget_reparent");
   begin
      Internal (Get_Object (Widget), Get_Object (New_Parent));
   end Reparent;

   ---------------------
   -- Reset_Rc_Styles --
   ---------------------

   procedure Reset_Rc_Styles (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_reset_rc_styles");
   begin
      Internal (Get_Object (Widget));
   end Reset_Rc_Styles;

   -----------------
   -- Reset_Style --
   -----------------

   procedure Reset_Style (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_reset_style");
   begin
      Internal (Get_Object (Widget));
   end Reset_Style;

   -----------------
   -- Send_Expose --
   -----------------

   function Send_Expose
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Gint
   is
      function Internal
         (Widget : System.Address;
          Event  : Gdk.Event.Gdk_Event) return Gint;
      pragma Import (C, Internal, "gtk_widget_send_expose");
   begin
      return Internal (Get_Object (Widget), Event);
   end Send_Expose;

   -----------------------
   -- Send_Focus_Change --
   -----------------------

   function Send_Focus_Change
      (Widget : not null access Gtk_Widget_Record;
       Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Widget : System.Address;
          Event  : Gdk.Event.Gdk_Event) return Integer;
      pragma Import (C, Internal, "gtk_widget_send_focus_change");
   begin
      return Boolean'Val (Internal (Get_Object (Widget), Event));
   end Send_Focus_Change;

   --------------------
   -- Set_Accel_Path --
   --------------------

   procedure Set_Accel_Path
      (Widget      : not null access Gtk_Widget_Record;
       Accel_Path  : UTF8_String := "";
       Accel_Group : access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class)
   is
      procedure Internal
         (Widget      : System.Address;
          Accel_Path  : Interfaces.C.Strings.chars_ptr;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_accel_path");
      Tmp_Accel_Path : Interfaces.C.Strings.chars_ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Accel_Path := New_String (Accel_Path);
      end if;
      Internal (Get_Object (Widget), Tmp_Accel_Path, Get_Object_Or_Null (GObject (Accel_Group)));
      Free (Tmp_Accel_Path);
   end Set_Accel_Path;

   --------------------
   -- Set_Allocation --
   --------------------

   procedure Set_Allocation
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : in out Gtk_Allocation)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : in out Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_set_allocation");
   begin
      Internal (Get_Object (Widget), Allocation);
   end Set_Allocation;

   -----------------------
   -- Set_App_Paintable --
   -----------------------

   procedure Set_App_Paintable
      (Widget        : not null access Gtk_Widget_Record;
       App_Paintable : Boolean)
   is
      procedure Internal (Widget : System.Address; App_Paintable : Integer);
      pragma Import (C, Internal, "gtk_widget_set_app_paintable");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (App_Paintable));
   end Set_App_Paintable;

   ---------------------
   -- Set_Can_Default --
   ---------------------

   procedure Set_Can_Default
      (Widget      : not null access Gtk_Widget_Record;
       Can_Default : Boolean)
   is
      procedure Internal (Widget : System.Address; Can_Default : Integer);
      pragma Import (C, Internal, "gtk_widget_set_can_default");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Can_Default));
   end Set_Can_Default;

   -------------------
   -- Set_Can_Focus --
   -------------------

   procedure Set_Can_Focus
      (Widget    : not null access Gtk_Widget_Record;
       Can_Focus : Boolean)
   is
      procedure Internal (Widget : System.Address; Can_Focus : Integer);
      pragma Import (C, Internal, "gtk_widget_set_can_focus");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Can_Focus));
   end Set_Can_Focus;

   -----------------------
   -- Set_Child_Visible --
   -----------------------

   procedure Set_Child_Visible
      (Widget     : not null access Gtk_Widget_Record;
       Is_Visible : Boolean)
   is
      procedure Internal (Widget : System.Address; Is_Visible : Integer);
      pragma Import (C, Internal, "gtk_widget_set_child_visible");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Is_Visible));
   end Set_Child_Visible;

   ------------------------
   -- Set_Composite_Name --
   ------------------------

   procedure Set_Composite_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String)
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_set_composite_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name);
      Free (Tmp_Name);
   end Set_Composite_Name;

   ------------------------
   -- Set_Device_Enabled --
   ------------------------

   procedure Set_Device_Enabled
      (Widget  : not null access Gtk_Widget_Record;
       Device  : not null access Gdk.Device.Gdk_Device_Record'Class;
       Enabled : Boolean)
   is
      procedure Internal
         (Widget  : System.Address;
          Device  : System.Address;
          Enabled : Integer);
      pragma Import (C, Internal, "gtk_widget_set_device_enabled");
   begin
      Internal (Get_Object (Widget), Get_Object (Device), Boolean'Pos (Enabled));
   end Set_Device_Enabled;

   -----------------------
   -- Set_Device_Events --
   -----------------------

   procedure Set_Device_Events
      (Widget : not null access Gtk_Widget_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class;
       Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
         (Widget : System.Address;
          Device : System.Address;
          Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_set_device_events");
   begin
      Internal (Get_Object (Widget), Get_Object (Device), Events);
   end Set_Device_Events;

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction
      (Widget : not null access Gtk_Widget_Record;
       Dir    : Gtk.Enums.Gtk_Text_Direction)
   is
      procedure Internal
         (Widget : System.Address;
          Dir    : Gtk.Enums.Gtk_Text_Direction);
      pragma Import (C, Internal, "gtk_widget_set_direction");
   begin
      Internal (Get_Object (Widget), Dir);
   end Set_Direction;

   -------------------------
   -- Set_Double_Buffered --
   -------------------------

   procedure Set_Double_Buffered
      (Widget          : not null access Gtk_Widget_Record;
       Double_Buffered : Boolean)
   is
      procedure Internal
         (Widget          : System.Address;
          Double_Buffered : Integer);
      pragma Import (C, Internal, "gtk_widget_set_double_buffered");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Double_Buffered));
   end Set_Double_Buffered;

   ----------------
   -- Set_Events --
   ----------------

   procedure Set_Events
      (Widget : not null access Gtk_Widget_Record;
       Events : Gdk.Event.Gdk_Event_Mask)
   is
      procedure Internal
         (Widget : System.Address;
          Events : Gdk.Event.Gdk_Event_Mask);
      pragma Import (C, Internal, "gtk_widget_set_events");
   begin
      Internal (Get_Object (Widget), Events);
   end Set_Events;

   ----------------
   -- Set_Halign --
   ----------------

   procedure Set_Halign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align)
   is
      procedure Internal (Widget : System.Address; Align : Gtk_Align);
      pragma Import (C, Internal, "gtk_widget_set_halign");
   begin
      Internal (Get_Object (Widget), Align);
   end Set_Halign;

   ---------------------
   -- Set_Has_Tooltip --
   ---------------------

   procedure Set_Has_Tooltip
      (Widget      : not null access Gtk_Widget_Record;
       Has_Tooltip : Boolean)
   is
      procedure Internal (Widget : System.Address; Has_Tooltip : Integer);
      pragma Import (C, Internal, "gtk_widget_set_has_tooltip");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Has_Tooltip));
   end Set_Has_Tooltip;

   --------------------
   -- Set_Has_Window --
   --------------------

   procedure Set_Has_Window
      (Widget     : not null access Gtk_Widget_Record;
       Has_Window : Boolean)
   is
      procedure Internal (Widget : System.Address; Has_Window : Integer);
      pragma Import (C, Internal, "gtk_widget_set_has_window");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Has_Window));
   end Set_Has_Window;

   -----------------
   -- Set_Hexpand --
   -----------------

   procedure Set_Hexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean)
   is
      procedure Internal (Widget : System.Address; Expand : Integer);
      pragma Import (C, Internal, "gtk_widget_set_hexpand");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Expand));
   end Set_Hexpand;

   ---------------------
   -- Set_Hexpand_Set --
   ---------------------

   procedure Set_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean)
   is
      procedure Internal (Widget : System.Address; Set : Integer);
      pragma Import (C, Internal, "gtk_widget_set_hexpand_set");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Set));
   end Set_Hexpand_Set;

   ----------------
   -- Set_Mapped --
   ----------------

   procedure Set_Mapped
      (Widget : not null access Gtk_Widget_Record;
       Mapped : Boolean)
   is
      procedure Internal (Widget : System.Address; Mapped : Integer);
      pragma Import (C, Internal, "gtk_widget_set_mapped");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Mapped));
   end Set_Mapped;

   -----------------------
   -- Set_Margin_Bottom --
   -----------------------

   procedure Set_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record;
       Margin : Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_bottom");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Bottom;

   ---------------------
   -- Set_Margin_Left --
   ---------------------

   procedure Set_Margin_Left
      (Widget : not null access Gtk_Widget_Record;
       Margin : Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_left");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Left;

   ----------------------
   -- Set_Margin_Right --
   ----------------------

   procedure Set_Margin_Right
      (Widget : not null access Gtk_Widget_Record;
       Margin : Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_right");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Right;

   --------------------
   -- Set_Margin_Top --
   --------------------

   procedure Set_Margin_Top
      (Widget : not null access Gtk_Widget_Record;
       Margin : Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_top");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Top;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String)
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_set_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   ---------------------
   -- Set_No_Show_All --
   ---------------------

   procedure Set_No_Show_All
      (Widget      : not null access Gtk_Widget_Record;
       No_Show_All : Boolean)
   is
      procedure Internal (Widget : System.Address; No_Show_All : Integer);
      pragma Import (C, Internal, "gtk_widget_set_no_show_all");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (No_Show_All));
   end Set_No_Show_All;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
      (Widget : not null access Gtk_Widget_Record;
       Parent : not null access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_parent");
   begin
      Internal (Get_Object (Widget), Get_Object (Parent));
   end Set_Parent;

   -----------------------
   -- Set_Parent_Window --
   -----------------------

   procedure Set_Parent_Window
      (Widget        : not null access Gtk_Widget_Record;
       Parent_Window : Gdk.Gdk_Window)
   is
      procedure Internal
         (Widget        : System.Address;
          Parent_Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_set_parent_window");
   begin
      Internal (Get_Object (Widget), Parent_Window);
   end Set_Parent_Window;

   ------------------
   -- Set_Realized --
   ------------------

   procedure Set_Realized
      (Widget   : not null access Gtk_Widget_Record;
       Realized : Boolean)
   is
      procedure Internal (Widget : System.Address; Realized : Integer);
      pragma Import (C, Internal, "gtk_widget_set_realized");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Realized));
   end Set_Realized;

   --------------------------
   -- Set_Receives_Default --
   --------------------------

   procedure Set_Receives_Default
      (Widget           : not null access Gtk_Widget_Record;
       Receives_Default : Boolean)
   is
      procedure Internal
         (Widget           : System.Address;
          Receives_Default : Integer);
      pragma Import (C, Internal, "gtk_widget_set_receives_default");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Receives_Default));
   end Set_Receives_Default;

   ----------------------------
   -- Set_Redraw_On_Allocate --
   ----------------------------

   procedure Set_Redraw_On_Allocate
      (Widget             : not null access Gtk_Widget_Record;
       Redraw_On_Allocate : Boolean)
   is
      procedure Internal
         (Widget             : System.Address;
          Redraw_On_Allocate : Integer);
      pragma Import (C, Internal, "gtk_widget_set_redraw_on_allocate");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Redraw_On_Allocate));
   end Set_Redraw_On_Allocate;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
      (Widget    : not null access Gtk_Widget_Record;
       Sensitive : Boolean := True)
   is
      procedure Internal (Widget : System.Address; Sensitive : Integer);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : Gint := -1;
       Height : Gint := -1)
   is
      procedure Internal
         (Widget : System.Address;
          Width  : Gint;
          Height : Gint);
      pragma Import (C, Internal, "gtk_widget_set_size_request");
   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_Size_Request;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Widget : not null access Gtk_Widget_Record;
       State  : Gtk.Enums.Gtk_State_Type)
   is
      procedure Internal
         (Widget : System.Address;
          State  : Gtk.Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_widget_set_state");
   begin
      Internal (Get_Object (Widget), State);
   end Set_State;

   ---------------------
   -- Set_State_Flags --
   ---------------------

   procedure Set_State_Flags
      (Widget : not null access Gtk_Widget_Record;
       Flags  : Gtk.Enums.Gtk_State_Flags;
       Clear  : Boolean)
   is
      procedure Internal
         (Widget : System.Address;
          Flags  : Gtk.Enums.Gtk_State_Flags;
          Clear  : Integer);
      pragma Import (C, Internal, "gtk_widget_set_state_flags");
   begin
      Internal (Get_Object (Widget), Flags, Boolean'Pos (Clear));
   end Set_State_Flags;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
      (Widget : not null access Gtk_Widget_Record;
       Style  : access Gtk.Style.Gtk_Style_Record'Class)
   is
      procedure Internal (Widget : System.Address; Style : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_style");
   begin
      Internal (Get_Object (Widget), Get_Object_Or_Null (GObject (Style)));
   end Set_Style;

   -----------------------------
   -- Set_Support_Multidevice --
   -----------------------------

   procedure Set_Support_Multidevice
      (Widget              : not null access Gtk_Widget_Record;
       Support_Multidevice : Boolean)
   is
      procedure Internal
         (Widget              : System.Address;
          Support_Multidevice : Integer);
      pragma Import (C, Internal, "gtk_widget_set_support_multidevice");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Support_Multidevice));
   end Set_Support_Multidevice;

   ------------------------
   -- Set_Tooltip_Markup --
   ------------------------

   procedure Set_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record;
       Markup : UTF8_String := "")
   is
      procedure Internal
         (Widget : System.Address;
          Markup : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_markup");
      Tmp_Markup : Interfaces.C.Strings.chars_ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Markup := New_String (Markup);
      end if;
      Internal (Get_Object (Widget), Tmp_Markup);
      Free (Tmp_Markup);
   end Set_Tooltip_Markup;

   ----------------------
   -- Set_Tooltip_Text --
   ----------------------

   procedure Set_Tooltip_Text
      (Widget : not null access Gtk_Widget_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Widget : System.Address;
          Text   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Widget), Tmp_Text);
      Free (Tmp_Text);
   end Set_Tooltip_Text;

   ------------------------
   -- Set_Tooltip_Window --
   ------------------------

   procedure Set_Tooltip_Window
      (Widget        : not null access Gtk_Widget_Record;
       Custom_Window : access Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Widget        : System.Address;
          Custom_Window : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_window");
   begin
      Internal (Get_Object (Widget), Get_Object_Or_Null (GObject (Custom_Window)));
   end Set_Tooltip_Window;

   ----------------
   -- Set_Valign --
   ----------------

   procedure Set_Valign
      (Widget : not null access Gtk_Widget_Record;
       Align  : Gtk_Align)
   is
      procedure Internal (Widget : System.Address; Align : Gtk_Align);
      pragma Import (C, Internal, "gtk_widget_set_valign");
   begin
      Internal (Get_Object (Widget), Align);
   end Set_Valign;

   -----------------
   -- Set_Vexpand --
   -----------------

   procedure Set_Vexpand
      (Widget : not null access Gtk_Widget_Record;
       Expand : Boolean)
   is
      procedure Internal (Widget : System.Address; Expand : Integer);
      pragma Import (C, Internal, "gtk_widget_set_vexpand");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Expand));
   end Set_Vexpand;

   ---------------------
   -- Set_Vexpand_Set --
   ---------------------

   procedure Set_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record;
       Set    : Boolean)
   is
      procedure Internal (Widget : System.Address; Set : Integer);
      pragma Import (C, Internal, "gtk_widget_set_vexpand_set");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Set));
   end Set_Vexpand_Set;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Widget  : not null access Gtk_Widget_Record;
       Visible : Boolean)
   is
      procedure Internal (Widget : System.Address; Visible : Integer);
      pragma Import (C, Internal, "gtk_widget_set_visible");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Visible));
   end Set_Visible;

   ----------------
   -- Set_Visual --
   ----------------

   procedure Set_Visual
      (Widget : not null access Gtk_Widget_Record;
       Visual : Gdk.Visual.Gdk_Visual)
   is
      procedure Internal
         (Widget : System.Address;
          Visual : Gdk.Visual.Gdk_Visual);
      pragma Import (C, Internal, "gtk_widget_set_visual");
   begin
      Internal (Get_Object (Widget), Visual);
   end Set_Visual;

   ----------------
   -- Set_Window --
   ----------------

   procedure Set_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Widget : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_set_window");
   begin
      Internal (Get_Object (Widget), Window);
   end Set_Window;

   --------------------------
   -- Shape_Combine_Region --
   --------------------------

   procedure Shape_Combine_Region
      (Widget : not null access Gtk_Widget_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Widget : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gtk_widget_shape_combine_region");
   begin
      Internal (Get_Object (Widget), Region);
   end Shape_Combine_Region;

   ----------
   -- Show --
   ----------

   procedure Show (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Show;

   --------------
   -- Show_All --
   --------------

   procedure Show_All (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_all");
   begin
      Internal (Get_Object (Widget));
   end Show_All;

   --------------
   -- Show_Now --
   --------------

   procedure Show_Now (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_now");
   begin
      Internal (Get_Object (Widget));
   end Show_Now;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : in out Gtk_Allocation)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : in out Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_size_allocate");
   begin
      Internal (Get_Object (Widget), Allocation);
   end Size_Allocate;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
      (Widget      : not null access Gtk_Widget_Record;
       Requisition : out Gtk_Requisition)
   is
      procedure Internal
         (Widget      : System.Address;
          Requisition : out Gtk_Requisition);
      pragma Import (C, Internal, "gtk_widget_size_request");
      Tmp_Requisition : aliased Gtk_Requisition;
   begin
      Internal (Get_Object (Widget), Tmp_Requisition);
      Requisition := Tmp_Requisition;
   end Size_Request;

   ------------------
   -- Style_Attach --
   ------------------

   procedure Style_Attach (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_style_attach");
   begin
      Internal (Get_Object (Widget));
   end Style_Attach;

   ------------------------
   -- Style_Get_Property --
   ------------------------

   procedure Style_Get_Property
      (Widget        : not null access Gtk_Widget_Record;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Widget        : System.Address;
          Property_Name : Interfaces.C.Strings.chars_ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_widget_style_get_property");
      Tmp_Property_Name : Interfaces.C.Strings.chars_ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Widget), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Style_Get_Property;

   -----------------------
   -- Thaw_Child_Notify --
   -----------------------

   procedure Thaw_Child_Notify (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_thaw_child_notify");
   begin
      Internal (Get_Object (Widget));
   end Thaw_Child_Notify;

   ---------------------------
   -- Translate_Coordinates --
   ---------------------------

   procedure Translate_Coordinates
      (Widget      : not null access Gtk_Widget_Record;
       Dest_Widget : not null access Gtk_Widget_Record'Class;
       Src_X       : Gint;
       Src_Y       : Gint;
       Dest_X      : out Gint;
       Dest_Y      : out Gint;
       Result      : out Boolean)
   is
      function Internal
         (Widget      : System.Address;
          Dest_Widget : System.Address;
          Src_X       : Gint;
          Src_Y       : Gint;
          Acc_Dest_X  : access Gint;
          Acc_Dest_Y  : access Gint) return Integer;
      pragma Import (C, Internal, "gtk_widget_translate_coordinates");
      Acc_Dest_X : aliased Gint;
      Acc_Dest_Y : aliased Gint;
      Tmp_Return : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Get_Object (Dest_Widget), Src_X, Src_Y, Acc_Dest_X'Access, Acc_Dest_Y'Access);
      Dest_X := Acc_Dest_X;
      Dest_Y := Acc_Dest_Y;
      Result := Boolean'Val (Tmp_Return);
   end Translate_Coordinates;

   ---------------------------
   -- Trigger_Tooltip_Query --
   ---------------------------

   procedure Trigger_Tooltip_Query
      (Widget : not null access Gtk_Widget_Record)
   is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_trigger_tooltip_query");
   begin
      Internal (Get_Object (Widget));
   end Trigger_Tooltip_Query;

   -----------
   -- Unmap --
   -----------

   procedure Unmap (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_unmap");
   begin
      Internal (Get_Object (Widget));
   end Unmap;

   --------------
   -- Unparent --
   --------------

   procedure Unparent (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_unparent");
   begin
      Internal (Get_Object (Widget));
   end Unparent;

   ---------------
   -- Unrealize --
   ---------------

   procedure Unrealize (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_unrealize");
   begin
      Internal (Get_Object (Widget));
   end Unrealize;

   -----------------------
   -- Unset_State_Flags --
   -----------------------

   procedure Unset_State_Flags
      (Widget : not null access Gtk_Widget_Record;
       Flags  : Gtk.Enums.Gtk_State_Flags)
   is
      procedure Internal
         (Widget : System.Address;
          Flags  : Gtk.Enums.Gtk_State_Flags);
      pragma Import (C, Internal, "gtk_widget_unset_state_flags");
   begin
      Internal (Get_Object (Widget), Flags);
   end Unset_State_Flags;

   ---------------------------
   -- Get_Default_Direction --
   ---------------------------

   function Get_Default_Direction return Gtk.Enums.Gtk_Text_Direction is
      function Internal return Gtk.Enums.Gtk_Text_Direction;
      pragma Import (C, Internal, "gtk_widget_get_default_direction");
   begin
      return Internal;
   end Get_Default_Direction;

   -----------------------
   -- Get_Default_Style --
   -----------------------

   function Get_Default_Style return Gtk.Style.Gtk_Style is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_default_style");
      Stub_Gtk_Style : Gtk.Style.Gtk_Style_Record;
   begin
      return Gtk.Style.Gtk_Style (Get_User_Data (Internal, Stub_Gtk_Style));
   end Get_Default_Style;

   -------------------------
   -- Pop_Composite_Child --
   -------------------------

   procedure Pop_Composite_Child is
      procedure Internal;
      pragma Import (C, Internal, "gtk_widget_pop_composite_child");
   begin
      Internal;
   end Pop_Composite_Child;

   --------------------------
   -- Push_Composite_Child --
   --------------------------

   procedure Push_Composite_Child is
      procedure Internal;
      pragma Import (C, Internal, "gtk_widget_push_composite_child");
   begin
      Internal;
   end Push_Composite_Child;

   ---------------------------
   -- Set_Default_Direction --
   ---------------------------

   procedure Set_Default_Direction (Dir : Gtk.Enums.Gtk_Text_Direction) is
      procedure Internal (Dir : Gtk.Enums.Gtk_Text_Direction);
      pragma Import (C, Internal, "gtk_widget_set_default_direction");
   begin
      Internal (Dir);
   end Set_Default_Direction;

   ------------------------
   -- Should_Draw_Window --
   ------------------------

   function Should_Draw_Window
      (Cr     : Cairo.Cairo_Context;
       Window : Gdk.Gdk_Window) return Boolean
   is
      function Internal
         (Cr     : Cairo.Cairo_Context;
          Window : Gdk.Gdk_Window) return Integer;
      pragma Import (C, Internal, "gtk_cairo_should_draw_window");
   begin
      return Boolean'Val (Internal (Cr, Window));
   end Should_Draw_Window;

   -------------------------
   -- Transform_To_Window --
   -------------------------

   procedure Transform_To_Window
      (Cr     : Cairo.Cairo_Context;
       Widget : not null access Gtk_Widget_Record'Class;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal
         (Cr     : Cairo.Cairo_Context;
          Widget : System.Address;
          Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_cairo_transform_to_window");
   begin
      Internal (Cr, Get_Object (Widget), Window);
   end Transform_To_Window;

end Gtk.Widget;
