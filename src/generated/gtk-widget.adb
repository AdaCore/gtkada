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
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

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

   function Inherited_Draw
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context) return Boolean
   is
      function Internal (Klass : Ada_GObject_Class;
         Widget : System.Address;
         Cr : Cairo.Cairo_Context) return Gboolean;
      pragma Import (C, Internal, "ada_inherited_WIDGET_CLASS_draw");
   begin
      return Internal (Klass, Widget.Get_Object, Cr) /= 0;
   end Inherited_Draw;

   procedure Inherited_Get_Preferred_Width
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Minimum_Size, Natural_Size : out Glib.Gint)
   is
      procedure Internal (Klass : Ada_GObject_Class;
         Widget : System.Address;
         Min, Nat : out Glib.Gint);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_get_preferred_width");
   begin
      Internal (Klass, Widget.Get_Object, Minimum_Size, Natural_Size);
   end Inherited_Get_Preferred_Width;

   procedure Inherited_Get_Preferred_Width_For_Height
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Height     : Glib.Gint;
      Minimum_Size, Natural_Size : out Glib.Gint)
   is
      procedure Internal (Klass    : Ada_GObject_Class;
         Widget   : System.Address;
         Height   : Glib.Gint;
         Min, Nat : out Glib.Gint);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_get_preferred_width_for_height");
   begin
      Internal (Klass, Widget.Get_Object, Height, Minimum_Size, Natural_Size);
   end Inherited_Get_Preferred_Width_For_Height;

   procedure Inherited_Get_Preferred_Height
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Minimum_Size, Natural_Size : out Glib.Gint)
   is
      procedure Internal (Klass : Ada_GObject_Class;
         Widget : System.Address;
         Min, Nat : out Glib.Gint);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_get_preferred_height");
   begin
      Internal (Klass, Widget.Get_Object, Minimum_Size, Natural_Size);
   end Inherited_Get_Preferred_Height;

   procedure Inherited_Get_Preferred_Height_For_Width
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Width      : Glib.Gint;
      Minimum_Size, Natural_Size : out Glib.Gint)
   is
      procedure Internal (Klass : Ada_GObject_Class;
         Widget : System.Address;
         Width      : Glib.Gint;
         Min, Nat : out Glib.Gint);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_get_preferred_height_for_width");
   begin
      Internal (Klass, Widget.Get_Object, Width, Minimum_Size, Natural_Size);
   end Inherited_Get_Preferred_Height_For_Width;

   procedure Inherited_Size_Allocate
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class;
      Allocation : Gtk_Allocation)
   is
      procedure Internal (Klass : Ada_GObject_Class;
         Widget : System.Address;
         Allocation : Gtk_Allocation);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_size_allocate");
   begin
      Internal (Klass, Widget.Get_Object, Allocation);
   end Inherited_Size_Allocate;

   procedure Inherited_Realize
     (Klass      : Glib.Object.Ada_GObject_Class;
      Widget     : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Klass : Ada_GObject_Class;
         Widget : System.Address);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_realize");
   begin
      Internal (Klass, Widget.Get_Object);
   end Inherited_Realize;

   procedure Set_Default_Draw_Handler
     (Klass : Glib.Object.GObject_Class; Handler : Draw_Handler)
   is
      procedure Internal (K : GObject_Class; H : System.Address);
      pragma Import (C, Internal, "ada_WIDGET_CLASS_override_draw");
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

   function C_Gtk_Widget_Add_Tick_Callback
      (Widget    : System.Address;
       Callback  : System.Address;
       User_Data : System.Address;
       Notify    : System.Address) return Guint;
   pragma Import (C, C_Gtk_Widget_Add_Tick_Callback, "gtk_widget_add_tick_callback");
   --  Queues an animation frame update and adds a callback to be called
   --  before each frame. Until the tick callback is removed, it will be called
   --  frequently (usually at the frame rate of the output device or as quickly
   --  as the application can be repainted, whichever is slower). For this
   --  reason, is most suitable for handling graphics that change every frame
   --  or every few frames. The tick callback does not automatically imply a
   --  relayout or repaint. If you want a repaint or relayout, and aren't
   --  changing widget properties that would trigger that (for example,
   --  changing the text of a Gtk.Label.Gtk_Label), then you will have to call
   --  Gtk.Widget.Queue_Resize or Gtk.Widget.Queue_Draw_Area yourself.
   --  Gdk.Frame_Clock.Get_Frame_Time should generally be used for timing
   --  continuous animations and
   --  Gdk.Frame_Timings.Get_Predicted_Presentation_Time if you are trying to
   --  display isolated frames at particular times.
   --  This is a more convenient alternative to connecting directly to the
   --  Gdk.Frame_Clock.Gdk_Frame_Clock::update signal of
   --  Gdk.Frame_Clock.Gdk_Frame_Clock, since you don't have to worry about
   --  when a Gdk.Frame_Clock.Gdk_Frame_Clock is assigned to a widget.
   --  Since: gtk+ 3.8
   --  "callback": function to call for updating animations
   --  "user_data": data to pass to Callback
   --  "notify": function to call to free User_Data when the callback is
   --  removed.

   procedure C_Gtk_Widget_Class_Set_Connect_Func
      (Self                 : Glib.Object.GObject_Class;
       Connect_Func         : System.Address;
       Connect_Data         : System.Address;
       Connect_Data_Destroy : Glib.G_Destroy_Notify_Address);
   pragma Import (C, C_Gtk_Widget_Class_Set_Connect_Func, "gtk_widget_class_set_connect_func");
   --  For use in language bindings, this will override the default
   --  Gtk_Builder_Connect_Func to be used when parsing GtkBuilder XML from
   --  this class's template data.
   --  Note that this must be called from a composite widget classes class
   --  initializer after calling gtk_widget_class_set_template.
   --  Since: gtk+ 3.10
   --  "connect_func": The Gtk_Builder_Connect_Func to use when connecting
   --  signals in the class template
   --  "connect_data": The data to pass to Connect_Func
   --  "connect_data_destroy": The Glib.G_Destroy_Notify_Address to free
   --  Connect_Data, this will only be used at class finalization time, when no
   --  classes of type Widget_Type are in use anymore.

   function To_Gtk_Tick_Callback is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Tick_Callback);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Tick_Callback, System.Address);

   function To_Gtk_Builder_Connect_Func is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Builder_Connect_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk_Builder_Connect_Func, System.Address);

   procedure Internal_Gtk_Builder_Connect_Func
      (Builder        : System.Address;
       Object         : System.Address;
       Signal_Name    : Gtkada.Types.Chars_Ptr;
       Handler_Name   : Gtkada.Types.Chars_Ptr;
       Connect_Object : System.Address;
       Flags          : Glib.G_Connect_Flags;
       User_Data      : System.Address);
   pragma Convention (C, Internal_Gtk_Builder_Connect_Func);
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "object": object to connect a signal to
   --  "signal_name": name of the signal
   --  "handler_name": name of the handler
   --  "connect_object": a Glib.Object.GObject, if non-null, use
   --  g_signal_connect_object
   --  "flags": Glib.G_Connect_Flags to use
   --  "user_data": user data

   function Internal_Gtk_Tick_Callback
      (Widget      : System.Address;
       Frame_Clock : System.Address;
       User_Data   : System.Address) return Glib.Gboolean;
   pragma Convention (C, Internal_Gtk_Tick_Callback);
   --  "widget": the widget
   --  "frame_clock": the frame clock for the widget (same as calling
   --  Gtk.Widget.Get_Frame_Clock)
   --  "user_data": user data passed to Gtk.Widget.Add_Tick_Callback.

   ---------------------------------------
   -- Internal_Gtk_Builder_Connect_Func --
   ---------------------------------------

   procedure Internal_Gtk_Builder_Connect_Func
      (Builder        : System.Address;
       Object         : System.Address;
       Signal_Name    : Gtkada.Types.Chars_Ptr;
       Handler_Name   : Gtkada.Types.Chars_Ptr;
       Connect_Object : System.Address;
       Flags          : Glib.G_Connect_Flags;
       User_Data      : System.Address)
   is
      Func             : constant Gtk_Builder_Connect_Func := To_Gtk_Builder_Connect_Func (User_Data);
      Stub_Gtk_Builder : Gtk.Builder.Gtk_Builder_Record;
      Stub_GObject     : Glib.Object.GObject_Record;
   begin
      Func (Gtk.Builder.Gtk_Builder (Get_User_Data (Builder, Stub_Gtk_Builder)), Get_User_Data (Object, Stub_GObject), Gtkada.Bindings.Value_Allowing_Null (Signal_Name), Gtkada.Bindings.Value_Allowing_Null (Handler_Name), Get_User_Data (Connect_Object, Stub_GObject), Flags);
   end Internal_Gtk_Builder_Connect_Func;

   --------------------------------
   -- Internal_Gtk_Tick_Callback --
   --------------------------------

   function Internal_Gtk_Tick_Callback
      (Widget      : System.Address;
       Frame_Clock : System.Address;
       User_Data   : System.Address) return Glib.Gboolean
   is
      Func                 : constant Gtk_Tick_Callback := To_Gtk_Tick_Callback (User_Data);
      Stub_Gtk_Widget      : Gtk_Widget_Record;
      Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
   begin
      return Boolean'Pos (Func (Gtk.Widget.Gtk_Widget (Get_User_Data (Widget, Stub_Gtk_Widget)), Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Frame_Clock, Stub_Gdk_Frame_Clock))));
   end Internal_Gtk_Tick_Callback;

   package Type_Conversion_Gtk_Widget is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Widget_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Widget);

   --------------
   -- Activate --
   --------------

   function Activate
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Activate;

   ---------------------
   -- Add_Accelerator --
   ---------------------

   procedure Add_Accelerator
      (Widget       : not null access Gtk_Widget_Record;
       Accel_Signal : Glib.Signal_Name;
       Accel_Group  : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key    : Gdk.Types.Gdk_Key_Type;
       Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
       Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags)
   is
      procedure Internal
         (Widget       : System.Address;
          Accel_Signal : Gtkada.Types.Chars_Ptr;
          Accel_Group  : System.Address;
          Accel_Key    : Gdk.Types.Gdk_Key_Type;
          Accel_Mods   : Gdk.Types.Gdk_Modifier_Type;
          Accel_Flags  : Gtk.Accel_Group.Gtk_Accel_Flags);
      pragma Import (C, Internal, "gtk_widget_add_accelerator");
      Tmp_Accel_Signal : Gtkada.Types.Chars_Ptr := New_String (String (Accel_Signal));
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

   -----------------------
   -- Add_Tick_Callback --
   -----------------------

   function Add_Tick_Callback
      (Widget   : not null access Gtk_Widget_Record;
       Callback : Gtk_Tick_Callback) return Guint
   is
   begin
      if Callback = null then
         return C_Gtk_Widget_Add_Tick_Callback (Get_Object (Widget), System.Null_Address, System.Null_Address, System.Null_Address);
      else
         return C_Gtk_Widget_Add_Tick_Callback (Get_Object (Widget), Internal_Gtk_Tick_Callback'Address, To_Address (Callback), System.Null_Address);
      end if;
   end Add_Tick_Callback;

   package body Add_Tick_Callback_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Tick_Callback is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Tick_Callback);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Tick_Callback, System.Address);

      function Internal_Cb
         (Widget      : System.Address;
          Frame_Clock : System.Address;
          User_Data   : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Cb);
      --  Callback type for adding a function to update animations. See
      --  Gtk.Widget.Add_Tick_Callback.
      --  Since: gtk+ 3.8
      --  "widget": the widget
      --  "frame_clock": the frame clock for the widget (same as calling
      --  Gtk.Widget.Get_Frame_Clock)
      --  "user_data": user data passed to Gtk.Widget.Add_Tick_Callback.

      -----------------------
      -- Add_Tick_Callback --
      -----------------------

      function Add_Tick_Callback
         (Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
          Callback  : Gtk_Tick_Callback;
          User_Data : User_Data_Type) return Guint
      is
         D : System.Address;
      begin
         if Callback = null then
            return C_Gtk_Widget_Add_Tick_Callback (Get_Object (Widget), System.Null_Address, System.Null_Address, Users.Free_Data'Address);
         else
            D := Users.Build (To_Address (Callback), User_Data);
            return C_Gtk_Widget_Add_Tick_Callback (Get_Object (Widget), Internal_Cb'Address, D, Users.Free_Data'Address);
         end if;
      end Add_Tick_Callback;

      -----------------
      -- Internal_Cb --
      -----------------

      function Internal_Cb
         (Widget      : System.Address;
          Frame_Clock : System.Address;
          User_Data   : System.Address) return Glib.Gboolean
      is
         D                    : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Widget      : Gtk.Widget.Gtk_Widget_Record;
         Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
      begin
         return Boolean'Pos (To_Gtk_Tick_Callback (D.Func) (Gtk.Widget.Gtk_Widget (Get_User_Data (Widget, Stub_Gtk_Widget)), Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Frame_Clock, Stub_Gdk_Frame_Clock)), D.Data.all));
      end Internal_Cb;

   end Add_Tick_Callback_User_Data;

   ------------------------------
   -- Bind_Template_Child_Full --
   ------------------------------

   procedure Bind_Template_Child_Full
      (Self           : Glib.Object.GObject_Class;
       Name           : UTF8_String;
       Internal_Child : Boolean;
       Struct_Offset  : Gssize)
   is
      procedure Internal
         (Self           : Glib.Object.GObject_Class;
          Name           : Gtkada.Types.Chars_Ptr;
          Internal_Child : Glib.Gboolean;
          Struct_Offset  : Gssize);
      pragma Import (C, Internal, "gtk_widget_class_bind_template_child_full");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Self, Tmp_Name, Boolean'Pos (Internal_Child), Struct_Offset);
      Free (Tmp_Name);
   end Bind_Template_Child_Full;

   ------------------------
   -- Can_Activate_Accel --
   ------------------------

   function Can_Activate_Accel
      (Widget    : not null access Gtk_Widget_Record;
       Signal_Id : Guint) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Signal_Id : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_can_activate_accel");
   begin
      return Internal (Get_Object (Widget), Signal_Id) /= 0;
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
          Direction : Gtk.Enums.Gtk_Direction_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_child_focus");
   begin
      return Internal (Get_Object (Widget), Direction) /= 0;
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
          Child_Property : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_child_notify");
      Tmp_Child_Property : Gtkada.Types.Chars_Ptr := New_String (Child_Property);
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
          Orientation : Gtk.Enums.Gtk_Orientation) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_compute_expand");
   begin
      return Internal (Get_Object (Widget), Orientation) /= 0;
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
          Text   : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_widget_create_pango_layout");
      Tmp_Text          : Gtkada.Types.Chars_Ptr;
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
      Tmp_Return        : System.Address;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Text);
      Free (Tmp_Text);
      return Pango.Layout.Pango_Layout (Get_User_Data (Tmp_Return, Stub_Pango_Layout));
   end Create_Pango_Layout;

   -------------
   -- Destroy --
   -------------

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
          Device : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_device_is_shadowed");
   begin
      return Internal (Get_Object (Widget), Get_Object (Device)) /= 0;
   end Device_Is_Shadowed;

   ---------------------------------
   -- Drag_Begin_With_Coordinates --
   ---------------------------------

   function Drag_Begin_With_Coordinates
      (Widget  : not null access Gtk_Widget_Record;
       Targets : Gtk.Target_List.Gtk_Target_List;
       Actions : Gdk.Drag_Contexts.Gdk_Drag_Action;
       Button  : Glib.Gint;
       Event   : Gdk.Event.Gdk_Event;
       X       : Glib.Gint;
       Y       : Glib.Gint) return Gdk.Drag_Contexts.Drag_Context
   is
      function Internal
         (Widget  : System.Address;
          Targets : System.Address;
          Actions : Gdk.Drag_Contexts.Gdk_Drag_Action;
          Button  : Glib.Gint;
          Event   : Gdk.Event.Gdk_Event;
          X       : Glib.Gint;
          Y       : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_drag_begin_with_coordinates");
      Stub_Drag_Context : Gdk.Drag_Contexts.Drag_Context_Record;
   begin
      return Gdk.Drag_Contexts.Drag_Context (Get_User_Data (Internal (Get_Object (Widget), Get_Object (Targets), Actions, Button, Event, X, Y), Stub_Drag_Context));
   end Drag_Begin_With_Coordinates;

   --------------------------
   -- Drag_Check_Threshold --
   --------------------------

   function Drag_Check_Threshold
      (Widget    : not null access Gtk_Widget_Record;
       Start_X   : Glib.Gint;
       Start_Y   : Glib.Gint;
       Current_X : Glib.Gint;
       Current_Y : Glib.Gint) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Start_X   : Glib.Gint;
          Start_Y   : Glib.Gint;
          Current_X : Glib.Gint;
          Current_Y : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_drag_check_threshold");
   begin
      return Internal (Get_Object (Widget), Start_X, Start_Y, Current_X, Current_Y) /= 0;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_drag_dest_get_track_motion");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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
          Use_Coordinates : Glib.Gboolean);
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
      procedure Internal
         (Widget       : System.Address;
          Track_Motion : Glib.Gboolean);
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
       Time    : Guint32)
   is
      procedure Internal
         (Widget  : System.Address;
          Context : System.Address;
          Target  : Gdk.Types.Gdk_Atom;
          Time    : Guint32);
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
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
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
          Stock_Id : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_drag_source_set_icon_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
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
          Event  : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_event");
   begin
      return Internal (Get_Object (Widget), Event) /= 0;
   end Event;

   -------------------------
   -- Find_Style_Property --
   -------------------------

   function Find_Style_Property
      (Self          : Glib.Object.GObject_Class;
       Property_Name : UTF8_String) return Glib.Param_Spec
   is
      function Internal
         (Self          : Glib.Object.GObject_Class;
          Property_Name : Gtkada.Types.Chars_Ptr) return Glib.Param_Spec;
      pragma Import (C, Internal, "gtk_widget_class_find_style_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
      Tmp_Return        : Glib.Param_Spec;
   begin
      Tmp_Return := Internal (Self, Tmp_Property_Name);
      Free (Tmp_Property_Name);
      return Tmp_Return;
   end Find_Style_Property;

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

   ----------------------
   -- Get_Action_Group --
   ----------------------

   function Get_Action_Group
      (Widget : not null access Gtk_Widget_Record;
       Prefix : UTF8_String) return Glib.Action_Group.Gaction_Group
   is
      function Internal
         (Widget : System.Address;
          Prefix : Gtkada.Types.Chars_Ptr)
          return Glib.Action_Group.Gaction_Group;
      pragma Import (C, Internal, "gtk_widget_get_action_group");
      Tmp_Prefix : Gtkada.Types.Chars_Ptr := New_String (Prefix);
      Tmp_Return : Glib.Action_Group.Gaction_Group;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Prefix);
      Free (Tmp_Prefix);
      return Tmp_Return;
   end Get_Action_Group;

   ----------------------------
   -- Get_Allocated_Baseline --
   ----------------------------

   function Get_Allocated_Baseline
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_allocated_baseline");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocated_Baseline;

   --------------------------
   -- Get_Allocated_Height --
   --------------------------

   function Get_Allocated_Height
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_allocated_height");
   begin
      return Internal (Get_Object (Widget));
   end Get_Allocated_Height;

   ------------------------
   -- Get_Allocated_Size --
   ------------------------

   procedure Get_Allocated_Size
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : out Gtk_Allocation;
       Baseline   : out Glib.Gint)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : out Gtk_Allocation;
          Baseline   : out Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_get_allocated_size");
   begin
      Internal (Get_Object (Widget), Allocation, Baseline);
   end Get_Allocated_Size;

   -------------------------
   -- Get_Allocated_Width --
   -------------------------

   function Get_Allocated_Width
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_app_paintable");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_App_Paintable;

   ---------------------
   -- Get_Can_Default --
   ---------------------

   function Get_Can_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_can_default");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Can_Default;

   -------------------
   -- Get_Can_Focus --
   -------------------

   function Get_Can_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_can_focus");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_child_visible");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Child_Visible;

   --------------
   -- Get_Clip --
   --------------

   procedure Get_Clip
      (Widget : not null access Gtk_Widget_Record;
       Clip   : out Gtk_Allocation)
   is
      procedure Internal
         (Widget : System.Address;
          Clip   : out Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_get_clip");
   begin
      Internal (Get_Object (Widget), Clip);
   end Get_Clip;

   ------------------------
   -- Get_Composite_Name --
   ------------------------

   function Get_Composite_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_widget_get_composite_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Widget)));
   end Get_Composite_Name;

   ------------------
   -- Get_Css_Name --
   ------------------

   function Get_Css_Name
      (Self : Glib.Object.GObject_Class) return UTF8_String
   is
      function Internal
         (Self : Glib.Object.GObject_Class) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_widget_class_get_css_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Css_Name;

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
          Device : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_device_enabled");
   begin
      return Internal (Get_Object (Widget), Get_Object (Device)) /= 0;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_double_buffered");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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

   ------------------------
   -- Get_Focus_On_Click --
   ------------------------

   function Get_Focus_On_Click
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_focus_on_click");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Focus_On_Click;

   ------------------
   -- Get_Font_Map --
   ------------------

   function Get_Font_Map
      (Widget : not null access Gtk_Widget_Record)
       return Pango.Font_Map.Pango_Font_Map
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_font_map");
      Stub_Pango_Font_Map : Pango.Font_Map.Pango_Font_Map_Record;
   begin
      return Pango.Font_Map.Pango_Font_Map (Get_User_Data (Internal (Get_Object (Widget)), Stub_Pango_Font_Map));
   end Get_Font_Map;

   ----------------------
   -- Get_Font_Options --
   ----------------------

   function Get_Font_Options
      (Widget : not null access Gtk_Widget_Record)
       return Cairo.Cairo_Font_Options
   is
      function Internal
         (Widget : System.Address) return Cairo.Cairo_Font_Options;
      pragma Import (C, Internal, "gtk_widget_get_font_options");
   begin
      return Internal (Get_Object (Widget));
   end Get_Font_Options;

   ---------------------
   -- Get_Frame_Clock --
   ---------------------

   function Get_Frame_Clock
      (Widget : not null access Gtk_Widget_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_frame_clock");
      Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
   begin
      return Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gdk_Frame_Clock));
   end Get_Frame_Clock;

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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_has_tooltip");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Has_Tooltip;

   --------------------
   -- Get_Has_Window --
   --------------------

   function Get_Has_Window
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_has_window");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Has_Window;

   -----------------
   -- Get_Hexpand --
   -----------------

   function Get_Hexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_hexpand");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Hexpand;

   ---------------------
   -- Get_Hexpand_Set --
   ---------------------

   function Get_Hexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_hexpand_set");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Hexpand_Set;

   ----------------
   -- Get_Mapped --
   ----------------

   function Get_Mapped
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_mapped");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Mapped;

   -----------------------
   -- Get_Margin_Bottom --
   -----------------------

   function Get_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_bottom");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Bottom;

   --------------------
   -- Get_Margin_End --
   --------------------

   function Get_Margin_End
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_end");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_End;

   ---------------------
   -- Get_Margin_Left --
   ---------------------

   function Get_Margin_Left
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_left");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Left;

   ----------------------
   -- Get_Margin_Right --
   ----------------------

   function Get_Margin_Right
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_right");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Right;

   ----------------------
   -- Get_Margin_Start --
   ----------------------

   function Get_Margin_Start
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_start");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Start;

   --------------------
   -- Get_Margin_Top --
   --------------------

   function Get_Margin_Top
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_margin_top");
   begin
      return Internal (Get_Object (Widget));
   end Get_Margin_Top;

   -----------------------
   -- Get_Modifier_Mask --
   -----------------------

   function Get_Modifier_Mask
      (Widget : not null access Gtk_Widget_Record;
       Intent : Gdk_Modifier_Intent) return Gdk.Types.Gdk_Modifier_Type
   is
      function Internal
         (Widget : System.Address;
          Intent : Gdk_Modifier_Intent) return Gdk.Types.Gdk_Modifier_Type;
      pragma Import (C, Internal, "gtk_widget_get_modifier_mask");
   begin
      return Internal (Get_Object (Widget), Intent);
   end Get_Modifier_Mask;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_no_show_all");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_No_Show_All;

   -----------------
   -- Get_Opacity --
   -----------------

   function Get_Opacity
      (Widget : not null access Gtk_Widget_Record) return Gdouble
   is
      function Internal (Widget : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_widget_get_opacity");
   begin
      return Internal (Get_Object (Widget));
   end Get_Opacity;

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
       X      : out Glib.Gint;
       Y      : out Glib.Gint)
   is
      procedure Internal
         (Widget : System.Address;
          X      : out Glib.Gint;
          Y      : out Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_get_pointer");
   begin
      Internal (Get_Object (Widget), X, Y);
   end Get_Pointer;

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Widget         : not null access Gtk_Widget_Record;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Widget         : System.Address;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_height");
   begin
      Internal (Get_Object (Widget), Minimum_Height, Natural_Height);
   end Get_Preferred_Height;

   -------------------------------------------------
   -- Get_Preferred_Height_And_Baseline_For_Width --
   -------------------------------------------------

   procedure Get_Preferred_Height_And_Baseline_For_Width
      (Widget           : not null access Gtk_Widget_Record;
       Width            : Glib.Gint;
       Minimum_Height   : out Glib.Gint;
       Natural_Height   : out Glib.Gint;
       Minimum_Baseline : out Glib.Gint;
       Natural_Baseline : out Glib.Gint)
   is
      procedure Internal
         (Widget           : System.Address;
          Width            : Glib.Gint;
          Minimum_Height   : out Glib.Gint;
          Natural_Height   : out Glib.Gint;
          Minimum_Baseline : out Glib.Gint;
          Natural_Baseline : out Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_height_and_baseline_for_width");
   begin
      Internal (Get_Object (Widget), Width, Minimum_Height, Natural_Height, Minimum_Baseline, Natural_Baseline);
   end Get_Preferred_Height_And_Baseline_For_Width;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Widget         : not null access Gtk_Widget_Record;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Widget         : System.Address;
          Width          : Glib.Gint;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
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
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Widget        : System.Address;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_get_preferred_width");
   begin
      Internal (Get_Object (Widget), Minimum_Width, Natural_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Widget        : not null access Gtk_Widget_Record;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Widget        : System.Address;
          Height        : Glib.Gint;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_realized");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Realized;

   --------------------------
   -- Get_Receives_Default --
   --------------------------

   function Get_Receives_Default
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_receives_default");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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

   ----------------------
   -- Get_Scale_Factor --
   ----------------------

   function Get_Scale_Factor
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_scale_factor");
   begin
      return Internal (Get_Object (Widget));
   end Get_Scale_Factor;

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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_sensitive");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Sensitive;

   ----------------------
   -- Get_Size_Request --
   ----------------------

   procedure Get_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Widget : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_support_multidevice");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Support_Multidevice;

   ------------------------
   -- Get_Template_Child --
   ------------------------

   function Get_Template_Child
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType;
       Name        : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Widget      : System.Address;
          Widget_Type : GType;
          Name        : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_template_child");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_GObject : Glib.Object.GObject_Record;
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Widget_Type, Tmp_Name);
      Free (Tmp_Name);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Get_Template_Child;

   ------------------------
   -- Get_Tooltip_Markup --
   ------------------------

   function Get_Tooltip_Markup
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
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
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
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

   ------------------------------
   -- Get_Valign_With_Baseline --
   ------------------------------

   function Get_Valign_With_Baseline
      (Widget : not null access Gtk_Widget_Record) return Gtk_Align
   is
      function Internal (Widget : System.Address) return Gtk_Align;
      pragma Import (C, Internal, "gtk_widget_get_valign_with_baseline");
   begin
      return Internal (Get_Object (Widget));
   end Get_Valign_With_Baseline;

   -----------------
   -- Get_Vexpand --
   -----------------

   function Get_Vexpand
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_vexpand");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Vexpand;

   ---------------------
   -- Get_Vexpand_Set --
   ---------------------

   function Get_Vexpand_Set
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_vexpand_set");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Vexpand_Set;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_visible");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_default");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Has_Default;

   ---------------
   -- Has_Focus --
   ---------------

   function Has_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_focus");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Has_Focus;

   --------------
   -- Has_Grab --
   --------------

   function Has_Grab
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_grab");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Has_Grab;

   ------------------
   -- Has_Rc_Style --
   ------------------

   function Has_Rc_Style
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_rc_style");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Has_Rc_Style;

   ----------------
   -- Has_Screen --
   ----------------

   function Has_Screen
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_screen");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Has_Screen;

   -----------------------
   -- Has_Visible_Focus --
   -----------------------

   function Has_Visible_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_visible_focus");
   begin
      return Internal (Get_Object (Widget)) /= 0;
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
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_hide_on_delete");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Hide_On_Delete;

   --------------------
   -- In_Destruction --
   --------------------

   function In_Destruction
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_in_destruction");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end In_Destruction;

   -------------------
   -- Init_Template --
   -------------------

   procedure Init_Template (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_init_template");
   begin
      Internal (Get_Object (Widget));
   end Init_Template;

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
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_intersect");
      Acc_Intersection : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Area, Acc_Intersection'Access);
      if Intersection /= null then
         Intersection.all := Acc_Intersection;
      end if;
      return Tmp_Return /= 0;
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
          Ancestor : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_ancestor");
   begin
      return Internal (Get_Object (Widget), Get_Object (Ancestor)) /= 0;
   end Is_Ancestor;

   -------------------
   -- Is_Composited --
   -------------------

   function Is_Composited
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_composited");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Composited;

   -----------------
   -- Is_Drawable --
   -----------------

   function Is_Drawable
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_drawable");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Drawable;

   --------------
   -- Is_Focus --
   --------------

   function Is_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_focus");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Focus;

   ------------------
   -- Is_Sensitive --
   ------------------

   function Is_Sensitive
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_sensitive");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Sensitive;

   -----------------
   -- Is_Toplevel --
   -----------------

   function Is_Toplevel
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_toplevel");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Toplevel;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_is_visible");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Is_Visible;

   -------------------
   -- Keynav_Failed --
   -------------------

   function Keynav_Failed
      (Widget    : not null access Gtk_Widget_Record;
       Direction : Gtk.Enums.Gtk_Direction_Type) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Direction : Gtk.Enums.Gtk_Direction_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_keynav_failed");
   begin
      return Internal (Get_Object (Widget), Direction) /= 0;
   end Keynav_Failed;

   --------------------------
   -- List_Action_Prefixes --
   --------------------------

   function List_Action_Prefixes
      (Widget : not null access Gtk_Widget_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Widget : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_widget_list_action_prefixes");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Widget)));
   end List_Action_Prefixes;

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
          Group_Cycling : Glib.Gboolean) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_mnemonic_activate");
   begin
      return Internal (Get_Object (Widget), Boolean'Pos (Group_Cycling)) /= 0;
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
          Primary   : System.Address;
          Secondary : System.Address);
      pragma Import (C, Internal, "gtk_widget_modify_cursor");
   begin
      Internal (Get_Object (Widget), Gdk.Color.Gdk_Color_Or_Null (Primary'Address), Gdk.Color.Gdk_Color_Or_Null (Secondary'Address));
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
          Name   : Gtkada.Types.Chars_Ptr;
          Color  : System.Address);
      pragma Import (C, Internal, "gtk_widget_override_symbolic_color");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name, Gdk.RGBA.Gdk_RGBA_Or_Null (Color'Address));
      Free (Tmp_Name);
   end Override_Symbolic_Color;

   --------------------
   -- Queue_Allocate --
   --------------------

   procedure Queue_Allocate (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_allocate");
   begin
      Internal (Get_Object (Widget));
   end Queue_Allocate;

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
       X      : Glib.Gint;
       Y      : Glib.Gint;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Widget : System.Address;
          X      : Glib.Gint;
          Y      : Glib.Gint;
          Width  : Glib.Gint;
          Height : Glib.Gint);
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

   ---------------------
   -- Register_Window --
   ---------------------

   procedure Register_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Widget : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_register_window");
   begin
      Internal (Get_Object (Widget), Window);
   end Register_Window;

   ------------------------
   -- Remove_Accelerator --
   ------------------------

   function Remove_Accelerator
      (Widget      : not null access Gtk_Widget_Record;
       Accel_Group : not null access Gtk.Accel_Group.Gtk_Accel_Group_Record'Class;
       Accel_Key   : Gdk.Types.Gdk_Key_Type;
       Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Widget      : System.Address;
          Accel_Group : System.Address;
          Accel_Key   : Gdk.Types.Gdk_Key_Type;
          Accel_Mods  : Gdk.Types.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_remove_accelerator");
   begin
      return Internal (Get_Object (Widget), Get_Object (Accel_Group), Accel_Key, Accel_Mods) /= 0;
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

   --------------------------
   -- Remove_Tick_Callback --
   --------------------------

   procedure Remove_Tick_Callback
      (Widget : not null access Gtk_Widget_Record;
       Id     : Guint)
   is
      procedure Internal (Widget : System.Address; Id : Guint);
      pragma Import (C, Internal, "gtk_widget_remove_tick_callback");
   begin
      Internal (Get_Object (Widget), Id);
   end Remove_Tick_Callback;

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
          Stock_Id : Gtkada.Types.Chars_Ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size;
          Detail   : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_widget_render_icon");
      Tmp_Stock_Id    : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Detail      : Gtkada.Types.Chars_Ptr;
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      if Detail = "" then
         Tmp_Detail := Gtkada.Types.Null_Ptr;
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
          Stock_Id : Gtkada.Types.Chars_Ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_widget_render_icon_pixbuf");
      Tmp_Stock_Id    : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
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
       Event  : Gdk.Event.Gdk_Event) return Glib.Gint
   is
      function Internal
         (Widget : System.Address;
          Event  : Gdk.Event.Gdk_Event) return Glib.Gint;
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
          Event  : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_send_focus_change");
   begin
      return Internal (Get_Object (Widget), Event) /= 0;
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
          Accel_Path  : Gtkada.Types.Chars_Ptr;
          Accel_Group : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_accel_path");
      Tmp_Accel_Path : Gtkada.Types.Chars_Ptr;
   begin
      if Accel_Path = "" then
         Tmp_Accel_Path := Gtkada.Types.Null_Ptr;
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
      procedure Internal
         (Widget        : System.Address;
          App_Paintable : Glib.Gboolean);
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
      procedure Internal
         (Widget      : System.Address;
          Can_Default : Glib.Gboolean);
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
      procedure Internal
         (Widget    : System.Address;
          Can_Focus : Glib.Gboolean);
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
      procedure Internal
         (Widget     : System.Address;
          Is_Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_child_visible");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Is_Visible));
   end Set_Child_Visible;

   --------------
   -- Set_Clip --
   --------------

   procedure Set_Clip
      (Widget : not null access Gtk_Widget_Record;
       Clip   : in out Gtk_Allocation)
   is
      procedure Internal
         (Widget : System.Address;
          Clip   : in out Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_set_clip");
   begin
      Internal (Get_Object (Widget), Clip);
   end Set_Clip;

   ------------------------
   -- Set_Composite_Name --
   ------------------------

   procedure Set_Composite_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String)
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_set_composite_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name);
      Free (Tmp_Name);
   end Set_Composite_Name;

   ----------------------
   -- Set_Connect_Func --
   ----------------------

   procedure Set_Connect_Func
      (Self                 : Glib.Object.GObject_Class;
       Connect_Func         : Gtk_Builder_Connect_Func;
       Connect_Data_Destroy : Glib.G_Destroy_Notify_Address)
   is
   begin
      if Connect_Func = null then
         C_Gtk_Widget_Class_Set_Connect_Func (Self, System.Null_Address, System.Null_Address, Connect_Data_Destroy);
      else
         C_Gtk_Widget_Class_Set_Connect_Func (Self, Internal_Gtk_Builder_Connect_Func'Address, To_Address (Connect_Func), Connect_Data_Destroy);
      end if;
   end Set_Connect_Func;

   package body Set_Connect_Func_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Builder_Connect_Func is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Builder_Connect_Func);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Builder_Connect_Func, System.Address);

      procedure Internal_Cb
         (Builder        : System.Address;
          Object         : System.Address;
          Signal_Name    : Gtkada.Types.Chars_Ptr;
          Handler_Name   : Gtkada.Types.Chars_Ptr;
          Connect_Object : System.Address;
          Flags          : Glib.G_Connect_Flags;
          User_Data      : System.Address);
      pragma Convention (C, Internal_Cb);
      --  This is the signature of a function used to connect signals. It is
      --  used by the Gtk.Builder.Connect_Signals and
      --  Gtk.Builder.Connect_Signals_Full methods. It is mainly intended for
      --  interpreted language bindings, but could be useful where the
      --  programmer wants more control over the signal connection process.
      --  Note that this function can only be called once, subsequent calls
      --  will do nothing.
      --  Since: gtk+ 2.12
      --  "builder": a Gtk.Builder.Gtk_Builder
      --  "object": object to connect a signal to
      --  "signal_name": name of the signal
      --  "handler_name": name of the handler
      --  "connect_object": a Glib.Object.GObject, if non-null, use
      --  g_signal_connect_object
      --  "flags": Glib.G_Connect_Flags to use
      --  "user_data": user data

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb
         (Builder        : System.Address;
          Object         : System.Address;
          Signal_Name    : Gtkada.Types.Chars_Ptr;
          Handler_Name   : Gtkada.Types.Chars_Ptr;
          Connect_Object : System.Address;
          Flags          : Glib.G_Connect_Flags;
          User_Data      : System.Address)
      is
         D                : constant Users.Internal_Data_Access := Users.Convert (User_Data);
         Stub_Gtk_Builder : Gtk.Builder.Gtk_Builder_Record;
         Stub_GObject     : Glib.Object.GObject_Record;
      begin
         To_Gtk_Builder_Connect_Func (D.Func) (Gtk.Builder.Gtk_Builder (Get_User_Data (Builder, Stub_Gtk_Builder)), Get_User_Data (Object, Stub_GObject), Gtkada.Bindings.Value_Allowing_Null (Signal_Name), Gtkada.Bindings.Value_Allowing_Null (Handler_Name), Get_User_Data (Connect_Object, Stub_GObject), Flags, D.Data.all);
      end Internal_Cb;

      ----------------------
      -- Set_Connect_Func --
      ----------------------

      procedure Set_Connect_Func
         (Self                 : Glib.Object.GObject_Class;
          Connect_Func         : Gtk_Builder_Connect_Func;
          Connect_Data         : User_Data_Type;
          Connect_Data_Destroy : Glib.G_Destroy_Notify_Address)
      is
         D : System.Address;
      begin
         if Connect_Func = null then
            C_Gtk_Widget_Class_Set_Connect_Func (Self, System.Null_Address, System.Null_Address, Connect_Data_Destroy);
         else
            D := Users.Build (To_Address (Connect_Func), Connect_Data);
            C_Gtk_Widget_Class_Set_Connect_Func (Self, Internal_Cb'Address, D, Connect_Data_Destroy);
         end if;
      end Set_Connect_Func;

   end Set_Connect_Func_User_Data;

   ------------------
   -- Set_Css_Name --
   ------------------

   procedure Set_Css_Name
      (Self : Glib.Object.GObject_Class;
       Name : UTF8_String)
   is
      procedure Internal
         (Self : Glib.Object.GObject_Class;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_class_set_css_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Self, Tmp_Name);
      Free (Tmp_Name);
   end Set_Css_Name;

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
          Enabled : Glib.Gboolean);
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
          Double_Buffered : Glib.Gboolean);
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

   ------------------------
   -- Set_Focus_On_Click --
   ------------------------

   procedure Set_Focus_On_Click
      (Widget         : not null access Gtk_Widget_Record;
       Focus_On_Click : Boolean)
   is
      procedure Internal
         (Widget         : System.Address;
          Focus_On_Click : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_focus_on_click");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Focus_On_Click));
   end Set_Focus_On_Click;

   ------------------
   -- Set_Font_Map --
   ------------------

   procedure Set_Font_Map
      (Widget   : not null access Gtk_Widget_Record;
       Font_Map : access Pango.Font_Map.Pango_Font_Map_Record'Class)
   is
      procedure Internal
         (Widget   : System.Address;
          Font_Map : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_font_map");
   begin
      Internal (Get_Object (Widget), Get_Object_Or_Null (GObject (Font_Map)));
   end Set_Font_Map;

   ----------------------
   -- Set_Font_Options --
   ----------------------

   procedure Set_Font_Options
      (Widget  : not null access Gtk_Widget_Record;
       Options : in out Cairo.Cairo_Font_Options)
   is
      procedure Internal
         (Widget  : System.Address;
          Options : in out Cairo.Cairo_Font_Options);
      pragma Import (C, Internal, "gtk_widget_set_font_options");
   begin
      Internal (Get_Object (Widget), Options);
   end Set_Font_Options;

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
      procedure Internal
         (Widget      : System.Address;
          Has_Tooltip : Glib.Gboolean);
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
      procedure Internal
         (Widget     : System.Address;
          Has_Window : Glib.Gboolean);
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
      procedure Internal (Widget : System.Address; Expand : Glib.Gboolean);
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
      procedure Internal (Widget : System.Address; Set : Glib.Gboolean);
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
      procedure Internal (Widget : System.Address; Mapped : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_mapped");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Mapped));
   end Set_Mapped;

   -----------------------
   -- Set_Margin_Bottom --
   -----------------------

   procedure Set_Margin_Bottom
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_bottom");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Bottom;

   --------------------
   -- Set_Margin_End --
   --------------------

   procedure Set_Margin_End
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_end");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_End;

   ---------------------
   -- Set_Margin_Left --
   ---------------------

   procedure Set_Margin_Left
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_left");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Left;

   ----------------------
   -- Set_Margin_Right --
   ----------------------

   procedure Set_Margin_Right
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_right");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Right;

   ----------------------
   -- Set_Margin_Start --
   ----------------------

   procedure Set_Margin_Start
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_set_margin_start");
   begin
      Internal (Get_Object (Widget), Margin);
   end Set_Margin_Start;

   --------------------
   -- Set_Margin_Top --
   --------------------

   procedure Set_Margin_Top
      (Widget : not null access Gtk_Widget_Record;
       Margin : Glib.Gint)
   is
      procedure Internal (Widget : System.Address; Margin : Glib.Gint);
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
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
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
      procedure Internal
         (Widget      : System.Address;
          No_Show_All : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_no_show_all");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (No_Show_All));
   end Set_No_Show_All;

   -----------------
   -- Set_Opacity --
   -----------------

   procedure Set_Opacity
      (Widget  : not null access Gtk_Widget_Record;
       Opacity : Gdouble)
   is
      procedure Internal (Widget : System.Address; Opacity : Gdouble);
      pragma Import (C, Internal, "gtk_widget_set_opacity");
   begin
      Internal (Get_Object (Widget), Opacity);
   end Set_Opacity;

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
      procedure Internal (Widget : System.Address; Realized : Glib.Gboolean);
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
          Receives_Default : Glib.Gboolean);
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
          Redraw_On_Allocate : Glib.Gboolean);
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
      procedure Internal
         (Widget    : System.Address;
          Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Sensitive));
   end Set_Sensitive;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
      (Widget : not null access Gtk_Widget_Record;
       Width  : Glib.Gint := -1;
       Height : Glib.Gint := -1)
   is
      procedure Internal
         (Widget : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
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
          Clear  : Glib.Gboolean);
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
          Support_Multidevice : Glib.Gboolean);
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
          Markup : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Gtkada.Types.Null_Ptr;
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
       Text   : UTF8_String := "")
   is
      procedure Internal
         (Widget : System.Address;
          Text   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
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
      procedure Internal (Widget : System.Address; Expand : Glib.Gboolean);
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
      procedure Internal (Widget : System.Address; Set : Glib.Gboolean);
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
      procedure Internal (Widget : System.Address; Visible : Glib.Gboolean);
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
       Allocation : Gtk_Allocation)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : Gtk_Allocation);
      pragma Import (C, Internal, "gtk_widget_size_allocate");
   begin
      Internal (Get_Object (Widget), Allocation);
   end Size_Allocate;

   ---------------------------------
   -- Size_Allocate_With_Baseline --
   ---------------------------------

   procedure Size_Allocate_With_Baseline
      (Widget     : not null access Gtk_Widget_Record;
       Allocation : in out Gtk_Allocation;
       Baseline   : Glib.Gint)
   is
      procedure Internal
         (Widget     : System.Address;
          Allocation : in out Gtk_Allocation;
          Baseline   : Glib.Gint);
      pragma Import (C, Internal, "gtk_widget_size_allocate_with_baseline");
   begin
      Internal (Get_Object (Widget), Allocation, Baseline);
   end Size_Allocate_With_Baseline;

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
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_widget_style_get_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
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
       Src_X       : Glib.Gint;
       Src_Y       : Glib.Gint;
       Dest_X      : out Glib.Gint;
       Dest_Y      : out Glib.Gint;
       Result      : out Boolean)
   is
      function Internal
         (Widget      : System.Address;
          Dest_Widget : System.Address;
          Src_X       : Glib.Gint;
          Src_Y       : Glib.Gint;
          Acc_Dest_X  : access Glib.Gint;
          Acc_Dest_Y  : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_translate_coordinates");
      Acc_Dest_X : aliased Glib.Gint;
      Acc_Dest_Y : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Get_Object (Dest_Widget), Src_X, Src_Y, Acc_Dest_X'Access, Acc_Dest_Y'Access);
      Dest_X := Acc_Dest_X;
      Dest_Y := Acc_Dest_Y;
      Result := Tmp_Return /= 0;
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
   -- Unregister_Window --
   -----------------------

   procedure Unregister_Window
      (Widget : not null access Gtk_Widget_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Widget : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_widget_unregister_window");
   begin
      Internal (Get_Object (Widget), Window);
   end Unregister_Window;

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
          Window : Gdk.Gdk_Window) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_cairo_should_draw_window");
   begin
      return Internal (Cr, Window) /= 0;
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

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Guint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Guint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Param_Spec_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Param_Spec_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Param_Spec_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Param_Spec_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Configure_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Configure_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Configure_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Configure_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Expose_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Expose_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Expose_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Expose_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Text_Direction_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Text_Direction_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Direction_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Direction_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Drag_Context_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Drag_Context_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Drag_Context_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Drag_Context_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Cairo_Context_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Cairo_Context_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Cairo_Context_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Cairo_Context_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Crossing_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Crossing_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Direction_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Direction_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Focus_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Focus_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Focus_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Focus_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Grab_Broken_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Grab_Broken_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Key_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Key_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Key_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Key_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Any_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Any_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Any_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Any_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Motion_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Motion_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Motion_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Motion_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Property_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Property_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Property_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Property_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Proximity_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Proximity_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Boolean_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Boolean_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Screen_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Screen_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Screen_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Screen_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Scroll_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Scroll_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Selection_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Selection_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Selection_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Selection_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Selection_Data_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Selection_Data_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Help_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Help_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Allocation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Allocation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Allocation_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Allocation_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_State_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_State_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_State_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_State_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_State_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_State_Flags_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_State_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_State_Flags_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Style_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Style_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Style_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Style_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Visibility_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Visibility_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Window_State_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Window_State_Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Guint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Param_Spec_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Configure_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Expose_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Text_Direction_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Cairo_Context_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Motion_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Property_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Screen_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Allocation_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_State_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_State_Flags_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Style_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Button_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Param_Spec_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Configure_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Expose_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Direction_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Cairo_Context_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Crossing_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Focus_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Grab_Broken_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Key_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Any_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Motion_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Property_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Proximity_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Screen_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Scroll_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Selection_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Selection_Data_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Help_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Allocation_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_State_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_State_Flags_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Style_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Visibility_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Window_State_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Void);

   procedure Marsh_GObject_Cairo_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Cairo_Context_Boolean);

   procedure Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean);

   procedure Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean);

   procedure Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_GObject_Drag_Context_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Guint_Void);

   procedure Marsh_GObject_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Drag_Context_Void);

   procedure Marsh_GObject_Gdk_Event_Any_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Any_Boolean);

   procedure Marsh_GObject_Gdk_Event_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Boolean);

   procedure Marsh_GObject_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Button_Boolean);

   procedure Marsh_GObject_Gdk_Event_Configure_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Configure_Boolean);

   procedure Marsh_GObject_Gdk_Event_Crossing_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Crossing_Boolean);

   procedure Marsh_GObject_Gdk_Event_Expose_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Expose_Boolean);

   procedure Marsh_GObject_Gdk_Event_Focus_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Focus_Boolean);

   procedure Marsh_GObject_Gdk_Event_Grab_Broken_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Grab_Broken_Boolean);

   procedure Marsh_GObject_Gdk_Event_Key_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Key_Boolean);

   procedure Marsh_GObject_Gdk_Event_Motion_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Motion_Boolean);

   procedure Marsh_GObject_Gdk_Event_Property_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Property_Boolean);

   procedure Marsh_GObject_Gdk_Event_Proximity_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Proximity_Boolean);

   procedure Marsh_GObject_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Scroll_Boolean);

   procedure Marsh_GObject_Gdk_Event_Selection_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Selection_Boolean);

   procedure Marsh_GObject_Gdk_Event_Visibility_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Visibility_Boolean);

   procedure Marsh_GObject_Gdk_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Void);

   procedure Marsh_GObject_Gdk_Event_Window_State_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Window_State_Boolean);

   procedure Marsh_GObject_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Screen_Void);

   procedure Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean);

   procedure Marsh_GObject_Gtk_Allocation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Allocation_Void);

   procedure Marsh_GObject_Gtk_Direction_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Boolean);

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Void);

   procedure Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_GObject_Gtk_Selection_Data_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Selection_Data_Guint_Void);

   procedure Marsh_GObject_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_State_Flags_Void);

   procedure Marsh_GObject_Gtk_State_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_State_Type_Void);

   procedure Marsh_GObject_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Style_Void);

   procedure Marsh_GObject_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Direction_Void);

   procedure Marsh_GObject_Gtk_Widget_Help_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Help_Type_Boolean);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Boolean);

   procedure Marsh_GObject_Param_Spec_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Param_Spec_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Widget_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Boolean);

   procedure Marsh_Gtk_Widget_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Boolean_Boolean);

   procedure Marsh_Gtk_Widget_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Boolean_Void);

   procedure Marsh_Gtk_Widget_Cairo_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Cairo_Context_Boolean);

   procedure Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean);

   procedure Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean);

   procedure Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_Gtk_Widget_Drag_Context_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Guint_Void);

   procedure Marsh_Gtk_Widget_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Drag_Context_Void);

   procedure Marsh_Gtk_Widget_Gdk_Event_Any_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Any_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Button_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Key_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Key_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Property_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Property_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Void);

   procedure Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean);

   procedure Marsh_Gtk_Widget_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gdk_Screen_Void);

   procedure Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean);

   procedure Marsh_Gtk_Widget_Gtk_Allocation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Allocation_Void);

   procedure Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean);

   procedure Marsh_Gtk_Widget_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Direction_Type_Void);

   procedure Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void);

   procedure Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void);

   procedure Marsh_Gtk_Widget_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_State_Flags_Void);

   procedure Marsh_Gtk_Widget_Gtk_State_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_State_Type_Void);

   procedure Marsh_Gtk_Widget_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Style_Void);

   procedure Marsh_Gtk_Widget_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Text_Direction_Void);

   procedure Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean);

   procedure Marsh_Gtk_Widget_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Widget_Void);

   procedure Marsh_Gtk_Widget_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Guint_Boolean);

   procedure Marsh_Gtk_Widget_Param_Spec_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Param_Spec_Void);

   procedure Marsh_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Guint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Guint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Param_Spec_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Param_Spec_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Configure_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Expose_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Text_Direction_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Text_Direction_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Drag_Context_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Drag_Context_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Cairo_Context_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Cairo_Context_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Key_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Any_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Motion_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Property_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Property_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Screen_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Screen_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Allocation_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Allocation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_State_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_State_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_State_Flags_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_State_Flags_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Style_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gtk_Style_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
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
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Button_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Param_Spec_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Param_Spec_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Configure_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Configure_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Expose_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Expose_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Direction_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Direction_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Drag_Context_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Drag_Context_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Cairo_Context_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Cairo_Context_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Crossing_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Crossing_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Focus_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Focus_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Grab_Broken_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Grab_Broken_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Key_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Key_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Any_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Any_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Motion_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Motion_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
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
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Property_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Property_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Proximity_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Proximity_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Screen_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Screen_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Scroll_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Scroll_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Selection_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Selection_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Selection_Data_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Selection_Data_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Help_Type_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Help_Type_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Allocation_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Allocation_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_State_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_State_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_State_Flags_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_State_Flags_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Style_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Style_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Visibility_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Visibility_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Window_State_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Window_State_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------
   -- Marsh_GObject_Boolean --
   ---------------------------

   procedure Marsh_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   --------------------------------
   -- Marsh_GObject_Boolean_Void --
   --------------------------------

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Void;

   -----------------------------------------
   -- Marsh_GObject_Cairo_Context_Boolean --
   -----------------------------------------

   procedure Marsh_GObject_Cairo_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Cairo_Context_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Cairo_Context (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Cairo_Context_Boolean;

   ------------------------------------------------------------------------------
   -- Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void --
   ------------------------------------------------------------------------------

   procedure Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3), Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 4)), Unchecked_To_Guint (Params, 5), Unchecked_To_Guint (Params, 6));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;

   --------------------------------------------------------
   -- Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean --
   --------------------------------------------------------

   procedure Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3), Unchecked_To_Guint (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Gint_Gint_Guint_Boolean;

   --------------------------------------------------------
   -- Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean --
   --------------------------------------------------------

   procedure Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gtk_Drag_Result (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Gtk_Drag_Result_Boolean;

   --------------------------------------------------------------------
   -- Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void --
   --------------------------------------------------------------------

   procedure Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 2)), Unchecked_To_Guint (Params, 3), Unchecked_To_Guint (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;

   -------------------------------------------
   -- Marsh_GObject_Drag_Context_Guint_Void --
   -------------------------------------------

   procedure Marsh_GObject_Drag_Context_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Guint_Void;

   -------------------------------------
   -- Marsh_GObject_Drag_Context_Void --
   -------------------------------------

   procedure Marsh_GObject_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Drag_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Drag_Context_Void;

   -----------------------------------------
   -- Marsh_GObject_Gdk_Event_Any_Boolean --
   -----------------------------------------

   procedure Marsh_GObject_Gdk_Event_Any_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Any_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Any (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Any_Boolean;

   -------------------------------------
   -- Marsh_GObject_Gdk_Event_Boolean --
   -------------------------------------

   procedure Marsh_GObject_Gdk_Event_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Boolean;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Button_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Button (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Button_Boolean;

   -----------------------------------------------
   -- Marsh_GObject_Gdk_Event_Configure_Boolean --
   -----------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Configure_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Configure_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Configure (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Configure_Boolean;

   ----------------------------------------------
   -- Marsh_GObject_Gdk_Event_Crossing_Boolean --
   ----------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Crossing_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Crossing_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Crossing (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Crossing_Boolean;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Expose_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Expose_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Expose_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Expose (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Expose_Boolean;

   -------------------------------------------
   -- Marsh_GObject_Gdk_Event_Focus_Boolean --
   -------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Focus_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Focus_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Focus (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Focus_Boolean;

   -------------------------------------------------
   -- Marsh_GObject_Gdk_Event_Grab_Broken_Boolean --
   -------------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Grab_Broken_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Grab_Broken_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Grab_Broken (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Grab_Broken_Boolean;

   -----------------------------------------
   -- Marsh_GObject_Gdk_Event_Key_Boolean --
   -----------------------------------------

   procedure Marsh_GObject_Gdk_Event_Key_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Key_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Key (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Key_Boolean;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Motion_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Motion_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Motion_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Motion (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Motion_Boolean;

   ----------------------------------------------
   -- Marsh_GObject_Gdk_Event_Property_Boolean --
   ----------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Property_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Property_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Property (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Property_Boolean;

   -----------------------------------------------
   -- Marsh_GObject_Gdk_Event_Proximity_Boolean --
   -----------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Proximity_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Proximity_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Proximity (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Proximity_Boolean;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Scroll_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Scroll_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Scroll (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Scroll_Boolean;

   -----------------------------------------------
   -- Marsh_GObject_Gdk_Event_Selection_Boolean --
   -----------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Selection_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Selection_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Selection (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Selection_Boolean;

   ------------------------------------------------
   -- Marsh_GObject_Gdk_Event_Visibility_Boolean --
   ------------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Visibility_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Visibility_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Visibility (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Visibility_Boolean;

   ----------------------------------
   -- Marsh_GObject_Gdk_Event_Void --
   ----------------------------------

   procedure Marsh_GObject_Gdk_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gdk_Event (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Void;

   --------------------------------------------------
   -- Marsh_GObject_Gdk_Event_Window_State_Boolean --
   --------------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Window_State_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Window_State_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Window_State (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Window_State_Boolean;

   -----------------------------------
   -- Marsh_GObject_Gdk_Screen_Void --
   -----------------------------------

   procedure Marsh_GObject_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Screen_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gdk.Screen.Gdk_Screen (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Screen_Void;

   -----------------------------------------------------
   -- Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean --
   -----------------------------------------------------

   procedure Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Boolean_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Object (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean;

   ---------------------------------------
   -- Marsh_GObject_Gtk_Allocation_Void --
   ---------------------------------------

   procedure Marsh_GObject_Gtk_Allocation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Allocation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Allocation (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Allocation_Void;

   ----------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Boolean --
   ----------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Boolean;

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

   -------------------------------------------------------
   -- Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void --
   -------------------------------------------------------

   procedure Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Guint (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Selection_Data_Guint_Guint_Void;

   -------------------------------------------------
   -- Marsh_GObject_Gtk_Selection_Data_Guint_Void --
   -------------------------------------------------

   procedure Marsh_GObject_Gtk_Selection_Data_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Selection_Data_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Selection_Data_Guint_Void;

   ----------------------------------------
   -- Marsh_GObject_Gtk_State_Flags_Void --
   ----------------------------------------

   procedure Marsh_GObject_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_State_Flags_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_State_Flags (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_State_Flags_Void;

   ---------------------------------------
   -- Marsh_GObject_Gtk_State_Type_Void --
   ---------------------------------------

   procedure Marsh_GObject_Gtk_State_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_State_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_State_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_State_Type_Void;

   ----------------------------------
   -- Marsh_GObject_Gtk_Style_Void --
   ----------------------------------

   procedure Marsh_GObject_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Style_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Style.Gtk_Style (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Style_Void;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Text_Direction_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Direction_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Direction (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Direction_Void;

   ------------------------------------------------
   -- Marsh_GObject_Gtk_Widget_Help_Type_Boolean --
   ------------------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Help_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Help_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Widget_Help_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Help_Type_Boolean;

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

   ---------------------------------
   -- Marsh_GObject_Guint_Boolean --
   ---------------------------------

   procedure Marsh_GObject_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Guint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Boolean;

   -----------------------------------
   -- Marsh_GObject_Param_Spec_Void --
   -----------------------------------

   procedure Marsh_GObject_Param_Spec_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Param_Spec_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Param_Spec (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Param_Spec_Void;

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

   ------------------------------
   -- Marsh_Gtk_Widget_Boolean --
   ------------------------------

   procedure Marsh_Gtk_Widget_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj);
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Boolean;

   --------------------------------------
   -- Marsh_Gtk_Widget_Boolean_Boolean --
   --------------------------------------

   procedure Marsh_Gtk_Widget_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Boolean_Boolean;

   -----------------------------------
   -- Marsh_Gtk_Widget_Boolean_Void --
   -----------------------------------

   procedure Marsh_Gtk_Widget_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Boolean_Void;

   --------------------------------------------
   -- Marsh_Gtk_Widget_Cairo_Context_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_Widget_Cairo_Context_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Cairo_Context_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Cairo_Context (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Cairo_Context_Boolean;

   ---------------------------------------------------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void --
   ---------------------------------------------------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3), Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 4)), Unchecked_To_Guint (Params, 5), Unchecked_To_Guint (Params, 6));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;

   -----------------------------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3), Unchecked_To_Guint (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;

   -----------------------------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Gtk_Drag_Result (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean;

   -----------------------------------------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void --
   -----------------------------------------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 2)), Unchecked_To_Guint (Params, 3), Unchecked_To_Guint (Params, 4));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;

   ----------------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Guint_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Guint_Void;

   ----------------------------------------
   -- Marsh_Gtk_Widget_Drag_Context_Void --
   ----------------------------------------

   procedure Marsh_Gtk_Widget_Drag_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Drag_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Drag_Contexts.Drag_Context (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Drag_Context_Void;

   --------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Any_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Any_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Any_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Any (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Any_Boolean;

   ----------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Boolean --
   ----------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Boolean;

   -----------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Button_Boolean --
   -----------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Button (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Button_Boolean;

   --------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean --
   --------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Configure_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Configure (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Configure_Boolean;

   -------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean --
   -------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Crossing (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Crossing_Boolean;

   -----------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean --
   -----------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Expose_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Expose (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Expose_Boolean;

   ----------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean --
   ----------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Focus_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Focus (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Focus_Boolean;

   ----------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean --
   ----------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Grab_Broken (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Key_Boolean --
   --------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Key_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Key_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Key (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Key_Boolean;

   -----------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean --
   -----------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Motion_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Motion (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Motion_Boolean;

   -------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Property_Boolean --
   -------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Property_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Property_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Property (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Property_Boolean;

   --------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean --
   --------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Proximity (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Proximity_Boolean;

   -----------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean --
   -----------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Scroll (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Scroll_Boolean;

   --------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean --
   --------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Selection_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Selection (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Selection_Boolean;

   ---------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean --
   ---------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Visibility (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Visibility_Boolean;

   -------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Void --
   -------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gdk_Event (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Void;

   -----------------------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean --
   -----------------------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Window_State (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Event_Window_State_Boolean;

   --------------------------------------
   -- Marsh_Gtk_Widget_Gdk_Screen_Void --
   --------------------------------------

   procedure Marsh_Gtk_Widget_Gdk_Screen_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gdk_Screen_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gdk.Screen.Gdk_Screen (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gdk_Screen_Void;

   --------------------------------------------------------
   -- Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean --
   --------------------------------------------------------

   procedure Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3), Unchecked_To_Object (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;

   ------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Allocation_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Allocation_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Allocation_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Allocation (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Allocation_Void;

   -------------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean --
   -------------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Direction_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Direction_Type_Boolean;

   ----------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Direction_Type_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Direction_Type_Void;

   ----------------------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void --
   ----------------------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Guint (Params, 2), Unchecked_To_Guint (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void;

   ----------------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void --
   ----------------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Selection_Data.From_Object (Unchecked_To_Address (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Selection_Data_Guint_Void;

   -------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_State_Flags_Void --
   -------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_State_Flags_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_State_Flags (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_State_Flags_Void;

   ------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_State_Type_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_State_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_State_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_State_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_State_Type_Void;

   -------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Style_Void --
   -------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Style_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Style_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Style.Gtk_Style (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Style_Void;

   ----------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Text_Direction_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Text_Direction_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Text_Direction (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Text_Direction_Void;

   ---------------------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean --
   ---------------------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Widget_Help_Type (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Widget_Help_Type_Boolean;

   --------------------------------------
   -- Marsh_Gtk_Widget_Gtk_Widget_Void --
   --------------------------------------

   procedure Marsh_Gtk_Widget_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Gtk_Widget_Void;

   ------------------------------------
   -- Marsh_Gtk_Widget_Guint_Boolean --
   ------------------------------------

   procedure Marsh_Gtk_Widget_Guint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Guint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Guint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Guint_Boolean;

   --------------------------------------
   -- Marsh_Gtk_Widget_Param_Spec_Void --
   --------------------------------------

   procedure Marsh_Gtk_Widget_Param_Spec_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Param_Spec_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Param_Spec (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Param_Spec_Void;

   ---------------------------
   -- Marsh_Gtk_Widget_Void --
   ---------------------------

   procedure Marsh_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Widget := Gtk_Widget (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Widget_Void;

   -------------------------------
   -- On_Accel_Closures_Changed --
   -------------------------------

   procedure On_Accel_Closures_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "accel-closures-changed" & ASCII.NUL, Call, After);
   end On_Accel_Closures_Changed;

   -------------------------------
   -- On_Accel_Closures_Changed --
   -------------------------------

   procedure On_Accel_Closures_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "accel-closures-changed" & ASCII.NUL, Call, After, Slot);
   end On_Accel_Closures_Changed;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "button-press-event" & ASCII.NUL, Call, After);
   end On_Button_Press_Event;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "button-press-event" & ASCII.NUL, Call, After, Slot);
   end On_Button_Press_Event;

   -----------------------------
   -- On_Button_Release_Event --
   -----------------------------

   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "button-release-event" & ASCII.NUL, Call, After);
   end On_Button_Release_Event;

   -----------------------------
   -- On_Button_Release_Event --
   -----------------------------

   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "button-release-event" & ASCII.NUL, Call, After, Slot);
   end On_Button_Release_Event;

   ---------------------------
   -- On_Can_Activate_Accel --
   ---------------------------

   procedure On_Can_Activate_Accel
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Guint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "can-activate-accel" & ASCII.NUL, Call, After);
   end On_Can_Activate_Accel;

   ---------------------------
   -- On_Can_Activate_Accel --
   ---------------------------

   procedure On_Can_Activate_Accel
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "can-activate-accel" & ASCII.NUL, Call, After, Slot);
   end On_Can_Activate_Accel;

   ---------------------
   -- On_Child_Notify --
   ---------------------

   procedure On_Child_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Param_Spec_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "child-notify" & ASCII.NUL, Call, After);
   end On_Child_Notify;

   ---------------------
   -- On_Child_Notify --
   ---------------------

   procedure On_Child_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Param_Spec_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "child-notify" & ASCII.NUL, Call, After, Slot);
   end On_Child_Notify;

   ---------------------------
   -- On_Composited_Changed --
   ---------------------------

   procedure On_Composited_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "composited-changed" & ASCII.NUL, Call, After);
   end On_Composited_Changed;

   ---------------------------
   -- On_Composited_Changed --
   ---------------------------

   procedure On_Composited_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "composited-changed" & ASCII.NUL, Call, After, Slot);
   end On_Composited_Changed;

   ------------------------
   -- On_Configure_Event --
   ------------------------

   procedure On_Configure_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Configure_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "configure-event" & ASCII.NUL, Call, After);
   end On_Configure_Event;

   ------------------------
   -- On_Configure_Event --
   ------------------------

   procedure On_Configure_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Configure_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "configure-event" & ASCII.NUL, Call, After, Slot);
   end On_Configure_Event;

   ---------------------
   -- On_Damage_Event --
   ---------------------

   procedure On_Damage_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Expose_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "damage-event" & ASCII.NUL, Call, After);
   end On_Damage_Event;

   ---------------------
   -- On_Damage_Event --
   ---------------------

   procedure On_Damage_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Expose_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "damage-event" & ASCII.NUL, Call, After, Slot);
   end On_Damage_Event;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   procedure On_Delete_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-event" & ASCII.NUL, Call, After);
   end On_Delete_Event;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   procedure On_Delete_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-event" & ASCII.NUL, Call, After, Slot);
   end On_Delete_Event;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "destroy" & ASCII.NUL, Call, After);
   end On_Destroy;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "destroy" & ASCII.NUL, Call, After, Slot);
   end On_Destroy;

   ----------------------
   -- On_Destroy_Event --
   ----------------------

   procedure On_Destroy_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "destroy-event" & ASCII.NUL, Call, After);
   end On_Destroy_Event;

   ----------------------
   -- On_Destroy_Event --
   ----------------------

   procedure On_Destroy_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "destroy-event" & ASCII.NUL, Call, After, Slot);
   end On_Destroy_Event;

   --------------------------
   -- On_Direction_Changed --
   --------------------------

   procedure On_Direction_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Text_Direction_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "direction-changed" & ASCII.NUL, Call, After);
   end On_Direction_Changed;

   --------------------------
   -- On_Direction_Changed --
   --------------------------

   procedure On_Direction_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Text_Direction_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "direction-changed" & ASCII.NUL, Call, After, Slot);
   end On_Direction_Changed;

   -------------------
   -- On_Drag_Begin --
   -------------------

   procedure On_Drag_Begin
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-begin" & ASCII.NUL, Call, After);
   end On_Drag_Begin;

   -------------------
   -- On_Drag_Begin --
   -------------------

   procedure On_Drag_Begin
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-begin" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Begin;

   -------------------------
   -- On_Drag_Data_Delete --
   -------------------------

   procedure On_Drag_Data_Delete
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-data-delete" & ASCII.NUL, Call, After);
   end On_Drag_Data_Delete;

   -------------------------
   -- On_Drag_Data_Delete --
   -------------------------

   procedure On_Drag_Data_Delete
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-data-delete" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Data_Delete;

   ----------------------
   -- On_Drag_Data_Get --
   ----------------------

   procedure On_Drag_Data_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-data-get" & ASCII.NUL, Call, After);
   end On_Drag_Data_Get;

   ----------------------
   -- On_Drag_Data_Get --
   ----------------------

   procedure On_Drag_Data_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-data-get" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Data_Get;

   ---------------------------
   -- On_Drag_Data_Received --
   ---------------------------

   procedure On_Drag_Data_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-data-received" & ASCII.NUL, Call, After);
   end On_Drag_Data_Received;

   ---------------------------
   -- On_Drag_Data_Received --
   ---------------------------

   procedure On_Drag_Data_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-data-received" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Data_Received;

   ------------------
   -- On_Drag_Drop --
   ------------------

   procedure On_Drag_Drop
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-drop" & ASCII.NUL, Call, After);
   end On_Drag_Drop;

   ------------------
   -- On_Drag_Drop --
   ------------------

   procedure On_Drag_Drop
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-drop" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Drop;

   -----------------
   -- On_Drag_End --
   -----------------

   procedure On_Drag_End
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-end" & ASCII.NUL, Call, After);
   end On_Drag_End;

   -----------------
   -- On_Drag_End --
   -----------------

   procedure On_Drag_End
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-end" & ASCII.NUL, Call, After, Slot);
   end On_Drag_End;

   --------------------
   -- On_Drag_Failed --
   --------------------

   procedure On_Drag_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gtk_Drag_Result_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-failed" & ASCII.NUL, Call, After);
   end On_Drag_Failed;

   --------------------
   -- On_Drag_Failed --
   --------------------

   procedure On_Drag_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gtk_Drag_Result_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-failed" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Failed;

   -------------------
   -- On_Drag_Leave --
   -------------------

   procedure On_Drag_Leave
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-leave" & ASCII.NUL, Call, After);
   end On_Drag_Leave;

   -------------------
   -- On_Drag_Leave --
   -------------------

   procedure On_Drag_Leave
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-leave" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Leave;

   --------------------
   -- On_Drag_Motion --
   --------------------

   procedure On_Drag_Motion
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Drag_Context_Gint_Gint_Guint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "drag-motion" & ASCII.NUL, Call, After);
   end On_Drag_Motion;

   --------------------
   -- On_Drag_Motion --
   --------------------

   procedure On_Drag_Motion
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Drag_Context_Gint_Gint_Guint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "drag-motion" & ASCII.NUL, Call, After, Slot);
   end On_Drag_Motion;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Cairo_Context_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "draw" & ASCII.NUL, Call, After);
   end On_Draw;

   -------------
   -- On_Draw --
   -------------

   procedure On_Draw
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Cairo_Context_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "draw" & ASCII.NUL, Call, After, Slot);
   end On_Draw;

   ---------------------------
   -- On_Enter_Notify_Event --
   ---------------------------

   procedure On_Enter_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "enter-notify-event" & ASCII.NUL, Call, After);
   end On_Enter_Notify_Event;

   ---------------------------
   -- On_Enter_Notify_Event --
   ---------------------------

   procedure On_Enter_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Crossing_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "enter-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Enter_Notify_Event;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "event" & ASCII.NUL, Call, After);
   end On_Event;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "event" & ASCII.NUL, Call, After, Slot);
   end On_Event;

   --------------------
   -- On_Event_After --
   --------------------

   procedure On_Event_After
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "event-after" & ASCII.NUL, Call, After);
   end On_Event_After;

   --------------------
   -- On_Event_After --
   --------------------

   procedure On_Event_After
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "event-after" & ASCII.NUL, Call, After, Slot);
   end On_Event_After;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus" & ASCII.NUL, Call, After);
   end On_Focus;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus" & ASCII.NUL, Call, After, Slot);
   end On_Focus;

   -----------------------
   -- On_Focus_In_Event --
   -----------------------

   procedure On_Focus_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus-in-event" & ASCII.NUL, Call, After);
   end On_Focus_In_Event;

   -----------------------
   -- On_Focus_In_Event --
   -----------------------

   procedure On_Focus_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Focus_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus-in-event" & ASCII.NUL, Call, After, Slot);
   end On_Focus_In_Event;

   ------------------------
   -- On_Focus_Out_Event --
   ------------------------

   procedure On_Focus_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Focus_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus-out-event" & ASCII.NUL, Call, After);
   end On_Focus_Out_Event;

   ------------------------
   -- On_Focus_Out_Event --
   ------------------------

   procedure On_Focus_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Focus_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus-out-event" & ASCII.NUL, Call, After, Slot);
   end On_Focus_Out_Event;

   --------------------------
   -- On_Grab_Broken_Event --
   --------------------------

   procedure On_Grab_Broken_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Grab_Broken_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "grab-broken-event" & ASCII.NUL, Call, After);
   end On_Grab_Broken_Event;

   --------------------------
   -- On_Grab_Broken_Event --
   --------------------------

   procedure On_Grab_Broken_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Grab_Broken_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "grab-broken-event" & ASCII.NUL, Call, After, Slot);
   end On_Grab_Broken_Event;

   -------------------
   -- On_Grab_Focus --
   -------------------

   procedure On_Grab_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "grab-focus" & ASCII.NUL, Call, After);
   end On_Grab_Focus;

   -------------------
   -- On_Grab_Focus --
   -------------------

   procedure On_Grab_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "grab-focus" & ASCII.NUL, Call, After, Slot);
   end On_Grab_Focus;

   --------------------
   -- On_Grab_Notify --
   --------------------

   procedure On_Grab_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "grab-notify" & ASCII.NUL, Call, After);
   end On_Grab_Notify;

   --------------------
   -- On_Grab_Notify --
   --------------------

   procedure On_Grab_Notify
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "grab-notify" & ASCII.NUL, Call, After, Slot);
   end On_Grab_Notify;

   -------------
   -- On_Hide --
   -------------

   procedure On_Hide
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "hide" & ASCII.NUL, Call, After);
   end On_Hide;

   -------------
   -- On_Hide --
   -------------

   procedure On_Hide
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "hide" & ASCII.NUL, Call, After, Slot);
   end On_Hide;

   --------------------------
   -- On_Hierarchy_Changed --
   --------------------------

   procedure On_Hierarchy_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "hierarchy-changed" & ASCII.NUL, Call, After);
   end On_Hierarchy_Changed;

   --------------------------
   -- On_Hierarchy_Changed --
   --------------------------

   procedure On_Hierarchy_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "hierarchy-changed" & ASCII.NUL, Call, After, Slot);
   end On_Hierarchy_Changed;

   ------------------------
   -- On_Key_Press_Event --
   ------------------------

   procedure On_Key_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "key-press-event" & ASCII.NUL, Call, After);
   end On_Key_Press_Event;

   ------------------------
   -- On_Key_Press_Event --
   ------------------------

   procedure On_Key_Press_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Key_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "key-press-event" & ASCII.NUL, Call, After, Slot);
   end On_Key_Press_Event;

   --------------------------
   -- On_Key_Release_Event --
   --------------------------

   procedure On_Key_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Key_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "key-release-event" & ASCII.NUL, Call, After);
   end On_Key_Release_Event;

   --------------------------
   -- On_Key_Release_Event --
   --------------------------

   procedure On_Key_Release_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Key_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "key-release-event" & ASCII.NUL, Call, After, Slot);
   end On_Key_Release_Event;

   ----------------------
   -- On_Keynav_Failed --
   ----------------------

   procedure On_Keynav_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "keynav-failed" & ASCII.NUL, Call, After);
   end On_Keynav_Failed;

   ----------------------
   -- On_Keynav_Failed --
   ----------------------

   procedure On_Keynav_Failed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "keynav-failed" & ASCII.NUL, Call, After, Slot);
   end On_Keynav_Failed;

   ---------------------------
   -- On_Leave_Notify_Event --
   ---------------------------

   procedure On_Leave_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Crossing_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "leave-notify-event" & ASCII.NUL, Call, After);
   end On_Leave_Notify_Event;

   ---------------------------
   -- On_Leave_Notify_Event --
   ---------------------------

   procedure On_Leave_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Crossing_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "leave-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Leave_Notify_Event;

   ------------
   -- On_Map --
   ------------

   procedure On_Map
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "map" & ASCII.NUL, Call, After);
   end On_Map;

   ------------
   -- On_Map --
   ------------

   procedure On_Map
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "map" & ASCII.NUL, Call, After, Slot);
   end On_Map;

   ------------------
   -- On_Map_Event --
   ------------------

   procedure On_Map_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "map-event" & ASCII.NUL, Call, After);
   end On_Map_Event;

   ------------------
   -- On_Map_Event --
   ------------------

   procedure On_Map_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Any_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "map-event" & ASCII.NUL, Call, After, Slot);
   end On_Map_Event;

   --------------------------
   -- On_Mnemonic_Activate --
   --------------------------

   procedure On_Mnemonic_Activate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "mnemonic-activate" & ASCII.NUL, Call, After);
   end On_Mnemonic_Activate;

   --------------------------
   -- On_Mnemonic_Activate --
   --------------------------

   procedure On_Mnemonic_Activate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "mnemonic-activate" & ASCII.NUL, Call, After, Slot);
   end On_Mnemonic_Activate;

   ----------------------------
   -- On_Motion_Notify_Event --
   ----------------------------

   procedure On_Motion_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Motion_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "motion-notify-event" & ASCII.NUL, Call, After);
   end On_Motion_Notify_Event;

   ----------------------------
   -- On_Motion_Notify_Event --
   ----------------------------

   procedure On_Motion_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Motion_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "motion-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Motion_Notify_Event;

   -------------------
   -- On_Move_Focus --
   -------------------

   procedure On_Move_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-focus" & ASCII.NUL, Call, After);
   end On_Move_Focus;

   -------------------
   -- On_Move_Focus --
   -------------------

   procedure On_Move_Focus
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-focus" & ASCII.NUL, Call, After, Slot);
   end On_Move_Focus;

   -------------------
   -- On_Parent_Set --
   -------------------

   procedure On_Parent_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "parent-set" & ASCII.NUL, Call, After);
   end On_Parent_Set;

   -------------------
   -- On_Parent_Set --
   -------------------

   procedure On_Parent_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "parent-set" & ASCII.NUL, Call, After, Slot);
   end On_Parent_Set;

   -------------------
   -- On_Popup_Menu --
   -------------------

   procedure On_Popup_Menu
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popup-menu" & ASCII.NUL, Call, After);
   end On_Popup_Menu;

   -------------------
   -- On_Popup_Menu --
   -------------------

   procedure On_Popup_Menu
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popup-menu" & ASCII.NUL, Call, After, Slot);
   end On_Popup_Menu;

   ------------------------------
   -- On_Property_Notify_Event --
   ------------------------------

   procedure On_Property_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Property_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "property-notify-event" & ASCII.NUL, Call, After);
   end On_Property_Notify_Event;

   ------------------------------
   -- On_Property_Notify_Event --
   ------------------------------

   procedure On_Property_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Property_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "property-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Property_Notify_Event;

   ---------------------------
   -- On_Proximity_In_Event --
   ---------------------------

   procedure On_Proximity_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "proximity-in-event" & ASCII.NUL, Call, After);
   end On_Proximity_In_Event;

   ---------------------------
   -- On_Proximity_In_Event --
   ---------------------------

   procedure On_Proximity_In_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Proximity_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "proximity-in-event" & ASCII.NUL, Call, After, Slot);
   end On_Proximity_In_Event;

   ----------------------------
   -- On_Proximity_Out_Event --
   ----------------------------

   procedure On_Proximity_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Proximity_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "proximity-out-event" & ASCII.NUL, Call, After);
   end On_Proximity_Out_Event;

   ----------------------------
   -- On_Proximity_Out_Event --
   ----------------------------

   procedure On_Proximity_Out_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Proximity_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "proximity-out-event" & ASCII.NUL, Call, After, Slot);
   end On_Proximity_Out_Event;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "query-tooltip" & ASCII.NUL, Call, After);
   end On_Query_Tooltip;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gint_Gint_Boolean_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "query-tooltip" & ASCII.NUL, Call, After, Slot);
   end On_Query_Tooltip;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "realize" & ASCII.NUL, Call, After);
   end On_Realize;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "realize" & ASCII.NUL, Call, After, Slot);
   end On_Realize;

   -----------------------
   -- On_Screen_Changed --
   -----------------------

   procedure On_Screen_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Screen_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "screen-changed" & ASCII.NUL, Call, After);
   end On_Screen_Changed;

   -----------------------
   -- On_Screen_Changed --
   -----------------------

   procedure On_Screen_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Screen_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "screen-changed" & ASCII.NUL, Call, After, Slot);
   end On_Screen_Changed;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   procedure On_Scroll_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Scroll_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll-event" & ASCII.NUL, Call, After);
   end On_Scroll_Event;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   procedure On_Scroll_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Scroll_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll-event" & ASCII.NUL, Call, After, Slot);
   end On_Scroll_Event;

   ------------------------------
   -- On_Selection_Clear_Event --
   ------------------------------

   procedure On_Selection_Clear_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-clear-event" & ASCII.NUL, Call, After);
   end On_Selection_Clear_Event;

   ------------------------------
   -- On_Selection_Clear_Event --
   ------------------------------

   procedure On_Selection_Clear_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-clear-event" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Clear_Event;

   ----------------------
   -- On_Selection_Get --
   ----------------------

   procedure On_Selection_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-get" & ASCII.NUL, Call, After);
   end On_Selection_Get;

   ----------------------
   -- On_Selection_Get --
   ----------------------

   procedure On_Selection_Get
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Selection_Data_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-get" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Get;

   -------------------------------
   -- On_Selection_Notify_Event --
   -------------------------------

   procedure On_Selection_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-notify-event" & ASCII.NUL, Call, After);
   end On_Selection_Notify_Event;

   -------------------------------
   -- On_Selection_Notify_Event --
   -------------------------------

   procedure On_Selection_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Notify_Event;

   ---------------------------
   -- On_Selection_Received --
   ---------------------------

   procedure On_Selection_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Selection_Data_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-received" & ASCII.NUL, Call, After);
   end On_Selection_Received;

   ---------------------------
   -- On_Selection_Received --
   ---------------------------

   procedure On_Selection_Received
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Selection_Data_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-received" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Received;

   --------------------------------
   -- On_Selection_Request_Event --
   --------------------------------

   procedure On_Selection_Request_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Selection_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "selection-request-event" & ASCII.NUL, Call, After);
   end On_Selection_Request_Event;

   --------------------------------
   -- On_Selection_Request_Event --
   --------------------------------

   procedure On_Selection_Request_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Selection_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "selection-request-event" & ASCII.NUL, Call, After, Slot);
   end On_Selection_Request_Event;

   -------------
   -- On_Show --
   -------------

   procedure On_Show
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "show" & ASCII.NUL, Call, After);
   end On_Show;

   -------------
   -- On_Show --
   -------------

   procedure On_Show
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "show" & ASCII.NUL, Call, After, Slot);
   end On_Show;

   ------------------
   -- On_Show_Help --
   ------------------

   procedure On_Show_Help
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Widget_Help_Type_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "show-help" & ASCII.NUL, Call, After);
   end On_Show_Help;

   ------------------
   -- On_Show_Help --
   ------------------

   procedure On_Show_Help
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Widget_Help_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "show-help" & ASCII.NUL, Call, After, Slot);
   end On_Show_Help;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Allocation_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "size-allocate" & ASCII.NUL, Call, After);
   end On_Size_Allocate;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Allocation_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "size-allocate" & ASCII.NUL, Call, After, Slot);
   end On_Size_Allocate;

   ----------------------
   -- On_State_Changed --
   ----------------------

   procedure On_State_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_State_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "state-changed" & ASCII.NUL, Call, After);
   end On_State_Changed;

   ----------------------
   -- On_State_Changed --
   ----------------------

   procedure On_State_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_State_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "state-changed" & ASCII.NUL, Call, After, Slot);
   end On_State_Changed;

   ----------------------------
   -- On_State_Flags_Changed --
   ----------------------------

   procedure On_State_Flags_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_State_Flags_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "state-flags-changed" & ASCII.NUL, Call, After);
   end On_State_Flags_Changed;

   ----------------------------
   -- On_State_Flags_Changed --
   ----------------------------

   procedure On_State_Flags_Changed
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_State_Flags_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "state-flags-changed" & ASCII.NUL, Call, After, Slot);
   end On_State_Flags_Changed;

   ------------------
   -- On_Style_Set --
   ------------------

   procedure On_Style_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gtk_Style_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "style-set" & ASCII.NUL, Call, After);
   end On_Style_Set;

   ------------------
   -- On_Style_Set --
   ------------------

   procedure On_Style_Set
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gtk_Style_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "style-set" & ASCII.NUL, Call, After, Slot);
   end On_Style_Set;

   ----------------------
   -- On_Style_Updated --
   ----------------------

   procedure On_Style_Updated
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "style-updated" & ASCII.NUL, Call, After);
   end On_Style_Updated;

   ----------------------
   -- On_Style_Updated --
   ----------------------

   procedure On_Style_Updated
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "style-updated" & ASCII.NUL, Call, After, Slot);
   end On_Style_Updated;

   --------------------
   -- On_Touch_Event --
   --------------------

   procedure On_Touch_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "touch-event" & ASCII.NUL, Call, After);
   end On_Touch_Event;

   --------------------
   -- On_Touch_Event --
   --------------------

   procedure On_Touch_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "touch-event" & ASCII.NUL, Call, After, Slot);
   end On_Touch_Event;

   --------------
   -- On_Unmap --
   --------------

   procedure On_Unmap
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unmap" & ASCII.NUL, Call, After);
   end On_Unmap;

   --------------
   -- On_Unmap --
   --------------

   procedure On_Unmap
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unmap" & ASCII.NUL, Call, After, Slot);
   end On_Unmap;

   --------------------
   -- On_Unmap_Event --
   --------------------

   procedure On_Unmap_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Any_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unmap-event" & ASCII.NUL, Call, After);
   end On_Unmap_Event;

   --------------------
   -- On_Unmap_Event --
   --------------------

   procedure On_Unmap_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Any_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unmap-event" & ASCII.NUL, Call, After, Slot);
   end On_Unmap_Event;

   ------------------
   -- On_Unrealize --
   ------------------

   procedure On_Unrealize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "unrealize" & ASCII.NUL, Call, After);
   end On_Unrealize;

   ------------------
   -- On_Unrealize --
   ------------------

   procedure On_Unrealize
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "unrealize" & ASCII.NUL, Call, After, Slot);
   end On_Unrealize;

   --------------------------------
   -- On_Visibility_Notify_Event --
   --------------------------------

   procedure On_Visibility_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Visibility_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "visibility-notify-event" & ASCII.NUL, Call, After);
   end On_Visibility_Notify_Event;

   --------------------------------
   -- On_Visibility_Notify_Event --
   --------------------------------

   procedure On_Visibility_Notify_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Visibility_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "visibility-notify-event" & ASCII.NUL, Call, After, Slot);
   end On_Visibility_Notify_Event;

   ---------------------------
   -- On_Window_State_Event --
   ---------------------------

   procedure On_Window_State_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_Gtk_Widget_Gdk_Event_Window_State_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "window-state-event" & ASCII.NUL, Call, After);
   end On_Window_State_Event;

   ---------------------------
   -- On_Window_State_Event --
   ---------------------------

   procedure On_Window_State_Event
      (Self  : not null access Gtk_Widget_Record;
       Call  : Cb_GObject_Gdk_Event_Window_State_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "window-state-event" & ASCII.NUL, Call, After, Slot);
   end On_Window_State_Event;

end Gtk.Widget;
