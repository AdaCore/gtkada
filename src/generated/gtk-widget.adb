------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

package body Gtk.Widget is

   function From_Object_Free (B : access Gtk_Requisition) return Gtk_Requisition is
      Result : constant Gtk_Requisition := B.all;
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


   procedure Inherited_Measure
     (Klass                              : Glib.Object.Ada_GObject_Class;
      Widget                             : access Gtk_Widget_Record'Class;
      Orientation                        : Gtk.Enums.Gtk_Orientation;
      For_Size                           : Glib.Gint;
      Minimum, Natural                   : out Glib.Gint;
      Minimum_Baseline, Natural_Baseline : out Glib.Gint)
   is
      procedure Internal
        (Klass                              : Glib.Object.Ada_GObject_Class;
         Widget                             : System.Address;
         Orientation                        : Gtk.Enums.Gtk_Orientation;
         For_Size                           : Glib.Gint;
         Minimum, Natural                   : out Glib.Gint;
         Minimum_Baseline, Natural_Baseline : out Glib.Gint);
      pragma Import (C, Internal, "ada_inherited_WIDGET_CLASS_measure");
   begin
      Internal
        (Klass, Widget.Get_Object, Orientation, For_Size,
         Minimum, Natural, Minimum_Baseline, Natural_Baseline);
   end Inherited_Measure;

   procedure Inherited_Size_Allocate
     (Klass                   : Glib.Object.Ada_GObject_Class;
      Widget                  : access Gtk_Widget_Record'Class;
      Width, Height, Baseline : Glib.Gint)
   is
      procedure Internal
        (Klass                   : Glib.Object.Ada_GObject_Class;
         Widget                  : System.Address;
         Width, Height, Baseline : Glib.Gint);
      pragma Import
        (C, Internal, "ada_inherited_WIDGET_CLASS_size_allocate");
   begin
      Internal (Klass, Widget.Get_Object, Width, Height, Baseline);
   end Inherited_Size_Allocate;

   procedure Inherited_Realize
     (Klass  : Glib.Object.Ada_GObject_Class;
      Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Klass  : Glib.Object.Ada_GObject_Class;
         Widget : System.Address);
      pragma Import (C, Internal, "ada_inherited_WIDGET_CLASS_realize");
   begin
      Internal (Klass, Widget.Get_Object);
   end Inherited_Realize;

   procedure Inherited_Snapshot
     (Klass    : Glib.Object.Ada_GObject_Class;
      Widget   : access Gtk_Widget_Record'Class;
      Snapshot : System.Address)
   is
      procedure Internal
        (Klass    : Glib.Object.Ada_GObject_Class;
         Widget   : System.Address;
         Snapshot : System.Address);
      pragma Import (C, Internal, "ada_inherited_WIDGET_CLASS_snapshot");
   begin
      Internal (Klass, Widget.Get_Object, Snapshot);
   end Inherited_Snapshot;

   package Type_Conversion_Gtk_Widget is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Widget_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Widget);

   ------------------------
   -- Action_Set_Enabled --
   ------------------------

   procedure Action_Set_Enabled
      (Widget      : not null access Gtk_Widget_Record;
       Action_Name : UTF8_String;
       Enabled     : Boolean)
   is
      procedure Internal
         (Widget      : System.Address;
          Action_Name : Gtkada.Types.Chars_Ptr;
          Enabled     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_action_set_enabled");
      Tmp_Action_Name : Gtkada.Types.Chars_Ptr := New_String (Action_Name);
   begin
      Internal (Get_Object (Widget), Tmp_Action_Name, Boolean'Pos (Enabled));
      Free (Tmp_Action_Name);
   end Action_Set_Enabled;

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

   -----------------------------
   -- Activate_Action_Variant --
   -----------------------------

   function Activate_Action_Variant
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Args   : Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Widget : System.Address;
          Name   : Gtkada.Types.Chars_Ptr;
          Args   : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_activate_action_variant");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Name, Get_Object (Args));
      Free (Tmp_Name);
      return Tmp_Return /= 0;
   end Activate_Action_Variant;

   ----------------------
   -- Activate_Default --
   ----------------------

   procedure Activate_Default (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_activate_default");
   begin
      Internal (Get_Object (Widget));
   end Activate_Default;

   -------------------
   -- Add_Css_Class --
   -------------------

   procedure Add_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String)
   is
      procedure Internal
         (Widget    : System.Address;
          Css_Class : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_add_css_class");
      Tmp_Css_Class : Gtkada.Types.Chars_Ptr := New_String (Css_Class);
   begin
      Internal (Get_Object (Widget), Tmp_Css_Class);
      Free (Tmp_Css_Class);
   end Add_Css_Class;

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

   --------------
   -- Contains --
   --------------

   function Contains
      (Widget : not null access Gtk_Widget_Record;
       X      : Gdouble;
       Y      : Gdouble) return Boolean
   is
      function Internal
         (Widget : System.Address;
          X      : Gdouble;
          Y      : Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_contains");
   begin
      return Internal (Get_Object (Widget), X, Y) /= 0;
   end Contains;

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

   ----------------------
   -- Dispose_Template --
   ----------------------

   procedure Dispose_Template
      (Widget      : not null access Gtk_Widget_Record;
       Widget_Type : GType)
   is
      procedure Internal (Widget : System.Address; Widget_Type : GType);
      pragma Import (C, Internal, "gtk_widget_dispose_template");
   begin
      Internal (Get_Object (Widget), Widget_Type);
   end Dispose_Template;

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

   ----------------
   -- Error_Bell --
   ----------------

   procedure Error_Bell (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_error_bell");
   begin
      Internal (Get_Object (Widget));
   end Error_Bell;

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

   ------------------
   -- Get_Baseline --
   ------------------

   function Get_Baseline
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_baseline");
   begin
      return Internal (Get_Object (Widget));
   end Get_Baseline;

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

   --------------------
   -- Get_Can_Target --
   --------------------

   function Get_Can_Target
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_can_target");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Can_Target;

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

   ---------------------
   -- Get_Css_Classes --
   ---------------------

   function Get_Css_Classes
      (Widget : not null access Gtk_Widget_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Widget : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_widget_get_css_classes");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Widget)));
   end Get_Css_Classes;

   ------------------
   -- Get_Css_Name --
   ------------------

   function Get_Css_Name
      (Widget : not null access Gtk_Widget_Record) return UTF8_String
   is
      function Internal
         (Widget : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_widget_get_css_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Css_Name;

   ---------------------
   -- Get_First_Child --
   ---------------------

   function Get_First_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_first_child");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_First_Child;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_focus_child");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Focus_Child;

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

   -------------------
   -- Get_Focusable --
   -------------------

   function Get_Focusable
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_focusable");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Focusable;

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

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_height");
   begin
      return Internal (Get_Object (Widget));
   end Get_Height;

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

   --------------------
   -- Get_Last_Child --
   --------------------

   function Get_Last_Child
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_last_child");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Last_Child;

   ------------------------
   -- Get_Layout_Manager --
   ------------------------

   function Get_Layout_Manager
      (Widget : not null access Gtk_Widget_Record)
       return Gtk.Layout_Manager.Gtk_Layout_Manager
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_layout_manager");
      Stub_Gtk_Layout_Manager : Gtk.Layout_Manager.Gtk_Layout_Manager_Record;
   begin
      return Gtk.Layout_Manager.Gtk_Layout_Manager (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Layout_Manager));
   end Get_Layout_Manager;

   ----------------------
   -- Get_Limit_Events --
   ----------------------

   function Get_Limit_Events
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_get_limit_events");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Limit_Events;

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

   ----------------------
   -- Get_Next_Sibling --
   ----------------------

   function Get_Next_Sibling
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_next_sibling");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Next_Sibling;

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

   ----------------------
   -- Get_Prev_Sibling --
   ----------------------

   function Get_Prev_Sibling
      (Widget : not null access Gtk_Widget_Record) return Gtk_Widget
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_prev_sibling");
      Stub_Gtk_Widget : Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Widget));
   end Get_Prev_Sibling;

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
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
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
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Widget)));
   end Get_Tooltip_Text;

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

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Widget : not null access Gtk_Widget_Record) return Glib.Gint
   is
      function Internal (Widget : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_widget_get_width");
   begin
      return Internal (Get_Object (Widget));
   end Get_Width;

   ----------------
   -- Grab_Focus --
   ----------------

   function Grab_Focus
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_grab_focus");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Grab_Focus;

   -------------------
   -- Has_Css_Class --
   -------------------

   function Has_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String) return Boolean
   is
      function Internal
         (Widget    : System.Address;
          Css_Class : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_has_css_class");
      Tmp_Css_Class : Gtkada.Types.Chars_Ptr := New_String (Css_Class);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Widget), Tmp_Css_Class);
      Free (Tmp_Css_Class);
      return Tmp_Return /= 0;
   end Has_Css_Class;

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

   -------------------------
   -- Insert_Action_Group --
   -------------------------

   procedure Insert_Action_Group
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String;
       Group  : Glib.Action_Group.Gaction_Group)
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Gtkada.Types.Chars_Ptr;
          Group  : Glib.Action_Group.Gaction_Group);
      pragma Import (C, Internal, "gtk_widget_insert_action_group");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Get_Object (Widget), Tmp_Name, Group);
      Free (Tmp_Name);
   end Insert_Action_Group;

   ------------------
   -- Insert_After --
   ------------------

   procedure Insert_After
      (Widget           : not null access Gtk_Widget_Record;
       Parent           : not null access Gtk_Widget_Record'Class;
       Previous_Sibling : access Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Widget           : System.Address;
          Parent           : System.Address;
          Previous_Sibling : System.Address);
      pragma Import (C, Internal, "gtk_widget_insert_after");
   begin
      Internal (Get_Object (Widget), Get_Object (Parent), Get_Object_Or_Null (GObject (Previous_Sibling)));
   end Insert_After;

   -------------------
   -- Insert_Before --
   -------------------

   procedure Insert_Before
      (Widget       : not null access Gtk_Widget_Record;
       Parent       : not null access Gtk_Widget_Record'Class;
       Next_Sibling : access Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Widget       : System.Address;
          Parent       : System.Address;
          Next_Sibling : System.Address);
      pragma Import (C, Internal, "gtk_widget_insert_before");
   begin
      Internal (Get_Object (Widget), Get_Object (Parent), Get_Object_Or_Null (GObject (Next_Sibling)));
   end Insert_Before;

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

   --------------------
   -- Queue_Allocate --
   --------------------

   procedure Queue_Allocate (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_allocate");
   begin
      Internal (Get_Object (Widget));
   end Queue_Allocate;

   ----------------
   -- Queue_Draw --
   ----------------

   procedure Queue_Draw (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_draw");
   begin
      Internal (Get_Object (Widget));
   end Queue_Draw;

   ------------------
   -- Queue_Resize --
   ------------------

   procedure Queue_Resize (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_queue_resize");
   begin
      Internal (Get_Object (Widget));
   end Queue_Resize;

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
   -- Remove_Css_Class --
   ----------------------

   procedure Remove_Css_Class
      (Widget    : not null access Gtk_Widget_Record;
       Css_Class : UTF8_String)
   is
      procedure Internal
         (Widget    : System.Address;
          Css_Class : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_remove_css_class");
      Tmp_Css_Class : Gtkada.Types.Chars_Ptr := New_String (Css_Class);
   begin
      Internal (Get_Object (Widget), Tmp_Css_Class);
      Free (Tmp_Css_Class);
   end Remove_Css_Class;

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

   --------------------
   -- Set_Can_Target --
   --------------------

   procedure Set_Can_Target
      (Widget     : not null access Gtk_Widget_Record;
       Can_Target : Boolean)
   is
      procedure Internal
         (Widget     : System.Address;
          Can_Target : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_can_target");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Can_Target));
   end Set_Can_Target;

   -----------------------
   -- Set_Child_Visible --
   -----------------------

   procedure Set_Child_Visible
      (Widget        : not null access Gtk_Widget_Record;
       Child_Visible : Boolean)
   is
      procedure Internal
         (Widget        : System.Address;
          Child_Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_child_visible");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Child_Visible));
   end Set_Child_Visible;

   ---------------------
   -- Set_Css_Classes --
   ---------------------

   procedure Set_Css_Classes
      (Widget  : not null access Gtk_Widget_Record;
       Classes : GNAT.Strings.String_List)
   is
      procedure Internal
         (Widget  : System.Address;
          Classes : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_widget_set_css_classes");
      Tmp_Classes : Gtkada.Types.chars_ptr_array := From_String_List (Classes);
   begin
      Internal (Get_Object (Widget), Tmp_Classes);
      Gtkada.Types.Free (Tmp_Classes);
   end Set_Css_Classes;

   --------------------------
   -- Set_Cursor_From_Name --
   --------------------------

   procedure Set_Cursor_From_Name
      (Widget : not null access Gtk_Widget_Record;
       Name   : UTF8_String := "")
   is
      procedure Internal
         (Widget : System.Address;
          Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_widget_set_cursor_from_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Name = "" then
         Tmp_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Name := New_String (Name);
      end if;
      Internal (Get_Object (Widget), Tmp_Name);
      Free (Tmp_Name);
   end Set_Cursor_From_Name;

   ---------------------
   -- Set_Focus_Child --
   ---------------------

   procedure Set_Focus_Child
      (Widget : not null access Gtk_Widget_Record;
       Child  : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Widget : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_focus_child");
   begin
      Internal (Get_Object (Widget), Get_Object_Or_Null (GObject (Child)));
   end Set_Focus_Child;

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

   -------------------
   -- Set_Focusable --
   -------------------

   procedure Set_Focusable
      (Widget    : not null access Gtk_Widget_Record;
       Focusable : Boolean)
   is
      procedure Internal
         (Widget    : System.Address;
          Focusable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_focusable");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Focusable));
   end Set_Focusable;

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

   ------------------------
   -- Set_Layout_Manager --
   ------------------------

   procedure Set_Layout_Manager
      (Widget         : not null access Gtk_Widget_Record;
       Layout_Manager : access Gtk.Layout_Manager.Gtk_Layout_Manager_Record'Class)
   is
      procedure Internal
         (Widget         : System.Address;
          Layout_Manager : System.Address);
      pragma Import (C, Internal, "gtk_widget_set_layout_manager");
   begin
      Internal (Get_Object (Widget), Get_Object_Or_Null (GObject (Layout_Manager)));
   end Set_Layout_Manager;

   ----------------------
   -- Set_Limit_Events --
   ----------------------

   procedure Set_Limit_Events
      (Widget       : not null access Gtk_Widget_Record;
       Limit_Events : Boolean)
   is
      procedure Internal
         (Widget       : System.Address;
          Limit_Events : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_widget_set_limit_events");
   begin
      Internal (Get_Object (Widget), Boolean'Pos (Limit_Events));
   end Set_Limit_Events;

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

   -------------------
   -- Should_Layout --
   -------------------

   function Should_Layout
      (Widget : not null access Gtk_Widget_Record) return Boolean
   is
      function Internal (Widget : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_should_layout");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Should_Layout;

   ----------
   -- Show --
   ----------

   procedure Show (Widget : not null access Gtk_Widget_Record) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Show;

   ---------------------------
   -- Translate_Coordinates --
   ---------------------------

   procedure Translate_Coordinates
      (Widget      : not null access Gtk_Widget_Record;
       Dest_Widget : not null access Gtk_Widget_Record'Class;
       Src_X       : Gdouble;
       Src_Y       : Gdouble;
       Dest_X      : out Gdouble;
       Dest_Y      : out Gdouble;
       Result      : out Boolean)
   is
      function Internal
         (Widget      : System.Address;
          Dest_Widget : System.Address;
          Src_X       : Gdouble;
          Src_Y       : Gdouble;
          Acc_Dest_X  : access Gdouble;
          Acc_Dest_Y  : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_widget_translate_coordinates");
      Acc_Dest_X : aliased Gdouble;
      Acc_Dest_Y : aliased Gdouble;
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

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Text_Direction_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Text_Direction_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Direction_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Direction_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Direction_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Direction_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Boolean_GObject_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Boolean_GObject_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Widget_Gtk_State_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Widget_Gtk_State_Flags_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_State_Flags_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_State_Flags_Void);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Text_Direction_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Widget_Gtk_State_Flags_Void;
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
       Handler : Cb_GObject_Gtk_Text_Direction_Void;
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
       Handler : Cb_GObject_Boolean_Boolean;
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
       Handler : Cb_GObject_Gint_Gint_Boolean_GObject_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Widget_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_State_Flags_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Boolean_GObject_Boolean);

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

   procedure Marsh_GObject_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_State_Flags_Void);

   procedure Marsh_GObject_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Direction_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Widget_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Boolean_Boolean);

   procedure Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gint_Gint_Boolean_GObject_Boolean);

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

   procedure Marsh_Gtk_Widget_Gtk_State_Flags_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_State_Flags_Void);

   procedure Marsh_Gtk_Widget_Gtk_Text_Direction_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Widget_Gtk_Text_Direction_Void);

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

end Gtk.Widget;
