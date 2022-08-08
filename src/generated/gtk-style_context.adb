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
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Style_Context is

   function Get_Style_Context
     (Widget : not null access Gtk_Widget_Record'Class)
   return Gtk_Style_Context
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_style_context");
      Stub_Gtk_Style_Context : Gtk_Style_Context_Record;
   begin
      return Gtk_Style_Context
        (Get_User_Data (Internal (Get_Object (Widget)), Stub_Gtk_Style_Context));
   end Get_Style_Context;

   package Type_Conversion_Gtk_Style_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Style_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Style_Context);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Style_Context) is
   begin
      Self := new Gtk_Style_Context_Record;
      Gtk.Style_Context.Initialize (Self);
   end Gtk_New;

   ---------------------------
   -- Gtk_Style_Context_New --
   ---------------------------

   function Gtk_Style_Context_New return Gtk_Style_Context is
      Self : constant Gtk_Style_Context := new Gtk_Style_Context_Record;
   begin
      Gtk.Style_Context.Initialize (Self);
      return Self;
   end Gtk_Style_Context_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Style_Context_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_context_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Class_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_style_context_add_class");
      Tmp_Class_Name : Gtkada.Types.Chars_Ptr := New_String (Class_Name);
   begin
      Internal (Get_Object (Self), Tmp_Class_Name);
      Free (Tmp_Class_Name);
   end Add_Class;

   ------------------
   -- Add_Provider --
   ------------------

   procedure Add_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider;
          Priority : Guint);
      pragma Import (C, Internal, "gtk_style_context_add_provider");
   begin
      Internal (Get_Object (Self), Provider, Priority);
   end Add_Provider;

   ----------------
   -- Add_Region --
   ----------------

   procedure Add_Region
      (Self        : not null access Gtk_Style_Context_Record;
       Region_Name : UTF8_String;
       Flags       : Gtk.Enums.Gtk_Region_Flags)
   is
      procedure Internal
         (Self        : System.Address;
          Region_Name : Gtkada.Types.Chars_Ptr;
          Flags       : Gtk.Enums.Gtk_Region_Flags);
      pragma Import (C, Internal, "gtk_style_context_add_region");
      Tmp_Region_Name : Gtkada.Types.Chars_Ptr := New_String (Region_Name);
   begin
      Internal (Get_Object (Self), Tmp_Region_Name, Flags);
      Free (Tmp_Region_Name);
   end Add_Region;

   -----------------------
   -- Cancel_Animations --
   -----------------------

   procedure Cancel_Animations
      (Self      : not null access Gtk_Style_Context_Record;
       Region_Id : System.Address)
   is
      procedure Internal (Self : System.Address; Region_Id : System.Address);
      pragma Import (C, Internal, "gtk_style_context_cancel_animations");
   begin
      Internal (Get_Object (Self), Region_Id);
   end Cancel_Animations;

   --------------------------
   -- Get_Background_Color --
   --------------------------

   procedure Get_Background_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_style_context_get_background_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Background_Color;

   ----------------
   -- Get_Border --
   ----------------

   procedure Get_Border
      (Self   : not null access Gtk_Style_Context_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Border : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self   : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Border : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_style_context_get_border");
      Tmp_Border : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Border);
      Border := Tmp_Border;
   end Get_Border;

   ----------------------
   -- Get_Border_Color --
   ----------------------

   procedure Get_Border_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_style_context_get_border_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Border_Color;

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_style_context_get_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Color;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_Text_Direction
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Text_Direction;
      pragma Import (C, Internal, "gtk_style_context_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Self  : not null access Gtk_Style_Context_Record;
       State : Gtk.Enums.Gtk_State_Flags)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags)
          return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "gtk_style_context_get_font");
   begin
      return Internal (Get_Object (Self), State);
   end Get_Font;

   ---------------------
   -- Get_Frame_Clock --
   ---------------------

   function Get_Frame_Clock
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Frame_Clock.Gdk_Frame_Clock
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_get_frame_clock");
      Stub_Gdk_Frame_Clock : Gdk.Frame_Clock.Gdk_Frame_Clock_Record;
   begin
      return Gdk.Frame_Clock.Gdk_Frame_Clock (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Frame_Clock));
   end Get_Frame_Clock;

   ------------------------
   -- Get_Junction_Sides --
   ------------------------

   function Get_Junction_Sides
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_Junction_Sides
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Junction_Sides;
      pragma Import (C, Internal, "gtk_style_context_get_junction_sides");
   begin
      return Internal (Get_Object (Self));
   end Get_Junction_Sides;

   ----------------
   -- Get_Margin --
   ----------------

   procedure Get_Margin
      (Self   : not null access Gtk_Style_Context_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Margin : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self   : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Margin : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_style_context_get_margin");
      Tmp_Margin : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Margin);
      Margin := Tmp_Margin;
   end Get_Margin;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Self    : not null access Gtk_Style_Context_Record;
       State   : Gtk.Enums.Gtk_State_Flags;
       Padding : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self    : System.Address;
          State   : Gtk.Enums.Gtk_State_Flags;
          Padding : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_style_context_get_padding");
      Tmp_Padding : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Padding);
      Padding := Tmp_Padding;
   end Get_Padding;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk_Style_Context
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_get_parent");
      Stub_Gtk_Style_Context : Gtk_Style_Context_Record;
   begin
      return Gtk.Style_Context.Gtk_Style_Context (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Style_Context));
   end Get_Parent;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Widget.Gtk_Widget_Path
   is
      function Internal
         (Self : System.Address) return access Gtk.Widget.Gtk_Widget_Path;
      pragma Import (C, Internal, "gtk_style_context_get_path");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Path;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
      (Self     : not null access Gtk_Style_Context_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          State    : Gtk.Enums.Gtk_State_Flags;
          Value    : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_style_context_get_property");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Property, State, Value);
      Free (Tmp_Property);
   end Get_Property;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
      (Self : not null access Gtk_Style_Context_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_style_context_get_scale");
   begin
      return Internal (Get_Object (Self));
   end Get_Scale;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Screen));
   end Get_Screen;

   -----------------
   -- Get_Section --
   -----------------

   function Get_Section
      (Self     : not null access Gtk_Style_Context_Record;
       Property : UTF8_String) return Gtk.Css_Section.Gtk_Css_Section
   is
      function Internal
         (Self     : System.Address;
          Property : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_get_section");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Property);
      Free (Tmp_Property);
      return From_Object (Tmp_Return);
   end Get_Section;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.Gtk_State_Flags
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_State_Flags;
      pragma Import (C, Internal, "gtk_style_context_get_state");
   begin
      return Internal (Get_Object (Self));
   end Get_State;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Self          : not null access Gtk_Style_Context_Record;
       Property_Name : UTF8_String;
       Value         : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_style_context_get_style_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Get_Style_Property;

   ---------------
   -- Has_Class --
   ---------------

   function Has_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Class_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_context_has_class");
      Tmp_Class_Name : Gtkada.Types.Chars_Ptr := New_String (Class_Name);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Class_Name);
      Free (Tmp_Class_Name);
      return Tmp_Return /= 0;
   end Has_Class;

   ----------------
   -- Has_Region --
   ----------------

   procedure Has_Region
      (Self         : not null access Gtk_Style_Context_Record;
       Region_Name  : UTF8_String;
       Flags_Return : out Gtk.Enums.Gtk_Region_Flags;
       Is_Defined   : out Boolean)
   is
      function Internal
         (Self             : System.Address;
          Region_Name      : Gtkada.Types.Chars_Ptr;
          Acc_Flags_Return : access Gtk.Enums.Gtk_Region_Flags)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_context_has_region");
      Acc_Flags_Return : aliased Gtk.Enums.Gtk_Region_Flags;
      Tmp_Region_Name  : Gtkada.Types.Chars_Ptr := New_String (Region_Name);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Region_Name, Acc_Flags_Return'Access);
      Free (Tmp_Region_Name);
      Flags_Return := Acc_Flags_Return;
      Is_Defined := Tmp_Return /= 0;
   end Has_Region;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate (Self : not null access Gtk_Style_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_style_context_invalidate");
   begin
      Internal (Get_Object (Self));
   end Invalidate;

   ------------------
   -- List_Classes --
   ------------------

   function List_Classes
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_list_classes");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end List_Classes;

   ------------------
   -- List_Regions --
   ------------------

   function List_Regions
      (Self : not null access Gtk_Style_Context_Record)
       return Gtk.Enums.String_List.Glist
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_list_regions");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Get_Object (Self)));
      return Tmp_Return;
   end List_Regions;

   ------------------
   -- Lookup_Color --
   ------------------

   procedure Lookup_Color
      (Self       : not null access Gtk_Style_Context_Record;
       Color_Name : UTF8_String;
       Color      : out Gdk.RGBA.Gdk_RGBA;
       Found      : out Boolean)
   is
      function Internal
         (Self       : System.Address;
          Color_Name : Gtkada.Types.Chars_Ptr;
          Acc_Color  : access Gdk.RGBA.Gdk_RGBA) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_context_lookup_color");
      Acc_Color      : aliased Gdk.RGBA.Gdk_RGBA;
      Tmp_Color_Name : Gtkada.Types.Chars_Ptr := New_String (Color_Name);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Color_Name, Acc_Color'Access);
      Free (Tmp_Color_Name);
      Color := Acc_Color;
      Found := Tmp_Return /= 0;
   end Lookup_Color;

   -------------------------
   -- Notify_State_Change --
   -------------------------

   procedure Notify_State_Change
      (Self        : not null access Gtk_Style_Context_Record;
       Window      : Gdk.Gdk_Window;
       Region_Id   : System.Address;
       State       : Gtk.Enums.Gtk_State_Type;
       State_Value : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Window      : Gdk.Gdk_Window;
          Region_Id   : System.Address;
          State       : Gtk.Enums.Gtk_State_Type;
          State_Value : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_style_context_notify_state_change");
   begin
      Internal (Get_Object (Self), Window, Region_Id, State, Boolean'Pos (State_Value));
   end Notify_State_Change;

   ---------------------------
   -- Pop_Animatable_Region --
   ---------------------------

   procedure Pop_Animatable_Region
      (Self : not null access Gtk_Style_Context_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_style_context_pop_animatable_region");
   begin
      Internal (Get_Object (Self));
   end Pop_Animatable_Region;

   ----------------------------
   -- Push_Animatable_Region --
   ----------------------------

   procedure Push_Animatable_Region
      (Self      : not null access Gtk_Style_Context_Record;
       Region_Id : System.Address)
   is
      procedure Internal (Self : System.Address; Region_Id : System.Address);
      pragma Import (C, Internal, "gtk_style_context_push_animatable_region");
   begin
      Internal (Get_Object (Self), Region_Id);
   end Push_Animatable_Region;

   ------------------
   -- Remove_Class --
   ------------------

   procedure Remove_Class
      (Self       : not null access Gtk_Style_Context_Record;
       Class_Name : UTF8_String)
   is
      procedure Internal
         (Self       : System.Address;
          Class_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_style_context_remove_class");
      Tmp_Class_Name : Gtkada.Types.Chars_Ptr := New_String (Class_Name);
   begin
      Internal (Get_Object (Self), Tmp_Class_Name);
      Free (Tmp_Class_Name);
   end Remove_Class;

   ---------------------
   -- Remove_Provider --
   ---------------------

   procedure Remove_Provider
      (Self     : not null access Gtk_Style_Context_Record;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider)
   is
      procedure Internal
         (Self     : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider);
      pragma Import (C, Internal, "gtk_style_context_remove_provider");
   begin
      Internal (Get_Object (Self), Provider);
   end Remove_Provider;

   -------------------
   -- Remove_Region --
   -------------------

   procedure Remove_Region
      (Self        : not null access Gtk_Style_Context_Record;
       Region_Name : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Region_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_style_context_remove_region");
      Tmp_Region_Name : Gtkada.Types.Chars_Ptr := New_String (Region_Name);
   begin
      Internal (Get_Object (Self), Tmp_Region_Name);
      Free (Tmp_Region_Name);
   end Remove_Region;

   -------------
   -- Restore --
   -------------

   procedure Restore (Self : not null access Gtk_Style_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_style_context_restore");
   begin
      Internal (Get_Object (Self));
   end Restore;

   ----------
   -- Save --
   ----------

   procedure Save (Self : not null access Gtk_Style_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_style_context_save");
   begin
      Internal (Get_Object (Self));
   end Save;

   -----------------------
   -- Scroll_Animations --
   -----------------------

   procedure Scroll_Animations
      (Self   : not null access Gtk_Style_Context_Record;
       Window : Gdk.Gdk_Window;
       Dx     : Glib.Gint;
       Dy     : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Window : Gdk.Gdk_Window;
          Dx     : Glib.Gint;
          Dy     : Glib.Gint);
      pragma Import (C, Internal, "gtk_style_context_scroll_animations");
   begin
      Internal (Get_Object (Self), Window, Dx, Dy);
   end Scroll_Animations;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
      (Self   : not null access Gtk_Style_Context_Record;
       Window : Gdk.Gdk_Window)
   is
      procedure Internal (Self : System.Address; Window : Gdk.Gdk_Window);
      pragma Import (C, Internal, "gtk_style_context_set_background");
   begin
      Internal (Get_Object (Self), Window);
   end Set_Background;

   -------------------
   -- Set_Direction --
   -------------------

   procedure Set_Direction
      (Self      : not null access Gtk_Style_Context_Record;
       Direction : Gtk.Enums.Gtk_Text_Direction)
   is
      procedure Internal
         (Self      : System.Address;
          Direction : Gtk.Enums.Gtk_Text_Direction);
      pragma Import (C, Internal, "gtk_style_context_set_direction");
   begin
      Internal (Get_Object (Self), Direction);
   end Set_Direction;

   ---------------------
   -- Set_Frame_Clock --
   ---------------------

   procedure Set_Frame_Clock
      (Self        : not null access Gtk_Style_Context_Record;
       Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Frame_Clock : System.Address);
      pragma Import (C, Internal, "gtk_style_context_set_frame_clock");
   begin
      Internal (Get_Object (Self), Get_Object (Frame_Clock));
   end Set_Frame_Clock;

   ------------------------
   -- Set_Junction_Sides --
   ------------------------

   procedure Set_Junction_Sides
      (Self  : not null access Gtk_Style_Context_Record;
       Sides : Gtk.Enums.Gtk_Junction_Sides)
   is
      procedure Internal
         (Self  : System.Address;
          Sides : Gtk.Enums.Gtk_Junction_Sides);
      pragma Import (C, Internal, "gtk_style_context_set_junction_sides");
   begin
      Internal (Get_Object (Self), Sides);
   end Set_Junction_Sides;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
      (Self   : not null access Gtk_Style_Context_Record;
       Parent : access Gtk_Style_Context_Record'Class)
   is
      procedure Internal (Self : System.Address; Parent : System.Address);
      pragma Import (C, Internal, "gtk_style_context_set_parent");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Parent)));
   end Set_Parent;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
      (Self : not null access Gtk_Style_Context_Record;
       Path : Gtk.Widget.Gtk_Widget_Path)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtk.Widget.Gtk_Widget_Path);
      pragma Import (C, Internal, "gtk_style_context_set_path");
   begin
      Internal (Get_Object (Self), Path);
   end Set_Path;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
      (Self  : not null access Gtk_Style_Context_Record;
       Scale : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Scale : Glib.Gint);
      pragma Import (C, Internal, "gtk_style_context_set_scale");
   begin
      Internal (Get_Object (Self), Scale);
   end Set_Scale;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Self   : not null access Gtk_Style_Context_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Self : System.Address; Screen : System.Address);
      pragma Import (C, Internal, "gtk_style_context_set_screen");
   begin
      Internal (Get_Object (Self), Get_Object (Screen));
   end Set_Screen;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk.Enums.Gtk_State_Flags)
   is
      procedure Internal
         (Self  : System.Address;
          Flags : Gtk.Enums.Gtk_State_Flags);
      pragma Import (C, Internal, "gtk_style_context_set_state");
   begin
      Internal (Get_Object (Self), Flags);
   end Set_State;

   ----------------------
   -- State_Is_Running --
   ----------------------

   procedure State_Is_Running
      (Self       : not null access Gtk_Style_Context_Record;
       State      : Gtk.Enums.Gtk_State_Type;
       Progress   : out Gdouble;
       Is_Running : out Boolean)
   is
      function Internal
         (Self         : System.Address;
          State        : Gtk.Enums.Gtk_State_Type;
          Acc_Progress : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_style_context_state_is_running");
      Acc_Progress : aliased Gdouble;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), State, Acc_Progress'Access);
      Progress := Acc_Progress;
      Is_Running := Tmp_Return /= 0;
   end State_Is_Running;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Self  : not null access Gtk_Style_Context_Record;
       Flags : Gtk_Style_Context_Print_Flags) return UTF8_String
   is
      function Internal
         (Self  : System.Address;
          Flags : Gtk_Style_Context_Print_Flags)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_style_context_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Flags));
   end To_String;

   -----------------------------
   -- Add_Provider_For_Screen --
   -----------------------------

   procedure Add_Provider_For_Screen
      (Screen   : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint)
   is
      procedure Internal
         (Screen   : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider;
          Priority : Guint);
      pragma Import (C, Internal, "gtk_style_context_add_provider_for_screen");
   begin
      Internal (Get_Object (Screen), Provider, Priority);
   end Add_Provider_For_Screen;

   --------------------------------
   -- Remove_Provider_For_Screen --
   --------------------------------

   procedure Remove_Provider_For_Screen
      (Screen   : not null access Gdk.Screen.Gdk_Screen_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider)
   is
      procedure Internal
         (Screen   : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider);
      pragma Import (C, Internal, "gtk_style_context_remove_provider_for_screen");
   begin
      Internal (Get_Object (Screen), Provider);
   end Remove_Provider_For_Screen;

   ---------------------
   -- Render_Activity --
   ---------------------

   procedure Render_Activity
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_activity");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Activity;

   ------------------
   -- Render_Arrow --
   ------------------

   procedure Render_Arrow
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Angle   : Gdouble;
       X       : Gdouble;
       Y       : Gdouble;
       Size    : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          Angle   : Gdouble;
          X       : Gdouble;
          Y       : Gdouble;
          Size    : Gdouble);
      pragma Import (C, Internal, "gtk_render_arrow");
   begin
      Internal (Get_Object (Context), Cr, Angle, X, Y, Size);
   end Render_Arrow;

   -----------------------
   -- Render_Background --
   -----------------------

   procedure Render_Background
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_background");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Background;

   ------------------
   -- Render_Check --
   ------------------

   procedure Render_Check
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_check");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Check;

   ---------------------
   -- Render_Expander --
   ---------------------

   procedure Render_Expander
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_expander");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Expander;

   ----------------------
   -- Render_Extension --
   ----------------------

   procedure Render_Extension
      (Context  : not null access Gtk_Style_Context_Record'Class;
       Cr       : Cairo.Cairo_Context;
       X        : Gdouble;
       Y        : Gdouble;
       Width    : Gdouble;
       Height   : Gdouble;
       Gap_Side : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Context  : System.Address;
          Cr       : Cairo.Cairo_Context;
          X        : Gdouble;
          Y        : Gdouble;
          Width    : Gdouble;
          Height   : Gdouble;
          Gap_Side : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_render_extension");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height, Gap_Side);
   end Render_Extension;

   ------------------
   -- Render_Focus --
   ------------------

   procedure Render_Focus
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_focus");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Focus;

   ------------------
   -- Render_Frame --
   ------------------

   procedure Render_Frame
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_frame");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Frame;

   ----------------------
   -- Render_Frame_Gap --
   ----------------------

   procedure Render_Frame_Gap
      (Context  : not null access Gtk_Style_Context_Record'Class;
       Cr       : Cairo.Cairo_Context;
       X        : Gdouble;
       Y        : Gdouble;
       Width    : Gdouble;
       Height   : Gdouble;
       Gap_Side : Gtk.Enums.Gtk_Position_Type;
       Xy0_Gap  : Gdouble;
       Xy1_Gap  : Gdouble)
   is
      procedure Internal
         (Context  : System.Address;
          Cr       : Cairo.Cairo_Context;
          X        : Gdouble;
          Y        : Gdouble;
          Width    : Gdouble;
          Height   : Gdouble;
          Gap_Side : Gtk.Enums.Gtk_Position_Type;
          Xy0_Gap  : Gdouble;
          Xy1_Gap  : Gdouble);
      pragma Import (C, Internal, "gtk_render_frame_gap");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height, Gap_Side, Xy0_Gap, Xy1_Gap);
   end Render_Frame_Gap;

   -------------------
   -- Render_Handle --
   -------------------

   procedure Render_Handle
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_handle");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Handle;

   -----------------
   -- Render_Icon --
   -----------------

   procedure Render_Icon
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Pixbuf  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
       X       : Gdouble;
       Y       : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          Pixbuf  : System.Address;
          X       : Gdouble;
          Y       : Gdouble);
      pragma Import (C, Internal, "gtk_render_icon");
   begin
      Internal (Get_Object (Context), Cr, Get_Object (Pixbuf), X, Y);
   end Render_Icon;

   -------------------------
   -- Render_Icon_Surface --
   -------------------------

   procedure Render_Icon_Surface
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       Surface : Cairo.Cairo_Surface;
       X       : Gdouble;
       Y       : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          Surface : Cairo.Cairo_Surface;
          X       : Gdouble;
          Y       : Gdouble);
      pragma Import (C, Internal, "gtk_render_icon_surface");
   begin
      Internal (Get_Object (Context), Cr, Surface, X, Y);
   end Render_Icon_Surface;

   -------------------
   -- Render_Layout --
   -------------------

   procedure Render_Layout
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Layout  : not null access Pango.Layout.Pango_Layout_Record'Class)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Layout  : System.Address);
      pragma Import (C, Internal, "gtk_render_layout");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Get_Object (Layout));
   end Render_Layout;

   -----------------
   -- Render_Line --
   -----------------

   procedure Render_Line
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X0      : Gdouble;
       Y0      : Gdouble;
       X1      : Gdouble;
       Y1      : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X0      : Gdouble;
          Y0      : Gdouble;
          X1      : Gdouble;
          Y1      : Gdouble);
      pragma Import (C, Internal, "gtk_render_line");
   begin
      Internal (Get_Object (Context), Cr, X0, Y0, X1, Y1);
   end Render_Line;

   -------------------
   -- Render_Option --
   -------------------

   procedure Render_Option
      (Context : not null access Gtk_Style_Context_Record'Class;
       Cr      : Cairo.Cairo_Context;
       X       : Gdouble;
       Y       : Gdouble;
       Width   : Gdouble;
       Height  : Gdouble)
   is
      procedure Internal
         (Context : System.Address;
          Cr      : Cairo.Cairo_Context;
          X       : Gdouble;
          Y       : Gdouble;
          Width   : Gdouble;
          Height  : Gdouble);
      pragma Import (C, Internal, "gtk_render_option");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height);
   end Render_Option;

   -------------------
   -- Render_Slider --
   -------------------

   procedure Render_Slider
      (Context     : not null access Gtk_Style_Context_Record'Class;
       Cr          : Cairo.Cairo_Context;
       X           : Gdouble;
       Y           : Gdouble;
       Width       : Gdouble;
       Height      : Gdouble;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Context     : System.Address;
          Cr          : Cairo.Cairo_Context;
          X           : Gdouble;
          Y           : Gdouble;
          Width       : Gdouble;
          Height      : Gdouble;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_render_slider");
   begin
      Internal (Get_Object (Context), Cr, X, Y, Width, Height, Orientation);
   end Render_Slider;

   -------------------
   -- Reset_Widgets --
   -------------------

   procedure Reset_Widgets
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Screen : System.Address);
      pragma Import (C, Internal, "gtk_style_context_reset_widgets");
   begin
      Internal (Get_Object (Screen));
   end Reset_Widgets;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Style_Context_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Style_Context_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Style_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Style_Context_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Style_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Style_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Style_Context_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Style_Context_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Style_Context_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Style_Context_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Style_Context_Record'Class;
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

   ----------------------------------
   -- Marsh_Gtk_Style_Context_Void --
   ----------------------------------

   procedure Marsh_Gtk_Style_Context_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Style_Context_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Style_Context := Gtk_Style_Context (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Style_Context_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Style_Context_Record;
       Call  : Cb_Gtk_Style_Context_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Style_Context_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gtk.Style_Context;
