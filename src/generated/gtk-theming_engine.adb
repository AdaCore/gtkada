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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Theming_Engine is

   package Type_Conversion_Gtk_Theming_Engine is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Theming_Engine_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Theming_Engine);

   --------------------------
   -- Get_Background_Color --
   --------------------------

   procedure Get_Background_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_theming_engine_get_background_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Background_Color;

   ----------------
   -- Get_Border --
   ----------------

   procedure Get_Border
      (Self   : not null access Gtk_Theming_Engine_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Border : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self   : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Border : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_theming_engine_get_border");
      Tmp_Border : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Border);
      Border := Tmp_Border;
   end Get_Border;

   ----------------------
   -- Get_Border_Color --
   ----------------------

   procedure Get_Border_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_theming_engine_get_border_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Border_Color;

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_theming_engine_get_color");
   begin
      Internal (Get_Object (Self), State, Color);
   end Get_Color;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_Text_Direction
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Text_Direction;
      pragma Import (C, Internal, "gtk_theming_engine_get_direction");
   begin
      return Internal (Get_Object (Self));
   end Get_Direction;

   --------------
   -- Get_Font --
   --------------

   function Get_Font
      (Self  : not null access Gtk_Theming_Engine_Record;
       State : Gtk.Enums.Gtk_State_Flags)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Enums.Gtk_State_Flags)
          return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "gtk_theming_engine_get_font");
   begin
      return Internal (Get_Object (Self), State);
   end Get_Font;

   ------------------------
   -- Get_Junction_Sides --
   ------------------------

   function Get_Junction_Sides
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_Junction_Sides
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Junction_Sides;
      pragma Import (C, Internal, "gtk_theming_engine_get_junction_sides");
   begin
      return Internal (Get_Object (Self));
   end Get_Junction_Sides;

   ----------------
   -- Get_Margin --
   ----------------

   procedure Get_Margin
      (Self   : not null access Gtk_Theming_Engine_Record;
       State  : Gtk.Enums.Gtk_State_Flags;
       Margin : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self   : System.Address;
          State  : Gtk.Enums.Gtk_State_Flags;
          Margin : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_theming_engine_get_margin");
      Tmp_Margin : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Margin);
      Margin := Tmp_Margin;
   end Get_Margin;

   -----------------
   -- Get_Padding --
   -----------------

   procedure Get_Padding
      (Self    : not null access Gtk_Theming_Engine_Record;
       State   : Gtk.Enums.Gtk_State_Flags;
       Padding : out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (Self    : System.Address;
          State   : Gtk.Enums.Gtk_State_Flags;
          Padding : out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_theming_engine_get_padding");
      Tmp_Padding : aliased Gtk.Style.Gtk_Border;
   begin
      Internal (Get_Object (Self), State, Tmp_Padding);
      Padding := Tmp_Padding;
   end Get_Padding;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Widget.Gtk_Widget_Path
   is
      function Internal
         (Self : System.Address) return access Gtk.Widget.Gtk_Widget_Path;
      pragma Import (C, Internal, "gtk_theming_engine_get_path");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Path;

   ------------------
   -- Get_Property --
   ------------------

   procedure Get_Property
      (Self     : not null access Gtk_Theming_Engine_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          State    : Gtk.Enums.Gtk_State_Flags;
          Value    : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_theming_engine_get_property");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
   begin
      Internal (Get_Object (Self), Tmp_Property, State, Value);
      Free (Tmp_Property);
   end Get_Property;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_theming_engine_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Screen));
   end Get_Screen;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
      (Self : not null access Gtk_Theming_Engine_Record)
       return Gtk.Enums.Gtk_State_Flags
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_State_Flags;
      pragma Import (C, Internal, "gtk_theming_engine_get_state");
   begin
      return Internal (Get_Object (Self));
   end Get_State;

   ------------------------
   -- Get_Style_Property --
   ------------------------

   procedure Get_Style_Property
      (Self          : not null access Gtk_Theming_Engine_Record;
       Property_Name : UTF8_String;
       Value         : out Glib.Values.GValue)
   is
      procedure Internal
         (Self          : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr;
          Value         : out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_theming_engine_get_style_property");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
   begin
      Internal (Get_Object (Self), Tmp_Property_Name, Value);
      Free (Tmp_Property_Name);
   end Get_Style_Property;

   ---------------
   -- Has_Class --
   ---------------

   function Has_Class
      (Self        : not null access Gtk_Theming_Engine_Record;
       Style_Class : UTF8_String) return Boolean
   is
      function Internal
         (Self        : System.Address;
          Style_Class : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_theming_engine_has_class");
      Tmp_Style_Class : Gtkada.Types.Chars_Ptr := New_String (Style_Class);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Style_Class);
      Free (Tmp_Style_Class);
      return Tmp_Return /= 0;
   end Has_Class;

   ----------------
   -- Has_Region --
   ----------------

   function Has_Region
      (Self         : not null access Gtk_Theming_Engine_Record;
       Style_Region : UTF8_String;
       Flags        : access Gtk.Enums.Gtk_Region_Flags) return Boolean
   is
      function Internal
         (Self         : System.Address;
          Style_Region : Gtkada.Types.Chars_Ptr;
          Acc_Flags    : access Gtk.Enums.Gtk_Region_Flags)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_theming_engine_has_region");
      Acc_Flags        : aliased Gtk.Enums.Gtk_Region_Flags;
      Tmp_Style_Region : Gtkada.Types.Chars_Ptr := New_String (Style_Region);
      Tmp_Return       : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Style_Region, Acc_Flags'Access);
      Free (Tmp_Style_Region);
      if Flags /= null then
         Flags.all := Acc_Flags;
      end if;
      return Tmp_Return /= 0;
   end Has_Region;

   ------------------
   -- Lookup_Color --
   ------------------

   function Lookup_Color
      (Self       : not null access Gtk_Theming_Engine_Record;
       Color_Name : UTF8_String;
       Color      : access Gdk.RGBA.Gdk_RGBA) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Color_Name : Gtkada.Types.Chars_Ptr;
          Acc_Color  : access Gdk.RGBA.Gdk_RGBA) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_theming_engine_lookup_color");
      Acc_Color      : aliased Gdk.RGBA.Gdk_RGBA;
      Tmp_Color_Name : Gtkada.Types.Chars_Ptr := New_String (Color_Name);
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Color_Name, Acc_Color'Access);
      Free (Tmp_Color_Name);
      Color.all := Acc_Color;
      return Tmp_Return /= 0;
   end Lookup_Color;

   ----------------------
   -- State_Is_Running --
   ----------------------

   function State_Is_Running
      (Self     : not null access Gtk_Theming_Engine_Record;
       State    : Gtk.Enums.Gtk_State_Type;
       Progress : access Gdouble) return Boolean
   is
      function Internal
         (Self         : System.Address;
          State        : Gtk.Enums.Gtk_State_Type;
          Acc_Progress : access Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_theming_engine_state_is_running");
      Acc_Progress : aliased Gdouble;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), State, Acc_Progress'Access);
      Progress.all := Acc_Progress;
      return Tmp_Return /= 0;
   end State_Is_Running;

   ----------
   -- Load --
   ----------

   function Load (Name : UTF8_String) return Gtk_Theming_Engine is
      function Internal
         (Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_theming_engine_load");
      Tmp_Name                : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_Gtk_Theming_Engine : Gtk_Theming_Engine_Record;
      Tmp_Return              : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Name);
      Free (Tmp_Name);
      return Gtk.Theming_Engine.Gtk_Theming_Engine (Get_User_Data (Tmp_Return, Stub_Gtk_Theming_Engine));
   end Load;

end Gtk.Theming_Engine;
