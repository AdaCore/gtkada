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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

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
        (Get_User_Data (Internal (Get_Object (Widget)),
            Stub_Gtk_Style_Context));
   end Get_Style_Context;

   package Type_Conversion_Gtk_Style_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Style_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Style_Context);

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

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Self  : not null access Gtk_Style_Context_Record;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_style_context_get_color");
   begin
      Internal (Get_Object (Self), Color);
   end Get_Color;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Style_Context_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_context_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

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
      Color := Acc_Color;
      Free (Tmp_Color_Name);
      Found := Tmp_Return /= 0;
   end Lookup_Color;

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

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
      (Self    : not null access Gtk_Style_Context_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
   is
      procedure Internal (Self : System.Address; Display : System.Address);
      pragma Import (C, Internal, "gtk_style_context_set_display");
   begin
      Internal (Get_Object (Self), Get_Object (Display));
   end Set_Display;

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

   ------------------------------
   -- Add_Provider_For_Display --
   ------------------------------

   procedure Add_Provider_For_Display
      (Display  : not null access Gdk.Display.Gdk_Display_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider;
       Priority : Guint)
   is
      procedure Internal
         (Display  : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider;
          Priority : Guint);
      pragma Import (C, Internal, "gtk_style_context_add_provider_for_display");
   begin
      Internal (Get_Object (Display), Provider, Priority);
   end Add_Provider_For_Display;

   ---------------------------------
   -- Remove_Provider_For_Display --
   ---------------------------------

   procedure Remove_Provider_For_Display
      (Display  : not null access Gdk.Display.Gdk_Display_Record'Class;
       Provider : Gtk.Style_Provider.Gtk_Style_Provider)
   is
      procedure Internal
         (Display  : System.Address;
          Provider : Gtk.Style_Provider.Gtk_Style_Provider);
      pragma Import (C, Internal, "gtk_style_context_remove_provider_for_display");
   begin
      Internal (Get_Object (Display), Provider);
   end Remove_Provider_For_Display;

end Gtk.Style_Context;
