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

with System;

package body Gtk.Color_Selection is

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Color_Array)
   is
      procedure Internal (Colorsel : System.Address; Color : out Color_Array);
      pragma Import (C, Internal, "gtk_color_selection_get_color");

   begin
      Color (Opacity) := 0.0;
      Internal (Get_Object (Colorsel), Color);
   end Get_Color;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Selection) is
   begin
      Widget := new Gtk_Color_Selection_Record;
      Gtk.Color_Selection.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Color_Selection_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_selection_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ------------------
   -- Is_Adjusting --
   ------------------

   function Is_Adjusting
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean
   is
      function Internal (Colorsel : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_selection_is_adjusting");

   begin
      return To_Boolean (Internal (Get_Object (Colorsel)));
   end Is_Adjusting;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Color_Array)
   is
      procedure Internal (Colorsel : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_color_selection_set_color");

   begin
      Internal (Get_Object (Colorsel), Color'Address);
   end Set_Color;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (Colorsel : access Gtk_Color_Selection_Record;
      Policy   : Enums.Gtk_Update_Type)
   is
      procedure Internal
        (Colorsel : System.Address; Policy : Enums.Gtk_Update_Type);
      pragma Import (C, Internal, "gtk_color_selection_set_update_policy");

   begin
      Internal (Get_Object (Colorsel), Policy);
   end Set_Update_Policy;

   -----------------------------
   -- Get_Has_Opacity_Control --
   -----------------------------

   function Get_Has_Opacity_Control
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean
   is
      function Internal (Colorsel : System.Address) return Gboolean;
      pragma Import
        (C, Internal, "gtk_color_selection_get_has_opacity_control");
   begin
      return To_Boolean (Internal (Get_Object (Colorsel)));
   end Get_Has_Opacity_Control;

   -----------------------------
   -- Set_Has_Opacity_Control --
   -----------------------------

   procedure Set_Has_Opacity_Control
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Opacity : Boolean)
   is
      procedure Internal
        (Colorsel    : System.Address;
         Has_Opacity : Gboolean);
      pragma Import
        (C, Internal, "gtk_color_selection_set_has_opacity_control");
   begin
      Internal (Get_Object (Colorsel), To_Gboolean (Has_Opacity));
   end Set_Has_Opacity_Control;

   ---------------------
   -- Get_Has_Palette --
   ---------------------

   function Get_Has_Palette
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean
   is
      function Internal (Colorsel : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_selection_get_has_palette");
   begin
      return To_Boolean (Internal (Get_Object (Colorsel)));
   end Get_Has_Palette;

   ---------------------
   -- Set_Has_Palette --
   ---------------------

   procedure Set_Has_Palette
     (Colorsel    : access Gtk_Color_Selection_Record;
      Has_Palette : Boolean)
   is
      procedure Internal
        (Colorsel    : System.Address;
         Has_Palette : Gboolean);
      pragma Import (C, Internal, "gtk_color_selection_set_has_palette");
   begin
      Internal (Get_Object (Colorsel), To_Gboolean (Has_Palette));
   end Set_Has_Palette;

   -----------------------
   -- Set_Current_Color --
   -----------------------

   procedure Set_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Colorsel : System.Address;
         Color    : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_color_selection_set_current_color");
   begin
      Internal (Get_Object (Colorsel), Color);
   end Set_Current_Color;

   -----------------------
   -- Set_Current_Alpha --
   -----------------------

   procedure Set_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16)
   is
      procedure Internal
        (Colorsel : System.Address;
         Alpha    : Guint16);
      pragma Import (C, Internal, "gtk_color_selection_set_current_alpha");
   begin
      Internal (Get_Object (Colorsel), Alpha);
   end Set_Current_Alpha;

   -----------------------
   -- Get_Current_Color --
   -----------------------

   procedure Get_Current_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Colorsel : System.Address;
         Color    : System.Address);
      pragma Import (C, Internal, "gtk_color_selection_get_current_color");
      Col : aliased Gdk.Color.Gdk_Color;
   begin
      Internal (Get_Object (Colorsel), Col'Address);
      Color := Col;
   end Get_Current_Color;

   -----------------------
   -- Get_Current_Alpha --
   -----------------------

   function Get_Current_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16
   is
      function Internal (Colorsel : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_color_selection_get_current_alpha");
   begin
      return Internal (Get_Object (Colorsel));
   end Get_Current_Alpha;

   ------------------------
   -- Set_Previous_Color --
   ------------------------

   procedure Set_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Colorsel : System.Address;
         Color    : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gtk_color_selection_set_previous_color");
   begin
      Internal (Get_Object (Colorsel), Color);
   end Set_Previous_Color;

   ------------------------
   -- Set_Previous_Alpha --
   ------------------------

   procedure Set_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record;
      Alpha    : Guint16)
   is
      procedure Internal
        (Colorsel : System.Address;
         Alpha    : Guint16);
      pragma Import (C, Internal, "gtk_color_selection_set_previous_alpha");
   begin
      Internal (Get_Object (Colorsel), Alpha);
   end Set_Previous_Alpha;

   ------------------------
   -- Get_Previous_Color --
   ------------------------

   procedure Get_Previous_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Colorsel : System.Address;
         Color    : System.Address);
      pragma Import (C, Internal, "gtk_color_selection_get_previous_color");
      Col : aliased Gdk.Color.Gdk_Color;
   begin
      Internal (Get_Object (Colorsel), Col'Address);
      Color := Col;
   end Get_Previous_Color;

   ------------------------
   -- Get_Previous_Alpha --
   ------------------------

   function Get_Previous_Alpha
     (Colorsel : access Gtk_Color_Selection_Record) return Guint16
   is
      function Internal (Colorsel : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_color_selection_get_previous_alpha");
   begin
      return Internal (Get_Object (Colorsel));
   end Get_Previous_Alpha;

   -----------------
   -- To_Absolute --
   -----------------

   function To_Absolute (Color : Gdouble) return Gushort is
   begin
      return Gushort (Gdouble (Gushort'Last) * Color);
   end To_Absolute;

   ----------------
   -- To_Percent --
   ----------------

   function To_Percent (Color : Gushort) return Gdouble is
   begin
      return Gdouble (Color) / Gdouble (Gushort'Last);
   end To_Percent;

end Gtk.Color_Selection;
