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

   -------------------
   -- Get_Old_Color --
   -------------------

   procedure Get_Old_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : out Color_Array)
   is
      procedure Internal (Colorsel : System.Address; Color : out Color_Array);
      pragma Import (C, Internal, "gtk_color_selection_get_old_color");

   begin
      Color (Opacity) := 0.0;
      Internal (Get_Object (Colorsel), Color);
   end Get_Old_Color;

   -----------------------
   -- Get_Palette_Color --
   -----------------------

   procedure Get_Palette_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      X        : Gint;
      Y        : Gint;
      Color    : out Color_Array;
      Status   : out Boolean)
   is
      function Internal
        (Colorsel : System.Address;
         X        : Gint;
         Y        : Gint;
         Color    : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_selection_get_palette_color");

      Tmp : aliased Color_Array;

   begin
      Tmp (Opacity) := 0.0;
      Status := To_Boolean
        (Internal (Get_Object (Colorsel), X, Y, Tmp'Address));

      if Status then
         Color := Tmp;
      end if;
   end Get_Palette_Color;

   ---------------------
   -- Get_Use_Opacity --
   ---------------------

   function Get_Use_Opacity
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean
   is
      function Internal (Colorsel : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_selection_get_use_opacity");

   begin
      return To_Boolean (Internal (Get_Object (Colorsel)));
   end Get_Use_Opacity;

   ---------------------
   -- Get_Use_Palette --
   ---------------------

   function Get_Use_Palette
     (Colorsel : access Gtk_Color_Selection_Record) return Boolean
   is
      function Internal (Colorsel : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_color_selection_get_use_palette");

   begin
      return To_Boolean (Internal (Get_Object (Colorsel)));
   end Get_Use_Palette;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Selection) is
   begin
      Widget := new Gtk_Color_Selection_Record;
      Initialize (Widget);
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

   -------------------
   -- Set_Old_Color --
   -------------------

   procedure Set_Old_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      Color    : Color_Array)
   is
      procedure Internal (Colorsel : System.Address; Color : System.Address);
      pragma Import (C, Internal, "gtk_color_selection_set_old_color");

   begin
      Internal (Get_Object (Colorsel), Color'Address);
   end Set_Old_Color;

   -----------------------
   -- Set_Palette_Color --
   -----------------------

   procedure Set_Palette_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      X        : Gint;
      Y        : Gint;
      Color    : Color_Array)
   is
      procedure Internal
        (Colorsel : System.Address;
         X        : Gint;
         Y        : Gint;
         Color    : System.Address);
      pragma Import (C, Internal, "gtk_color_selection_set_palette_color");

   begin
      Internal (Get_Object (Colorsel), X, Y, Color'Address);
   end Set_Palette_Color;

   ---------------------
   -- Set_Use_Opacity --
   ---------------------

   procedure Set_Use_Opacity
     (Colorsel    : access Gtk_Color_Selection_Record;
      Use_Opacity : Boolean)
   is
      procedure Internal (Colorsel : System.Address; Use_Opacity : Gboolean);
      pragma Import (C, Internal, "gtk_color_selection_set_use_opacity");

   begin
      Internal (Get_Object (Colorsel), To_Gboolean (Use_Opacity));
   end Set_Use_Opacity;

   ---------------------
   -- Set_Use_Palette --
   ---------------------

   procedure Set_Use_Palette
     (Colorsel    : access Gtk_Color_Selection_Record;
      Use_Palette : Boolean)
   is
      procedure Internal (Colorsel : System.Address; Use_Opacity : Gboolean);
      pragma Import (C, Internal, "gtk_color_selection_set_use_palette");

   begin
      Internal (Get_Object (Colorsel), To_Gboolean (Use_Palette));
   end Set_Use_Palette;

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

   -------------------------
   -- Unset_Palette_Color --
   -------------------------

   procedure Unset_Palette_Color
     (Colorsel : access Gtk_Color_Selection_Record;
      X        : Gint;
      Y        : Gint)
   is
      procedure Internal
        (Colorsel : System.Address; X, Y : Gint);
      pragma Import (C, Internal, "gtk_color_selection_unset_palette_color");

   begin
      Internal (Get_Object (Colorsel), X, Y);
   end Unset_Palette_Color;

end Gtk.Color_Selection;
