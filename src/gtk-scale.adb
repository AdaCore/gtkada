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

package body Gtk.Scale is

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
     (Scale      : out Gtk_Scale;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Hscale (Scale, Adjustment);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
     (Scale : out Gtk_Scale;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Hscale (Scale, Min, Max, Step);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
     (Scale      : out Gtk_Scale;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Vscale (Scale, Adjustment);
   end Gtk_New_Vscale;

   --------------------
   -- Gtk_New_Vscale --
   --------------------

   procedure Gtk_New_Vscale
     (Scale : out Gtk_Scale;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble) is
   begin
      Scale := new Gtk_Scale_Record;
      Initialize_Vscale (Scale, Min, Max, Step);
   end Gtk_New_Vscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
     (Scale      : access Gtk_Scale_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Set_Object (Scale, Internal (Adj));
      Initialize_User_Data (Scale);
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
     (Scale : access Gtk_Scale_Record'Class;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble)
   is
      function Internal
        (Min  : Gdouble; Max  : Gdouble; Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_hscale_new_with_range");
   begin
      Set_Object (Scale, Internal (Min, Max, Step));
      Initialize_User_Data (Scale);
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
     (Scale      : access Gtk_Scale_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Set_Object (Scale, Internal (Adj));
      Initialize_User_Data (Scale);
   end Initialize_Vscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
     (Scale : access Gtk_Scale_Record'Class;
      Min   : Gdouble;
      Max   : Gdouble;
      Step  : Gdouble)
   is
      function Internal
        (Min  : Gdouble; Max  : Gdouble; Step : Gdouble) return System.Address;
      pragma Import (C, Internal, "gtk_vscale_new_with_range");
   begin
      Set_Object (Scale, Internal (Min, Max, Step));
      Initialize_User_Data (Scale);
   end Initialize_Vscale;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits (Scale : access Gtk_Scale_Record; The_Digits : Gint) is
      procedure Internal (Scale : System.Address; The_Digits : Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");

   begin
      Internal (Get_Object (Scale), The_Digits);
   end Set_Digits;

   ----------------
   -- Get_Digits --
   ----------------

   function Get_Digits (Scale : access Gtk_Scale_Record) return Gint
   is
      function Internal (Scale : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_scale_get_digits");
   begin
      return Internal (Get_Object (Scale));
   end Get_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
     (Scale : access Gtk_Scale_Record; Draw_Value : Boolean)
   is
      procedure Internal (Scale : System.Address; Draw_Value : Gint);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");

   begin
      Internal (Get_Object (Scale), Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   --------------------
   -- Get_Draw_Value --
   --------------------

   function Get_Draw_Value (Scale : access Gtk_Scale_Record) return Boolean
   is
      function Internal (Scale : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_scale_get_draw_value");
   begin
      return To_Boolean (Internal (Get_Object (Scale)));
   end Get_Draw_Value;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
     (Scale : access Gtk_Scale_Record; Pos : Gtk_Position_Type)
   is
      procedure Internal (Scale : System.Address; Pos : Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");

   begin
      Internal (Get_Object (Scale), Pos);
   end Set_Value_Pos;

   -------------------
   -- Get_Value_Pos --
   -------------------

   function Get_Value_Pos
     (Scale : access Gtk_Scale_Record) return Gtk_Position_Type
   is
      function Internal (Scale : System.Address) return Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_scale_get_value_pos");
   begin
      return Internal (Get_Object (Scale));
   end Get_Value_Pos;

end Gtk.Scale;
