-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                 Copyright (C) 2000-2006, AdaCore                  --
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

with Interfaces.C.Strings;
with System;

package body Gtk.Progress is

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Progress : access Gtk_Progress_Record;
      Value    : Gdouble;
      Min      : Gdouble;
      Max      : Gdouble)
   is
      procedure Internal
        (Progress : System.Address;
         Value    : Gdouble;
         Min      : Gdouble;
         Max      : Gdouble);
      pragma Import (C, Internal, "gtk_progress_configure");

   begin
      Internal (Get_Object (Progress), Value, Min, Max);
   end Configure;

   -----------------------
   -- Get_Activity_Mode --
   -----------------------

   function Get_Activity_Mode
     (Progress : access Gtk_Progress_Record) return Boolean
   is
      function Internal (Progress : System.Address) return Guint;
      pragma Import (C, Internal, "ada_progress_get_activity_mode");

   begin
      return Boolean'Val (Internal (Get_Object (Progress)));
   end Get_Activity_Mode;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (Widget : access Gtk_Progress_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_progress_get_adjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Widget)), Stub));
   end Get_Adjustment;

   ----------------------------
   -- Get_Current_Percentage --
   ----------------------------

   function Get_Current_Percentage
     (Progress : access Gtk_Progress_Record) return Gdouble
   is
      function Internal (Progress : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_get_current_percentage");

   begin
      return Internal (Get_Object (Progress));
   end Get_Current_Percentage;

   ----------------------
   -- Get_Current_Text --
   ----------------------

   function Get_Current_Text
     (Progress : access Gtk_Progress_Record) return UTF8_String
   is
      function Internal
        (Progress : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_progress_get_current_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Progress)));
   end Get_Current_Text;

   -------------------------------
   -- Get_Percentage_From_Value --
   -------------------------------

   function Get_Percentage_From_Value
     (Progress : access Gtk_Progress_Record;
      Value    : Gdouble) return Gdouble
   is
      function Internal
        (Progress : System.Address; Value : Gdouble) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_get_percentage_from_value");

   begin
      return Internal (Get_Object (Progress), Value);
   end Get_Percentage_From_Value;

   -------------------------
   -- Get_Text_From_Value --
   -------------------------

   function Get_Text_From_Value
     (Progress : access Gtk_Progress_Record;
      Value    : Gdouble) return UTF8_String
   is
      function Internal
        (Progress : System.Address;
         Value    : Gdouble) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_progress_get_text_from_value");

   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (Progress), Value));
   end Get_Text_From_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Progress : access Gtk_Progress_Record) return Gdouble is
      function Internal (Progress : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_progress_get_value");

   begin
      return Internal (Get_Object (Progress));
   end Get_Value;

   -----------------------
   -- Set_Activity_Mode --
   -----------------------

   procedure Set_Activity_Mode
     (Progress      : access Gtk_Progress_Record;
      Activity_Mode : Boolean)
   is
      procedure Internal
        (Progress      : System.Address;
         Activity_Mode : Guint);
      pragma Import (C, Internal, "gtk_progress_set_activity_mode");

   begin
      Internal (Get_Object (Progress), Boolean'Pos (Activity_Mode));
   end Set_Activity_Mode;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Progress   : access Gtk_Progress_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Progress   : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_progress_set_adjustment");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Internal (Get_Object (Progress), Adj);
   end Set_Adjustment;

   -----------------------
   -- Set_Format_String --
   -----------------------

   procedure Set_Format_String
     (Progress : access Gtk_Progress_Record;
      Format   : UTF8_String)
   is
      procedure Internal
        (Progress : System.Address;
         Format   : UTF8_String);
      pragma Import (C, Internal, "gtk_progress_set_format_string");

   begin
      Internal (Get_Object (Progress), Format & ASCII.NUL);
   end Set_Format_String;

   --------------------
   -- Set_Percentage --
   --------------------

   procedure Set_Percentage
     (Progress   : access Gtk_Progress_Record;
      Percentage : Gdouble)
   is
      procedure Internal
        (Progress   : System.Address;
         Percentage : Gdouble);
      pragma Import (C, Internal, "gtk_progress_set_percentage");

   begin
      Internal (Get_Object (Progress), Percentage);
   end Set_Percentage;

   -------------------
   -- Set_Show_Text --
   -------------------

   procedure Set_Show_Text
     (Progress  : access Gtk_Progress_Record;
      Show_Text : Boolean)
   is
      procedure Internal
        (Progress  : System.Address;
         Show_Text : Integer);
      pragma Import (C, Internal, "gtk_progress_set_show_text");

   begin
      Internal (Get_Object (Progress), Boolean'Pos (Show_Text));
   end Set_Show_Text;

   ------------------------
   -- Set_Text_Alignment --
   ------------------------

   procedure Set_Text_Alignment
     (Progress : access Gtk_Progress_Record;
      X_Align  : Gfloat;
      Y_Align  : Gfloat)
   is
      procedure Internal
        (Progress : System.Address;
         X_Align  : Gfloat;
         Y_Align  : Gfloat);
      pragma Import (C, Internal, "gtk_progress_set_text_alignment");

   begin
      Internal (Get_Object (Progress), X_Align, Y_Align);
   end Set_Text_Alignment;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Progress : access Gtk_Progress_Record;
      Value    : Gdouble)
   is
      procedure Internal
        (Progress : System.Address;
         Value    : Gdouble);
      pragma Import (C, Internal, "gtk_progress_set_value");

   begin
      Internal (Get_Object (Progress), Value);
   end Set_Value;

end Gtk.Progress;
