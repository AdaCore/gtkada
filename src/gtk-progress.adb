-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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

with Gdk; use Gdk;
with Gtk.Adjustment;
with Interfaces.C.Strings;
with System;

package body Gtk.Progress is

   -----------------------
   -- Get_Activity_Mode --
   -----------------------

   function Get_Activity_Mode (Progress : in Gtk_Progress) return Boolean is
      function Internal (Progress : System.Address) return Guint;
      pragma Import (C, Internal, "ada_progress_get_activity_mode");
   begin
      return Boolean'Val (Internal (Get_Object (Progress)));
   end Get_Activity_Mode;

   ----------------------------
   -- Get_Current_Percentage --
   ----------------------------

   function Get_Current_Percentage (Progress : in Gtk_Progress)
                                    return        Gfloat
   is
      function Internal (Progress : in System.Address)
                         return        Gfloat;
      pragma Import (C, Internal, "gtk_progress_get_current_percentage");
   begin
      return Internal (Get_Object (Progress));
   end Get_Current_Percentage;

   ----------------------
   -- Get_Current_Text --
   ----------------------

   function Get_Current_Text (Progress : in Gtk_Progress)
                              return        String
   is
      function Internal (Progress : in System.Address)
                         return        Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_progress_get_current_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Progress)));
   end Get_Current_Text;

   -------------------------------
   -- Get_Percentage_From_Value --
   -------------------------------

   function Get_Percentage_From_Value
      (Progress : in Gtk_Progress;
       Value    : in Gfloat)
       return        Gfloat
   is
      function Internal
         (Progress : in System.Address;
          Value    : in Gfloat)
          return        Gfloat;
      pragma Import (C, Internal, "gtk_progress_get_percentage_from_value");
   begin
      return Internal (Get_Object (Progress),
                       Value);
   end Get_Percentage_From_Value;

   -------------------------
   -- Get_Text_From_Value --
   -------------------------

   function Get_Text_From_Value
      (Progress : in Gtk_Progress;
       Value    : in Gfloat)
       return        String
   is
      function Internal
         (Progress : in System.Address;
          Value    : in Gfloat)
          return        Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_progress_get_text_from_value");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Progress),
                                                   Value));
   end Get_Text_From_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Progress : in Gtk_Progress)
                       return        Gfloat
   is
      function Internal (Progress : in System.Address)
                         return        Gfloat;
      pragma Import (C, Internal, "gtk_progress_get_value");
   begin
      return Internal (Get_Object (Progress));
   end Get_Value;

   -----------------
   -- Reconfigure --
   -----------------

   procedure Reconfigure
      (Progress : in Gtk_Progress;
       Value    : in Gfloat;
       Min      : in Gfloat;
       Max      : in Gfloat)
   is
      procedure Internal
         (Progress : in System.Address;
          Value    : in Gfloat;
          Min      : in Gfloat;
          Max      : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_reconfigure");
   begin
      Internal (Get_Object (Progress),
                Value,
                Min,
                Max);
   end Reconfigure;

   -----------------------
   -- Set_Activity_Mode --
   -----------------------

   procedure Set_Activity_Mode
      (Progress      : in Gtk_Progress;
       Activity_Mode : in Boolean)
   is
      procedure Internal
         (Progress      : in System.Address;
          Activity_Mode : in Guint);
      pragma Import (C, Internal, "gtk_progress_set_activity_mode");
   begin
      Internal (Get_Object (Progress),
                Boolean'Pos (Activity_Mode));
   end Set_Activity_Mode;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
      (Progress   : in Gtk_Progress;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
         (Progress   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_progress_set_adjustment");
   begin
      Internal (Get_Object (Progress),
                Get_Object (Adjustment));
   end Set_Adjustment;

   -----------------------
   -- Set_Format_String --
   -----------------------

   procedure Set_Format_String
      (Progress : in Gtk_Progress;
       Format   : in String)
   is
      procedure Internal
         (Progress : in System.Address;
          Format   : in String);
      pragma Import (C, Internal, "gtk_progress_set_format_string");
   begin
      Internal (Get_Object (Progress),
                Format & Ascii.NUL);
   end Set_Format_String;

   --------------------
   -- Set_Percentage --
   --------------------

   procedure Set_Percentage
      (Progress   : in Gtk_Progress;
       Percentage : in Gfloat)
   is
      procedure Internal
         (Progress   : in System.Address;
          Percentage : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_set_percentage");
   begin
      Internal (Get_Object (Progress),
                Percentage);
   end Set_Percentage;

   -------------------
   -- Set_Show_Text --
   -------------------

   procedure Set_Show_Text
      (Progress  : in Gtk_Progress;
       Show_Text : in Boolean)
   is
      procedure Internal
         (Progress  : in System.Address;
          Show_Text : in Integer);
      pragma Import (C, Internal, "gtk_progress_set_show_text");
   begin
      Internal (Get_Object (Progress),
                Boolean'Pos (Show_Text));
   end Set_Show_Text;

   ------------------------
   -- Set_Text_Alignment --
   ------------------------

   procedure Set_Text_Alignment
      (Progress : in Gtk_Progress;
       X_Align  : in Gfloat;
       Y_Align  : in Gfloat)
   is
      procedure Internal
         (Progress : in System.Address;
          X_Align  : in Gfloat;
          Y_Align  : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_set_text_alignment");
   begin
      Internal (Get_Object (Progress),
                X_Align,
                Y_Align);
   end Set_Text_Alignment;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
      (Progress : in Gtk_Progress;
       Value    : in Gfloat)
   is
      procedure Internal
         (Progress : in System.Address;
          Value    : in Gfloat);
      pragma Import (C, Internal, "gtk_progress_set_value");
   begin
      Internal (Get_Object (Progress),
                Value);
   end Set_Value;

end Gtk.Progress;
