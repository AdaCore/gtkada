-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
with Gdk; use Gdk;

package body Gtk.Scale is

   ----------------
   -- Draw_Value --
   ----------------

   procedure Draw_Value (Scale : access Gtk_Scale_Record)
   is
      procedure Internal (Scale : in System.Address);
      pragma Import (C, Internal, "gtk_scale_draw_value");
   begin
      Internal (Get_Object (Scale));
   end Draw_Value;

   ---------------------
   -- Get_Value_Width --
   ---------------------

   function Get_Value_Width (Scale  : access Gtk_Scale_Record) return Gint is
      function Internal (Scale  : in System.Address)
                         return      Gint;
      pragma Import (C, Internal, "gtk_scale_get_value_width");
   begin
      return Internal (Get_Object (Scale));
   end Get_Value_Width;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Hscale
     (Widget     : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
   begin
      Widget := new Gtk_Scale_Record;
      Initialize_Hscale (Widget, Adjustment);
   end Gtk_New_Hscale;

   --------------------
   -- Gtk_New_Hscale --
   --------------------

   procedure Gtk_New_Vscale
     (Widget     : out Gtk_Scale;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
   begin
      Widget := new Gtk_Scale_Record;
      Initialize_Vscale (Widget, Adjustment);
   end Gtk_New_Vscale;

   -----------------------
   -- Initialize_Hscale --
   -----------------------

   procedure Initialize_Hscale
     (Widget     : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_hscale_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Widget);
   end Initialize_Hscale;

   -----------------------
   -- Initialize_Vscale --
   -----------------------

   procedure Initialize_Vscale
     (Widget     : access Gtk_Scale_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_vscale_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Widget);
   end Initialize_Vscale;

   ----------------
   -- Set_Digits --
   ----------------

   procedure Set_Digits
      (Scale      : access Gtk_Scale_Record;
       The_Digits : in Gint)
   is
      procedure Internal
         (Scale      : in System.Address;
          The_Digits : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_digits");
   begin
      Internal (Get_Object (Scale),
                The_Digits);
   end Set_Digits;

   --------------------
   -- Set_Draw_Value --
   --------------------

   procedure Set_Draw_Value
      (Scale      : access Gtk_Scale_Record;
       Draw_Value : in Boolean)
   is
      procedure Internal
         (Scale      : in System.Address;
          Draw_Value : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_draw_value");
   begin
      Internal (Get_Object (Scale),
                Boolean'Pos (Draw_Value));
   end Set_Draw_Value;

   -------------------
   -- Set_Value_Pos --
   -------------------

   procedure Set_Value_Pos
      (Scale : access Gtk_Scale_Record;
       Pos   : in Gtk_Position_Type)
   is
      procedure Internal
         (Scale : in System.Address;
          Pos   : in Gint);
      pragma Import (C, Internal, "gtk_scale_set_value_pos");
   begin
      Internal (Get_Object (Scale),
                Gtk_Position_Type'Pos (Pos));
   end Set_Value_Pos;

end Gtk.Scale;
