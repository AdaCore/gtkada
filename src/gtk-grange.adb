-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gtk.GRange is

   ---------------------
   -- Default_Hmotion --
   ---------------------

   procedure Default_Hmotion
     (The_Range : access Gtk_Range_Record;
      Xdelta    : in Gint;
      Ydelta    : in Gint)
   is
      procedure Internal
        (The_Range : in System.Address;
         Xdelta    : in Gint;
         Ydelta    : in Gint);
      pragma Import (C, Internal, "gtk_range_default_hmotion");

   begin
      Internal (Get_Object (The_Range), Xdelta, Ydelta);
   end Default_Hmotion;

   ----------------------------
   -- Default_Hslider_Update --
   ----------------------------

   procedure Default_Hslider_Update (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_default_hslider_update");

   begin
      Internal (Get_Object (The_Range));
   end Default_Hslider_Update;

   ---------------------------
   -- Default_Htrough_Click --
   ---------------------------

   procedure Default_Htrough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    :    out Gint)
   is
      function Internal
        (The_Range : in System.Address;
         X         : in Gint;
         Y         : in Gint;
         Jump_Perc : in System.Address)
         return         Gint;
      pragma Import (C, Internal, "gtk_range_default_htrough_click");
      Jump : aliased Gfloat := Jump_Perc;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Result := Internal (Get_Object (The_Range), X, Y, Jump'Address);
      Jump_Perc := Jump;
   end Default_Htrough_Click;

   ---------------------
   -- Default_Vmotion --
   ---------------------

   procedure Default_Vmotion
     (The_Range : access Gtk_Range_Record;
      Xdelta    : in Gint;
      Ydelta    : in Gint)
   is
      procedure Internal
        (The_Range : in System.Address;
         Xdelta    : in Gint;
         Ydelta    : in Gint);
      pragma Import (C, Internal, "gtk_range_default_vmotion");

   begin
      Internal (Get_Object (The_Range), Xdelta, Ydelta);
   end Default_Vmotion;

   ----------------------------
   -- Default_Vslider_Update --
   ----------------------------

   procedure Default_Vslider_Update (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_default_vslider_update");

   begin
      Internal (Get_Object (The_Range));
   end Default_Vslider_Update;

   ---------------------------
   -- Default_Vtrough_Click --
   ---------------------------

   procedure Default_Vtrough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    :    out Gint)
   is
      function Internal
        (The_Range : in System.Address;
         X         : in Gint;
         Y         : in Gint;
         Jump_Perc : in System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_range_default_vtrough_click");

      Jump : aliased Gfloat := Jump_Perc;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Result := Internal (Get_Object (The_Range), X, Y, Jump'Address);
      Jump_Perc := Jump;
   end Default_Vtrough_Click;

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_draw_background");

   begin
      Internal (Get_Object (The_Range));
   end Draw_Background;

   -----------------
   -- Draw_Slider --
   -----------------

   procedure Draw_Slider (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_draw_slider");

   begin
      Internal (Get_Object (The_Range));
   end Draw_Slider;

   --------------------
   -- Draw_Step_Back --
   --------------------

   procedure Draw_Step_Back (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_draw_step_back");

   begin
      Internal (Get_Object (The_Range));
   end Draw_Step_Back;

   --------------------
   -- Draw_Step_Forw --
   --------------------

   procedure Draw_Step_Forw (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_draw_step_forw");

   begin
      Internal (Get_Object (The_Range));
   end Draw_Step_Forw;

   -----------------
   -- Draw_Trough --
   -----------------

   procedure Draw_Trough (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_draw_trough");

   begin
      Internal (Get_Object (The_Range));
   end Draw_Trough;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (The_Range  : access Gtk_Range_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (The_Range  : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_range_get_adjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (The_Range)), Stub));
   end Get_Adjustment;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (The_Range  : access Gtk_Range_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (The_Range  : in System.Address;
         Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_range_set_adjustment");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Internal (Get_Object (The_Range), Adj);
   end Set_Adjustment;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (The_Range : access Gtk_Range_Record;
      Policy    : in Gtk_Update_Type)
   is
      procedure Internal
        (The_Range : in System.Address;
         Policy    : in Gint);
      pragma Import (C, Internal, "gtk_range_set_update_policy");

   begin
      Internal (Get_Object (The_Range), Gtk_Update_Type'Pos (Policy));
   end Set_Update_Policy;

   -------------------
   -- Slider_Update --
   -------------------

   procedure Slider_Update (The_Range : access Gtk_Range_Record) is
      procedure Internal (The_Range : in System.Address);
      pragma Import (C, Internal, "gtk_range_slider_update");

   begin
      Internal (Get_Object (The_Range));
   end Slider_Update;

   ------------------
   -- Trough_Click --
   ------------------

   procedure Trough_Click
     (The_Range : access Gtk_Range_Record;
      X         : in Gint;
      Y         : in Gint;
      Jump_Perc : in out Gfloat;
      Result    :    out Gint)
   is
      function Internal
        (The_Range : in System.Address;
         X         : in Gint;
         Y         : in Gint;
         Jump_Perc : in System.Address)
         return Gint;
      pragma Import (C, Internal, "gtk_range_trough_click");

      Jump : aliased Gfloat := Jump_Perc;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.
   begin
      Result := Internal (Get_Object (The_Range), X, Y, Jump'Address);
      Jump_Perc := Jump;
   end Trough_Click;

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
   begin
      Widget.Generate (N, File);
      Gen_Set (N, "GRange", "Update_Policy", "policy", File => File);
   end Generate;

   procedure Generate
     (The_Range : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      S : String_Ptr;

   begin
      Widget.Generate (The_Range, N);

      S := Get_Field (N, "policy");

      if S /= null then
         Set_Update_Policy
           (Gtk_Range (The_Range),
            Gtk_Update_Type'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.GRange;
