-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

package body Gtk.GRange is

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (The_Range : access Gtk_Range_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (The_Range : System.Address) return System.Address;
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
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (The_Range  : System.Address;
         Adjustment : System.Address);
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
      Policy    : Gtk_Update_Type)
   is
      procedure Internal
        (The_Range : System.Address;
         Policy    : Gtk_Update_Type);
      pragma Import (C, Internal, "gtk_range_set_update_policy");

   begin
      Internal (Get_Object (The_Range), Policy);
   end Set_Update_Policy;

   -----------------------
   -- Get_Update_Policy --
   -----------------------

   function Get_Update_Policy
     (The_Range : access Gtk_Range_Record) return Gtk_Update_Type
   is
      function Internal (The_Range : System.Address) return Gtk_Update_Type;
      pragma Import (C, Internal, "gtk_range_get_update_policy");

   begin
      return Internal (Get_Object (The_Range));
   end Get_Update_Policy;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted
     (The_Range : access Gtk_Range_Record;
      Setting   : Boolean := True)
   is
      procedure Internal
        (The_Range : System.Address;
         Setting   : Gboolean);
      pragma Import (C, Internal, "gtk_range_set_inverted");

   begin
      Internal (Get_Object (The_Range), Boolean'Pos (Setting));
   end Set_Inverted;

   ------------------
   -- Get_Inverted --
   ------------------

   function Get_Inverted
     (The_Range : access Gtk_Range_Record) return Boolean
   is
      function Internal (The_Range : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_range_get_inverted");

   begin
      return Boolean'Val (Internal (Get_Object (The_Range)));
   end Get_Inverted;

   --------------------
   -- Set_Increments --
   --------------------

   procedure Set_Increments
     (The_Range : access Gtk_Range_Record;
      Step      : Gdouble;
      Page      : Gdouble)
   is
      procedure Internal
        (The_Range : System.Address;
         Step      : Gdouble;
         Page      : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_increments");
   begin
      Internal (Get_Object (The_Range), Step, Page);
   end Set_Increments;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (The_Range : access Gtk_Range_Record;
      Min       : Gdouble;
      Max       : Gdouble)
   is
      procedure Internal
        (The_Range : System.Address;
         Min       : Gdouble;
         Max       : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_range");
   begin
      Internal (Get_Object (The_Range), Min, Max);
   end Set_Range;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (The_Range : access Gtk_Range_Record;
      Value     : Gdouble)
   is
      procedure Internal (The_Range : System.Address; Value : Gdouble);
      pragma Import (C, Internal, "gtk_range_set_value");
   begin
      Internal (Get_Object (The_Range), Value);
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (The_Range : access Gtk_Range_Record) return Gdouble
   is
      function Internal (The_Range : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_range_get_value");
   begin
      return Internal (Get_Object (The_Range));
   end Get_Value;

end Gtk.GRange;
