-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gtk.Dial is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget     : out Gtk_Dial;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Widget := new Gtk_Dial_Record;
      Initialize (Widget, Adjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget     : access Gtk_Dial_Record'Class;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal (Adjustment : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_dial_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
   end Initialize;

   --------------------
   -- Get_Adjustment --
   --------------------

   function Get_Adjustment (Dial   : access Gtk_Dial_Record)
                            return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Dial : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_dial_get_adjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Dial)), Stub));
   end Get_Adjustment;

   --------------------
   -- Get_Percentage --
   --------------------

   function Get_Percentage (Dial   : access Gtk_Dial_Record)
                            return Gfloat
   is
      function Internal (Dial   : System.Address)
                         return Gfloat;
      pragma Import (C, Internal, "gtk_dial_get_percentage");
   begin
      return Internal (Get_Object (Dial));
   end Get_Percentage;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Dial   : access Gtk_Dial_Record)
                       return Gfloat
   is
      function Internal (Dial   : System.Address)
                         return Gfloat;
      pragma Import (C, Internal, "gtk_dial_get_value");
   begin
      return Internal (Get_Object (Dial));
   end Get_Value;

   --------------------
   -- Set_Adjustment --
   --------------------

   procedure Set_Adjustment
     (Dial       : access Gtk_Dial_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Dial       : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_dial_set_adjustment");
   begin
      Internal (Get_Object (Dial),
                Get_Object (Adjustment));
   end Set_Adjustment;

   --------------------
   -- Set_Percentage --
   --------------------

   function Set_Percentage
     (Dial    : access Gtk_Dial_Record;
      Percent : Gfloat)
      return Gfloat
   is
      function Internal
        (Dial    : System.Address;
         Percent : Gfloat)
         return Gfloat;
      pragma Import (C, Internal, "gtk_dial_set_percentage");
   begin
      return Internal (Get_Object (Dial),
                       Percent);
   end Set_Percentage;

   -----------------------
   -- Set_Update_Policy --
   -----------------------

   procedure Set_Update_Policy
     (Dial   : access Gtk_Dial_Record;
      Policy : Gtk_Update_Type)
   is
      procedure Internal
        (Dial   : System.Address;
         Policy : Gint);
      pragma Import (C, Internal, "gtk_dial_set_update_policy");
   begin
      Internal (Get_Object (Dial),
                Gtk_Update_Type'Pos (Policy));
   end Set_Update_Policy;

   ---------------
   -- Set_Value --
   ---------------

   function Set_Value
     (Dial   : access Gtk_Dial_Record;
      Value  : Gfloat)
      return Gfloat
   is
      function Internal
        (Dial   : System.Address;
         Value  : Gfloat)
         return Gfloat;
      pragma Import (C, Internal, "gtk_dial_set_value");
   begin
      return Internal (Get_Object (Dial),
                       Value);
   end Set_Value;

   -------------------
   -- Set_View_Only --
   -------------------

   procedure Set_View_Only
     (Dial      : access Gtk_Dial_Record;
      View_Only : Boolean)
   is
      procedure Internal
        (Dial      : System.Address;
         View_Only : Gint);
      pragma Import (C, Internal, "gtk_dial_set_view_only");
   begin
      Internal (Get_Object (Dial),
                Boolean'Pos (View_Only));
   end Set_View_Only;

end Gtk.Dial;
