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

package body Gtk.Adjustment is

   ----------------
   -- Clamp_Page --
   ----------------

   procedure Clamp_Page (Adjustment : access Gtk_Adjustment_Record;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat)
   is
      procedure Internal (Adjustment : in System.Address;
                          Lower      : in Gfloat;
                          Upper      : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_clamp_page");
   begin
      Internal (Get_Object (Adjustment), Lower, Upper);
   end Clamp_Page;

   ---------------
   -- Get_Lower --
   ---------------

   function Get_Lower (Adjustment : access Gtk_Adjustment_Record) return Gfloat
   is
      function Internal (Adj : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_adjustment_get_lower");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Lower;

   ---------------
   -- Get_Upper --
   ---------------

   function Get_Upper (Adjustment : access Gtk_Adjustment_Record) return Gfloat
   is
      function Internal (Adj : System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_adjustment_get_upper");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Upper;

   ------------------------
   -- Get_Step_Increment --
   ------------------------

   function Get_Step_Increment (Adjustment : access Gtk_Adjustment_Record)
                                return Gfloat
   is
      function Internal (Adj : in System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_adjustment_get_step_increment");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Step_Increment;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Adjustment : access Gtk_Adjustment_Record) return Gfloat
   is
      function Internal (Adjustment : in System.Address) return Gfloat;
      pragma Import (C, Internal, "ada_gtk_adjustment_get_value");
   begin
      return Internal (Get_Object (Adjustment));
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Adjustment     : in out Gtk_Adjustment;
                      Value          : in     Gfloat;
                      Lower          : in     Gfloat;
                      Upper          : in     Gfloat;
                      Step_Increment : in     Gfloat;
                      Page_Increment : in     Gfloat;
                      Page_Size      : in     Gfloat) is
   begin
      Adjustment := new Gtk_Adjustment_Record;
      Initialize (Adjustment, Value, Lower, Upper, Step_Increment,
                  Page_Increment, Page_Size);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Adjustment     : access Gtk_Adjustment_Record'Class;
                         Value          : in     Gfloat;
                         Lower          : in     Gfloat;
                         Upper          : in     Gfloat;
                         Step_Increment : in     Gfloat;
                         Page_Increment : in     Gfloat;
                         Page_Size      : in     Gfloat) is
      function Internal (Value          : in Gfloat;
                         Lower          : in Gfloat;
                         Upper          : in Gfloat;
                         Step_Increment : in Gfloat;
                         Page_Increment : in Gfloat;
                         Page_Size      : in Gfloat)
                         return System.Address;
      pragma Import (C, Internal, "gtk_adjustment_new");

   begin
      Set_Object (Adjustment,
                  Internal (Value, Lower, Upper, Step_Increment,
                            Page_Increment, Page_Size));
      Initialize_User_Data (Adjustment);
   end Initialize;

   ---------------
   -- Set_Lower --
   ---------------

   procedure Set_Lower (Adjustment : access Gtk_Adjustment_Record;
                        Lower : Gfloat)
   is
      procedure Internal (Adj : System.Address; Lower : Gfloat);
      pragma Import (C, Internal, "ada_gtk_adjustment_set_lower");
   begin
      Internal (Get_Object (Adjustment), Lower);
   end Set_Lower;

   ------------------------
   -- Set_Page_Increment --
   ------------------------

   procedure Set_Page_Increment (Adjustment : access Gtk_Adjustment_Record;
                                 Page_Increment : in Gfloat)
   is
      procedure Internal (Adj : System.Address; Value : Gfloat);
      pragma Import (C, Internal, "ada_adjustment_set_page_increment");
   begin
      Internal (Get_Object (Adjustment), Page_Increment);
   end Set_Page_Increment;

   -------------------
   -- Set_Page_Size --
   -------------------

   procedure Set_Page_Size (Adjustment : access Gtk_Adjustment_Record;
                            Page_Size  : in Gfloat)
   is
      procedure Internal (Adj : System.Address; Value : Gfloat);
      pragma Import (C, Internal, "ada_adjustment_set_page_size");
   begin
      Internal (Get_Object (Adjustment), Page_Size);
   end Set_Page_Size;

   ---------------
   -- Set_Upper --
   ---------------

   procedure Set_Upper (Adjustment : access Gtk_Adjustment_Record;
                        Upper : Gfloat)
   is
      procedure Internal (Adj : System.Address; Upper : Gfloat);
      pragma Import (C, Internal, "ada_gtk_adjustment_set_upper");
   begin
      Internal (Get_Object (Adjustment), Upper);
   end Set_Upper;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Adjustment : access Gtk_Adjustment_Record;
                        Value : Gfloat)
   is
      procedure Internal (Adjustment : in System.Address; Value : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_set_value");
   begin
      Internal (Get_Object (Adjustment), Value);
   end Set_Value;

end Gtk.Adjustment;
