-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Adjustment is


   ------------------
   --  Clamp_Page  --
   ------------------

   procedure Clamp_Page (Adjustment : in out Gtk_Adjustment'Class;
                         Lower      : in     Gfloat;
                         Upper      : in     Gfloat) is

      procedure Internal (Adjustment : in System.Address;
                          Lower      : in Gfloat;
                          Upper      : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_clamp_page");

   begin
      Internal (Get_Object (Adjustment), Lower, Upper);
   end Clamp_Page;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Adjustment : out Gtk_Adjustment;
                      Value          : in Gfloat;
                      Lower          : in Gfloat;
                      Upper          : in Gfloat;
                      Step_Increment : in Gfloat;
                      Page_Increment : in Gfloat;
                      Page_Size      : in Gfloat) is

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
   end Gtk_New;


   ---------------------------
   --  Null_Gtk_Adjustment  --
   ---------------------------

   function Null_Adjustment return Gtk_Adjustment is
      Result : Gtk_Adjustment;
   begin
      Set_Object (Result, System.Null_Address);
      return Result;
   end Null_Adjustment;


   -----------------
   --  set_Value  --
   -----------------

   procedure Set_Value (Adjustment : in out Gtk_Adjustment'Class;
                        Value      : in     Gfloat) is

      procedure Internal (Adjustment : in System.Address;
                          Value      : in Gfloat);
      pragma Import (C, Internal, "gtk_adjustment_set_value");

   begin
      Internal (Get_Object (Adjustment), Value);
   end Set_Value;


end Gtk.Adjustment;
