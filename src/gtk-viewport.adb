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

package body Gtk.Viewport is

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
     (Widget : access Gtk_Viewport_Record) return Gdk.Gdk_Window
   is
      function Internal (Viewport : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "ada_gtk_viewport_get_bin_window");
   begin
      return Internal (Get_Object (Widget));
   end Get_Bin_Window;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
     (Viewport : access Gtk_Viewport_Record)
      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : in System.Address)
        return System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Viewport : access Gtk_Viewport_Record)
     return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Viewport : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Viewport)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Viewport    : out Gtk_Viewport;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment) is
   begin
      Viewport := new Gtk_Viewport_Record;
      Initialize (Viewport, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Viewport    : access Gtk_Viewport_Record'Class;
      Hadjustment : in Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal
        (Hadjustment : in System.Address;
         Vadjustment : in System.Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");

      Hadj, Vadj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Hadjustment = null then
         Hadj := System.Null_Address;
      else
         Hadj := Get_Object (Hadjustment);
      end if;

      if Vadjustment = null then
         Vadj := System.Null_Address;
      else
         Vadj := Get_Object (Vadjustment);
      end if;

      Set_Object (Viewport, Internal (Hadj, Vadj));
      Initialize_User_Data (Viewport);
   end Initialize;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Viewport   : in System.Address;
         Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_hadjustment");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Internal (Get_Object (Viewport), Adj);
   end Set_Hadjustment;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
     (Viewport : access Gtk_Viewport_Record;
      The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
        (Viewport : in System.Address;
         The_Type : in Gint);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");

   begin
      Internal (Get_Object (Viewport), Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Viewport   : in System.Address;
         Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_vadjustment");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Internal (Get_Object (Viewport), Adj);
   end Set_Vadjustment;

end Gtk.Viewport;
