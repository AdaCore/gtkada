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

with System;
with Gdk; use Gdk;

package body Gtk.Scrolled_Window is

   ---------------
   -- Construct --
   ---------------

   procedure Construct
     (Scrolled_Window : in out Gtk_Scrolled_Window;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment) is
      procedure Internal (Scrolled_Window : in System.Address;
                          Hadjustment, Vadjustment : in System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_construct");
   begin
      Internal (Get_Object (Scrolled_Window),
                Get_Object (Hadjustment), Get_Object (Vadjustment));
   end Construct;

   ----------------------
   -- Get_Hadjustement --
   ----------------------

   function Get_Hadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window)
      return Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
      Adjust : Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adjust, Internal (Get_Object (Scrolled_Window)));
      return Adjust;
   end Get_Hadjustment;

   ----------------------
   -- Get_Vadjustement --
   ----------------------

   function Get_Vadjustment
     (Scrolled_Window : in Gtk_Scrolled_Window)
      return               Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
      Adjust : Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adjust, Internal (Get_Object (Scrolled_Window)));
      return Adjust;
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Scrolled_Window :    out Gtk_Scrolled_Window;
      Hadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment;
      Vadjustment     : in     Adjustment.Gtk_Adjustment'Class :=
        Adjustment.Null_Adjustment) is
      function Internal (Hadjustment, Vadjustment : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");
   begin
      Set_Object (Scrolled_Window, Internal (Get_Object (Hadjustment),
                                             Get_Object (Vadjustment)));
   end Gtk_New;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy (Scrolled_Window    : in out Gtk_Scrolled_Window;
                         H_Scrollbar_Policy : in     Enums.Gtk_Policy_Type;
                         V_Scrollbar_Policy : in     Enums.Gtk_Policy_Type) is
      procedure Internal (Scrolled_Window : in System.Address;
                          H_Scrollbar_Policy : in Enums.Gtk_Policy_Type;
                          V_Scrollbar_Policy : in Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window),
                H_Scrollbar_Policy,
                V_Scrollbar_Policy);
   end Set_Policy;

end Gtk.Scrolled_Window;
