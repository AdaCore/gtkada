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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Util; use Gtk.Util;

package body Gtk.Scrolled_Window is

   -----------------------
   -- Add_With_Viewport --
   -----------------------

   procedure Add_With_Viewport
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Child           : access Gtk.Widget.Gtk_Widget_Record'Class) is
      procedure Internal (Scrolled_Window : System.Address;
                          Child           : System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_add_with_viewport");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Child));
   end Add_With_Viewport;

   ----------------------
   -- Get_Hadjustement --
   ----------------------

   function Get_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_hadjustment");
      Stub : Adjustment.Gtk_Adjustment_Record;
   begin
      return Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Hadjustment;

   ----------------------
   -- Get_Vadjustement --
   ----------------------

   function Get_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record)
      return               Adjustment.Gtk_Adjustment
   is
      function Internal (Scrolled_Window : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_get_vadjustment");
      Stub : Adjustment.Gtk_Adjustment_Record;
   begin
      return Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Scrolled_Window)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Scrolled_Window :    out Gtk_Scrolled_Window;
      Hadjustment     : access Adjustment.Gtk_Adjustment_Record'Class
        := Adjustment.Null_Adjustment;
      Vadjustment     : access Adjustment.Gtk_Adjustment_Record'Class
        := Adjustment.Null_Adjustment)
   is
   begin
      Scrolled_Window := new Gtk_Scrolled_Window_Record;
      Initialize (Scrolled_Window, Hadjustment, Vadjustment);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Hadjustment     : access Adjustment.Gtk_Adjustment_Record'Class
        := Adjustment.Null_Adjustment;
      Vadjustment     : access Adjustment.Gtk_Adjustment_Record'Class
        := Adjustment.Null_Adjustment)
   is
      function Internal (Hadjustment, Vadjustment : in System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_scrolled_window_new");
   begin
      Set_Object (Scrolled_Window, Internal (Get_Object (Hadjustment),
                                             Get_Object (Vadjustment)));
      Initialize_User_Data (Scrolled_Window);
   end Initialize;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Hadjustment     : access Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Scrolled_Window : in System.Address;
                          Hadjustment     : in System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_hadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Hadjustment));
   end Set_Hadjustment;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (Scrolled_Window    : access Gtk_Scrolled_Window_Record;
      H_Scrollbar_Policy : in     Enums.Gtk_Policy_Type;
      V_Scrollbar_Policy : in     Enums.Gtk_Policy_Type)
   is
      procedure Internal (Scrolled_Window : in System.Address;
                          H_Scrollbar_Policy : in Enums.Gtk_Policy_Type;
                          V_Scrollbar_Policy : in Enums.Gtk_Policy_Type);
      pragma Import (C, Internal, "gtk_scrolled_window_set_policy");
   begin
      Internal (Get_Object (Scrolled_Window),
                H_Scrollbar_Policy,
                V_Scrollbar_Policy);
   end Set_Policy;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Scrolled_Window : access Gtk_Scrolled_Window_Record;
      Vadjustment     : access Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Scrolled_Window : in System.Address;
                          Vadjustment     : in System.Address);
      pragma Import (C, Internal, "gtk_scrolled_window_set_vadjustment");
   begin
      Internal (Get_Object (Scrolled_Window), Get_Object (Vadjustment));
   end Set_Vadjustment;

   --------------
   -- Generate --
   --------------
 
   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
   begin
      Gen_New (N, "Scrolled_Window", File => File);
      Container.Generate (N, File);
      Gen_Set (N, "Scrolled_Window", "Policy", "hscrollbar_policy",
        "vscrollbar_policy", "", "", File);
   end Generate;
 
   procedure Generate
     (Scrolled_Window : in out Gtk.Object.Gtk_Object; N : in Node_Ptr)
   is
      S, S2 : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Scrolled_Window (Scrolled_Window));
         Set_Object (Get_Field (N, "name"), Scrolled_Window);
         N.Specific_Data.Created := True;
      end if;
 
      Container.Generate (Scrolled_Window, N);
 
      S := Get_Field (N, "hscrollbar_policy");
      S2 := Get_Field (N, "vscrollbar_policy");
 
      if S /= null and then S2 /= null then
         Set_Policy (Gtk_Scrolled_Window (Scrolled_Window),
           Gtk_Policy_Type'Value (S (S'First + 4 .. S'Last)),
           Gtk_Policy_Type'Value (S2 (S2'First + 4 .. S2'Last)));
      end if;
   end Generate;

end Gtk.Scrolled_Window;
