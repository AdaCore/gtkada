------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Scrollable is

   ----------------
   -- Get_Border --
   ----------------

   function Get_Border
      (Self   : Gtk_Scrollable;
       Border : access Gtk.Style.Gtk_Border) return Boolean
   is
      function Internal
         (Self       : Gtk_Scrollable;
          Acc_Border : access Gtk.Style.Gtk_Border) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrollable_get_border");
      Acc_Border     : aliased Gtk.Style.Gtk_Border;
      Tmp_Acc_Border : aliased Gtk.Style.Gtk_Border;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Self, Tmp_Acc_Border'Access);
      Acc_Border := Tmp_Acc_Border;
      Border.all := Acc_Border;
      return Tmp_Return /= 0;
   end Get_Border;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : Gtk_Scrollable) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Self), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Self : Gtk_Scrollable) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : Gtk_Scrollable) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Self), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : Gtk_Scrollable;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : Gtk_Scrollable;
          Hadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_hadjustment");
   begin
      Internal (Self, Get_Object_Or_Null (GObject (Hadjustment)));
   end Set_Hadjustment;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self        : Gtk_Scrollable;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : Gtk_Scrollable;
          Vadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_vadjustment");
   begin
      Internal (Self, Get_Object_Or_Null (GObject (Vadjustment)));
   end Set_Vadjustment;

   function "+" (W : Gtk_Scrollable) return Gtk_Scrollable is
   begin
      return W;
   end "+";

end Gtk.Scrollable;
