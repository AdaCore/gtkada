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

package body Gtk.Layout is

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Layout : access Gtk_Layout_Record)
                             return          Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Layout : access Gtk_Layout_Record)
                             return          Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Gtk_Layout;
      Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Widget := new Gtk_Layout_Record;
      Initialize (Widget, Hadjustment, Vadjustment);
   end Gtk_New;

   -----------------
   -- Initialize  --
   -----------------

   procedure Initialize
     (Widget : access Gtk_Layout_Record;
      Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      function Internal (Hadjustment : in System.Address;
                         Vadjustment : in System.Address)
                         return           System.Address;
      pragma Import (C, Internal, "gtk_layout_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadjustment),
                                    Get_Object (Vadjustment)));
      Initialize_User_Data (Widget);
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move (Layout : access Gtk_Layout_Record;
                   Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                   X      : in     Gint;
                   Y      : in     Gint) is
      procedure Internal (Layout : in System.Address;
                          Widget : in System.Address;
                          X      : in Gint;
                          Y      : in Gint);
      pragma Import (C, Internal, "gtk_layout_move");
   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put (Layout : access Gtk_Layout_Record;
                  Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                  X      : in     Gint;
                  Y      : in     Gint)
   is
      procedure Internal (Layout : in System.Address;
                          Widget : in System.Address;
                          X      : in Gint;
                          Y      : in Gint);
      pragma Import (C, Internal, "gtk_layout_put");
   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Put;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Layout     : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_layout_set_hadjustment");
   begin
      Internal (Get_Object (Layout), Get_Object (Adjustment));
   end Set_Hadjustment;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Layout : access Gtk_Layout_Record;
                       Width  : in     Guint;
                       Height : in     Guint)
   is
      procedure Internal (Layout : in System.Address;
                          Width  : in Guint;
                          Height : in Guint);
      pragma Import (C, Internal, "gtk_layout_set_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Set_Size;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Layout     : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_layout_set_vadjustment");
   begin
      Internal (Get_Object (Layout), Get_Object (Adjustment));
   end Set_Vadjustment;

end Gtk.Layout;
