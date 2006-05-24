-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

   function Get_Hadjustment
     (Layout : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_get_hadjustment");

      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;

   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Layout)), Stub));
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
     (Layout : access Gtk_Layout_Record) return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Layout : System.Address) return System.Address;
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
     (Layout      : out Gtk_Layout;
      Hadjustment : Adjustment.Gtk_Adjustment := null;
      Vadjustment : Adjustment.Gtk_Adjustment := null) is
   begin
      Layout := new Gtk_Layout_Record;
      Initialize (Layout, Hadjustment, Vadjustment);
   end Gtk_New;

   -----------------
   -- Initialize  --
   -----------------

   procedure Initialize
     (Layout      : access Gtk_Layout_Record'Class;
      Hadjustment : Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal
        (Hadjustment : System.Address;
         Vadjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_layout_new");

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

      Set_Object (Layout, Internal (Hadj, Vadj));
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint)
   is
      procedure Internal
        (Layout : System.Address;
         Widget : System.Address;
         X      : Gint;
         Y      : Gint);
      pragma Import (C, Internal, "gtk_layout_move");

   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
     (Layout : access Gtk_Layout_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint;
      Y      : Gint)
   is
      procedure Internal
        (Layout : System.Address;
         Widget : System.Address;
         X      : Gint;
         Y      : Gint);
      pragma Import (C, Internal, "gtk_layout_put");

   begin
      Internal (Get_Object (Layout), Get_Object (Widget), X, Y);
   end Put;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Layout     : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_layout_set_hadjustment");

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Internal (Get_Object (Layout), System.Null_Address);
      else
         Internal (Get_Object (Layout), Get_Object (Adjustment));
      end if;
   end Set_Hadjustment;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Layout : access Gtk_Layout_Record;
      Width  : Guint;
      Height : Guint)
   is
      procedure Internal
        (Layout : System.Address;
         Width  : Guint;
         Height : Guint);
      pragma Import (C, Internal, "gtk_layout_set_size");

   begin
      Internal (Get_Object (Layout), Width, Height);
   end Set_Size;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      procedure Internal
        (Layout     : System.Address;
         Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_layout_set_vadjustment");

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Internal (Get_Object (Layout), System.Null_Address);
      else
         Internal (Get_Object (Layout), Get_Object (Adjustment));
      end if;
   end Set_Vadjustment;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Layout : access Gtk_Layout_Record) return Guint is
      function Internal (Layout : System.Address) return Guint;
      pragma Import (C, Internal, "ada_gtk_layout_get_width");

   begin
      return Internal (Get_Object (Layout));
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Layout : access Gtk_Layout_Record) return Guint is
      function Internal (Layout : System.Address) return Guint;
      pragma Import (C, Internal, "ada_gtk_layout_get_height");

   begin
      return Internal (Get_Object (Layout));
   end Get_Height;

   --------------------
   -- Get_Bin_Window --
   --------------------

   function Get_Bin_Window
     (Widget : access Gtk_Layout_Record) return Gdk.Window.Gdk_Window
   is
      function Internal (Layout : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "ada_gtk_layout_get_bin_window");

   begin
      return Internal (Get_Object (Widget));
   end Get_Bin_Window;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Layout : access Gtk_Layout_Record) is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "gtk_layout_freeze");
   begin
      Internal (Get_Object (Layout));
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Layout : access Gtk_Layout_Record) is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "gtk_layout_thaw");
   begin
      Internal (Get_Object (Layout));
   end Thaw;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Layout : access Gtk_Layout_Record;
      Width  : out Guint;
      Height : out Guint)
   is
      procedure Internal (Layout : System.Address; W, H : out Guint);
      pragma Import (C, Internal, "gtk_layout_get_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Size;

end Gtk.Layout;
