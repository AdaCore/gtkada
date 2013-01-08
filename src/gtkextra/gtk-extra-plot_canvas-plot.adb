------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with System;
with Gtk.Extra.Plot_Data; use Gtk.Extra.Plot_Data;

package body Gtk.Extra.Plot_Canvas.Plot is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child    : out Gtk_Plot_Canvas_Plot;
      Plot     : access Gtk.Extra.Plot.Gtk_Plot_Record'Class)
   is
      function Internal (Plot : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_plot_new");
   begin
      Child := new Gtk_Plot_Canvas_Plot_Record;
      Set_Object (Child, Internal (Get_Object (Plot)));
   end Gtk_New;

   -------------
   -- Get_Pos --
   -------------

   function Get_Pos
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Plot_Canvas_Plot_Pos
   is
      function Internal (Child : System.Address) return Plot_Canvas_Plot_Pos;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_pos");
   begin
      return Internal (Get_Object (Child));
   end Get_Pos;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Child : access Gtk_Plot_Canvas_Plot_Record)
      return Gtk.Extra.Plot_Data.Gtk_Plot_Data
   is
      function Internal (Child : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_data");
      Stub : Gtk_Plot_Data_Record;
   begin
      return Gtk_Plot_Data
        (Get_User_Data (Internal (Get_Object (Child)), Stub));
   end Get_Data;

   -------------------
   -- Get_Datapoint --
   -------------------

   function Get_Datapoint
     (Child : access Gtk_Plot_Canvas_Plot_Record) return Gint
   is
      function Internal (Child : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_get_datapoint");
   begin
      return Internal (Get_Object (Child));
   end Get_Datapoint;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags)
   is
      procedure Internal (Child : System.Address; Flags : Integer);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_set_flags");
   begin
      Internal (Get_Object (Child), Integer (Flags));
   end Set_Flags;

   -----------------
   -- Unset_Flags --
   -----------------

   procedure Unset_Flags
     (Child : access Gtk_Plot_Canvas_Plot_Record;
      Flags : Plot_Canvas_Plot_Flags)
   is
      procedure Internal (Child : System.Address; Flags : Integer);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_plot_unset_flags");
   begin
      Internal (Get_Object (Child), Integer (Flags));
   end Unset_Flags;

end Gtk.Extra.Plot_Canvas.Plot;
