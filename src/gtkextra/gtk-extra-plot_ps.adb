------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with System;

package body Gtk.Extra.Plot_Ps is

   --------------------
   -- Plot_Export_Ps --
   --------------------

   procedure Plot_Export_Ps
      (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       Psfile      : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Page_Size   : in Ps_Page_Size)
   is
      procedure Internal
        (Plot        : System.Address;
         Psfile      : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Page_Size   : Ps_Page_Size);
      pragma Import (C, Internal, "gtk_plot_export_ps");

   begin
      Internal (Get_Object (Plot),
                Psfile & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Page_Size);
   end Plot_Export_Ps;

   ------------------------------
   -- Plot_Export_Ps_With_Size --
   ------------------------------

   procedure Plot_Export_Ps_With_Size
      (Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       Psfile      : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Units       : in Ps_Units;
       Width       : in Gint;
       Height      : in Gint)
   is
      procedure Internal
        (Plot        : System.Address;
         Psfile      : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Units       : Ps_Units;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_plot_export_ps_with_size");

   begin
      Internal (Get_Object (Plot),
                Psfile & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Units,
                Width,
                Height);
   end Plot_Export_Ps_With_Size;

   ---------------------------
   -- Plot_Canvas_Export_Ps --
   ---------------------------

   procedure Plot_Canvas_Export_Ps
      (Canvas      : access Gtk.Extra.Plot_Canvas.Gtk_Plot_Canvas_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Page_Size   : in Ps_Page_Size)
   is
      procedure Internal
        (Canvas      : System.Address;
         File_Name   : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Page_Size   : Ps_Page_Size);
      pragma Import (C, Internal, "gtk_plot_canvas_export_ps");

   begin
      Internal (Get_Object (Canvas),
                File_Name & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Page_Size);
   end Plot_Canvas_Export_Ps;

   -------------------------------------
   -- Plot_Canvas_Export_Ps_With_Size --
   -------------------------------------

   procedure Plot_Canvas_Export_Ps_With_Size
      (Canvas      : access Gtk.Extra.Plot_Canvas.Gtk_Plot_Canvas_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Units       : in Ps_Units;
       Width       : in Gint;
       Height      : in Gint)
   is
      procedure Internal
        (Canvas      : System.Address;
         File_Name   : String;
         Orientation : Ps_Orientation;
         Epsflag     : Gint;
         Units       : Ps_Units;
         Width       : Gint;
         Height      : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_export_ps_with_size");

   begin
      Internal (Get_Object (Canvas),
                File_Name & ASCII.NUL,
                Orientation,
                Boolean'Pos (Epsflag),
                Units,
                Width,
                Height);
   end Plot_Canvas_Export_Ps_With_Size;

end Gtk.Extra.Plot_Ps;
