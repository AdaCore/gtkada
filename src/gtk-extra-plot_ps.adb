-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Gdk;                   use Gdk;
with Gtk.Extra.Plot;        use Gtk.Extra.Plot;
with Gtk.Extra.Plot_Layout; use Gtk.Extra.Plot_Layout;
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
         (Plot        : in System.Address;
          Psfile      : in String;
          Orientation : in Gint;
          Epsflag     : in Gint;
          Page_Size   : in Gint);
      pragma Import (C, Internal, "gtk_plot_export_ps");
   begin
      Internal (Get_Object (Plot),
                Psfile & Ascii.NUL,
                Ps_Orientation'Pos (Orientation),
                Boolean'Pos (Epsflag),
                Ps_Page_Size'Pos (Page_Size));
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
         (Plot        : in System.Address;
          Psfile      : in String;
          Orientation : in Gint;
          Epsflag     : in Gint;
          Units       : in Gint;
          Width       : in Gint;
          Height      : in Gint);
      pragma Import (C, Internal, "gtk_plot_export_ps_with_size");
   begin
      Internal (Get_Object (Plot),
                Psfile & Ascii.NUL,
                Ps_Orientation'Pos (Orientation),
                Boolean'Pos (Epsflag),
                Ps_Units'Pos (Units),
                Width,
                Height);
   end Plot_Export_Ps_With_Size;

   ---------------------------
   -- Plot_Layout_Export_Ps --
   ---------------------------

   procedure Plot_Layout_Export_Ps
      (Layout      : access Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Page_Size   : in Ps_Page_Size)
   is
      procedure Internal
         (Layout      : in System.Address;
          File_Name   : in String;
          Orientation : in Gint;
          Epsflag     : in Gint;
          Page_Size   : in Gint);
      pragma Import (C, Internal, "gtk_plot_layout_export_ps");
   begin
      Internal (Get_Object (Layout),
                File_Name & Ascii.NUL,
                Ps_Orientation'Pos (Orientation),
                Boolean'Pos (Epsflag),
                Ps_Page_Size'Pos (Page_Size));
   end Plot_Layout_Export_Ps;

   -------------------------------------
   -- Plot_Layout_Export_Ps_With_Size --
   -------------------------------------

   procedure Plot_Layout_Export_Ps_With_Size
      (Layout      : access Gtk.Extra.Plot_Layout.Gtk_Plot_Layout_Record'Class;
       File_Name   : in String;
       Orientation : in Ps_Orientation;
       Epsflag     : in Boolean;
       Units       : in Ps_Units;
       Width       : in Gint;
       Height      : in Gint)
   is
      procedure Internal
         (Layout      : in System.Address;
          File_Name   : in String;
          Orientation : in Gint;
          Epsflag     : in Gint;
          Units       : in Gint;
          Width       : in Gint;
          Height      : in Gint);
      pragma Import (C, Internal, "gtk_plot_layout_export_ps_with_size");
   begin
      Internal (Get_Object (Layout),
                File_Name & Ascii.NUL,
                Ps_Orientation'Pos (Orientation),
                Boolean'Pos (Epsflag),
                Ps_Units'Pos (Units),
                Width,
                Height);
   end Plot_Layout_Export_Ps_With_Size;

end Gtk.Extra.Plot_Ps;
