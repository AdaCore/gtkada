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

with Gdk; use Gdk;
with Gtk.Extra.Plot;
with System;

package body Gtk.Extra.Plot_Canvas is

   --------------
   -- Add_Plot --
   --------------

   procedure Add_Plot
      (Plot_Canvas : access Gtk_Plot_Canvas_Record;
       Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class;
       X           : in Gdouble;
       Y           : in Gdouble)
   is
      procedure Internal
         (Plot_Canvas : in System.Address;
          Plot        : in System.Address;
          X           : in Gdouble;
          Y           : in Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_add_plot");
   begin
      Internal (Get_Object (Plot_Canvas),
                Get_Object (Plot),
                X,
                Y);
   end Add_Plot;

   -------------------
   -- Cancel_Action --
   -------------------

   procedure Cancel_Action (Plot_Canvas : access Gtk_Plot_Canvas_Record)
   is
      procedure Internal (Plot_Canvas : in System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_cancel_action");
   begin
      Internal (Get_Object (Plot_Canvas));
   end Cancel_Action;

   ------------------------
   -- Get_Active_Dataset --
   ------------------------

   function Get_Active_Dataset (Canvas : access Gtk_Plot_Canvas_Record)
                                return      Gtk.Extra.Plot.Gtk_Plot_Data
   is
      function Internal (Canvas : in System.Address)
                         return      Gtk.Extra.Plot.Gtk_Plot_Data;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_dataset");
   begin
      return Internal (Get_Object (Canvas));
   end Get_Active_Dataset;

   ---------------------
   -- Get_Active_Plot --
   ---------------------

   function Get_Active_Plot (Canvas : access Gtk_Plot_Canvas_Record)
                             return      Gtk.Extra.Plot.Gtk_Plot
   is
      function Internal (Canvas : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_plot");
      Stub : Gtk.Extra.Plot.Gtk_Plot_Record;
   begin
      return Gtk.Extra.Plot.Gtk_Plot
        (Get_User_Data (Internal (Get_Object (Canvas)), Stub));
   end Get_Active_Plot;

   ----------------------
   -- Get_Active_Point --
   ----------------------

   procedure Get_Active_Point (Canvas : access Gtk_Plot_Canvas_Record;
                               X      : out Gdouble;
                               Y      : out Gdouble)
   is
      procedure Internal (Canvas : in System.Address;
                          X      : out Gdouble;
                          Y      : out Gdouble);
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_point");
   begin
      Internal (Get_Object (Canvas), X, Y);
   end Get_Active_Point;

   ---------------------
   -- Get_Active_Text --
   ---------------------

   function Get_Active_Text (Canvas : access Gtk_Plot_Canvas_Record)
                             return      Gtk.Extra.Plot.Gtk_Plot_Text
   is
      function Internal (Canvas : in System.Address)
                         return      Gtk.Extra.Plot.Gtk_Plot_Text;
      pragma Import (C, Internal, "gtk_plot_canvas_get_active_text");
   begin
      return Internal (Get_Object (Canvas));
   end Get_Active_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Plot_Canvas;
                      Width  : in Gint;
                      Height : in Gint)
   is
   begin
      Widget := new Gtk_Plot_Canvas_Record;
      Initialize (Widget, Width, Height);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Plot_Canvas_Record'Class;
                         Width  : in Gint;
                         Height : in Gint)
   is
      function Internal (Width  : in Gint;
                         Height : in Gint)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_plot_canvas_new");
   begin
      Set_Object (Widget, Internal (Width, Height));
      Initialize_User_Data (Widget);
   end Initialize;

   ---------------------
   -- Set_Active_Plot --
   ---------------------

   procedure Set_Active_Plot
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Plot        : access Gtk.Extra.Plot.Gtk_Plot_Record'Class)
   is
      procedure Internal (Plot_Canvas : in System.Address;
                          Plot        : in System.Address);
      pragma Import (C, Internal, "gtk_plot_canvas_set_active_plot");
   begin
      Internal (Get_Object (Plot_Canvas), Get_Object (Plot));
   end Set_Active_Plot;

   -----------------------------
   -- Plot_Canvas_Flag_Is_Set --
   -----------------------------

   function Plot_Canvas_Flag_Is_Set
     (Plot_Canvas : access Gtk_Plot_Canvas_Record;
      Flag        : in Guint16)
     return Boolean
   is
      function Internal (Canvas : System.Address;
                         Flag   : Guint16)
                        return Guint16;
      pragma Import (C, Internal, "ada_gtk_plot_canvas_flag_is_set");
   begin
      return Internal (Get_Object (Plot_Canvas), Flag) /= 0;
   end Plot_Canvas_Flag_Is_Set;

   ---------------------------
   -- Set_Plot_Canvas_Flags --
   ---------------------------

   procedure Plot_Canvas_Set_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_set_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Set_Flags;

   -----------------------------
   -- Plot_Canvas_Unset_Flags --
   -----------------------------

   procedure Plot_Canvas_Unset_Flags
     (Plot_Canvas  : access Gtk_Plot_Canvas_Record;
      Flags        : in Guint16)
   is
      procedure Internal (Canvas : System.Address;
                          Flags  : Guint16);
      pragma Import (C, Internal, "ada_gtk_plot_canvas_unset_flags");
   begin
      Internal (Get_Object (Plot_Canvas), Flags);
   end Plot_Canvas_Unset_Flags;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size (Canvas  : access Gtk_Plot_Canvas_Record;
                       Width   : in Gint;
                       Height  : in Gint)
   is
      procedure Internal (Canvas : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_plot_canvas_set_size");
   begin
      Internal (Get_Object (Canvas), Width, Height);
   end Set_Size;

end Gtk.Extra.Plot_Canvas;
