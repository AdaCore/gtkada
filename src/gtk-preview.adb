------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with Glib.Type_Conversion_Hooks;

package body Gtk.Preview is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Preview_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------
   -- Draw_Row --
   --------------

   procedure Draw_Row
     (Preview : access Gtk_Preview_Record;
      Data    : Guchar_Array;
      X       : Gint;
      Y       : Gint;
      W       : Gint)
   is
      procedure Internal
        (Preview : System.Address;
         Data    : System.Address;
         X       : Gint;
         Y       : Gint;
         W       : Gint);
      pragma Import (C, Internal, "gtk_preview_draw_row");

   begin
      Internal (Get_Object (Preview), Data (Data'First)'Address, X, Y, W);
   end Draw_Row;

   --------------
   -- Get_Info --
   --------------

   function Get_Info return Gtk_Preview_Info is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_info");

      Stub : Gtk_Preview_Info_Record;

   begin
      return Gtk_Preview_Info (Get_User_Data (Internal, Stub));
   end Get_Info;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Preview : out Gtk_Preview; The_Type : Gtk_Preview_Type) is
   begin
      Preview := new Gtk_Preview_Record;
      Initialize (Preview, The_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Preview  : access Gtk_Preview_Record'Class;
      The_Type : Gtk_Preview_Type)
   is
      function Internal (The_Type : Gtk_Preview_Type) return System.Address;
      pragma Import (C, Internal, "gtk_preview_new");

   begin
      Set_Object (Preview, Internal (The_Type));
   end Initialize;

   ---------
   -- Put --
   ---------

   procedure Put
     (Preview : access Gtk_Preview_Record;
      Window  : Gdk.Window.Gdk_Window;
      Gc      : Gdk.GC.Gdk_GC;
      Srcx    : Gint;
      Srcy    : Gint;
      Destx   : Gint;
      Desty   : Gint;
      Width   : Gint;
      Height  : Gint)
   is
      procedure Internal
        (Preview : System.Address;
         Window  : Gdk.Window.Gdk_Window;
         Gc      : Gdk.GC.Gdk_GC;
         Srcx    : Gint;
         Srcy    : Gint;
         Destx   : Gint;
         Desty   : Gint;
         Width   : Gint;
         Height  : Gint);
      pragma Import (C, Internal, "gtk_preview_put");

   begin
      Internal
        (Get_Object (Preview), Window, Gc,
         Srcx, Srcy, Destx, Desty, Width, Height);
   end Put;

   --------------------
   -- Set_Color_Cube --
   --------------------

   procedure Set_Color_Cube
     (Nred_Shades   : Guint;
      Ngreen_Shades : Guint;
      Nblue_Shades  : Guint;
      Ngray_Shades  : Guint)
   is
      procedure Internal
        (Nred_Shades   : Guint;
         Ngreen_Shades : Guint;
         Nblue_Shades  : Guint;
         Ngray_Shades  : Guint);
      pragma Import (C, Internal, "gtk_preview_set_color_cube");

   begin
      Internal (Nred_Shades, Ngreen_Shades, Nblue_Shades, Ngray_Shades);
   end Set_Color_Cube;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
     (Preview : access Gtk_Preview_Record;
      Expand  : Boolean)
   is
      procedure Internal
        (Preview : System.Address;
         Expand  : Gint);
      pragma Import (C, Internal, "gtk_preview_set_expand");

   begin
      Internal (Get_Object (Preview), Boolean'Pos (Expand));
   end Set_Expand;

   ----------
   -- Size --
   ----------

   procedure Size
     (Preview : access Gtk_Preview_Record;
      Width   : Gint;
      Height  : Gint)
   is
      procedure Internal
        (Preview : System.Address;
         Width   : Gint;
         Height  : Gint);
      pragma Import (C, Internal, "gtk_preview_size");

   begin
      Internal (Get_Object (Preview), Width, Height);
   end Size;

end Gtk.Preview;
