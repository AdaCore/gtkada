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

with Gdk; use Gdk;
with System;
with Gtk.Util; use Gtk.Util;

package body Gtk.Preview is

   --------------
   -- Draw_Row --
   --------------

   procedure Draw_Row
     (Preview : access Gtk_Preview_Record;
      Data    : in Guchar_Array;
      X       : in Gint;
      Y       : in Gint;
      W       : in Gint)
   is
      procedure Internal
        (Preview : in System.Address;
         Data    : in System.Address;
         X       : in Gint;
         Y       : in Gint;
         W       : in Gint);
      pragma Import (C, Internal, "gtk_preview_draw_row");

   begin
      Internal (Get_Object (Preview), Data'Address, X, Y, W);
   end Draw_Row;

   --------------
   -- Get_Cmap --
   --------------

   function Get_Cmap return Gdk.Color.Gdk_Colormap'Class is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_cmap");
      Widget : Gdk.Color.Gdk_Colormap;

   begin
      Set_Object (Widget, Internal);
      return Widget;
   end Get_Cmap;

   --------------
   -- Get_Info --
   --------------

   function Get_Info return Gtk_Preview_Info is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_info");
      Stub : Gtk_Preview_Info_Record;
   begin
      return Gtk_Preview_Info
        (Get_User_Data (Internal, Stub));
   end Get_Info;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual return Gdk.Visual.Gdk_Visual'Class is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_visual");
      Widget : Gdk.Visual.Gdk_Visual;

   begin
      Set_Object (Widget, Internal);
      return Widget;
   end Get_Visual;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Preview  : out Gtk_Preview;
                      The_Type : in Gtk_Preview_Type) is
   begin
      Preview := new Gtk_Preview_Record;
      Initialize (Preview, The_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Preview  : access Gtk_Preview_Record;
                         The_Type : in Gtk_Preview_Type) is
      function Internal (The_Type : in Gint)
                         return System.Address;
      pragma Import (C, Internal, "gtk_preview_new");

   begin
      Set_Object (Preview, Internal (Gtk_Preview_Type'Pos (The_Type)));
      Initialize_User_Data (Preview);
   end Initialize;

   ---------
   -- Put --
   ---------

   procedure Put
     (Preview : access Gtk_Preview_Record;
      Window  : in Gdk.Window.Gdk_Window'Class;
      Gc      : in Gdk.GC.Gdk_GC'Class;
      Srcx    : in Gint;
      Srcy    : in Gint;
      Destx   : in Gint;
      Desty   : in Gint;
      Width   : in Gint;
      Height  : in Gint)
   is
      procedure Internal
        (Preview : in System.Address;
         Window  : in System.Address;
         Gc      : in System.Address;
         Srcx    : in Gint;
         Srcy    : in Gint;
         Destx   : in Gint;
         Desty   : in Gint;
         Width   : in Gint;
         Height  : in Gint);
      pragma Import (C, Internal, "gtk_preview_put");

   begin
      Internal
        (Get_Object (Preview), Get_Object (Window), Get_Object (Gc),
         Srcx, Srcy, Destx, Desty, Width, Height);
   end Put;

   -----------
   -- Reset --
   -----------

   procedure Reset is
      procedure Internal;
      pragma Import (C, Internal, "gtk_preview_reset");

   begin
      Internal;
   end Reset;

   --------------------
   -- Set_Color_Cube --
   --------------------

   procedure Set_Color_Cube
     (Nred_Shades   : in Guint;
      Ngreen_Shades : in Guint;
      Nblue_Shades  : in Guint;
      Ngray_Shades  : in Guint)
   is
      procedure Internal
        (Nred_Shades   : in Guint;
         Ngreen_Shades : in Guint;
         Nblue_Shades  : in Guint;
         Ngray_Shades  : in Guint);
      pragma Import (C, Internal, "gtk_preview_set_color_cube");

   begin
      Internal (Nred_Shades, Ngreen_Shades, Nblue_Shades, Ngray_Shades);
   end Set_Color_Cube;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
     (Preview : access Gtk_Preview_Record;
      Expand  : in Boolean)
   is
      procedure Internal
        (Preview : in System.Address;
         Expand  : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_expand");

   begin
      Internal (Get_Object (Preview), Boolean'Pos (Expand));
   end Set_Expand;

   ---------------
   -- Set_Gamma --
   ---------------

   procedure Set_Gamma (Gamma : in Gdouble) is
      procedure Internal (Gamma : in Gdouble);
      pragma Import (C, Internal, "gtk_preview_set_gamma");

   begin
      Internal (Gamma);
   end Set_Gamma;

   ----------------------
   -- Set_Install_Cmap --
   ----------------------

   procedure Set_Install_Cmap (Install_Cmap : in Gint) is
      procedure Internal (Install_Cmap : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_install_cmap");

   begin
      Internal (Install_Cmap);
   end Set_Install_Cmap;

   ------------------
   -- Set_Reserved --
   ------------------

   procedure Set_Reserved (Nreserved : in Gint) is
      procedure Internal (Nreserved : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_reserved");

   begin
      Internal (Nreserved);
   end Set_Reserved;

   ----------
   -- Size --
   ----------

   procedure Size
     (Preview : access Gtk_Preview_Record;
      Width   : in Gint;
      Height  : in Gint)
   is
      procedure Internal
        (Preview : in System.Address;
         Width   : in Gint;
         Height  : in Gint);
      pragma Import (C, Internal, "gtk_preview_size");

   begin
      Internal (Get_Object (Preview), Width, Height);
   end Size;

   ------------
   -- Uninit --
   ------------

   procedure Uninit is
      procedure Internal;
      pragma Import (C, Internal, "gtk_preview_uninit");
   begin
      Internal;
   end Uninit;

   --------------
   -- Generate --
   --------------

   procedure Generate (Preview : access Gtk_Preview_Record;
                       N         : in Node_Ptr;
                       File      : in File_Type) is
      use Widget;
   begin
      if Get_Field (N, "type").all = "True" then
         Gen_New (N, "Preview", "Preview_Color", File => File);
         Gen_New (N, "Preview", "Preview_Grayscale", File => File);
      end if;

      Generate (Gtk_Widget (Preview), N, File);
      Gen_Set (N, "Preview", "expand", File);
   end Generate;

   procedure Generate (Preview : access Gtk_Preview_Record;
                       N         : in Node_Ptr) is
      use Widget;

--      S : String_Ptr;
   begin
--         if not N.Specific_Data.Created then
--            if Get_Field (N, "type").all = "True" then
--               Gtk_New (Preview, Preview_Color);
--               Gtk_New (Preview, Preview_Grayscale);
--            end if;

--            Set_Object (Get_Field (N, "name"), Preview'Unchecked_Access);
--            N.Specific_Data.Created := True;
--         end if;

--         Generate (Gtk_Widget (Preview), N);
--         S := Get_Field (N, "expand");

--         if S /= null then
--            Set_Expand (Preview, Boolean'Value (S.all));
--         end if;
      null;
   end Generate;

end Gtk.Preview;
