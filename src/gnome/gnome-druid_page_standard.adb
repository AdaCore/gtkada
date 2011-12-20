------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gdk; use Gdk;
with Gtk; use Gtk;
with System;

package body Gnome.Druid_Page_Standard is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Druid_Page_Standard) is
   begin
      Widget := new Gnome_Druid_Page_Standard_Record;
      Gnome.Druid_Page_Standard.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_Druid_Page_Standard_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_druid_page_standard_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Title               : String)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Title               : String);
      pragma Import (C, Internal, "gnome_druid_page_standard_set_title");
   begin
      Internal (Get_Object (Druid_Page_Standard),
                Title & ASCII.NUL);
   end Set_Title;

   --------------
   -- Set_Logo --
   --------------

   procedure Set_Logo
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Logo                : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Logo                : System.Address);
      pragma Import (C, Internal, "gnome_druid_page_standard_set_logo");
   begin
      Internal (Get_Object (Druid_Page_Standard), Get_Object (Logo));
   end Set_Logo;

   -----------------------
   -- Set_Top_Watermark --
   -----------------------

   procedure Set_Top_Watermark
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Top_Watermark       : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Logo                : System.Address);
      pragma Import (C, Internal,
                     "gnome_druid_page_standard_set_top_watermark");
   begin
      Internal (Get_Object (Druid_Page_Standard), Get_Object (Top_Watermark));
   end Set_Top_Watermark;

   --------------------------
   -- Set_Title_Foreground --
   --------------------------

   procedure Set_Title_Foreground
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal,
                       "gnome_druid_page_standard_set_title_foreground");
   begin
      Internal (Get_Object (Druid_Page_Standard), Color);
   end Set_Title_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_standard_set_background");
   begin
      Internal (Get_Object (Druid_Page_Standard), Color);
   end Set_Background;

   -------------------------
   -- Set_Logo_Background --
   -------------------------

   procedure Set_Logo_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal,
                       "gnome_druid_page_standard_set_logo_background");
   begin
      Internal (Get_Object (Druid_Page_Standard), Color);
   end Set_Logo_Background;

   -----------------------------
   -- Set_Contents_Background --
   -----------------------------

   procedure Set_Contents_Background
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal,
                       "gnome_druid_page_standard_set_contents_background");
   begin
      Internal (Get_Object (Druid_Page_Standard), Color);
   end Set_Contents_Background;

end Gnome.Druid_Page_Standard;
