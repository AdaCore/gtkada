-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gdk.Color;
--  with Gdk.ImlibImage;
with Gdk; use Gdk;
with Gtk; use Gtk;
with System;

package body Gnome.Druid_Page_Standard is

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New
   --    (Widget : out Gnome_Druid_Page_Standard;
   --     Title  : String;
   --     Logo   : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --  begin
   --     Widget := new Gnome_Druid_Page_Standard_Record;
   --     Initialize (Widget, Title, Logo);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize
   --    (Widget: access Gnome_Druid_Page_Standard_Record'Class;
   --     Title : String;
   --     Logo  : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     function Internal
   --       (Title  : String;
   --        Logo   : GdkImlibImage)
   --        return System.Address;
   --     pragma Import
   --       (C, Internal, "gnome_druid_page_standard_new_with_vals");
   --  begin
   --     Set_Object (Widget, Internal (Title & ASCII.NUL, Logo));
   --     Initialize_User_Data (Widget);
   --  end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Druid_Page_Standard) is
   begin
      Widget := new Gnome_Druid_Page_Standard_Record;
      Initialize (Widget);
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
      Initialize_User_Data (Widget);
   end Initialize;

   ------------------
   -- Set_Bg_Color --
   ------------------

   procedure Set_Bg_Color
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_standard_set_bg_color");
   begin
      Internal (Get_Object (Druid_Page_Standard),
                Color);
   end Set_Bg_Color;

   --------------
   -- Set_Logo --
   --------------

   --  procedure Set_Logo
   --    (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
   --     Logo_Image          : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     procedure Internal
   --       (Druid_Page_Standard : System.Address;
   --        Logo_Image          : GdkImlibImage);
   --     pragma Import (C, Internal, "gnome_druid_page_standard_set_logo");
   --  begin
   --     Internal (Get_Object (Druid_Page_Standard),
   --               Logo_Image);
   --  end Set_Logo;

   -----------------------
   -- Set_Logo_Bg_Color --
   -----------------------

   procedure Set_Logo_Bg_Color
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import
        (C, Internal, "gnome_druid_page_standard_set_logo_bg_color");
   begin
      Internal (Get_Object (Druid_Page_Standard), Color);
   end Set_Logo_Bg_Color;

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

   ---------------------
   -- Set_Title_Color --
   ---------------------

   procedure Set_Title_Color
     (Druid_Page_Standard : access Gnome_Druid_Page_Standard_Record;
      Color               : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Standard : System.Address;
         Color               : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_standard_set_title_color");
   begin
      Internal (Get_Object (Druid_Page_Standard),
                Color);
   end Set_Title_Color;

end Gnome.Druid_Page_Standard;
