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

package body Gnome.Druid_Page_Start is

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New
   --    (Widget    : out Gnome_Druid_Page_Start;
   --     Title     : String;
   --     Text      : String;
   --     Logo      : Gdk.ImlibImage.Gdk_ImlibImage;
   --     Watermark : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --  begin
   --     Widget := new Gnome_Druid_Page_Start_Record;
   --     Initialize (Widget, Title, Text, Logo, Watermark);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize
   --    (Widget    : access Gnome_Druid_Page_Start_Record'Class;
   --     Title     : String;
   --     Text      : String;
   --     Logo      : Gdk.ImlibImage.Gdk_ImlibImage;
   --     Watermark : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     function Internal
   --       (Title     : String;
   --        Text      : String;
   --        Logo      : GdkImlibImage;
   --        Watermark : GdkImlibImage)
   --        return System.Address;
   --     pragma Import (C, Internal, "gnome_druid_page_start_new_with_vals");
   --  begin
   --     Set_Object (Widget, Internal (Title & ASCII.NUL,
   --                                   Text & ASCII.NUL,
   --                                   Logo,
   --                                   Watermark));
   --  end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Druid_Page_Start) is
   begin
      Widget := new Gnome_Druid_Page_Start_Record;
      Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_Druid_Page_Start_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_druid_page_start_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ------------------
   -- Set_Bg_Color --
   ------------------

   procedure Set_Bg_Color
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Color            : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Color            : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_start_set_bg_color");
   begin
      Internal (Get_Object (Druid_Page_Start), Color);
   end Set_Bg_Color;

   --------------
   -- Set_Logo --
   --------------

   --  procedure Set_Logo
   --    (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
   --     Logo_Image       : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     procedure Internal
   --       (Druid_Page_Start : System.Address;
   --        Logo_Image       : GdkImlibImage);
   --     pragma Import (C, Internal, "gnome_druid_page_start_set_logo");
   --  begin
   --     Internal (Get_Object (Druid_Page_Start),
   --               Logo_Image);
   --  end Set_Logo;

   -----------------------
   -- Set_Logo_Bg_Color --
   -----------------------

   procedure Set_Logo_Bg_Color
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Color            : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Color            : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_start_set_logo_bg_color");
   begin
      Internal (Get_Object (Druid_Page_Start), Color);
   end Set_Logo_Bg_Color;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Text             : String)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Text             : String);
      pragma Import (C, Internal, "gnome_druid_page_start_set_text");
   begin
      Internal (Get_Object (Druid_Page_Start), Text & ASCII.NUL);
   end Set_Text;

   --------------------
   -- Set_Text_Color --
   --------------------

   procedure Set_Text_Color
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Color            : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Color            : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_start_set_text_color");
   begin
      Internal (Get_Object (Druid_Page_Start), Color);
   end Set_Text_Color;

   -----------------------
   -- Set_Textbox_Color --
   -----------------------

   procedure Set_Textbox_Color
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Color            : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Color            : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_start_set_textbox_color");
   begin
      Internal (Get_Object (Druid_Page_Start), Color);
   end Set_Textbox_Color;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Title            : String)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Title            : String);
      pragma Import (C, Internal, "gnome_druid_page_start_set_title");
   begin
      Internal (Get_Object (Druid_Page_Start), Title & ASCII.NUL);
   end Set_Title;

   ---------------------
   -- Set_Title_Color --
   ---------------------

   procedure Set_Title_Color
     (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
      Color            : Gdk.Color.Gdk_Color)
   is
      procedure Internal
        (Druid_Page_Start : System.Address;
         Color            : Gdk.Color.Gdk_Color);
      pragma Import (C, Internal, "gnome_druid_page_start_set_title_color");
   begin
      Internal (Get_Object (Druid_Page_Start), Color);
   end Set_Title_Color;

   -------------------
   -- Set_Watermark --
   -------------------

   --  procedure Set_Watermark
   --    (Druid_Page_Start : access Gnome_Druid_Page_Start_Record;
   --     Watermark        : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     procedure Internal
   --       (Druid_Page_Start : System.Address;
   --        Watermark        : GdkImlibImage);
   --     pragma Import (C, Internal, "gnome_druid_page_start_set_watermark");
   --  begin
   --     Internal (Get_Object (Druid_Page_Start),
   --               Watermark);
   --  end Set_Watermark;

end Gnome.Druid_Page_Start;
