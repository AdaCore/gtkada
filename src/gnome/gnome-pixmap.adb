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

--  with Gdk.ImlibColor;
--  with Gdk.ImlibImage;
with Gtk; use Gtk;
with Gtkada.Types; use Gtkada.Types;
with System;

package body Gnome.Pixmap is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Xpm_Data, Width, Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer)
   is
      function Internal
        (Xpm_Data : Chars_Ptr_Array;
         Width    : Integer;
         Height   : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_xpm_d_at_size");
   begin
      Set_Object (Widget, Internal (Xpm_Data + Null_Ptr, Width, Height));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget  : out Gnome_Pixmap;
                        Gpixmap : access Gnome_Pixmap_Record)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Gpixmap);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget  : access Gnome_Pixmap_Record'Class;
                         Gpixmap : access Gnome_Pixmap_Record)
   is
      function Internal (Gpixmap : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_gnome_pixmap");
   begin
      Set_Object (Widget, Internal (Get_Object (Gpixmap)));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget   : out Gnome_Pixmap;
                        Xpm_Data : Chars_Ptr_Array)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Xpm_Data);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget   : access Gnome_Pixmap_Record'Class;
                         Xpm_Data : Chars_Ptr_Array)
   is
      function Internal (Xpm_Data : Chars_Ptr_Array)
                         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_xpm_d");
   begin
      Set_Object (Widget, Internal (Xpm_Data + Null_Ptr));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New
   --    (Widget      : out Gnome_Pixmap;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --  begin
   --     Widget := new Gnome_Pixmap_Record;
   --     Initialize (Widget, Data, Alpha, Rgb_Width, Rgb_Height, Shape_Color);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize
   --    (Widget      : access Gnome_Pixmap_Record'Class;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --     function Internal
   --       (Data        : String;
   --        Alpha       : String;
   --        Rgb_Width   : Integer;
   --        Rgb_Height  : Integer;
   --        Shape_Color : GdkImlibColor)
   --        return System.Address;
   --     pragma Import (C, Internal, "gnome_pixmap_new_from_rgb_d_shaped");
   --  begin
   --     Set_Object (Widget, Internal (Data & ASCII.NUL,
   --                                   Alpha & ASCII.NUL,
   --                                   Rgb_Width,
   --                                   Rgb_Height,
   --                                   Shape_Color));
   --  end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New
   --    (Widget : out Gnome_Pixmap;
   --     Im     : Gdk.ImlibImage.Gdk_ImlibImage;
   --     Width  : Integer;
   --     Height : Integer)
   --  is
   --  begin
   --     Widget := new Gnome_Pixmap_Record;
   --     Initialize (Widget, Im, Width, Height);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize
   --    (Widget : access Gnome_Pixmap_Record'Class;
   --     Im     : Gdk.ImlibImage.Gdk_ImlibImage;
   --     Width  : Integer;
   --     Height : Integer)
   --  is
   --     function Internal
   --       (Im     : GdkImlibImage;
   --        Width  : Integer;
   --        Height : Integer)
   --        return System.Address;
   --     pragma Import (C, Internal, "gnome_pixmap_new_from_imlib_at_size");
   --  begin
   --     Set_Object (Widget, Internal (Im,
   --                                   Width,
   --                                   Height));
   --  end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget     : out Gnome_Pixmap;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer;
      Width      : Integer;
      Height     : Integer)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Data, Alpha, Rgb_Width, Rgb_Height, Width, Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget     : access Gnome_Pixmap_Record'Class;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer;
      Width      : Integer;
      Height     : Integer)
   is
      function Internal
        (Data       : String;
         Alpha      : String;
         Rgb_Width  : Integer;
         Rgb_Height : Integer;
         Width      : Integer;
         Height     : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_rgb_d_at_size");
   begin
      Set_Object (Widget, Internal (Data & ASCII.NUL,
                                    Alpha & ASCII.NUL,
                                    Rgb_Width,
                                    Rgb_Height,
                                    Width,
                                    Height));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New
   --    (Widget      : out Gnome_Pixmap;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Width       : Integer;
   --     Height      : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --  begin
   --     Widget := new Gnome_Pixmap_Record;
   --     Initialize
   --       (Widget, Data, Alpha, Rgb_Width, Rgb_Height,
   --        Width, Height, Shape_Color);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize
   --    (Widget      : access Gnome_Pixmap_Record'Class;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Width       : Integer;
   --     Height      : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --     function Internal
   --       (Data        : String;
   --        Alpha       : String;
   --        Rgb_Width   : Integer;
   --        Rgb_Height  : Integer;
   --        Width       : Integer;
   --        Height      : Integer;
   --        Shape_Color : GdkImlibColor)
   --        return System.Address;
   --     pragma Import
   --       (C, Internal, "gnome_pixmap_new_from_rgb_d_shaped_at_size");
   --  begin
   --     Set_Object (Widget, Internal (Data & ASCII.NUL,
   --                                   Alpha & ASCII.NUL,
   --                                   Rgb_Width,
   --                                   Rgb_Height,
   --                                   Width,
   --                                   Height,
   --                                   Shape_Color));
   --  end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget   : out Gnome_Pixmap;
                        Filename : String)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Filename);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget   : access Gnome_Pixmap_Record'Class;
                         Filename : String)
   is
      function Internal (Filename : String)
                         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_file");
   begin
      Set_Object (Widget, Internal (Filename & ASCII.NUL));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget     : out Gnome_Pixmap;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Data, Alpha, Rgb_Width, Rgb_Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget     : access Gnome_Pixmap_Record'Class;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer)
   is
      function Internal
        (Data       : String;
         Alpha      : String;
         Rgb_Width  : Integer;
         Rgb_Height : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_rgb_d");
   begin
      Set_Object (Widget, Internal (Data & ASCII.NUL,
                                    Alpha & ASCII.NUL,
                                    Rgb_Width,
                                    Rgb_Height));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget   : out Gnome_Pixmap;
      Filename : String;
      Width    : Integer;
      Height   : Integer)
   is
   begin
      Widget := new Gnome_Pixmap_Record;
      Initialize (Widget, Filename, Width, Height);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Gnome_Pixmap_Record'Class;
      Filename : String;
      Width    : Integer;
      Height   : Integer)
   is
      function Internal
        (Filename : String;
         Width    : Integer;
         Height   : Integer)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_new_from_file_at_size");
   begin
      Set_Object (Widget, Internal (Filename & ASCII.NUL,
                                    Width,
                                    Height));
   end Initialize;

   ---------------
   -- Gnome_New --
   ---------------

   --  procedure Gnome_New (Widget: out Gnome_Pixmap;
   --                       Im : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --  begin
   --     Widget := new Gnome_Pixmap_Record;
   --     Initialize (Widget, Im);
   --  end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   --  procedure Initialize (Widget: access Gnome_Pixmap_Record'Class;
   --                        Im : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     function Internal (Im     : GdkImlibImage)
   --                        return System.Address;
   --     pragma Import (C, Internal, "gnome_pixmap_new_from_imlib");
   --  begin
   --     Set_Object (Widget, Internal (Im));
   --  end Initialize;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Filename : String);
      pragma Import (C, Internal, "gnome_pixmap_load_file");
   begin
      Internal (Get_Object (Gpixmap),
                Filename & ASCII.NUL);
   end Load_File;

   -----------------------
   -- Load_File_At_Size --
   -----------------------

   procedure Load_File_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Filename : String;
      Width    : Integer;
      Height   : Integer)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Filename : String;
         Width    : Integer;
         Height   : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_file_at_size");
   begin
      Internal (Get_Object (Gpixmap),
                Filename & ASCII.NUL,
                Width,
                Height);
   end Load_File_At_Size;

   ----------------
   -- Load_Imlib --
   ----------------

   --  procedure Load_Imlib
   --    (Gpixmap : access Gnome_Pixmap_Record;
   --     Im      : Gdk.ImlibImage.Gdk_ImlibImage)
   --  is
   --     procedure Internal
   --       (Gpixmap : System.Address;
   --        Im      : GdkImlibImage);
   --     pragma Import (C, Internal, "gnome_pixmap_load_imlib");
   --  begin
   --     Internal (Get_Object (Gpixmap),
   --               Im);
   --  end Load_Imlib;

   ------------------------
   -- Load_Imlib_At_Size --
   ------------------------

   --  procedure Load_Imlib_At_Size
   --    (Gpixmap : access Gnome_Pixmap_Record;
   --     Im      : Gdk.ImlibImage.Gdk_ImlibImage;
   --     Width   : Integer;
   --     Height  : Integer)
   --  is
   --     procedure Internal
   --       (Gpixmap : System.Address;
   --        Im      : GdkImlibImage;
   --        Width   : Integer;
   --        Height  : Integer);
   --     pragma Import (C, Internal, "gnome_pixmap_load_imlib_at_size");
   --  begin
   --     Internal (Get_Object (Gpixmap),
   --               Im,
   --               Width,
   --               Height);
   --  end Load_Imlib_At_Size;

   ----------------
   -- Load_Rgb_D --
   ----------------

   procedure Load_Rgb_D
     (Gpixmap    : access Gnome_Pixmap_Record;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer)
   is
      procedure Internal
        (Gpixmap    : System.Address;
         Data       : String;
         Alpha      : String;
         Rgb_Width  : Integer;
         Rgb_Height : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_rgb_d");
   begin
      Internal (Get_Object (Gpixmap),
                Data & ASCII.NUL,
                Alpha & ASCII.NUL,
                Rgb_Width,
                Rgb_Height);
   end Load_Rgb_D;

   ------------------------
   -- Load_Rgb_D_At_Size --
   ------------------------

   procedure Load_Rgb_D_At_Size
     (Gpixmap    : access Gnome_Pixmap_Record;
      Data       : String;
      Alpha      : String;
      Rgb_Width  : Integer;
      Rgb_Height : Integer;
      Width      : Integer;
      Height     : Integer)
   is
      procedure Internal
        (Gpixmap    : System.Address;
         Data       : String;
         Alpha      : String;
         Rgb_Width  : Integer;
         Rgb_Height : Integer;
         Width      : Integer;
         Height     : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_rgb_d_at_size");
   begin
      Internal (Get_Object (Gpixmap),
                Data & ASCII.NUL,
                Alpha & ASCII.NUL,
                Rgb_Width,
                Rgb_Height,
                Width,
                Height);
   end Load_Rgb_D_At_Size;

   -----------------------
   -- Load_Rgb_D_Shaped --
   -----------------------

   --  procedure Load_Rgb_D_Shaped
   --    (Gpixmap     : access Gnome_Pixmap_Record;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --     procedure Internal
   --       (Gpixmap     : System.Address;
   --        Data        : String;
   --        Alpha       : String;
   --        Rgb_Width   : Integer;
   --        Rgb_Height  : Integer;
   --        Shape_Color : GdkImlibColor);
   --     pragma Import (C, Internal, "gnome_pixmap_load_rgb_d_shaped");
   --  begin
   --     Internal (Get_Object (Gpixmap),
   --               Data & ASCII.NUL,
   --               Alpha & ASCII.NUL,
   --               Rgb_Width,
   --               Rgb_Height,
   --               Shape_Color);
   --  end Load_Rgb_D_Shaped;

   -------------------------------
   -- Load_Rgb_D_Shaped_At_Size --
   -------------------------------

   --  procedure Load_Rgb_D_Shaped_At_Size
   --    (Gpixmap     : access Gnome_Pixmap_Record;
   --     Data        : String;
   --     Alpha       : String;
   --     Rgb_Width   : Integer;
   --     Rgb_Height  : Integer;
   --     Width       : Integer;
   --     Height      : Integer;
   --     Shape_Color : Gdk.ImlibColor.Gdk_ImlibColor)
   --  is
   --     procedure Internal
   --       (Gpixmap     : System.Address;
   --        Data        : String;
   --        Alpha       : String;
   --        Rgb_Width   : Integer;
   --        Rgb_Height  : Integer;
   --        Width       : Integer;
   --        Height      : Integer;
   --        Shape_Color : GdkImlibColor);
   --     pragma Import
   --       (C, Internal, "gnome_pixmap_load_rgb_d_shaped_at_size");
   --  begin
   --     Internal (Get_Object (Gpixmap),
   --               Data & ASCII.NUL,
   --               Alpha & ASCII.NUL,
   --               Rgb_Width,
   --               Rgb_Height,
   --               Width,
   --               Height,
   --               Shape_Color);
   --  end Load_Rgb_D_Shaped_At_Size;

   ----------------
   -- Load_Xpm_D --
   ----------------

   procedure Load_Xpm_D
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Xpm_Data : Chars_Ptr_Array);
      pragma Import (C, Internal, "gnome_pixmap_load_xpm_d");
   begin
      Internal (Get_Object (Gpixmap), Xpm_Data);
   end Load_Xpm_D;

   ------------------------
   -- Load_Xpm_D_At_Size --
   ------------------------

   procedure Load_Xpm_D_At_Size
     (Gpixmap  : access Gnome_Pixmap_Record;
      Xpm_Data : Chars_Ptr_Array;
      Width    : Integer;
      Height   : Integer)
   is
      procedure Internal
        (Gpixmap  : System.Address;
         Xpm_Data : Chars_Ptr_Array;
         Width    : Integer;
         Height   : Integer);
      pragma Import (C, Internal, "gnome_pixmap_load_xpm_d_at_size");
   begin
      Internal (Get_Object (Gpixmap), Xpm_Data, Width, Height);
   end Load_Xpm_D_At_Size;

end Gnome.Pixmap;
