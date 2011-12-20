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

package body Gdk.Pixmap is

   ----------------------
   -- Create_From_Data --
   ----------------------

   procedure Create_From_Data
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Data   : String;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint;
      Fg     : Color.Gdk_Color;
      Bg     : Color.Gdk_Color)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Data   : String;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint;
         Fg     : System.Address;
         Bg     : System.Address) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_data");

      use type Gdk.Color.Gdk_Color;

      Fg_Col : aliased Gdk.Color.Gdk_Color := Fg;
      Bg_Col : aliased Gdk.Color.Gdk_Color := Bg;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Fg_A : System.Address := Fg_Col'Address;
      Bg_A : System.Address := Bg_Col'Address;

   begin
      if Fg = Gdk.Color.Null_Color then
         Fg_A := System.Null_Address;
      end if;

      if Bg = Gdk.Color.Null_Color then
         Bg_A := System.Null_Address;
      end if;

      Pixmap := Internal
        (Window, Data & ASCII.NUL,
         Width, Height, Depth, Fg_A, Bg_A);
   end Create_From_Data;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Mask        : System.Address;
         Transparent : System.Address;
         Filename    : String) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap :=
        Internal
          (Window, Tmp'Address, Transparent_A, Filename & ASCII.NUL);
      Mask := Tmp;
   end Create_From_Xpm;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Filename    : String)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Colormap    : Gdk.Color.Gdk_Colormap;
         Mask        : System.Address;
         Transparent : System.Address;
         Filename    : String) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap :=
        Internal
          (Window => Window,
           Colormap => Colormap,
           Mask => Tmp'Address,
           Transparent => Transparent_A,
           Filename => Filename & ASCII.NUL);
      Mask := Tmp;
   end Create_From_Xpm;

   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Mask        : System.Address;
         Transparent : System.Address;
         Data        : Gtkada.Types.Chars_Ptr_Array) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm_d");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap := Internal (Window, Tmp'Address, Transparent_A, Data);
      Mask := Tmp;
   end Create_From_Xpm_D;

   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : Gdk.Window.Gdk_Window;
      Colormap    : Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : Gdk.Color.Gdk_Color;
      Data        : Gtkada.Types.Chars_Ptr_Array)
   is
      function Internal
        (Window      : Gdk.Window.Gdk_Window;
         Colormap    : Gdk.Color.Gdk_Colormap;
         Mask        : System.Address;
         Transparent : System.Address;
         Data        : Gtkada.Types.Chars_Ptr_Array) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm_d");

      use type Gdk.Color.Gdk_Color;

      Tmp : aliased Gdk.Bitmap.Gdk_Bitmap := Mask;
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;

      Pixmap := Internal (Window, Colormap, Tmp'Address, Transparent_A, Data);
      Mask := Tmp;
   end Create_From_Xpm_D;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
     (Pixmap : out Gdk_Pixmap;
      Window : Gdk.Window.Gdk_Window;
      Width  : Gint;
      Height : Gint;
      Depth  : Gint := -1)
   is
      function Internal
        (Window : Gdk.Window.Gdk_Window;
         Width  : Gint;
         Height : Gint;
         Depth  : Gint) return Gdk_Pixmap;
      pragma Import (C, Internal, "gdk_pixmap_new");

   begin
      Pixmap := Internal (Window, Width, Height, Depth);
   end Gdk_New;

end Gdk.Pixmap;
