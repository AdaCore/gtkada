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

package body Gdk.Pixmap is


   ------------------------
   --  Create_From_Data  --
   ------------------------

   procedure Create_From_Data (Pixmap :    out Gdk_Pixmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint;
                               Depth  : in     Gint;
                               Fg     : in     Color.Gdk_Color;
                               Bg     : in     Color.Gdk_Color) is
      function Internal (Window : in System.Address;
                         Data   : in String;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint;
                         Fg     : in System.Address;
                         Bg     : in System.Address) return System.Address;
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
      Set_Object (Pixmap, Internal (Get_Object (Window), Data & ASCII.NUL,
                                    Width, Height, Depth,
                                    Fg_A, Bg_A));
   end Create_From_Data;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm (Pixmap      :    out Gdk_Pixmap;
                              Window      : in     Gdk.Window.Gdk_Window'Class;
                              Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in     Gdk.Color.Gdk_Color;
                              Filename    : in     String)
   is
      function Internal (Window      : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Filename    : in String)
                         return           System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm");
      use type Gdk.Color.Gdk_Color;
      Tmp : aliased System.Address := Get_Object (Mask);

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;
   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;
      Set_Object (Pixmap, Internal (Get_Object (Window), Tmp'Address,
                                    Transparent_A,
                                    Filename & ASCII.Nul));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm;


   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm (Pixmap      :    out Gdk_Pixmap;
                              Window      : in     Gdk.Window.Gdk_Window'Class;
                              Colormap    : in     Gdk.Color.Gdk_Colormap;
                              Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in     Gdk.Color.Gdk_Color;
                              Filename    : in     String) is
      function Internal (Window      : in System.Address;
                         Colormap    : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Filename    : in String)
                         return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm");
      use type Gdk.Color.Gdk_Color;
      Tmp : aliased System.Address := Get_Object (Mask);

      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;
   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;
      Set_Object (Pixmap, Internal (Window => Get_Object (Window),
                                    Colormap => Get_Object (Colormap),
                                    Mask => Tmp'Address,
                                    Transparent => Transparent_A,
                                    Filename => Filename & ASCII.Nul));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm;


   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      :    out Gdk_Pixmap;
      Window      : in     Gdk.Window.Gdk_Window'Class;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in     Gdk.Color.Gdk_Color;
      Data        : in     Interfaces.C.Strings.chars_ptr_array)
   is
      function Internal (Window      : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Data        : in Interfaces.C.Strings.chars_ptr_array)
                         return           System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm_d");
      use type Gdk.Color.Gdk_Color;
      Tmp : aliased System.Address;
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;

   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;
      Set_Object (Pixmap, Internal (Get_Object (Window),
                                    Tmp'Address,
                                    Transparent_A,
                                    Data));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm_D;


   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      :    out Gdk_Pixmap;
      Window      : in     Gdk.Window.Gdk_Window'Class;
      Colormap    : in     Gdk.Color.Gdk_Colormap;
      Mask        : in out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in     Gdk.Color.Gdk_Color;
      Data        : in     Interfaces.C.Strings.chars_ptr_array) is
      function Internal (Window      : in System.Address;
                         Colormap    : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Data        : in Interfaces.C.Strings.chars_ptr_array)
                         return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_colormap_create_from_xpm_d");
      use type Gdk.Color.Gdk_Color;
      Tmp : aliased System.Address := Get_Object (Mask);
      Transp_Col : aliased Gdk.Color.Gdk_Color := Transparent;
      --  Need to use a local variable to avoid problems with 'Address if
      --  the parameter is passed in a register for instance.

      Transparent_A : System.Address := Transp_Col'Address;
   begin
      if Transparent = Gdk.Color.Null_Color then
         Transparent_A := System.Null_Address;
      end if;
      Set_Object (Pixmap, Internal (Get_Object (Window),
                                    Get_Object (Colormap),
                                    Tmp'Address,
                                    Transparent_A,
                                    Data));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm_D;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window'Class;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint := -1) is
      function Internal (Window : in System.Address;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_new");
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window),
                                    Width, Height, Depth));
   end Gdk_New;

   ---------
   -- Ref --
   ---------

   procedure Ref (Pixmap : in Gdk_Pixmap) is
      function Internal (Pixmap : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_ref");
      S : System.Address;
   begin
      S := Internal (Get_Object (Pixmap));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Pixmap : in out Gdk_Pixmap) is
      procedure Internal (Pixmap : in System.Address);
      pragma Import (C, Internal, "gdk_pixmap_unref");
   begin
      Internal (Get_Object (Pixmap));
      Set_Object (Pixmap, System.Null_Address);
   end Unref;

end Gdk.Pixmap;
