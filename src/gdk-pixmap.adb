-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
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
                               Fg     : in     Color.Gdk_Color'Class;
                               Bg     : in     Color.Gdk_Color'Class) is
      function Internal (Window : in System.Address;
                         Data   : in String;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint;
                         Fg     : in System.Address;
                         Bg     : in System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_data");
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window), Data & ASCII.NUL,
                                    Width, Height, Depth,
                                    Get_Object (Fg), Get_Object (Bg)));
   end Create_From_Data;

   ---------------------
   -- Create_From_Xpm --
   ---------------------

   procedure Create_From_Xpm (Pixmap      : out Gdk_Pixmap;
                              Window      : in  Gdk.Window.Gdk_Window'Class;
                              Mask        : out Gdk.Bitmap.Gdk_Bitmap;
                              Transparent : in  Gdk.Color.Gdk_Color'Class;
                              Filename    : in  String)
   is
      function Internal (Window      : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Filename    : in String)
                         return           System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm");
      Tmp : System.Address;
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window), Tmp'Address,
                                    Get_Object (Transparent),
                                    Filename & Ascii.NUL));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm;

   -----------------------
   -- Create_From_Xpm_D --
   -----------------------

   procedure Create_From_Xpm_D
     (Pixmap      : out Gdk_Pixmap;
      Window      : in  Gdk.Window.Gdk_Window'Class;
      Mask        : out Gdk.Bitmap.Gdk_Bitmap;
      Transparent : in  Gdk.Color.Gdk_Color'Class;
      Data        : in  Interfaces.C.Strings.chars_ptr_array)
   is
      function Internal (Window      : in System.Address;
                         Mask        : in System.Address;
                         Transparent : in System.Address;
                         Data        : in System.Address)
                         return           System.Address;
      pragma Import (C, Internal, "gdk_pixmap_create_from_xpm_d");
      Tmp : System.Address;
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window), Tmp'Address,
                                    Get_Object (Transparent),
                                    Data (0)'Address));
      Set_Object (Mask, Tmp);
   end Create_From_Xpm_D;

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Pixmap :    out Gdk_Pixmap;
                      Window : in     Gdk.Window.Gdk_Window;
                      Width  : in     Gint;
                      Height : in     Gint;
                      Depth  : in     Gint) is
      function Internal (Window : in System.Address;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_new");
   begin
      Set_Object (Pixmap, Internal (Get_Object (Window),
                                    Width, Height, Depth));
   end Gtk_New;

end Gdk.Pixmap;
