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

package body Gdk.Bitmap is

   ------------------------
   --  Create_From_Data  --
   ------------------------

   procedure Create_From_Data (Bitmap :    out Gdk_Bitmap;
                               Window : in     Gdk.Window.Gdk_Window'Class;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint) is
      function Internal (Window        : in System.Address;
                         Data          : in String;
                         Width, Height : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_bitmap_create_from_data");
   begin
      Set_Object (Bitmap, Internal (Get_Object (Window), Data & ASCII.NUL,
                                    Width, Height));
   end Create_From_Data;

   ---------------------
   --  Set_Clip_Mask  --
   ---------------------

   procedure Set_Clip_Mask (GC    : in out Gdk.GC.Gdk_GC'Class;
                            Mask  : in     Gdk_Bitmap) is
      procedure Internal (GC, Mask : in System.Address);
      pragma Import (C, Internal, "gdk_gc_set_clip_mask");
   begin
      Internal (Get_Object (GC), Get_Object (Mask));
   end Set_Clip_Mask;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Bitmap : out Gdk_Bitmap;
                      Window : in  Gdk.Window.Gdk_Window'Class;
                      Width  : in  Gint;
                      Height : in  Gint) is
      function Internal (Window : in System.Address;
                         Width  : in Gint;
                         Height : in Gint;
                         Depth  : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_pixmap_new");
   begin
      Set_Object (Bitmap, Internal (Get_Object (Window),
                                    Width, Height, 1));

   end Gdk_New;

   -----------------
   -- Null_Bitmap --
   -----------------

   function Null_Bitmap return Gdk_Bitmap is
      Bitmap : Gdk_Bitmap;
   begin
      return Bitmap;
   end Null_Bitmap;

   ------------------
   -- Unref_Bitmap --
   ------------------

   procedure Unref_Bitmap (Bitmap : in out Gdk_Bitmap) is
      procedure Internal (Pixmap : in System.Address);
      pragma Import (C, Internal, "gdk_bitmap_unref");
   begin
      Internal (Get_Object (Bitmap));
      Set_Object (Bitmap, System.Null_Address);
   end Unref_Bitmap;

end Gdk.Bitmap;
