-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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

package body Gdk.Rectangle is


   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Rectangle : in out Gdk_Rectangle'Class) is
      procedure Internal (Rectangle : in System.Address);
      pragma Import (C, Internal, "ada_gdk_rectangle_destroy");
   begin
      Internal (Get_Object (Rectangle));
      Set_Object (Rectangle, System.Null_Address);
   end Destroy;


   ------------------
   --  Get_Height  --
   ------------------

   function Get_Height (Rectangle : in Gdk_Rectangle) return Guint16 is
      function Internal (Rectangle : in System.Address) return Guint16;
      pragma Import (C, Internal, "ada_gdk_rectangle_get_height");
   begin
      return Internal (Get_Object (Rectangle));
   end Get_Height;


   ------------------
   --  Get_Values  --
   ------------------

   procedure Get_Values (Rectangle : in     Gdk_Rectangle;
                         X         :    out Gint16;
                         Y         :    out Gint16;
                         Width     :    out Guint16;
                         Height    :    out Guint16) is
   begin
      X := Get_X (Rectangle);
      Y := Get_Y (Rectangle);
      Width := Get_Width (Rectangle);
      Height := Get_Height (Rectangle);
   end Get_Values;


   -----------------
   --  Get_Width  --
   -----------------

   function Get_Width (Rectangle : in Gdk_Rectangle) return Guint16 is
      function Internal (Rectangle : in System.Address) return Guint16;
      pragma Import (C, Internal, "ada_gdk_rectangle_get_width");
   begin
      return Internal  (Get_Object (Rectangle));
   end Get_Width;


   -------------
   --  Get_X  --
   -------------

   function Get_X (Rectangle : in Gdk_Rectangle) return Gint16 is
      function Internal (Rectangle : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_rectangle_get_x");
   begin
      return Internal (Get_Object (Rectangle));
   end Get_X;


   -------------
   --  Get_Y  --
   -------------

   function Get_Y (Rectangle : in Gdk_Rectangle) return Gint16 is
      function Internal (Rectangle : in System.Address) return Gint16;
      pragma Import (C, Internal, "ada_gdk_rectangle_get_y");
   begin
      return Internal (Get_Object (Rectangle));
   end Get_Y;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Rectangle : out Gdk_Rectangle) is
   begin
      Gdk_New (Rectangle, 0, 0, 0, 0);
   end Gdk_New;


   ---------------
   --  Gdk_New  --
   ---------------

   procedure Gdk_New (Rectangle :    out Gdk_Rectangle;
                      X         : in     Gint16;
                      Y         : in     Gint16;
                      Width     : in     Guint16;
                      Height    : in     Guint16) is
      function Internal (X, Y : in Gint16; Width, Height : in Guint16)
                         return System.Address;
      pragma Import (C, Internal, "ada_gdk_rectangle_new_with_data");
   begin
      Set_Object (Rectangle, Internal (X, Y, Width, Height));
   end Gdk_New;


   -----------------
   --  Intersect  --
   -----------------

   procedure Intersect (Src1      : in     Gdk_Rectangle;
                        Src2      : in     Gdk_Rectangle;
                        Dest      : in out Gdk_Rectangle;
                        Intersect :    out Boolean) is
      function Internal (Src1, Src2, Dest : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_rectangle_intersect");
   begin
      Intersect := To_Boolean (Internal (Get_Object (Src1),
                                         Get_Object (Src2),
                                         Get_Object (Dest)));
   end Intersect;


   -------------------
   --   Set_Height  --
   -------------------

   procedure Set_Height (Rectangle : in out Gdk_Rectangle;
                         Height    : in     Guint16) is
      procedure Internal (Rectangle : in System.Address;
                          Height : in Guint16);
      pragma Import (C, Internal, "ada_gdk_rectangle_set_height");
   begin
      Internal (Get_Object (Rectangle), Height);
   end Set_Height;


   ------------------
   --  Set_Values  --
   ------------------

   procedure Set_Values (Rectangle : in out Gdk_Rectangle;
                         X         : in     Gint16;
                         Y         : in     Gint16;
                         Width     : in     Guint16;
                         Height    : in     Guint16) is
   begin
      Set_X (Rectangle => Rectangle, X => X);
      Set_Y (Rectangle => Rectangle, Y => Y);
      Set_Width (Rectangle => Rectangle, Width => Width);
      Set_Height (Rectangle => Rectangle, Height => Height);
   end Set_Values;


   -----------------
   --  Set_Width  --
   -----------------

   procedure Set_Width (Rectangle : in out Gdk_Rectangle;
                        Width     : in     Guint16) is
      procedure Internal (Rectangle : in System.Address; Width : in Guint16);
      pragma Import (C, Internal, "ada_gdk_rectangle_set_width");
   begin
      Internal (Get_Object (Rectangle), Width);
   end Set_Width;


   -------------
   --  Set_X  --
   -------------

   procedure Set_X (Rectangle : in out Gdk_Rectangle;
                    X         : in     Gint16) is
      procedure Internal (Rectangle : in System.Address; X : in Gint16);
      pragma Import (C, Internal, "ada_gdk_rectangle_set_x");
   begin
      Internal (Get_Object (Rectangle), X);
   end Set_X;


   -------------
   --  Set_Y  --
   -------------

   procedure Set_Y (Rectangle : in out Gdk_Rectangle;
                    Y         : in     Gint16) is
      procedure Internal (Rectangle : in System.Address; Y : in Gint16);
      pragma Import (C, Internal, "ada_gdk_rectangle_set_y");
   begin
      Internal (Get_Object (Rectangle), Y);
   end Set_Y;

end Gdk.Rectangle;
