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

package body Gdk.Rectangle is

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Rect : in Gdk_Rectangle) return Guint16 is
   begin
      return Rect.Height;
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Rect : in Gdk_Rectangle) return Guint16 is
   begin
      return Rect.Width;
   end Get_Width;

   -----------
   -- Get_X --
   -----------

   function Get_X (Rect : in Gdk_Rectangle) return Gint16 is
   begin
      return Rect.X;
   end Get_X;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Rect : in Gdk_Rectangle) return Gint16 is
   begin
      return Rect.Y;
   end Get_Y;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect (Src1      : in     Gdk_Rectangle;
                        Src2      : in     Gdk_Rectangle;
                        Dest      :    out Gdk_Rectangle;
                        Intersect :    out Boolean) is
      function Internal (Src1, Src2, Dest : in System.Address) return Gint;
      pragma Import (C, Internal, "gdk_rectangle_intersect");
   begin
      Intersect := To_Boolean (Internal (Src1'Address,
                                         Src2'Address,
                                         Dest'Address));
   end Intersect;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height (Rectangle : in out Gdk_Rectangle;
                         Height    : in     Guint16) is
   begin
      Rectangle.Height := Height;
   end Set_Height;

   ----------------
   -- Set_Values --
   ----------------

   procedure Set_Values (Rectangle : in out Gdk_Rectangle;
                         X         : in     Gint16;
                         Y         : in     Gint16;
                         Width     : in     Guint16;
                         Height    : in     Guint16) is
   begin
      Rectangle.X := X;
      Rectangle.Y := Y;
      Rectangle.Width := Width;
      Rectangle.Height := Height;
   end Set_Values;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width (Rectangle : in out Gdk_Rectangle;
                        Width     : in     Guint16) is
   begin
      Rectangle.Width := Width;
   end Set_Width;

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Rectangle : in out Gdk_Rectangle;
                    X         : in     Gint16) is
   begin
      Rectangle.X := X;
   end Set_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Rectangle : in out Gdk_Rectangle;
                    Y         : in     Gint16) is
   begin
      Rectangle.Y := Y;
   end Set_Y;

   function Ada_Gdk_Rectangle_Size return Guint;
   pragma Import (C, Ada_Gdk_Rectangle_Size);
begin
   pragma Assert (Gdk_Rectangle'Size / 8 = Ada_Gdk_Rectangle_Size);
   null;
end Gdk.Rectangle;
