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

with Glib; use Glib;

package Gdk.Font is

   type Gdk_Font is new Root_Type with private;
   Null_Font : constant Gdk_Font;

   procedure Load (Font      :    out Gdk_Font;
                   Font_Name : in     String);

   procedure Fontset_Load (Font         :   out Gdk_Font;
                           Fontset_Name : in    String);

   procedure Ref (Font : in out Gdk_Font);

   procedure Unref (Font : in out Gdk_Font);

   function Id (Font : in Gdk_Font) return Gint;

   function "=" (Fonta, Fontb : in Gdk_Font) return Boolean;

   function String_Width (Font : in Gdk_Font;
                          Str  : in String) return Gint;

   function Text_Width (Font : in Gdk_Font;
                        Text : in String) return Gint;

   function Char_Width (Font : in Gdk_Font;
                        Char : in Character) return Gint;

   function String_Measure (Font : in Gdk_Font;
                            Str  : in String) return Gint;

   function Text_Measure (Font : in Gdk_Font;
                          Text : in String) return Gint;

   function Char_Measure (Font : in Gdk_Font;
                          Char : in Character) return Gint;

private

   type Gdk_Font is new Root_Type with null record;
   Null_Font : constant Gdk_Font := (Ptr => System.Null_Address);


end Gdk.Font;
