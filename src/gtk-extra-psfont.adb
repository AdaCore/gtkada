-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Gdk.Font;
with Interfaces.C.Strings;
with System;

package body Gtk.Extra.PsFont is

   ---------
   -- Add --
   ---------

   procedure Add (Fontname : in String;
                  Psname   : in String;
                  Family   : in String;
                  Xstring  : in Gtkada.Types.Chars_Ptr_Array;
                  Italic   : in Boolean;
                  Bold     : in Boolean)
   is
      procedure Internal (Fontname : in String;
                          Psname   : in String;
                          Family   : in String;
                          Xstring  : in System.Address;
                          Italic   : in Gint;
                          Bold     : in Gint);
      pragma Import (C, Internal, "gtk_psfont_add_font");
   begin
      Internal (Fontname & ASCII.NUL,
                Psname & ASCII.NUL,
                Family & ASCII.NUL,
                Xstring (Xstring'First)'Address,
                Boolean'Pos (Italic),
                Boolean'Pos (Bold));
   end Add;

   --------------------
   -- Find_By_Family --
   --------------------

   function Find_By_Family (Name   : in String;
                            Italic : in Boolean;
                            Bold   : in Boolean)
                           return      Gtk_PsFont
   is
      function Internal (Name   : in String;
                         Italic : in Gint;
                         Bold   : in Gint)
                        return      Gtk_PsFont;
      pragma Import (C, Internal, "gtk_psfont_find_by_family");
   begin
      return Internal (Name & ASCII.NUL,
                       Boolean'Pos (Italic),
                       Boolean'Pos (Bold));
   end Find_By_Family;

   -----------------
   -- Get_Gdkfont --
   -----------------

   function Get_Gdkfont (Name   : in String;
                         Height : in Gint)
                        return      Gdk.Font.Gdk_Font
   is
      function Internal (Name   : in String;
                         Height : in Gint)
                        return      Gdk.Font.Gdk_Font;
      pragma Import (C, Internal, "gtk_psfont_get_gdkfont");
   begin
      return Internal (Name & ASCII.NUL, Height);
   end Get_Gdkfont;

   --------------------
   -- Get_Psfontname --
   --------------------

   function Get_Psfontname (Name   : in String)
                            return      String
   is
      function Internal (Name   : in String)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_psfont_get_psfontname");
   begin
      return Interfaces.C.Strings.Value (Internal (Name & ASCII.NUL));
   end Get_Psfontname;

   -------------
   -- Getfont --
   -------------

   function Getfont (Name   : in String) return Gtk_PsFont is
      function Internal (Name   : in String) return Gtk_PsFont;
      pragma Import (C, Internal, "gtk_psfont_get_font");
   begin
      return Internal (Name & ASCII.NUL);
   end Getfont;

   ----------------
   -- Get_Psname --
   ----------------

   function Get_Psname (Font : Gtk_PsFont) return String is
      function Internal (Font : Gtk_PsFont)
                        return  Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_psfont_get_psname");
   begin
      return Interfaces.C.Strings.Value (Internal (Font));
   end Get_Psname;

end Gtk.Extra.PsFont;
