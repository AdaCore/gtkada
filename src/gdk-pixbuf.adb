-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with System;

package body Gdk.Pixbuf is

   ---------------
   -- Add_Alpha --
   ---------------

   function Add_Alpha
     (Pixbuf           : Gdk_Pixbuf;
      Substitute_Color : Boolean := False;
      Red              : Guchar := 0;
      Green            : Guchar := 0;
      Blue             : Guchar := 0) return Gdk_Pixbuf
   is
      function Internal
        (Pixbuf           : Gdk_Pixbuf;
         Substitute_Color : Gboolean;
         Red              : Guchar;
         Green            : Guchar;
         Blue             : Guchar) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_add_alpha");

   begin
      return Internal
        (Pixbuf, To_Gboolean (Substitute_Color), Red, Green, Blue);
   end Add_Alpha;

   ----------
   -- Copy --
   ----------

   function Copy (Pixbuf : Gdk_Pixbuf) return Gdk_Pixbuf is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_copy");

   begin
      return Internal (Pixbuf);
   end Copy;

   ----------
   -- Copy --
   ----------

   function Copy (Src : Gdk_Pixbuf_Frame) return Gdk_Pixbuf_Frame is
      function Internal (Pixbuf : Gdk_Pixbuf_Frame) return Gdk_Pixbuf_Frame;
      pragma Import (C, Internal, "gdk_pixbuf_frame_copy");

   begin
      return Internal (Src);
   end Copy;

   -------------
   -- Gdk_New --
   -------------

   function Gdk_New
     (Colorspace      : Gdk_Colorspace := Colorspace_RGB;
      Has_Alpha       : Boolean := False;
      Bits_Per_Sample : Gint := 8;
      Width           : Gint;
      Height          : Gint) return Gdk_Pixbuf
   is
      function Internal
        (Colorspace      : Gdk_Colorspace;
         Has_Alpha       : Gboolean;
         Bits_Per_Sample : Gint;
         Width           : Gint;
         Height          : Gint) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_new");

   begin
      return Internal
        (Colorspace, To_Gboolean (Has_Alpha), Bits_Per_Sample, Width, Height);
   end Gdk_New;

   -----------------------
   -- Gdk_New_From_File --
   -----------------------

   procedure Gdk_New_From_File
     (Pixbuf   : out Gdk_Pixbuf;
      Filename : String;
      Error    : out GError)
   is
      function Internal
        (Filename : String;
         Error    : access GError) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_file");

      Err : aliased GError;

   begin
      Pixbuf := Internal (Filename & ASCII.NUL, Err'Access);
      Error := Err;
   end Gdk_New_From_File;

   -----------------------
   -- Gdk_New_From_File --
   -----------------------

   procedure Gdk_New_From_File
     (Animation : out Gdk_Pixbuf_Animation;
      Filename  : String;
      Error     : out GError)
   is
      function Internal
        (Filename : String;
         Error    : access GError) return Gdk_Pixbuf_Animation;
      pragma Import (C, Internal, "gdk_pixbuf_animation_new_from_file");

      Err : aliased GError;

   begin
      Animation := Internal (Filename & ASCII.NUL, Err'Access);
      Error := Err;
   end Gdk_New_From_File;

   -------------------------
   -- Gdk_New_From_Inline --
   -------------------------

   procedure Gdk_New_From_Inline
     (Pixbuf        : out Gdk_Pixbuf;
      Inline_Pixbuf : Guchar_Array;
      Copy_Pixels   : Boolean;
      Length        : Gint;
      Error         : out GError)
   is
      function Internal
        (Inline_Pixbuf : Guchar_Array;
         Copy_Pixels   : Gboolean;
         Length        : Gint;
         Error         : access GError) return Gdk_Pixbuf;
      pragma Import (C, Internal, "gdk_pixbuf_new_from_inline");

      Err : aliased GError;

   begin
      Pixbuf := Internal
        (Inline_Pixbuf, To_Gboolean (Copy_Pixels), Length, Err'Access);
      Error := Err;
   end Gdk_New_From_Inline;

   ----------------
   -- Get_Frames --
   ----------------

   function Get_Frames
     (Animation : Gdk_Pixbuf_Animation) return Frame_List.Glist
   is
      function Internal
        (Animation : Gdk_Pixbuf_Animation) return System.Address;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_frames");

      List : Frame_List.Glist;

   begin
      Frame_List.Set_Object (List, Internal (Animation));
      return List;
   end Get_Frames;

   -------------------
   -- Get_Has_Alpha --
   -------------------

   function Get_Has_Alpha (Pixbuf : Gdk_Pixbuf) return Boolean is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gboolean;
      pragma Import (C, Internal, "gdk_pixbuf_get_has_alpha");

   begin
      return To_Boolean (Internal (Pixbuf));
   end Get_Has_Alpha;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_height");

   begin
      return Internal (Pixbuf);
   end Get_Height;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Animation : Gdk_Pixbuf_Animation) return Gint is
      function Internal (Animation : Gdk_Pixbuf_Animation) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_height");

   begin
      return Internal (Animation);
   end Get_Height;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Pixbuf : Gdk_Pixbuf) return Gint is
      function Internal (Pixbuf : Gdk_Pixbuf) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_get_width");

   begin
      return Internal (Pixbuf);
   end Get_Width;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Animation : Gdk_Pixbuf_Animation) return Gint is
      function Internal (Animation : Gdk_Pixbuf_Animation) return Gint;
      pragma Import (C, Internal, "gdk_pixbuf_animation_get_width");

   begin
      return Internal (Animation);
   end Get_Width;

   ---------
   -- Ref --
   ---------

   procedure Ref (Pixbuf : Gdk_Pixbuf) is
      procedure Internal (Pixbuf : Gdk_Pixbuf);
      pragma Import (C, Internal, "gdk_pixbuf_ref");

   begin
      Internal (Pixbuf);
   end Ref;

   ---------
   -- Ref --
   ---------

   procedure Ref (Animation : Gdk_Pixbuf_Animation) is
      procedure Internal (Pixbuf : Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gdk_pixbuf_animation_ref");

   begin
      Internal (Animation);
   end Ref;

   ---------------------------
   -- Saturate_And_Pixelate --
   ---------------------------

   procedure Saturate_And_Pixelate
     (Src        : Gdk_Pixbuf;
      Dest       : Gdk_Pixbuf;
      Saturation : Gfloat;
      Pixelate   : Boolean := True)
   is
      procedure Internal
        (Src        : Gdk_Pixbuf;
         Dest       : Gdk_Pixbuf;
         Saturation : Gfloat;
         Pixelate   : Gboolean);
      pragma Import (C, Internal, "gdk_pixbuf_saturate_and_pixelate");

   begin
      Internal (Src, Dest, Saturation, To_Gboolean (Pixelate));
   end Saturate_And_Pixelate;

   ----------
   -- Save --
   ----------

   procedure Save
     (Pixbuf   : Gdk_Pixbuf;
      Filename : String;
      Format   : File_Format;
      Error    : out GError;
      Quality  : Image_Quality := Image_Quality'Last)
   is
      procedure Internal
        (Pixbuf   : Gdk_Pixbuf;
         Filename : String;
         Format   : String;
         Error    : out GError);

      procedure Internal
        (Pixbuf   : Gdk_Pixbuf;
         Filename : String;
         Format   : String;
         Error    : out GError;
         Key      : String;
         Value    : String;
         Term     : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gdk_pixbuf_save");

   begin
      case Format is
         when JPEG =>
            Internal
              (Pixbuf,
               Filename & ASCII.NUL,
               "jpeg" & ASCII.NUL,
               Error,
               "quality" & ASCII.NUL,
               Image_Quality'Image (Quality) & ASCII.NUL);

         when PNG =>
            Internal (Pixbuf, Filename & ASCII.NUL, "png" & ASCII.NUL, Error);
      end case;
   end Save;

   -----------
   -- Unref --
   -----------

   procedure Unref (Pixbuf : Gdk_Pixbuf) is
      procedure Internal (Pixbuf : Gdk_Pixbuf);
      pragma Import (C, Internal, "gdk_pixbuf_unref");

   begin
      Internal (Pixbuf);
   end Unref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Animation : Gdk_Pixbuf_Animation) is
      procedure Internal (Pixbuf : Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gdk_pixbuf_animation_unref");

   begin
      Internal (Animation);
   end Unref;

end Gdk.Pixbuf;

