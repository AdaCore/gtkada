------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gdk.Dmabuf_Formats is

   function From_Object_Free
     (B : access Gdk_Dmabuf_Formats'Class) return Gdk_Dmabuf_Formats
   is
      Result : constant Gdk_Dmabuf_Formats := Gdk_Dmabuf_Formats (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Dmabuf_Formats is
      S : Gdk_Dmabuf_Formats;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------
   -- Contains --
   --------------

   function Contains
      (Self     : Gdk_Dmabuf_Formats;
       Fourcc   : Guint32;
       Modifier : Guint64) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Fourcc   : Guint32;
          Modifier : Guint64) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_dmabuf_formats_contains");
   begin
      return Internal (Get_Object (Self), Fourcc, Modifier) /= 0;
   end Contains;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self     : Gdk_Dmabuf_Formats;
       Formats2 : Gdk_Dmabuf_Formats) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Formats2 : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_dmabuf_formats_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Formats2)) /= 0;
   end Equal;

   ----------------
   -- Get_Format --
   ----------------

   procedure Get_Format
      (Self     : Gdk_Dmabuf_Formats;
       Idx      : Gsize;
       Fourcc   : out Guint32;
       Modifier : out Guint64)
   is
      procedure Internal
         (Self     : System.Address;
          Idx      : Gsize;
          Fourcc   : out Guint32;
          Modifier : out Guint64);
      pragma Import (C, Internal, "gdk_dmabuf_formats_get_format");
   begin
      Internal (Get_Object (Self), Idx, Fourcc, Modifier);
   end Get_Format;

   -------------------
   -- Get_N_Formats --
   -------------------

   function Get_N_Formats (Self : Gdk_Dmabuf_Formats) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "gdk_dmabuf_formats_get_n_formats");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Formats;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Dmabuf_Formats) return Gdk_Dmabuf_Formats is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_dmabuf_formats_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Dmabuf_Formats) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_dmabuf_formats_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gdk.Dmabuf_Formats;
