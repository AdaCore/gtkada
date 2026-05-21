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

--  Provides information about supported DMA buffer formats.
--
--  You can query whether a given format is supported with
--  [methodGdk.DmabufFormats.contains] and you can iterate over the list of all
--  supported formats with [methodGdk.DmabufFormats.get_n_formats] and
--  [methodGdk.DmabufFormats.get_format].
--
--  The list of supported formats is sorted by preference, with the best
--  formats coming first.
--
--  The list may contains (format, modifier) pairs where the modifier is
--  `DMA_FORMAT_MOD_INVALID`, indicating that **_implicit modifiers_** may be
--  used with this format.
--
--  See [classGdk.DmabufTextureBuilder] for more information about DMA
--  buffers.
--
--  Note that DMA buffers only exist on Linux.

pragma Warnings (Off, "*is already use-visible*");
with Glib;   use Glib;
with System;

package Gdk.Dmabuf_Formats is

   type Gdk_Dmabuf_Formats is new Glib.C_Boxed with null record;
   Null_Gdk_Dmabuf_Formats : constant Gdk_Dmabuf_Formats;

   function From_Object (Object : System.Address) return Gdk_Dmabuf_Formats;
   function From_Object_Free (B : access Gdk_Dmabuf_Formats'Class) return Gdk_Dmabuf_Formats;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_dmabuf_formats_get_type");

   -------------
   -- Methods --
   -------------

   function Contains
      (Self     : Gdk_Dmabuf_Formats;
       Fourcc   : Guint32;
       Modifier : Guint64) return Boolean;
   --  Returns whether a given format is contained in Formats.
   --  Since: gtk+ 4.14
   --  @param Fourcc a format code
   --  @param Modifier a format modifier
   --  @return `TRUE` if the format specified by the arguments is part of
   --  Formats

   function Equal
      (Self     : Gdk_Dmabuf_Formats;
       Formats2 : Gdk_Dmabuf_Formats) return Boolean;
   --  Returns whether Formats1 and Formats2 contain the same dmabuf formats,
   --  in the same order.
   --  Since: gtk+ 4.14
   --  @param Formats2 another `GdkDmabufFormats`
   --  @return `TRUE` if Formats1 and Formats2 are equal

   procedure Get_Format
      (Self     : Gdk_Dmabuf_Formats;
       Idx      : Gsize;
       Fourcc   : out Guint32;
       Modifier : out Guint64);
   --  Gets the fourcc code and modifier for a format that is contained in
   --  Formats.
   --  Since: gtk+ 4.14
   --  @param Idx the index of the format to return
   --  @param Fourcc return location for the format code
   --  @param Modifier return location for the format modifier

   function Get_N_Formats (Self : Gdk_Dmabuf_Formats) return Gsize;
   --  Returns the number of formats that the Formats object contains.
   --  Note that DMA buffers are a Linux concept, so on other platforms,
   --  [methodGdk.DmabufFormats.get_n_formats] will always return zero.
   --  Since: gtk+ 4.14
   --  @return the number of formats

   function Ref (Self : Gdk_Dmabuf_Formats) return Gdk_Dmabuf_Formats;
   --  Increases the reference count of Formats.
   --  Since: gtk+ 4.14
   --  @return the passed-in object

   procedure Unref (Self : Gdk_Dmabuf_Formats);
   --  Decreases the reference count of Formats.
   --  When the reference count reaches zero, the object is freed.
   --  Since: gtk+ 4.14

private

   Null_Gdk_Dmabuf_Formats : constant Gdk_Dmabuf_Formats := (Glib.C_Boxed with null record);

end Gdk.Dmabuf_Formats;
