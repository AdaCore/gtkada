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

--  A simple refcounted data type representing an immutable sequence of zero
--  or more bytes from an unspecified origin.
--
--  The purpose of a Glib.Bytes.Gbytes is to keep the memory region that it
--  holds alive for as long as anyone holds a reference to the bytes. When the
--  last reference count is dropped, the memory is released. Multiple unrelated
--  callers can use byte data in the Glib.Bytes.Gbytes without coordinating
--  their activities, resting assured that the byte data will not change or
--  move while they hold a reference.
--
--  A Glib.Bytes.Gbytes can come from many different origins that may have
--  different procedures for freeing the memory region. Examples are memory
--  from g_malloc, from memory slices, from a Gmapped.File.Gmapped_File or
--  memory from other allocators.
--
--  Glib.Bytes.Gbytes work well as keys in GHash_Table. Use g_bytes_equal and
--  g_bytes_hash as parameters to g_hash_table_new or g_hash_table_new_full.
--  Glib.Bytes.Gbytes can also be used as keys in a Gtree.Gtree by passing the
--  g_bytes_compare function to g_tree_new.
--
--  The data pointed to by this bytes must not be modified. For a mutable
--  array of bytes see GByte_Array. Use g_bytes_unref_to_array to create a
--  mutable array for a Glib.Bytes.Gbytes sequence. To create an immutable
--  Glib.Bytes.Gbytes from a mutable GByte_Array, use the
--  g_byte_array_free_to_bytes function.

pragma Warnings (Off, "*is already use-visible*");

package Glib.Bytes is

   type Gbytes is new Glib.C_Boxed with null record;
   Null_Gbytes : constant Gbytes;

   function From_Object (Object : System.Address) return Gbytes;
   function From_Object_Free (B : access Gbytes'Class) return Gbytes;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gbytes; Data : Guint8_Array; Size : Gsize);
   --  Creates a new Glib.Bytes.Gbytes from Data.
   --  Data is copied. If Size is 0, Data may be null.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   function Gbytes_New (Data : Guint8_Array; Size : Gsize) return Gbytes;
   --  Creates a new Glib.Bytes.Gbytes from Data.
   --  Data is copied. If Size is 0, Data may be null.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   procedure G_New_Static
      (Self : out Gbytes;
       Data : Guint8_Array;
       Size : Gsize);
   --  Creates a new Glib.Bytes.Gbytes from static data.
   --  Data must be static (ie: never modified or freed). It may be null if
   --  Size is 0.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   function Gbytes_New_Static
      (Data : Guint8_Array;
       Size : Gsize) return Gbytes;
   --  Creates a new Glib.Bytes.Gbytes from static data.
   --  Data must be static (ie: never modified or freed). It may be null if
   --  Size is 0.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   procedure G_New_Take
      (Self : out Gbytes;
       Data : Guint8_Array;
       Size : Gsize);
   --  Creates a new Glib.Bytes.Gbytes from Data.
   --  After this call, Data belongs to the Glib.Bytes.Gbytes and may no
   --  longer be modified by the caller. The memory of Data has to be
   --  dynamically allocated and will eventually be freed with g_free.
   --  For creating Glib.Bytes.Gbytes with memory from other allocators, see
   --  g_bytes_new_with_free_func.
   --  Data may be null if Size is 0.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   function Gbytes_New_Take
      (Data : Guint8_Array;
       Size : Gsize) return Gbytes;
   --  Creates a new Glib.Bytes.Gbytes from Data.
   --  After this call, Data belongs to the Glib.Bytes.Gbytes and may no
   --  longer be modified by the caller. The memory of Data has to be
   --  dynamically allocated and will eventually be freed with g_free.
   --  For creating Glib.Bytes.Gbytes with memory from other allocators, see
   --  g_bytes_new_with_free_func.
   --  Data may be null if Size is 0.
   --  Since: gtk+ 2.32
   --  @param Data the data to be used for the bytes
   --  @param Size the size of Data

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_bytes_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Region
      (Self         : Gbytes;
       Element_Size : Gsize;
       Offset       : Gsize;
       N_Elements   : Gsize) return System.Address;
   --  Gets a pointer to a region in Bytes.
   --  The region starts at Offset many bytes from the start of the data and
   --  contains N_Elements many elements of Element_Size size.
   --  N_Elements may be zero, but Element_Size must always be non-zero.
   --  Ideally, Element_Size is a static constant (eg: sizeof a struct).
   --  This function does careful bounds checking (including checking for
   --  arithmetic overflows) and returns a non-null pointer if the specified
   --  region lies entirely within the Bytes. If the region is in some way out
   --  of range, or if an overflow has occurred, then null is returned.
   --  Note: it is possible to have a valid zero-size region. In this case,
   --  the returned pointer will be equal to the base pointer of the data of
   --  Bytes, plus Offset. This will be non-null except for the case where
   --  Bytes itself was a zero-sized region. Since it is unlikely that you will
   --  be using this function to check for a zero-sized region in a zero-sized
   --  Bytes, null effectively always means "error".
   --  Since: gtk+ 2.70
   --  @param Element_Size a non-zero element size
   --  @param Offset an offset to the start of the region within the Bytes
   --  @param N_Elements the number of elements in the region
   --  @return the requested region, or null in case of an error

   function Get_Size (Self : Gbytes) return Gsize;
   --  Get the size of the byte data in the Glib.Bytes.Gbytes.
   --  This function will always return the same value for a given
   --  Glib.Bytes.Gbytes.
   --  Since: gtk+ 2.32
   --  @return the size

   function New_From_Bytes
      (Self   : Gbytes;
       Offset : Gsize;
       Length : Gsize) return Gbytes;
   --  Creates a Glib.Bytes.Gbytes which is a subsection of another
   --  Glib.Bytes.Gbytes. The Offset + Length may not be longer than the size
   --  of Bytes.
   --  A reference to Bytes will be held by the newly created
   --  Glib.Bytes.Gbytes until the byte data is no longer needed.
   --  Since 2.56, if Offset is 0 and Length matches the size of Bytes, then
   --  Bytes will be returned with the reference count incremented by 1. If
   --  Bytes is a slice of another Glib.Bytes.Gbytes, then the resulting
   --  Glib.Bytes.Gbytes will reference the same Glib.Bytes.Gbytes instead of
   --  Bytes. This allows consumers to simplify the usage of Glib.Bytes.Gbytes
   --  when asynchronously writing to streams.
   --  Since: gtk+ 2.32
   --  @param Offset offset which subsection starts at
   --  @param Length length of subsection
   --  @return a new Glib.Bytes.Gbytes

   function Ref (Self : Gbytes) return Gbytes;
   --  Increase the reference count on Bytes.
   --  Since: gtk+ 2.32
   --  @return the Glib.Bytes.Gbytes

   procedure Unref (Self : Gbytes);
   --  Releases a reference on Bytes. This may result in the bytes being
   --  freed. If Bytes is null, it will return immediately.
   --  Since: gtk+ 2.32

private
   Null_Gbytes : constant Gbytes :=
      (Glib.C_Boxed with null record);

end Glib.Bytes;
