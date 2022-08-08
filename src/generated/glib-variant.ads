------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Glib.Variant.Gvariant is a variant datatype; it stores a value along with
--  information about the type of that value. The range of possible values is
--  determined by the type. The type system used by Glib.Variant.Gvariant is
--  Glib.Variant.Gvariant_Type.
--
--  Glib.Variant.Gvariant instances always have a type and a value (which are
--  given at construction time). The type and value of a Glib.Variant.Gvariant
--  instance can never change other than by the Glib.Variant.Gvariant itself
--  being destroyed. A Glib.Variant.Gvariant cannot contain a pointer.
--
--  Glib.Variant.Gvariant is reference counted using Glib.Variant.Ref and
--  Glib.Variant.Unref. Glib.Variant.Gvariant also has floating reference
--  counts -- see Glib.Variant.Ref_Sink.
--
--  Glib.Variant.Gvariant is completely threadsafe. A Glib.Variant.Gvariant
--  instance can be concurrently accessed in any way from any number of threads
--  without problems.
--
--  Glib.Variant.Gvariant is heavily optimised for dealing with data in
--  serialised form. It works particularly well with data located in
--  memory-mapped files. It can perform nearly all deserialisation operations
--  in a small constant time, usually touching only a single memory page.
--  Serialised Glib.Variant.Gvariant data can also be sent over the network.
--
--  Glib.Variant.Gvariant is largely compatible with D-Bus. Almost all types
--  of Glib.Variant.Gvariant instances can be sent over D-Bus. See
--  Glib.Variant.Gvariant_Type for exceptions. (However,
--  Glib.Variant.Gvariant's serialisation format is not the same as the
--  serialisation format of a D-Bus message body: use
--  Gdbus.Message.Gdbus_Message, in the gio library, for those.)
--
--  For space-efficiency, the Glib.Variant.Gvariant serialisation format does
--  not automatically include the variant's length, type or endianness, which
--  must either be implied from context (such as knowledge that a particular
--  file format always contains a little-endian G_VARIANT_TYPE_VARIANT which
--  occupies the whole length of the file) or supplied out-of-band (for
--  instance, a length, type and/or endianness indicator could be placed at the
--  beginning of a file, network message or network stream).
--
--  A Glib.Variant.Gvariant's size is limited mainly by any lower level
--  operating system constraints, such as the number of bits in Gsize. For
--  example, it is reasonable to have a 2GB file mapped into memory with
--  Gmapped.File.Gmapped_File, and call g_variant_new_from_data on it.
--
--  For convenience to C programmers, Glib.Variant.Gvariant features powerful
--  varargs-based value construction and destruction. This feature is designed
--  to be embedded in other libraries.
--
--  There is a Python-inspired text language for describing
--  Glib.Variant.Gvariant values. Glib.Variant.Gvariant includes a printer for
--  this language and a parser with type inferencing.
--
--  ## Memory Use
--
--  Glib.Variant.Gvariant tries to be quite efficient with respect to memory
--  use. This section gives a rough idea of how much memory is used by the
--  current implementation. The information here is subject to change in the
--  future.
--
--  The memory allocated by Glib.Variant.Gvariant can be grouped into 4 broad
--  purposes: memory for serialised data, memory for the type information
--  cache, buffer management memory and memory for the Glib.Variant.Gvariant
--  structure itself.
--
--  ## Serialised Data Memory
--
--  This is the memory that is used for storing GVariant data in serialised
--  form. This is what would be sent over the network or what would end up on
--  disk, not counting any indicator of the endianness, or of the length or
--  type of the top-level variant.
--
--  The amount of memory required to store a boolean is 1 byte. 16, 32 and 64
--  bit integers and double precision floating point numbers use their
--  "natural" size. Strings (including object path and signature strings) are
--  stored with a nul terminator, and as such use the length of the string plus
--  1 byte.
--
--  Maybe types use no space at all to represent the null value and use the
--  same amount of space (sometimes plus one byte) as the equivalent
--  non-maybe-typed value to represent the non-null case.
--
--  Arrays use the amount of space required to store each of their members,
--  concatenated. Additionally, if the items stored in an array are not of a
--  fixed-size (ie: strings, other arrays, etc) then an additional framing
--  offset is stored for each item. The size of this offset is either 1, 2 or 4
--  bytes depending on the overall size of the container. Additionally, extra
--  padding bytes are added as required for alignment of child values.
--
--  Tuples (including dictionary entries) use the amount of space required to
--  store each of their members, concatenated, plus one framing offset (as per
--  arrays) for each non-fixed-sized item in the tuple, except for the last
--  one. Additionally, extra padding bytes are added as required for alignment
--  of child values.
--
--  Variants use the same amount of space as the item inside of the variant,
--  plus 1 byte, plus the length of the type string for the item inside the
--  variant.
--
--  As an example, consider a dictionary mapping strings to variants. In the
--  case that the dictionary is empty, 0 bytes are required for the
--  serialisation.
--
--  If we add an item "width" that maps to the int32 value of 500 then we will
--  use 4 byte to store the int32 (so 6 for the variant containing it) and 6
--  bytes for the string. The variant must be aligned to 8 after the 6 bytes of
--  the string, so that's 2 extra bytes. 6 (string) + 2 (padding) + 6 (variant)
--  is 14 bytes used for the dictionary entry. An additional 1 byte is added to
--  the array as a framing offset making a total of 15 bytes.
--
--  If we add another entry, "title" that maps to a nullable string that
--  happens to have a value of null, then we use 0 bytes for the null value
--  (and 3 bytes for the variant to contain it along with its type string) plus
--  6 bytes for the string. Again, we need 2 padding bytes. That makes a total
--  of 6 + 2 + 3 = 11 bytes.
--
--  We now require extra padding between the two items in the array. After the
--  14 bytes of the first item, that's 2 bytes required. We now require 2
--  framing offsets for an extra two bytes. 14 + 2 + 11 + 2 = 29 bytes to
--  encode the entire two-item dictionary.
--
--  ## Type Information Cache
--
--  For each GVariant type that currently exists in the program a type
--  information structure is kept in the type information cache. The type
--  information structure is required for rapid deserialisation.
--
--  Continuing with the above example, if a Glib.Variant.Gvariant exists with
--  the type "a{sv}" then a type information struct will exist for "a{sv}",
--  "{sv}", "s", and "v". Multiple uses of the same type will share the same
--  type information. Additionally, all single-digit types are stored in
--  read-only static memory and do not contribute to the writable memory
--  footprint of a program using Glib.Variant.Gvariant.
--
--  Aside from the type information structures stored in read-only memory,
--  there are two forms of type information. One is used for container types
--  where there is a single element type: arrays and maybe types. The other is
--  used for container types where there are multiple element types: tuples and
--  dictionary entries.
--
--  Array type info structures are 6 * sizeof (void *), plus the memory
--  required to store the type string itself. This means that on 32-bit
--  systems, the cache entry for "a{sv}" would require 30 bytes of memory (plus
--  malloc overhead).
--
--  Tuple type info structures are 6 * sizeof (void *), plus 4 * sizeof (void
--  *) for each item in the tuple, plus the memory required to store the type
--  string itself. A 2-item tuple, for example, would have a type information
--  structure that consumed writable memory in the size of 14 * sizeof (void *)
--  (plus type string) This means that on 32-bit systems, the cache entry for
--  "{sv}" would require 61 bytes of memory (plus malloc overhead).
--
--  This means that in total, for our "a{sv}" example, 91 bytes of type
--  information would be allocated.
--
--  The type information cache, additionally, uses a GHash_Table to store and
--  lookup the cached items and stores a pointer to this hash table in static
--  storage. The hash table is freed when there are zero items in the type
--  cache.
--
--  Although these sizes may seem large it is important to remember that a
--  program will probably only have a very small number of different types of
--  values in it and that only one type information structure is required for
--  many different values of the same type.
--
--  ## Buffer Management Memory
--
--  Glib.Variant.Gvariant uses an internal buffer management structure to deal
--  with the various different possible sources of serialised data that it
--  uses. The buffer is responsible for ensuring that the correct call is made
--  when the data is no longer in use by Glib.Variant.Gvariant. This may
--  involve a g_free or a g_slice_free or even g_mapped_file_unref.
--
--  One buffer management structure is used for each chunk of serialised data.
--  The size of the buffer management structure is 4 * (void *). On 32-bit
--  systems, that's 16 bytes.
--
--  ## GVariant structure
--
--  The size of a Glib.Variant.Gvariant structure is 6 * (void *). On 32-bit
--  systems, that's 24 bytes.
--
--  Glib.Variant.Gvariant structures only exist if they are explicitly created
--  with API calls. For example, if a Glib.Variant.Gvariant is constructed out
--  of serialised data for the example given above (with the dictionary) then
--  although there are 9 individual values that comprise the entire dictionary
--  (two keys, two values, two variants containing the values, two dictionary
--  entries, plus the dictionary itself), only 1 Glib.Variant.Gvariant instance
--  exists -- the one referring to the dictionary.
--
--  If calls are made to start accessing the other values then
--  Glib.Variant.Gvariant instances will exist for those values only for as
--  long as they are in use (ie: until you call Glib.Variant.Unref). The type
--  information is shared. The serialised data and the buffer management
--  structure for that serialised data is shared by the child.
--
--  ## Summary
--
--  To put the entire example together, for our dictionary mapping strings to
--  variants (with two entries, as given above), we are using 91 bytes of
--  memory for type information, 29 byes of memory for the serialised data, 16
--  bytes for buffer management and 24 bytes for the Glib.Variant.Gvariant
--  instance, or a total of 160 bytes, plus malloc overhead. If we were to use
--  Glib.Variant.Get_Child_Value to access the two dictionary entries, we would
--  use an additional 48 bytes. If we were to have other dictionaries of the
--  same type, we would use more memory for the serialised data and buffer
--  management for those dictionaries, but the type information would be
--  shared.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Glib.Error;              use Glib.Error;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.String;             use Glib.String;

package Glib.Variant is

   type Gvariant is new Glib.C_Boxed with null record;
   Null_Gvariant : constant Gvariant;

   function From_Object (Object : System.Address) return Gvariant;
   function From_Object_Free (B : access Gvariant'Class) return Gvariant;
   pragma Inline (From_Object_Free, From_Object);

   type GVariant_Class is (
      Class_Tuple,
      Class_Array,
      Class_Boolean,
      Class_Double,
      Class_Signature,
      Class_Handle,
      Class_Int32,
      Class_Maybe,
      Class_Int16,
      Class_Object_Path,
      Class_Uint16,
      Class_String,
      Class_Uint64,
      Class_Uint32,
      Class_Variant,
      Class_Int64,
      Class_Byte,
      Class_Dict_Entry);
   pragma Convention (C, GVariant_Class);
   --  The range of possible top-level types of Glib.Variant.Gvariant
   --  instances.

   for GVariant_Class use (
      Class_Tuple => 40,
      Class_Array => 97,
      Class_Boolean => 98,
      Class_Double => 100,
      Class_Signature => 103,
      Class_Handle => 104,
      Class_Int32 => 105,
      Class_Maybe => 109,
      Class_Int16 => 110,
      Class_Object_Path => 111,
      Class_Uint16 => 113,
      Class_String => 115,
      Class_Uint64 => 116,
      Class_Uint32 => 117,
      Class_Variant => 118,
      Class_Int64 => 120,
      Class_Byte => 121,
      Class_Dict_Entry => 123);

   type Gvariant_Iter is new Glib.C_Proxy;

   type Gvariant_Type is new Glib.C_Proxy;

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GVariant_Class_Properties is
      new Generic_Internal_Discrete_Property (GVariant_Class);
   type Property_GVariant_Class is new GVariant_Class_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New_Boolean (Self : out Gvariant; Value : Boolean);
   --  Creates a new boolean Glib.Variant.Gvariant instance -- either True or
   --  False.
   --  Since: gtk+ 2.24
   --  "value": a Boolean value

   function Gvariant_New_Boolean (Value : Boolean) return Gvariant;
   --  Creates a new boolean Glib.Variant.Gvariant instance -- either True or
   --  False.
   --  Since: gtk+ 2.24
   --  "value": a Boolean value

   procedure G_New_Byte (Self : out Gvariant; Value : Guchar);
   --  Creates a new byte Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint8 value

   function Gvariant_New_Byte (Value : Guchar) return Gvariant;
   --  Creates a new byte Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint8 value

   procedure G_New_Bytestring (Self : out Gvariant; String : Gint_Array);
   --  Creates an array-of-bytes Glib.Variant.Gvariant with the contents of
   --  String. This function is just like g_variant_new_string except that the
   --  string need not be valid utf8.
   --  The nul terminator character at the end of the string is stored in the
   --  array.
   --  Since: gtk+ 2.26
   --  "string": a normal nul-terminated string in no particular encoding

   function Gvariant_New_Bytestring (String : Gint_Array) return Gvariant;
   --  Creates an array-of-bytes Glib.Variant.Gvariant with the contents of
   --  String. This function is just like g_variant_new_string except that the
   --  string need not be valid utf8.
   --  The nul terminator character at the end of the string is stored in the
   --  array.
   --  Since: gtk+ 2.26
   --  "string": a normal nul-terminated string in no particular encoding

   procedure G_New_Bytestring_Array
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize);
   --  Constructs an array of bytestring Glib.Variant.Gvariant from the given
   --  array of strings.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.26
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   function Gvariant_New_Bytestring_Array
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant;
   --  Constructs an array of bytestring Glib.Variant.Gvariant from the given
   --  array of strings.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.26
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   procedure G_New_Dict_Entry
      (Self  : out Gvariant;
       Key   : Gvariant;
       Value : Gvariant);
   --  Creates a new dictionary entry Glib.Variant.Gvariant. Key and Value
   --  must be non-null. Key must be a value of a basic type (ie: not a
   --  container).
   --  If the Key or Value are floating references (see
   --  Glib.Variant.Ref_Sink), the new instance takes ownership of them as if
   --  via Glib.Variant.Ref_Sink.
   --  Since: gtk+ 2.24
   --  "key": a basic Glib.Variant.Gvariant, the key
   --  "value": a Glib.Variant.Gvariant, the value

   procedure G_New_Dict_Entry
      (Self  : out Gvariant_Type;
       Key   : Gvariant_Type;
       Value : Gvariant_Type);
   --  Constructs the type corresponding to a dictionary entry with a key of
   --  type Key and a value of type Value.
   --  It is appropriate to call Glib.Variant.Free on the return value.
   --  "key": a basic Glib.Variant.Gvariant_Type
   --  "value": a Glib.Variant.Gvariant_Type

   function Gvariant_New_Dict_Entry
      (Key   : Gvariant;
       Value : Gvariant) return Gvariant;
   --  Creates a new dictionary entry Glib.Variant.Gvariant. Key and Value
   --  must be non-null. Key must be a value of a basic type (ie: not a
   --  container).
   --  If the Key or Value are floating references (see
   --  Glib.Variant.Ref_Sink), the new instance takes ownership of them as if
   --  via Glib.Variant.Ref_Sink.
   --  Since: gtk+ 2.24
   --  "key": a basic Glib.Variant.Gvariant, the key
   --  "value": a Glib.Variant.Gvariant, the value

   procedure G_New_Double (Self : out Gvariant; Value : Gdouble);
   --  Creates a new double Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gdouble floating point value

   function Gvariant_New_Double (Value : Gdouble) return Gvariant;
   --  Creates a new double Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gdouble floating point value

   procedure G_New_Handle (Self : out Gvariant; Value : Gint32);
   --  Creates a new handle Glib.Variant.Gvariant instance.
   --  By convention, handles are indexes into an array of file descriptors
   --  that are sent alongside a D-Bus message. If you're not interacting with
   --  D-Bus, you probably don't need them.
   --  Since: gtk+ 2.24
   --  "value": a Gint32 value

   function Gvariant_New_Handle (Value : Gint32) return Gvariant;
   --  Creates a new handle Glib.Variant.Gvariant instance.
   --  By convention, handles are indexes into an array of file descriptors
   --  that are sent alongside a D-Bus message. If you're not interacting with
   --  D-Bus, you probably don't need them.
   --  Since: gtk+ 2.24
   --  "value": a Gint32 value

   procedure G_New_Int16 (Self : out Gvariant; Value : Gint16);
   --  Creates a new int16 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint16 value

   function Gvariant_New_Int16 (Value : Gint16) return Gvariant;
   --  Creates a new int16 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint16 value

   procedure G_New_Int32 (Self : out Gvariant; Value : Gint32);
   --  Creates a new int32 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint32 value

   function Gvariant_New_Int32 (Value : Gint32) return Gvariant;
   --  Creates a new int32 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint32 value

   procedure G_New_Int64 (Self : out Gvariant; Value : Gint64);
   --  Creates a new int64 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint64 value

   function Gvariant_New_Int64 (Value : Gint64) return Gvariant;
   --  Creates a new int64 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Gint64 value

   procedure G_New_Object_Path
      (Self        : out Gvariant;
       Object_Path : UTF8_String);
   --  Creates a D-Bus object path Glib.Variant.Gvariant with the contents of
   --  String. String must be a valid D-Bus object path. Use
   --  Glib.Variant.Is_Object_Path if you're not sure.
   --  Since: gtk+ 2.24
   --  "object_path": a normal C nul-terminated string

   function Gvariant_New_Object_Path
      (Object_Path : UTF8_String) return Gvariant;
   --  Creates a D-Bus object path Glib.Variant.Gvariant with the contents of
   --  String. String must be a valid D-Bus object path. Use
   --  Glib.Variant.Is_Object_Path if you're not sure.
   --  Since: gtk+ 2.24
   --  "object_path": a normal C nul-terminated string

   procedure G_New_Objv
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize);
   --  Constructs an array of object paths Glib.Variant.Gvariant from the
   --  given array of strings.
   --  Each string must be a valid Glib.Variant.Gvariant object path; see
   --  Glib.Variant.Is_Object_Path.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.30
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   function Gvariant_New_Objv
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant;
   --  Constructs an array of object paths Glib.Variant.Gvariant from the
   --  given array of strings.
   --  Each string must be a valid Glib.Variant.Gvariant object path; see
   --  Glib.Variant.Is_Object_Path.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.30
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   procedure G_New_Signature (Self : out Gvariant; Signature : UTF8_String);
   --  Creates a D-Bus type signature Glib.Variant.Gvariant with the contents
   --  of String. String must be a valid D-Bus type signature. Use
   --  Glib.Variant.Is_Signature if you're not sure.
   --  Since: gtk+ 2.24
   --  "signature": a normal C nul-terminated string

   function Gvariant_New_Signature (Signature : UTF8_String) return Gvariant;
   --  Creates a D-Bus type signature Glib.Variant.Gvariant with the contents
   --  of String. String must be a valid D-Bus type signature. Use
   --  Glib.Variant.Is_Signature if you're not sure.
   --  Since: gtk+ 2.24
   --  "signature": a normal C nul-terminated string

   procedure G_New_String (Self : out Gvariant; String : UTF8_String);
   --  Creates a string Glib.Variant.Gvariant with the contents of String.
   --  String must be valid utf8.
   --  Since: gtk+ 2.24
   --  "string": a normal utf8 nul-terminated string

   function Gvariant_New_String (String : UTF8_String) return Gvariant;
   --  Creates a string Glib.Variant.Gvariant with the contents of String.
   --  String must be valid utf8.
   --  Since: gtk+ 2.24
   --  "string": a normal utf8 nul-terminated string

   procedure G_New_Strv
      (Self   : out Gvariant;
       Strv   : GNAT.Strings.String_List;
       Length : Gssize);
   --  Constructs an array of strings Glib.Variant.Gvariant from the given
   --  array of strings.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.24
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   function Gvariant_New_Strv
      (Strv   : GNAT.Strings.String_List;
       Length : Gssize) return Gvariant;
   --  Constructs an array of strings Glib.Variant.Gvariant from the given
   --  array of strings.
   --  If Length is -1 then Strv is null-terminated.
   --  Since: gtk+ 2.24
   --  "strv": an array of strings
   --  "length": the length of Strv, or -1

   procedure G_New_Take_String (Self : out Gvariant; String : UTF8_String);
   --  Creates a string Glib.Variant.Gvariant with the contents of String.
   --  String must be valid utf8.
   --  This function consumes String. g_free will be called on String when it
   --  is no longer required.
   --  You must not modify or access String in any other way after passing it
   --  to this function. It is even possible that String is immediately freed.
   --  Since: gtk+ 2.38
   --  "string": a normal utf8 nul-terminated string

   function Gvariant_New_Take_String (String : UTF8_String) return Gvariant;
   --  Creates a string Glib.Variant.Gvariant with the contents of String.
   --  String must be valid utf8.
   --  This function consumes String. g_free will be called on String when it
   --  is no longer required.
   --  You must not modify or access String in any other way after passing it
   --  to this function. It is even possible that String is immediately freed.
   --  Since: gtk+ 2.38
   --  "string": a normal utf8 nul-terminated string

   procedure G_New_Uint16 (Self : out Gvariant; Value : Guint16);
   --  Creates a new uint16 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint16 value

   function Gvariant_New_Uint16 (Value : Guint16) return Gvariant;
   --  Creates a new uint16 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint16 value

   procedure G_New_Uint32 (Self : out Gvariant; Value : Guint32);
   --  Creates a new uint32 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint32 value

   function Gvariant_New_Uint32 (Value : Guint32) return Gvariant;
   --  Creates a new uint32 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint32 value

   procedure G_New_Uint64 (Self : out Gvariant; Value : Guint64);
   --  Creates a new uint64 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint64 value

   function Gvariant_New_Uint64 (Value : Guint64) return Gvariant;
   --  Creates a new uint64 Glib.Variant.Gvariant instance.
   --  Since: gtk+ 2.24
   --  "value": a Guint64 value

   procedure G_New_Variant (Self : out Gvariant; Value : Gvariant);
   --  Boxes Value. The result is a Glib.Variant.Gvariant instance
   --  representing a variant containing the original value.
   --  If Child is a floating reference (see Glib.Variant.Ref_Sink), the new
   --  instance takes ownership of Child.
   --  Since: gtk+ 2.24
   --  "value": a Glib.Variant.Gvariant instance

   function Gvariant_New_Variant (Value : Gvariant) return Gvariant;
   --  Boxes Value. The result is a Glib.Variant.Gvariant instance
   --  representing a variant containing the original value.
   --  If Child is a floating reference (see Glib.Variant.Ref_Sink), the new
   --  instance takes ownership of Child.
   --  Since: gtk+ 2.24
   --  "value": a Glib.Variant.Gvariant instance

   procedure G_New (Self : out Gvariant_Type; Type_String : UTF8_String);
   --  Creates a new Glib.Variant.Gvariant_Type corresponding to the type
   --  string given by Type_String. It is appropriate to call Glib.Variant.Free
   --  on the return value.
   --  It is a programmer error to call this function with an invalid type
   --  string. Use Glib.Variant.String_Is_Valid if you are unsure.
   --  Since: gtk+ 2.24
   --  "type_string": a valid GVariant type string

   function Gvariant_Type_New
      (Type_String : UTF8_String) return Gvariant_Type;
   --  Creates a new Glib.Variant.Gvariant_Type corresponding to the type
   --  string given by Type_String. It is appropriate to call Glib.Variant.Free
   --  on the return value.
   --  It is a programmer error to call this function with an invalid type
   --  string. Use Glib.Variant.String_Is_Valid if you are unsure.
   --  Since: gtk+ 2.24
   --  "type_string": a valid GVariant type string

   function Gvariant_Type_New_Dict_Entry
      (Key   : Gvariant_Type;
       Value : Gvariant_Type) return Gvariant_Type;
   --  Constructs the type corresponding to a dictionary entry with a key of
   --  type Key and a value of type Value.
   --  It is appropriate to call Glib.Variant.Free on the return value.
   --  "key": a basic Glib.Variant.Gvariant_Type
   --  "value": a Glib.Variant.Gvariant_Type

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_variant_type_get_gtype");

   -------------
   -- Methods --
   -------------

   function Byteswap (Self : Gvariant) return Gvariant;
   --  Performs a byteswapping operation on the contents of Value. The result
   --  is that all multi-byte numeric data contained in Value is byteswapped.
   --  That includes 16, 32, and 64bit signed and unsigned integers as well as
   --  file handles and double precision floating point values.
   --  This function is an identity mapping on any value that does not contain
   --  multi-byte numeric data. That include strings, booleans, bytes and
   --  containers containing only these things (recursively).
   --  The returned value is always in normal form and is marked as trusted.
   --  Since: gtk+ 2.24

   function Check_Format_String
      (Self          : Gvariant;
       Format_String : UTF8_String;
       Copy_Only     : Boolean) return Boolean;
   --  Checks if calling g_variant_get with Format_String on Value would be
   --  valid from a type-compatibility standpoint. Format_String is assumed to
   --  be a valid format string (from a syntactic standpoint).
   --  If Copy_Only is True then this function additionally checks that it
   --  would be safe to call Glib.Variant.Unref on Value immediately after the
   --  call to g_variant_get without invalidating the result. This is only
   --  possible if deep copies are made (ie: there are no pointers to the data
   --  inside of the soon-to-be-freed Glib.Variant.Gvariant instance). If this
   --  check fails then a g_critical is printed and False is returned.
   --  This function is meant to be used by functions that wish to provide
   --  varargs accessors to Glib.Variant.Gvariant values of uncertain values
   --  (eg: g_variant_lookup or g_menu_model_get_item_attribute).
   --  Since: gtk+ 2.34
   --  "format_string": a valid Glib.Variant.Gvariant format string
   --  "copy_only": True to ensure the format string makes deep copies

   function Classify (Self : Gvariant) return GVariant_Class;
   --  Classifies Value according to its top-level type.
   --  Since: gtk+ 2.24

   function Dup_Bytestring_Array
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of array of bytes Glib.Variant.Gvariant.
   --  This call makes a deep copy; the return result should be released with
   --  g_strfreev.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.26
   --  "length": the length of the result, or null

   function Dup_Objv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of object paths Glib.Variant.Gvariant.
   --  This call makes a deep copy; the return result should be released with
   --  g_strfreev.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.30
   --  "length": the length of the result, or null

   function Dup_String
      (Self   : Gvariant;
       Length : access Gsize) return UTF8_String;
   --  Similar to Glib.Variant.Get_String except that instead of returning a
   --  constant string, the string is duplicated.
   --  The string will always be utf8 encoded.
   --  The return value must be freed using g_free.
   --  Since: gtk+ 2.24
   --  "length": a pointer to a Gsize, to store the length

   function Dup_String (Self : Gvariant_Type) return UTF8_String;
   --  Returns a newly-allocated copy of the type string corresponding to
   --  Type. The returned string is nul-terminated. It is appropriate to call
   --  g_free on the return value.

   function Dup_Strv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of strings Glib.Variant.Gvariant. This
   --  call makes a deep copy; the return result should be released with
   --  g_strfreev.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.24
   --  "length": the length of the result, or null

   function Get_Boolean (Self : Gvariant) return Boolean;
   --  Returns the boolean value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_BOOLEAN.
   --  Since: gtk+ 2.24

   function Get_Byte (Self : Gvariant) return Guchar;
   --  Returns the byte value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_BYTE.
   --  Since: gtk+ 2.24

   function Get_Bytestring_Array
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of array of bytes Glib.Variant.Gvariant.
   --  This call makes a shallow copy; the return result should be released
   --  with g_free, but the individual strings must not be modified.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.26
   --  "length": the length of the result, or null

   function Get_Child_Value (Self : Gvariant; Index : Gsize) return Gvariant;
   --  Reads a child item out of a container Glib.Variant.Gvariant instance.
   --  This includes variants, maybes, arrays, tuples and dictionary entries.
   --  It is an error to call this function on any other type of
   --  Glib.Variant.Gvariant.
   --  It is an error if Index_ is greater than the number of child items in
   --  the container. See Glib.Variant.N_Children.
   --  The returned value is never floating. You should free it with
   --  Glib.Variant.Unref when you're done with it.
   --  This function is O(1).
   --  Since: gtk+ 2.24
   --  "index_": the index of the child to fetch

   function Get_Double (Self : Gvariant) return Gdouble;
   --  Returns the double precision floating point value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_DOUBLE.
   --  Since: gtk+ 2.24

   function Get_Handle (Self : Gvariant) return Gint32;
   --  Returns the 32-bit signed integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_HANDLE.
   --  By convention, handles are indexes into an array of file descriptors
   --  that are sent alongside a D-Bus message. If you're not interacting with
   --  D-Bus, you probably don't need them.
   --  Since: gtk+ 2.24

   function Get_Int16 (Self : Gvariant) return Gint16;
   --  Returns the 16-bit signed integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_INT16.
   --  Since: gtk+ 2.24

   function Get_Int32 (Self : Gvariant) return Gint32;
   --  Returns the 32-bit signed integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_INT32.
   --  Since: gtk+ 2.24

   function Get_Int64 (Self : Gvariant) return Gint64;
   --  Returns the 64-bit signed integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_INT64.
   --  Since: gtk+ 2.24

   function Get_Maybe (Self : Gvariant) return Gvariant;
   --  Given a maybe-typed Glib.Variant.Gvariant instance, extract its value.
   --  If the value is Nothing, then this function returns null.
   --  Since: gtk+ 2.24

   function Get_Normal_Form (Self : Gvariant) return Gvariant;
   --  Gets a Glib.Variant.Gvariant instance that has the same value as Value
   --  and is trusted to be in normal form.
   --  If Value is already trusted to be in normal form then a new reference
   --  to Value is returned.
   --  If Value is not already trusted, then it is scanned to check if it is
   --  in normal form. If it is found to be in normal form then it is marked as
   --  trusted and a new reference to it is returned.
   --  If Value is found not to be in normal form then a new trusted
   --  Glib.Variant.Gvariant is created with the same value as Value.
   --  It makes sense to call this function if you've received
   --  Glib.Variant.Gvariant data from untrusted sources and you want to ensure
   --  your serialised output is definitely in normal form.
   --  Since: gtk+ 2.24

   function Get_Objv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of object paths Glib.Variant.Gvariant.
   --  This call makes a shallow copy; the return result should be released
   --  with g_free, but the individual strings must not be modified.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.30
   --  "length": the length of the result, or null

   function Get_Size (Self : Gvariant) return Gsize;
   --  Determines the number of bytes that would be required to store Value
   --  with Glib.Variant.Store.
   --  If Value has a fixed-sized type then this function always returned that
   --  fixed size.
   --  In the case that Value is already in serialised form or the size has
   --  already been calculated (ie: this function has been called before) then
   --  this function is O(1). Otherwise, the size is calculated, an operation
   --  which is approximately O(n) in the number of values involved.
   --  Since: gtk+ 2.24

   function Get_String
      (Self   : Gvariant;
       Length : access Gsize) return UTF8_String;
   --  Returns the string value of a Glib.Variant.Gvariant instance with a
   --  string type. This includes the types G_VARIANT_TYPE_STRING,
   --  G_VARIANT_TYPE_OBJECT_PATH and G_VARIANT_TYPE_SIGNATURE.
   --  The string will always be utf8 encoded.
   --  If Length is non-null then the length of the string (in bytes) is
   --  returned there. For trusted values, this information is already known.
   --  For untrusted values, a strlen will be performed.
   --  It is an error to call this function with a Value of any type other
   --  than those three.
   --  The return value remains valid as long as Value exists.
   --  Since: gtk+ 2.24
   --  "length": a pointer to a Gsize, to store the length

   function Get_Strv
      (Self   : Gvariant;
       Length : access Gsize) return GNAT.Strings.String_List;
   --  Gets the contents of an array of strings Glib.Variant.Gvariant. This
   --  call makes a shallow copy; the return result should be released with
   --  g_free, but the individual strings must not be modified.
   --  If Length is non-null then the number of elements in the result is
   --  stored there. In any case, the resulting array will be null-terminated.
   --  For an empty array, Length will be set to 0 and a pointer to a null
   --  pointer will be returned.
   --  Since: gtk+ 2.24
   --  "length": the length of the result, or null

   function Get_Type (Self : Gvariant) return Gvariant_Type;
   --  Determines the type of Value.
   --  The return value is valid for the lifetime of Value and must not be
   --  freed.
   --  Since: gtk+ 2.24

   function Get_Type_String (Self : Gvariant) return UTF8_String;
   --  Returns the type string of Value. Unlike the result of calling
   --  Glib.Variant.Peek_String, this string is nul-terminated. This string
   --  belongs to Glib.Variant.Gvariant and must not be freed.
   --  Since: gtk+ 2.24

   function Get_Uint16 (Self : Gvariant) return Guint16;
   --  Returns the 16-bit unsigned integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_UINT16.
   --  Since: gtk+ 2.24

   function Get_Uint32 (Self : Gvariant) return Guint32;
   --  Returns the 32-bit unsigned integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_UINT32.
   --  Since: gtk+ 2.24

   function Get_Uint64 (Self : Gvariant) return Guint64;
   --  Returns the 64-bit unsigned integer value of Value.
   --  It is an error to call this function with a Value of any type other
   --  than G_VARIANT_TYPE_UINT64.
   --  Since: gtk+ 2.24

   function Get_Variant (Self : Gvariant) return Gvariant;
   --  Unboxes Value. The result is the Glib.Variant.Gvariant instance that
   --  was contained in Value.
   --  Since: gtk+ 2.24

   function Hash (Self : Gvariant) return Guint;
   --  Generates a hash value for a Glib.Variant.Gvariant instance.
   --  The output of this function is guaranteed to be the same for a given
   --  value only per-process. It may change between different processor
   --  architectures or even different versions of GLib. Do not use this
   --  function as a basis for building protocols or file formats.
   --  The type of Value is gconstpointer only to allow use of this function
   --  with GHash_Table. Value must be a Glib.Variant.Gvariant.
   --  Since: gtk+ 2.24

   function Is_Container (Self : Gvariant) return Boolean;
   --  Checks if Value is a container.
   --  Since: gtk+ 2.24

   function Is_Container (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is a container type.
   --  Container types are any array, maybe, tuple, or dictionary entry types
   --  plus the variant type.
   --  This function returns True for any indefinite type for which every
   --  definite subtype is a container -- G_VARIANT_TYPE_ARRAY, for example.

   function Is_Floating (Self : Gvariant) return Boolean;
   --  Checks whether Value has a floating reference count.
   --  This function should only ever be used to assert that a given variant
   --  is or is not floating, or for debug purposes. To acquire a reference to
   --  a variant that might be floating, always use Glib.Variant.Ref_Sink or
   --  Glib.Variant.Take_Ref.
   --  See Glib.Variant.Ref_Sink for more information about floating reference
   --  counts.
   --  Since: gtk+ 2.26

   function Is_Normal_Form (Self : Gvariant) return Boolean;
   --  Checks if Value is in normal form.
   --  The main reason to do this is to detect if a given chunk of serialised
   --  data is in normal form: load the data into a Glib.Variant.Gvariant using
   --  g_variant_new_from_data and then use this function to check.
   --  If Value is found to be in normal form then it will be marked as being
   --  trusted. If the value was already marked as being trusted then this
   --  function will immediately return True.
   --  Since: gtk+ 2.24

   function Is_Of_Type
      (Self     : Gvariant;
       The_Type : Gvariant_Type) return Boolean;
   --  Checks if a value has a type matching the provided type.
   --  Since: gtk+ 2.24
   --  "type": a Glib.Variant.Gvariant_Type

   function Iter_New (Self : Gvariant) return Gvariant_Iter;
   --  Creates a heap-allocated Glib.Variant.Gvariant_Iter for iterating over
   --  the items in Value.
   --  Use g_variant_iter_free to free the return value when you no longer
   --  need it.
   --  A reference is taken to Value and will be released only when
   --  g_variant_iter_free is called.
   --  Since: gtk+ 2.24

   function Lookup_Value
      (Self          : Gvariant;
       Key           : UTF8_String;
       Expected_Type : Gvariant_Type) return Gvariant;
   --  Looks up a value in a dictionary Glib.Variant.Gvariant.
   --  This function works with dictionaries of the type a{s*} (and equally
   --  well with type a{o*}, but we only further discuss the string case for
   --  sake of clarity).
   --  In the event that Dictionary has the type a{sv}, the Expected_Type
   --  string specifies what type of value is expected to be inside of the
   --  variant. If the value inside the variant has a different type then null
   --  is returned. In the event that Dictionary has a value type other than v
   --  then Expected_Type must directly match the key type and it is used to
   --  unpack the value directly or an error occurs.
   --  In either case, if Key is not found in Dictionary, null is returned.
   --  If the key is found and the value has the correct type, it is returned.
   --  If Expected_Type was specified then any non-null return value will have
   --  this type.
   --  This function is currently implemented with a linear scan. If you plan
   --  to do many lookups then Gvariant.Dict.Gvariant_Dict may be more
   --  efficient.
   --  Since: gtk+ 2.28
   --  "key": the key to lookup in the dictionary
   --  "expected_type": a Glib.Variant.Gvariant_Type, or null

   function N_Children (Self : Gvariant) return Gsize;
   --  Determines the number of children in a container Glib.Variant.Gvariant
   --  instance. This includes variants, maybes, arrays, tuples and dictionary
   --  entries. It is an error to call this function on any other type of
   --  Glib.Variant.Gvariant.
   --  For variants, the return value is always 1. For values with maybe
   --  types, it is always zero or one. For arrays, it is the length of the
   --  array. For tuples it is the number of tuple items (which depends only on
   --  the type). For dictionary entries, it is always 2
   --  This function is O(1).
   --  Since: gtk+ 2.24

   function Print
      (Self          : Gvariant;
       Type_Annotate : Boolean) return UTF8_String;
   --  Pretty-prints Value in the format understood by Glib.Variant.Parse.
   --  The format is described [here][gvariant-text].
   --  If Type_Annotate is True, then type information is included in the
   --  output.
   --  Since: gtk+ 2.24
   --  "type_annotate": True if type information should be included in the
   --  output

   function Print_String
      (Self          : Gvariant;
       String        : Glib.String.Gstring;
       Type_Annotate : Boolean) return Glib.String.Gstring;
   --  Behaves as Glib.Variant.Print, but operates on a Glib.String.Gstring.
   --  If String is non-null then it is appended to and returned. Else, a new
   --  empty Glib.String.Gstring is allocated and it is returned.
   --  Since: gtk+ 2.24
   --  "string": a Glib.String.Gstring, or null
   --  "type_annotate": True if type information should be included in the
   --  output

   function Ref (Self : Gvariant) return Gvariant;
   --  Increases the reference count of Value.
   --  Since: gtk+ 2.24

   function Ref_Sink (Self : Gvariant) return Gvariant;
   --  Glib.Variant.Gvariant uses a floating reference count system. All
   --  functions with names starting with `g_variant_new_` return floating
   --  references.
   --  Calling Glib.Variant.Ref_Sink on a Glib.Variant.Gvariant with a
   --  floating reference will convert the floating reference into a full
   --  reference. Calling Glib.Variant.Ref_Sink on a non-floating
   --  Glib.Variant.Gvariant results in an additional normal reference being
   --  added.
   --  In other words, if the Value is floating, then this call "assumes
   --  ownership" of the floating reference, converting it to a normal
   --  reference. If the Value is not floating, then this call adds a new
   --  normal reference increasing the reference count by one.
   --  All calls that result in a Glib.Variant.Gvariant instance being
   --  inserted into a container will call Glib.Variant.Ref_Sink on the
   --  instance. This means that if the value was just created (and has only
   --  its floating reference) then the container will assume sole ownership of
   --  the value at that point and the caller will not need to unreference it.
   --  This makes certain common styles of programming much easier while still
   --  maintaining normal refcounting semantics in situations where values are
   --  not floating.
   --  Since: gtk+ 2.24

   procedure Store (Self : Gvariant; Data : System.Address);
   --  Stores the serialised form of Value at Data. Data should be large
   --  enough. See Glib.Variant.Get_Size.
   --  The stored data is in machine native byte order but may not be in
   --  fully-normalised form if read from an untrusted source. See
   --  Glib.Variant.Get_Normal_Form for a solution.
   --  As with g_variant_get_data, to be able to deserialise the serialised
   --  variant successfully, its type and (if the destination machine might be
   --  different) its endianness must also be available.
   --  This function is approximately O(n) in the size of Data.
   --  Since: gtk+ 2.24
   --  "data": the location to store the serialised data at

   function Take_Ref (Self : Gvariant) return Gvariant;
   --  If Value is floating, sink it. Otherwise, do nothing.
   --  Typically you want to use Glib.Variant.Ref_Sink in order to
   --  automatically do the correct thing with respect to floating or
   --  non-floating references, but there is one specific scenario where this
   --  function is helpful.
   --  The situation where this function is helpful is when creating an API
   --  that allows the user to provide a callback function that returns a
   --  Glib.Variant.Gvariant. We certainly want to allow the user the
   --  flexibility to return a non-floating reference from this callback (for
   --  the case where the value that is being returned already exists).
   --  At the same time, the style of the Glib.Variant.Gvariant API makes it
   --  likely that for newly-created Glib.Variant.Gvariant instances, the user
   --  can be saved some typing if they are allowed to return a
   --  Glib.Variant.Gvariant with a floating reference.
   --  Using this function on the return value of the user's callback allows
   --  the user to do whichever is more convenient for them. The caller will
   --  alway receives exactly one full reference to the value: either the one
   --  that was returned in the first place, or a floating reference that has
   --  been converted to a full reference.
   --  This function has an odd interaction when combined with
   --  Glib.Variant.Ref_Sink running at the same time in another thread on the
   --  same Glib.Variant.Gvariant instance. If Glib.Variant.Ref_Sink runs first
   --  then the result will be that the floating reference is converted to a
   --  hard reference. If Glib.Variant.Take_Ref runs first then the result will
   --  be that the floating reference is converted to a hard reference and an
   --  additional reference on top of that one is added. It is best to avoid
   --  this situation.

   procedure Unref (Self : Gvariant);
   --  Decreases the reference count of Value. When its reference count drops
   --  to 0, the memory used by the variant is freed.
   --  Since: gtk+ 2.24

   function Init (Self : Gvariant_Iter; Value : Gvariant) return Gsize;
   --  Initialises (without allocating) a Glib.Variant.Gvariant_Iter. Iter may
   --  be completely uninitialised prior to this call; its old value is
   --  ignored.
   --  The iterator remains valid for as long as Value exists, and need not be
   --  freed in any way.
   --  Since: gtk+ 2.24
   --  "value": a container Glib.Variant.Gvariant

   function Next_Value (Self : Gvariant_Iter) return Gvariant;
   --  Gets the next item in the container. If no more items remain then null
   --  is returned.
   --  Use Glib.Variant.Unref to drop your reference on the return value when
   --  you no longer need it.
   --  Here is an example for iterating with Glib.Variant.Next_Value: |[<!--
   --  language="C" --> // recursively iterate a container void
   --  iterate_container_recursive (GVariant *container) { GVariantIter iter;
   --  GVariant *child;
   --  g_variant_iter_init (&iter, container); while ((child =
   --  g_variant_iter_next_value (&iter))) { g_print ("type '%s'\n",
   --  g_variant_get_type_string (child));
   --  if (g_variant_is_container (child)) iterate_container_recursive
   --  (child);
   --  g_variant_unref (child); } } ]|
   --  Since: gtk+ 2.24

   function Copy (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, Copy, "g_variant_type_copy");
   --  Makes a copy of a Glib.Variant.Gvariant_Type. It is appropriate to call
   --  Glib.Variant.Free on the return value. Type may not be null.

   function Element (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, Element, "g_variant_type_element");
   --  Determines the element type of an array or maybe type.
   --  This function may only be used with array or maybe types.

   function First (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, First, "g_variant_type_first");
   --  Determines the first item type of a tuple or dictionary entry type.
   --  This function may only be used with tuple or dictionary entry types,
   --  but must not be used with the generic tuple type G_VARIANT_TYPE_TUPLE.
   --  In the case of a dictionary entry type, this returns the type of the
   --  key.
   --  null is returned in case of Type being G_VARIANT_TYPE_UNIT.
   --  This call, together with Glib.Variant.Next provides an iterator
   --  interface over tuple and dictionary entry types.

   procedure Free (Self : Gvariant_Type);
   pragma Import (C, Free, "g_variant_type_free");
   --  Frees a Glib.Variant.Gvariant_Type that was allocated with
   --  Glib.Variant.Copy, g_variant_type_new or one of the container type
   --  constructor functions.
   --  In the case that Type is null, this function does nothing.
   --  Since 2.24

   function Get_String_Length (Self : Gvariant_Type) return Gsize;
   pragma Import (C, Get_String_Length, "g_variant_type_get_string_length");
   --  Returns the length of the type string corresponding to the given Type.
   --  This function must be used to determine the valid extent of the memory
   --  region returned by Glib.Variant.Peek_String.

   function Is_Array (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is an array type. This is true if the type
   --  string for Type starts with an 'a'.
   --  This function returns True for any indefinite type for which every
   --  definite subtype is an array type -- G_VARIANT_TYPE_ARRAY, for example.

   function Is_Basic (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is a basic type.
   --  Basic types are booleans, bytes, integers, doubles, strings, object
   --  paths and signatures.
   --  Only a basic type may be used as the key of a dictionary entry.
   --  This function returns False for all indefinite types except
   --  G_VARIANT_TYPE_BASIC.

   function Is_Definite (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is definite (ie: not indefinite).
   --  A type is definite if its type string does not contain any indefinite
   --  type characters ('*', '?', or 'r').
   --  A Glib.Variant.Gvariant instance may not have an indefinite type, so
   --  calling this function on the result of Glib.Variant.Get_Type will always
   --  result in True being returned. Calling this function on an indefinite
   --  type like G_VARIANT_TYPE_ARRAY, however, will result in False being
   --  returned.

   function Is_Dict_Entry (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is a dictionary entry type. This is true
   --  if the type string for Type starts with a '{'.
   --  This function returns True for any indefinite type for which every
   --  definite subtype is a dictionary entry type --
   --  G_VARIANT_TYPE_DICT_ENTRY, for example.

   function Is_Maybe (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is a maybe type. This is true if the type
   --  string for Type starts with an 'm'.
   --  This function returns True for any indefinite type for which every
   --  definite subtype is a maybe type -- G_VARIANT_TYPE_MAYBE, for example.

   function Is_Subtype_Of
      (Self      : Gvariant_Type;
       Supertype : Gvariant_Type) return Boolean;
   --  Checks if Type is a subtype of Supertype.
   --  This function returns True if Type is a subtype of Supertype. All types
   --  are considered to be subtypes of themselves. Aside from that, only
   --  indefinite types can have subtypes.
   --  "supertype": a Glib.Variant.Gvariant_Type

   function Is_Tuple (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is a tuple type. This is true if the type
   --  string for Type starts with a '(' or if Type is G_VARIANT_TYPE_TUPLE.
   --  This function returns True for any indefinite type for which every
   --  definite subtype is a tuple type -- G_VARIANT_TYPE_TUPLE, for example.

   function Is_Variant (Self : Gvariant_Type) return Boolean;
   --  Determines if the given Type is the variant type.

   function Key (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, Key, "g_variant_type_key");
   --  Determines the key type of a dictionary entry type.
   --  This function may only be used with a dictionary entry type. Other than
   --  the additional restriction, this call is equivalent to
   --  Glib.Variant.First.

   function N_Items (Self : Gvariant_Type) return Gsize;
   pragma Import (C, N_Items, "g_variant_type_n_items");
   --  Determines the number of items contained in a tuple or dictionary entry
   --  type.
   --  This function may only be used with tuple or dictionary entry types,
   --  but must not be used with the generic tuple type G_VARIANT_TYPE_TUPLE.
   --  In the case of a dictionary entry type, this function will always
   --  return 2.

   function Next (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, Next, "g_variant_type_next");
   --  Determines the next item type of a tuple or dictionary entry type.
   --  Type must be the result of a previous call to Glib.Variant.First or
   --  Glib.Variant.Next.
   --  If called on the key type of a dictionary entry then this call returns
   --  the value type. If called on the value type of a dictionary entry then
   --  this call returns null.
   --  For tuples, null is returned when Type is the last item in a tuple.

   function Peek_String (Self : Gvariant_Type) return UTF8_String;
   --  Returns the type string corresponding to the given Type. The result is
   --  not nul-terminated; in order to determine its length you must call
   --  Glib.Variant.Get_String_Length.
   --  To get a nul-terminated string, see Glib.Variant.Dup_String.

   function Value (Self : Gvariant_Type) return Gvariant_Type;
   pragma Import (C, Value, "g_variant_type_value");
   --  Determines the value type of a dictionary entry type.
   --  This function may only be used with a dictionary entry type.

   ----------------------
   -- GtkAda additions --
   ----------------------

   Gvariant_Type_Boolean : constant Gvariant_Type;--  value is 0 or 1
   Gvariant_Type_Byte    : constant Gvariant_Type;--  from 0 to 255
   Gvariant_Type_Int16   : constant Gvariant_Type;--  from -32768..32767
   Gvariant_Type_Uint16  : constant Gvariant_Type;--  from 0 to 65535
   Gvariant_Type_Int32   : constant Gvariant_Type;
   Gvariant_Type_Uint32  : constant Gvariant_Type;
   Gvariant_Type_Int64   : constant Gvariant_Type;
   Gvariant_Type_Uint64  : constant Gvariant_Type;
   Gvariant_Type_Double  : constant Gvariant_Type;
   Gvariant_Type_String  : constant Gvariant_Type;

   pragma Import (C, Gvariant_Type_Boolean, "ada_gvariant_type_boolean");
   pragma Import (C, Gvariant_Type_Byte,    "ada_gvariant_type_byte");
   pragma Import (C, Gvariant_Type_Int16,   "ada_gvariant_type_int16");
   pragma Import (C, Gvariant_Type_Uint16,  "ada_gvariant_type_uint16");
   pragma Import (C, Gvariant_Type_Int32,   "ada_gvariant_type_int32");
   pragma Import (C, Gvariant_Type_Uint32,  "ada_gvariant_type_uint32");
   pragma Import (C, Gvariant_Type_Int64,   "ada_gvariant_type_int64");
   pragma Import (C, Gvariant_Type_Uint64,  "ada_gvariant_type_uint64");
   pragma Import (C, Gvariant_Type_Double,  "ada_gvariant_type_double");
   pragma Import (C, Gvariant_Type_String,  "ada_gvariant_type_string");

   ---------------
   -- Functions --
   ---------------

   function Is_Object_Path (String : UTF8_String) return Boolean;
   --  Determines if a given string is a valid D-Bus object path. You should
   --  ensure that a string is a valid D-Bus object path before passing it to
   --  g_variant_new_object_path.
   --  A valid object path starts with '/' followed by zero or more sequences
   --  of characters separated by '/' characters. Each sequence must contain
   --  only the characters "[A-Z][a-z][0-9]_". No sequence (including the one
   --  following the final '/' character) may be empty.
   --  Since: gtk+ 2.24
   --  "string": a normal C nul-terminated string

   function Is_Signature (String : UTF8_String) return Boolean;
   --  Determines if a given string is a valid D-Bus type signature. You
   --  should ensure that a string is a valid D-Bus type signature before
   --  passing it to g_variant_new_signature.
   --  D-Bus type signatures consist of zero or more definite
   --  Glib.Variant.Gvariant_Type strings in sequence.
   --  Since: gtk+ 2.24
   --  "string": a normal C nul-terminated string

   function Parse
      (The_Type : Gvariant_Type;
       Text     : UTF8_String;
       Limit    : UTF8_String := "";
       Endptr   : GNAT.Strings.String_List) return Gvariant;
   --  Parses a Glib.Variant.Gvariant from a text representation.
   --  A single Glib.Variant.Gvariant is parsed from the content of Text.
   --  The format is described [here][gvariant-text].
   --  The memory at Limit will never be accessed and the parser behaves as if
   --  the character at Limit is the nul terminator. This has the effect of
   --  bounding Text.
   --  If Endptr is non-null then Text is permitted to contain data following
   --  the value that this function parses and Endptr will be updated to point
   --  to the first character past the end of the text parsed by this function.
   --  If Endptr is null and there is extra data then an error is returned.
   --  If Type is non-null then the value will be parsed to have that type.
   --  This may result in additional parse errors (in the case that the parsed
   --  value doesn't fit the type) but may also result in fewer errors (in the
   --  case that the type would have been ambiguous, such as with empty
   --  arrays).
   --  In the event that the parsing is successful, the resulting
   --  Glib.Variant.Gvariant is returned.
   --  In case of any error, null will be returned. If Error is non-null then
   --  it will be set to reflect the error that occurred.
   --  Officially, the language understood by the parser is "any string
   --  produced by Glib.Variant.Print".
   --  "type": a Glib.Variant.Gvariant_Type, or null
   --  "text": a string containing a GVariant in text form
   --  "limit": a pointer to the end of Text, or null
   --  "endptr": a location to store the end pointer, or null

   function Parse_Error_Print_Context
      (Error      : Glib.Error.GError;
       Source_Str : UTF8_String) return UTF8_String;
   --  Pretty-prints a message showing the context of a Glib.Variant.Gvariant
   --  parse error within the string for which parsing was attempted.
   --  The resulting string is suitable for output to the console or other
   --  monospace media where newlines are treated in the usual way.
   --  The message will typically look something like one of the following:
   --  |[ unterminated string constant: (1, 2, 3, 'abc ^^^^ ]|
   --  or
   --  |[ unable to find a common type: [1, 2, 3, 'str'] ^ ^^^^^ ]|
   --  The format of the message may change in a future version.
   --  Error must have come from a failed attempt to Glib.Variant.Parse and
   --  Source_Str must be exactly the same string that caused the error. If
   --  Source_Str was not nul-terminated when you passed it to
   --  Glib.Variant.Parse then you must add nul termination before using this
   --  function.
   --  Since: gtk+ 2.40
   --  "error": a Gerror.Gerror from the GVariant_Parse_Error domain
   --  "source_str": the string that was given to the parser

   function Parse_Error_Quark return Glib.GQuark;

   function Parser_Get_Error_Quark return Glib.GQuark;
   pragma Obsolescent (Parser_Get_Error_Quark);
   --  Same as g_variant_error_quark.
   --  Deprecated since None, 1

   function String_Is_Valid (Type_String : UTF8_String) return Boolean;
   --  Checks if Type_String is a valid GVariant type string. This call is
   --  equivalent to calling g_variant_type_string_scan and confirming that the
   --  following character is a nul terminator.
   --  "type_string": a pointer to any string

private

   Null_Gvariant : constant Gvariant := (Glib.C_Boxed with null record);

end Glib.Variant;
