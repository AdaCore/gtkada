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

--  A set of unsigned integers.
--
--  Another name for this data structure is "bitmap".
--
--  The current implementation is based on [roaring
--  bitmaps](https://roaringbitmap.org/).
--
--  A bitset allows adding a set of integers and provides support for set
--  operations like unions, intersections and checks for equality or if a value
--  is contained in the set. `GtkBitset` also contains various functions to
--  query metadata about the bitset, such as the minimum or maximum values or
--  its size.
--
--  The fastest way to iterate values in a bitset is [structGtk.BitsetIter].
--
--  The main use case for `GtkBitset` is implementing complex selections for
--  [ifaceGtk.SelectionModel].

pragma Warnings (Off, "*is already use-visible*");
with Glib; use Glib;

package Gtk.Bitset is

   type Gtk_Bitset is new Glib.C_Boxed with null record;
   Null_Gtk_Bitset : constant Gtk_Bitset;

   function From_Object (Object : System.Address) return Gtk_Bitset;
   function From_Object_Free (B : access Gtk_Bitset'Class) return Gtk_Bitset;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_Empty (Self : out Gtk_Bitset);
   --  Creates a new empty bitset.

   function Gtk_Bitset_New_Empty return Gtk_Bitset;
   --  Creates a new empty bitset.

   procedure Gtk_New_Range
      (Self    : out Gtk_Bitset;
       Start   : Guint;
       N_Items : Guint);
   --  Creates a bitset with the given range set.
   --  @param Start first value to add
   --  @param N_Items number of consecutive values to add

   function Gtk_Bitset_New_Range
      (Start   : Guint;
       N_Items : Guint) return Gtk_Bitset;
   --  Creates a bitset with the given range set.
   --  @param Start first value to add
   --  @param N_Items number of consecutive values to add

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bitset_get_type");

   -------------
   -- Methods --
   -------------

   function Add (Self : Gtk_Bitset; Value : Guint) return Boolean;
   --  Adds Value to Self if it wasn't part of it before.
   --  @param Value value to add
   --  @return True if Value was not part of Self and Self was changed

   procedure Add_Range (Self : Gtk_Bitset; Start : Guint; N_Items : Guint);
   --  Adds all values from Start (inclusive) to Start + N_Items (exclusive)
   --  in Self.
   --  @param Start first value to add
   --  @param N_Items number of consecutive values to add

   procedure Add_Range_Closed
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint);
   --  Adds the closed range [First, Last], so First, Last and all values in
   --  between. First must be smaller than Last.
   --  @param First first value to add
   --  @param Last last value to add

   procedure Add_Rectangle
      (Self   : Gtk_Bitset;
       Start  : Guint;
       Width  : Guint;
       Height : Guint;
       Stride : Guint);
   --  Interprets the values as a 2-dimensional boolean grid with the given
   --  Stride and inside that grid, adds a rectangle with the given Width and
   --  Height.
   --  @param Start first value to add
   --  @param Width width of the rectangle
   --  @param Height height of the rectangle
   --  @param Stride row stride of the grid

   function Contains (Self : Gtk_Bitset; Value : Guint) return Boolean;
   --  Checks if the given Value has been added to Self
   --  @param Value the value to check
   --  @return True if Self contains Value

   function Copy (Self : Gtk_Bitset) return Gtk_Bitset;
   --  Creates a copy of Self.
   --  @return A new bitset that contains the same values as Self

   procedure Difference (Self : Gtk_Bitset; Other : Gtk_Bitset);
   --  Sets Self to be the symmetric difference of Self and Other.
   --  The symmetric difference is set Self to contain all values that were
   --  either contained in Self or in Other, but not in both. This operation is
   --  also called an XOR.
   --  It is allowed for Self and Other to be the same bitset. The bitset will
   --  be emptied in that case.
   --  @param Other the `GtkBitset` to compute the difference from

   function Equals (Self : Gtk_Bitset; Other : Gtk_Bitset) return Boolean;
   --  Returns True if Self and Other contain the same values.
   --  @param Other another `GtkBitset`
   --  @return True if Self and Other contain the same values

   function Get_Maximum (Self : Gtk_Bitset) return Guint;
   --  Returns the largest value in Self.
   --  If Self is empty, 0 is returned.
   --  @return The largest value in Self

   function Get_Minimum (Self : Gtk_Bitset) return Guint;
   --  Returns the smallest value in Self.
   --  If Self is empty, `G_MAXUINT` is returned.
   --  @return The smallest value in Self

   function Get_Nth (Self : Gtk_Bitset; Nth : Guint) return Guint;
   --  Returns the value of the Nth item in self.
   --  If Nth is >= the size of Self, 0 is returned.
   --  @param Nth index of the item to get
   --  @return the value of the Nth item in Self

   function Get_Size (Self : Gtk_Bitset) return Guint64;
   --  Gets the number of values that were added to the set.
   --  For example, if the set is empty, 0 is returned.
   --  Note that this function returns a `guint64`, because when all values
   --  are set, the return value is `G_MAXUINT + 1`. Unless you are sure this
   --  cannot happen (it can't with `GListModel`), be sure to use a 64bit type.
   --  @return The number of values in the set.

   function Get_Size_In_Range
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint) return Guint64;
   --  Gets the number of values that are part of the set from First to Last
   --  (inclusive).
   --  Note that this function returns a `guint64`, because when all values
   --  are set, the return value is `G_MAXUINT + 1`. Unless you are sure this
   --  cannot happen (it can't with `GListModel`), be sure to use a 64bit type.
   --  @param First the first element to include
   --  @param Last the last element to include
   --  @return The number of values in the set from First to Last.

   procedure Intersect (Self : Gtk_Bitset; Other : Gtk_Bitset);
   --  Sets Self to be the intersection of Self and Other.
   --  In other words, remove all values from Self that are not part of Other.
   --  It is allowed for Self and Other to be the same bitset. Nothing will
   --  happen in that case.
   --  @param Other the `GtkBitset` to intersect with

   function Is_Empty (Self : Gtk_Bitset) return Boolean;
   --  Check if no value is contained in bitset.
   --  @return True if Self is empty

   function Ref (Self : Gtk_Bitset) return Gtk_Bitset;
   --  Acquires a reference on the given `GtkBitset`.
   --  @return the `GtkBitset` with an additional reference

   function Remove (Self : Gtk_Bitset; Value : Guint) return Boolean;
   --  Removes Value from Self if it was part of it before.
   --  @param Value value to remove
   --  @return True if Value was part of Self and Self was changed

   procedure Remove_All (Self : Gtk_Bitset);
   --  Removes all values from the bitset so that it is empty again.

   procedure Remove_Range
      (Self    : Gtk_Bitset;
       Start   : Guint;
       N_Items : Guint);
   --  Removes all values from Start (inclusive) to Start + N_Items
   --  (exclusive) in Self.
   --  @param Start first value to remove
   --  @param N_Items number of consecutive values to remove

   procedure Remove_Range_Closed
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint);
   --  Removes the closed range [First, Last], so First, Last and all values
   --  in between. First must be smaller than Last.
   --  @param First first value to remove
   --  @param Last last value to remove

   procedure Remove_Rectangle
      (Self   : Gtk_Bitset;
       Start  : Guint;
       Width  : Guint;
       Height : Guint;
       Stride : Guint);
   --  Interprets the values as a 2-dimensional boolean grid with the given
   --  Stride and inside that grid, removes a rectangle with the given Width
   --  and Height.
   --  @param Start first value to remove
   --  @param Width width of the rectangle
   --  @param Height height of the rectangle
   --  @param Stride row stride of the grid

   procedure Shift_Left (Self : Gtk_Bitset; Amount : Guint);
   --  Shifts all values in Self to the left by Amount.
   --  Values smaller than Amount are discarded.
   --  @param Amount amount to shift all values to the left

   procedure Shift_Right (Self : Gtk_Bitset; Amount : Guint);
   --  Shifts all values in Self to the right by Amount.
   --  Values that end up too large to be held in a Guint are discarded.
   --  @param Amount amount to shift all values to the right

   procedure Splice
      (Self     : Gtk_Bitset;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);
   --  This is a support function for `GListModel` handling, by mirroring the
   --  `GlistModel::items-changed` signal.
   --  First, it "cuts" the values from Position to Removed from the bitset.
   --  That is, it removes all those values and shifts all larger values to the
   --  left by Removed places.
   --  Then, it "pastes" new room into the bitset by shifting all values
   --  larger than Position by Added spaces to the right. This frees up space
   --  that can then be filled.
   --  @param Position position at which to slice
   --  @param Removed number of values to remove
   --  @param Added number of values to add

   procedure Subtract (Self : Gtk_Bitset; Other : Gtk_Bitset);
   --  Sets Self to be the subtraction of Other from Self.
   --  In other words, remove all values from Self that are part of Other.
   --  It is allowed for Self and Other to be the same bitset. The bitset will
   --  be emptied in that case.
   --  @param Other the `GtkBitset` to subtract

   procedure Union (Self : Gtk_Bitset; Other : Gtk_Bitset);
   --  Sets Self to be the union of Self and Other.
   --  That is, add all values from Other into Self that weren't part of it.
   --  It is allowed for Self and Other to be the same bitset. Nothing will
   --  happen in that case.
   --  @param Other the `GtkBitset` to union with

   procedure Unref (Self : Gtk_Bitset);
   --  Releases a reference on the given `GtkBitset`.
   --  If the reference was the last, the resources associated to the Self are
   --  freed.

private

   Null_Gtk_Bitset : constant Gtk_Bitset := (Glib.C_Boxed with null record);

end Gtk.Bitset;
