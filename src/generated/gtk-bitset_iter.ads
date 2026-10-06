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

--  Iterates over the elements of a [structGtk.Bitset].
--
--  `GtkBitSetIter is an opaque, stack-allocated struct.
--
--  Before a `GtkBitsetIter` can be used, it needs to be initialized with
--  [funcGtk.BitsetIter.init_first], [funcGtk.BitsetIter.init_last] or
--  [funcGtk.BitsetIter.init_at].

pragma Warnings (Off, "*is already use-visible*");
with Glib;       use Glib;
with Gtk.Bitset; use Gtk.Bitset;

package Gtk.Bitset_Iter is

   type Gtk_Bitset_Iter is private;
   --  Iterates over the elements of a [structGtk.Bitset].
   --
   --  `GtkBitSetIter is an opaque, stack-allocated struct.
   --
   --  Before a `GtkBitsetIter` can be used, it needs to be initialized with
   --  [funcGtk.BitsetIter.init_first], [funcGtk.BitsetIter.init_last] or
   --  [funcGtk.BitsetIter.init_at].

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_bitset_iter_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Value (Self : access Gtk_Bitset_Iter) return Guint;
   pragma Import (C, Get_Value, "gtk_bitset_iter_get_value");
   --  Gets the current value that Iter points to.
   --  If Iter is not valid and [methodGtk.BitsetIter.is_valid] returns False,
   --  this function returns 0.
   --  @return The current value pointer to by Iter

   function Is_Valid (Self : access Gtk_Bitset_Iter) return Boolean;
   --  Checks if Iter points to a valid value.
   --  @return True if Iter points to a valid value

   function Next
      (Self  : access Gtk_Bitset_Iter;
       Value : access Guint := null) return Boolean;
   --  Moves Iter to the next value in the set.
   --  If it was already pointing to the last value in the set, False is
   --  returned and Iter is invalidated.
   --  @param Value Set to the next value
   --  @return True if a next value existed

   function Previous
      (Self  : access Gtk_Bitset_Iter;
       Value : access Guint := null) return Boolean;
   --  Moves Iter to the previous value in the set.
   --  If it was already pointing to the first value in the set, False is
   --  returned and Iter is invalidated.
   --  @param Value Set to the previous value
   --  @return True if a previous value existed

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gtk_Bitset_Iter) return Gtk_Bitset_Iter;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   ---------------
   -- Functions --
   ---------------

   function Init_At
      (Iter   : out Gtk_Bitset_Iter;
       Set    : Gtk.Bitset.Gtk_Bitset;
       Target : Guint;
       Value  : access Guint := null) return Boolean;
   --  Initializes Iter to point to Target.
   --  If Target is not found, finds the next value after it. If no value >=
   --  Target exists in Set, this function returns False.
   --  @param Iter a pointer to an uninitialized `GtkBitsetIter`
   --  @param Set a `GtkBitset`
   --  @param Target target value to start iterating at
   --  @param Value Set to the found value in Set
   --  @return True if a value was found.

   function Init_First
      (Iter  : out Gtk_Bitset_Iter;
       Set   : Gtk.Bitset.Gtk_Bitset;
       Value : access Guint := null) return Boolean;
   --  Initializes an iterator for Set and points it to the first value in
   --  Set.
   --  If Set is empty, False is returned and Value is set to G_MAXUINT.
   --  @param Iter a pointer to an uninitialized `GtkBitsetIter`
   --  @param Set a `GtkBitset`
   --  @param Value Set to the first value in Set
   --  @return True if Set isn't empty.

   function Init_Last
      (Iter  : out Gtk_Bitset_Iter;
       Set   : Gtk.Bitset.Gtk_Bitset;
       Value : access Guint := null) return Boolean;
   --  Initializes an iterator for Set and points it to the last value in Set.
   --  If Set is empty, False is returned.
   --  @param Iter a pointer to an uninitialized `GtkBitsetIter`
   --  @param Set a `GtkBitset`
   --  @param Value Set to the last value in Set
   --  @return True if Set isn't empty.

private
   type Gtk_Bitset_Iter is record
      Private_Data : Glib.Gpointer_Array (1 .. 10);
   end record;
   pragma Convention (C, Gtk_Bitset_Iter);

end Gtk.Bitset_Iter;
