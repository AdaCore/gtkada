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

package body Gtk.Bitset is

   function From_Object_Free
     (B : access Gtk_Bitset'Class) return Gtk_Bitset
   is
      Result : constant Gtk_Bitset := Gtk_Bitset (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Bitset is
      S : Gtk_Bitset;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------------------
   -- Gtk_Bitset_New_Empty --
   --------------------------

   function Gtk_Bitset_New_Empty return Gtk_Bitset is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_bitset_new_empty");
      Self : Gtk_Bitset;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtk_Bitset_New_Empty;

   --------------------------
   -- Gtk_Bitset_New_Range --
   --------------------------

   function Gtk_Bitset_New_Range
      (Start   : Guint;
       N_Items : Guint) return Gtk_Bitset
   is
      function Internal
         (Start   : Guint;
          N_Items : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_bitset_new_range");
      Self : Gtk_Bitset;
   begin
      Self.Set_Object (Internal (Start, N_Items));
      return Self;
   end Gtk_Bitset_New_Range;

   -------------------
   -- Gtk_New_Empty --
   -------------------

   procedure Gtk_New_Empty (Self : out Gtk_Bitset) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_bitset_new_empty");
   begin
      Self.Set_Object (Internal);
   end Gtk_New_Empty;

   -------------------
   -- Gtk_New_Range --
   -------------------

   procedure Gtk_New_Range
      (Self    : out Gtk_Bitset;
       Start   : Guint;
       N_Items : Guint)
   is
      function Internal
         (Start   : Guint;
          N_Items : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_bitset_new_range");
   begin
      Self.Set_Object (Internal (Start, N_Items));
   end Gtk_New_Range;

   ---------
   -- Add --
   ---------

   function Add (Self : Gtk_Bitset; Value : Guint) return Boolean is
      function Internal
         (Self  : System.Address;
          Value : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_add");
   begin
      return Internal (Get_Object (Self), Value) /= 0;
   end Add;

   ---------------
   -- Add_Range --
   ---------------

   procedure Add_Range (Self : Gtk_Bitset; Start : Guint; N_Items : Guint) is
      procedure Internal
         (Self    : System.Address;
          Start   : Guint;
          N_Items : Guint);
      pragma Import (C, Internal, "gtk_bitset_add_range");
   begin
      Internal (Get_Object (Self), Start, N_Items);
   end Add_Range;

   ----------------------
   -- Add_Range_Closed --
   ----------------------

   procedure Add_Range_Closed
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint)
   is
      procedure Internal
         (Self  : System.Address;
          First : Guint;
          Last  : Guint);
      pragma Import (C, Internal, "gtk_bitset_add_range_closed");
   begin
      Internal (Get_Object (Self), First, Last);
   end Add_Range_Closed;

   -------------------
   -- Add_Rectangle --
   -------------------

   procedure Add_Rectangle
      (Self   : Gtk_Bitset;
       Start  : Guint;
       Width  : Guint;
       Height : Guint;
       Stride : Guint)
   is
      procedure Internal
         (Self   : System.Address;
          Start  : Guint;
          Width  : Guint;
          Height : Guint;
          Stride : Guint);
      pragma Import (C, Internal, "gtk_bitset_add_rectangle");
   begin
      Internal (Get_Object (Self), Start, Width, Height, Stride);
   end Add_Rectangle;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Gtk_Bitset; Value : Guint) return Boolean is
      function Internal
         (Self  : System.Address;
          Value : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_contains");
   begin
      return Internal (Get_Object (Self), Value) /= 0;
   end Contains;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Gtk_Bitset) return Gtk_Bitset is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_bitset_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------------
   -- Difference --
   ----------------

   procedure Difference (Self : Gtk_Bitset; Other : Gtk_Bitset) is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "gtk_bitset_difference");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Difference;

   ------------
   -- Equals --
   ------------

   function Equals (Self : Gtk_Bitset; Other : Gtk_Bitset) return Boolean is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_equals");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Equals;

   -----------------
   -- Get_Maximum --
   -----------------

   function Get_Maximum (Self : Gtk_Bitset) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_bitset_get_maximum");
   begin
      return Internal (Get_Object (Self));
   end Get_Maximum;

   -----------------
   -- Get_Minimum --
   -----------------

   function Get_Minimum (Self : Gtk_Bitset) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_bitset_get_minimum");
   begin
      return Internal (Get_Object (Self));
   end Get_Minimum;

   -------------
   -- Get_Nth --
   -------------

   function Get_Nth (Self : Gtk_Bitset; Nth : Guint) return Guint is
      function Internal (Self : System.Address; Nth : Guint) return Guint;
      pragma Import (C, Internal, "gtk_bitset_get_nth");
   begin
      return Internal (Get_Object (Self), Nth);
   end Get_Nth;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Self : Gtk_Bitset) return Guint64 is
      function Internal (Self : System.Address) return Guint64;
      pragma Import (C, Internal, "gtk_bitset_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   -----------------------
   -- Get_Size_In_Range --
   -----------------------

   function Get_Size_In_Range
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint) return Guint64
   is
      function Internal
         (Self  : System.Address;
          First : Guint;
          Last  : Guint) return Guint64;
      pragma Import (C, Internal, "gtk_bitset_get_size_in_range");
   begin
      return Internal (Get_Object (Self), First, Last);
   end Get_Size_In_Range;

   ---------------
   -- Intersect --
   ---------------

   procedure Intersect (Self : Gtk_Bitset; Other : Gtk_Bitset) is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "gtk_bitset_intersect");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Intersect;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Gtk_Bitset) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_is_empty");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Empty;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtk_Bitset) return Gtk_Bitset is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_bitset_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ------------
   -- Remove --
   ------------

   function Remove (Self : Gtk_Bitset; Value : Guint) return Boolean is
      function Internal
         (Self  : System.Address;
          Value : Guint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bitset_remove");
   begin
      return Internal (Get_Object (Self), Value) /= 0;
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Self : Gtk_Bitset) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_bitset_remove_all");
   begin
      Internal (Get_Object (Self));
   end Remove_All;

   ------------------
   -- Remove_Range --
   ------------------

   procedure Remove_Range
      (Self    : Gtk_Bitset;
       Start   : Guint;
       N_Items : Guint)
   is
      procedure Internal
         (Self    : System.Address;
          Start   : Guint;
          N_Items : Guint);
      pragma Import (C, Internal, "gtk_bitset_remove_range");
   begin
      Internal (Get_Object (Self), Start, N_Items);
   end Remove_Range;

   -------------------------
   -- Remove_Range_Closed --
   -------------------------

   procedure Remove_Range_Closed
      (Self  : Gtk_Bitset;
       First : Guint;
       Last  : Guint)
   is
      procedure Internal
         (Self  : System.Address;
          First : Guint;
          Last  : Guint);
      pragma Import (C, Internal, "gtk_bitset_remove_range_closed");
   begin
      Internal (Get_Object (Self), First, Last);
   end Remove_Range_Closed;

   ----------------------
   -- Remove_Rectangle --
   ----------------------

   procedure Remove_Rectangle
      (Self   : Gtk_Bitset;
       Start  : Guint;
       Width  : Guint;
       Height : Guint;
       Stride : Guint)
   is
      procedure Internal
         (Self   : System.Address;
          Start  : Guint;
          Width  : Guint;
          Height : Guint;
          Stride : Guint);
      pragma Import (C, Internal, "gtk_bitset_remove_rectangle");
   begin
      Internal (Get_Object (Self), Start, Width, Height, Stride);
   end Remove_Rectangle;

   ----------------
   -- Shift_Left --
   ----------------

   procedure Shift_Left (Self : Gtk_Bitset; Amount : Guint) is
      procedure Internal (Self : System.Address; Amount : Guint);
      pragma Import (C, Internal, "gtk_bitset_shift_left");
   begin
      Internal (Get_Object (Self), Amount);
   end Shift_Left;

   -----------------
   -- Shift_Right --
   -----------------

   procedure Shift_Right (Self : Gtk_Bitset; Amount : Guint) is
      procedure Internal (Self : System.Address; Amount : Guint);
      pragma Import (C, Internal, "gtk_bitset_shift_right");
   begin
      Internal (Get_Object (Self), Amount);
   end Shift_Right;

   ------------
   -- Splice --
   ------------

   procedure Splice
      (Self     : Gtk_Bitset;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Removed  : Guint;
          Added    : Guint);
      pragma Import (C, Internal, "gtk_bitset_splice");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Splice;

   --------------
   -- Subtract --
   --------------

   procedure Subtract (Self : Gtk_Bitset; Other : Gtk_Bitset) is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "gtk_bitset_subtract");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Subtract;

   -----------
   -- Union --
   -----------

   procedure Union (Self : Gtk_Bitset; Other : Gtk_Bitset) is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "gtk_bitset_union");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Union;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtk_Bitset) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_bitset_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gtk.Bitset;
