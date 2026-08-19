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

--  Sorts items via a callback function.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.List_Store; use Glib.List_Store;
with Gtk.Sorter;      use Gtk.Sorter;

package Gtk.Custom_Sorter is

   type Gtk_Custom_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_Custom_Sorter is access all Gtk_Custom_Sorter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_custom_sorter_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
     (Self      : out Gtk_Custom_Sorter;
      Sort_Func : Glib.List_Store.Compare_Data_Func);
   procedure Initialize
     (Self      : not null access Gtk_Custom_Sorter_Record'Class;
      Sort_Func : Glib.List_Store.Compare_Data_Func);
   --  Creates a new `GtkSorter` that works by calling Sort_Func to compare
   --  items.
   --  If Sort_Func is null, all items are considered equal.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Sort_Func the `GCompareDataFunc` to use for sorting

   function Gtk_Custom_Sorter_New
     (Sort_Func : Glib.List_Store.Compare_Data_Func)
   return Gtk_Custom_Sorter;
   --  Creates a new `GtkSorter` that works by calling Sort_Func to compare
   --  items.
   --  If Sort_Func is null, all items are considered equal.
   --  @param Sort_Func the `GCompareDataFunc` to use for sorting

   -------------
   -- Methods --
   -------------

   procedure Set_Sort_Func
     (Self      : not null access Gtk_Custom_Sorter_Record;
      Sort_Func : Glib.List_Store.Compare_Data_Func);
   --  Sets (or unsets) the function used for sorting items.
   --  If Sort_Func is null, all items are considered equal.
   --  If the sort func changes its sorting behavior, Gtk.Sorter.Changed needs
   --  to be called.
   --  @param Sort_Func function to sort items

   -----------------------------
   -- Custom_Sorter_User_Data --
   -----------------------------

   generic
   type User_Data_Type (<>) is private;
   with procedure Destroy (Data : in out User_Data_Type) is null;
   package Custom_Sorter_User_Data is

      type Gcompare_Data_Func is access function
        (A         : not null access GObject_Record'Class;
         B         : not null access GObject_Record'Class;
         User_Data : User_Data_Type) return Glib.Gint;
      --  Specifies the type of a comparison function used to compare two values.
      --  The function should return a negative integer if the first value comes
      --  before the second, 0 if they are equal, or a positive integer if the
      --  first value comes after the second.
      --  @param A a value
      --  @param B a value to compare with
      --  @param User_Data user data
      --  @return negative value if A < B; zero if A = B; positive value if A > B

      procedure Gtk_New_User
        (Self      : out Gtk_Custom_Sorter;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type);
      procedure Initialize_User
        (Self      : not null access Gtk_Custom_Sorter_Record'Class;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type);
      --  Creates a new `GtkSorter` that works by calling Sort_Func to compare
      --  items.
      --  If Sort_Func is null, all items are considered equal.
      --  Initialize does nothing if the object was already created with another
      --  call to Initialize* or G_New.
      --  @param Sort_Func the `GCompareDataFunc` to use for sorting

      function Gtk_Custom_Sorter_New_User
        (Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type)
      return Gtk_Custom_Sorter;
      --  Creates a new `GtkSorter` that works by calling Sort_Func to compare
      --  items.
      --  If Sort_Func is null, all items are considered equal.
      --  @param Sort_Func the `GCompareDataFunc` to use for sorting

      procedure Set_Sort_Func_User
        (Self      : not null access Gtk.Custom_Sorter.Gtk_Custom_Sorter_Record'Class;
         Sort_Func : Gcompare_Data_Func;
         User_Data : User_Data_Type);
      --  Sets (or unsets) the function used for sorting items.
      --  If Sort_Func is null, all items are considered equal.
      --  If the sort func changes its sorting behavior, Gtk.Sorter.Changed
      --  needs to be called.
      --  If a previous function was set, its User_Destroy will be called now.
      --  @param Sort_Func function to sort items
      --  @param User_Data user data to pass to Match_Func

   end Custom_Sorter_User_Data;

end Gtk.Custom_Sorter;
