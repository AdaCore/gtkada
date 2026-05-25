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

--  Describes sorting criteria for a [classGtk.SortListModel].
--
--  Its primary user is [classGtk.SortListModel]
--
--  The model will use a sorter to determine the order in which its items
--  should appear by calling [methodGtk.Sorter.compare] for pairs of items.
--
--  Sorters may change their sorting behavior through their lifetime. In that
--  case, they will emit the [signalGtk.Sorter::changed] signal to notify that
--  the sort order is no longer valid and should be updated by calling
--  Gtk.Sorter.Compare again.
--
--  GTK provides various pre-made sorter implementations for common sorting
--  operations. [classGtk.ColumnView] has built-in support for sorting lists
--  via the [propertyGtk.ColumnViewColumn:sorter] property, where the user can
--  change the sorting by clicking on list headers.
--
--  Of course, in particular for large lists, it is also possible to subclass
--  `GtkSorter` and provide one's own sorter.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Gtk.Enums;               use Gtk.Enums;

package Gtk.Sorter is

   type Gtk_Sorter_Record is new GObject_Record with null record;
   type Gtk_Sorter is access all Gtk_Sorter_Record'Class;

   type Gtk_Sorter_Change is (
      Different,
      Inverted,
      Less_Strict,
      More_Strict);
   pragma Convention (C, Gtk_Sorter_Change);
   --  Describes changes in a sorter in more detail and allows users to
   --  optimize resorting.

   type Gtk_Sorter_Order is (
      Partial,
      None,
      Total);
   pragma Convention (C, Gtk_Sorter_Order);
   --  Describes the type of order that a `GtkSorter` may produce.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Sorter_Change_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Sorter_Change);
   type Property_Gtk_Sorter_Change is new Gtk_Sorter_Change_Properties.Property;

   package Gtk_Sorter_Order_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Sorter_Order);
   type Property_Gtk_Sorter_Order is new Gtk_Sorter_Order_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_sorter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed
      (Self   : not null access Gtk_Sorter_Record;
       Change : Gtk_Sorter_Change);
   --  Notifies all users of the sorter that it has changed.
   --  This emits the [signalGtk.Sorter::changed] signal. Users of the sorter
   --  should then update the sort order via [methodGtk.Sorter.compare].
   --  Depending on the Change parameter, it may be possible to update the
   --  sort order without a full resorting. Refer to the [enumGtk.SorterChange]
   --  documentation for details.
   --  This function is intended for implementers of `GtkSorter` subclasses
   --  and should not be called from other functions.
   --  @param Change How the sorter changed

   function Compare
      (Self  : not null access Gtk_Sorter_Record;
       Item1 : not null access Glib.Object.GObject_Record'Class;
       Item2 : not null access Glib.Object.GObject_Record'Class)
       return Gtk.Enums.Gtk_Ordering;
   --  Compares two given items according to the sort order implemented by the
   --  sorter.
   --  Sorters implement a partial order:
   --  * It is reflexive, ie a = a * It is antisymmetric, ie if a < b and b <
   --  a, then a = b * It is transitive, ie given any 3 items with a ≤ b and b
   --  ≤ c, then a ≤ c
   --  The sorter may signal it conforms to additional constraints via the
   --  return value of [methodGtk.Sorter.get_order].
   --  @param Item1 first item to compare
   --  @param Item2 second item to compare
   --  @return Gtk.Enums.Equal if Item1 == Item2, Gtk.Enums.Smaller if Item1 <
   --  Item2, Gtk.Enums.Larger if Item1 > Item2

   function Get_Order
      (Self : not null access Gtk_Sorter_Record) return Gtk_Sorter_Order;
   --  Gets the order that Self conforms to.
   --  See [enumGtk.SorterOrder] for details of the possible return values.
   --  This function is intended to allow optimizations.
   --  @return The order

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Sorter_Gtk_Sorter_Change_Void is not null access procedure
     (Self   : access Gtk_Sorter_Record'Class;
      Change : Gtk_Sorter_Change);

   type Cb_GObject_Gtk_Sorter_Change_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Sorter_Change);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Sorter_Record;
       Call  : Cb_Gtk_Sorter_Gtk_Sorter_Change_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Sorter_Record;
       Call  : Cb_GObject_Gtk_Sorter_Change_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the sorter changed.
   --
   --  Users of the sorter should then update the sort order again via
   --  Gtk.Sorter.Compare.
   --
   --  [classGtk.SortListModel] handles this signal automatically.
   --
   --  Depending on the Change parameter, it may be possible to update the
   --  sort order without a full resorting. Refer to the [enumGtk.SorterChange]
   --  documentation for details.

end Gtk.Sorter;
