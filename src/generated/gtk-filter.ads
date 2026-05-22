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

--  Describes the filtering to be performed by a [classGtk.FilterListModel].
--
--  The model will use the filter to determine if it should include items or
--  not by calling [methodGtk.Filter.match] for each item and only keeping the
--  ones that the function returns true for.
--
--  Filters may change what items they match through their lifetime. In that
--  case, they will emit the [signalGtk.Filter::changed] signal to notify that
--  previous filter results are no longer valid and that items should be
--  checked again via [methodGtk.Filter.match].
--
--  GTK provides various pre-made filter implementations for common filtering
--  operations. These filters often include properties that can be linked to
--  various widgets to easily allow searches.
--
--  However, in particular for large lists or complex search methods, it is
--  also possible to subclass `GtkFilter` and provide one's own filter.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;

package Gtk.Filter is

   type Gtk_Filter_Record is new GObject_Record with null record;
   type Gtk_Filter is access all Gtk_Filter_Record'Class;

   type Gtk_Filter_Change is (
      Different,
      Less_Strict,
      More_Strict,
      Different_Rewatch,
      Less_Strict_Rewatch,
      More_Strict_Rewatch);
   pragma Convention (C, Gtk_Filter_Change);
   --  Describes changes in a filter in more detail and allows objects using
   --  the filter to optimize refiltering items.
   --
   --  If you are writing an implementation and are not sure which value to
   --  pass, `GTK_FILTER_CHANGE_DIFFERENT` is always a correct choice.
   --
   --  New values may be added in the future.

   type Gtk_Filter_Match is (
      Match_Some,
      Match_None,
      Match_All);
   pragma Convention (C, Gtk_Filter_Match);
   --  Describes the known strictness of a filter.
   --
   --  Note that for filters where the strictness is not known,
   --  `GTK_FILTER_MATCH_SOME` is always an acceptable value, even if a filter
   --  does match all or no items.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Filter_Change_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Filter_Change);
   type Property_Gtk_Filter_Change is new Gtk_Filter_Change_Properties.Property;

   package Gtk_Filter_Match_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Filter_Match);
   type Property_Gtk_Filter_Match is new Gtk_Filter_Match_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_filter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed
      (Self   : not null access Gtk_Filter_Record;
       Change : Gtk_Filter_Change);
   --  Notifies all users of the filter that it has changed.
   --  This emits the [signalGtk.Filter::changed] signal. Users of the filter
   --  should then check items again via [methodGtk.Filter.match].
   --  Depending on the Change parameter, not all items need to be changed,
   --  but only some. Refer to the [enumGtk.FilterChange] documentation for
   --  details.
   --  This function is intended for implementers of `GtkFilter` subclasses
   --  and should not be called from other functions.
   --  @param Change how the filter changed

   function Get_Strictness
      (Self : not null access Gtk_Filter_Record) return Gtk_Filter_Match;
   --  Gets the known strictness of a filter.
   --  If the strictness is not known, [enumGtk.FilterMatch.some] is returned.
   --  This value may change after emission of the [signalGtk.Filter::changed]
   --  signal.
   --  This function is meant purely for optimization purposes. Filters can
   --  choose to omit implementing it, but `GtkFilterListModel` uses it.
   --  @return the strictness of Self

   function Match
      (Self : not null access Gtk_Filter_Record;
       Item : not null access Glib.Object.GObject_Record'Class)
       return Boolean;
   --  Checks if the given Item is matched by the filter or not.
   --  @param Item The item to check
   --  @return true if the filter matches the item

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Filter_Gtk_Filter_Change_Void is not null access procedure
     (Self   : access Gtk_Filter_Record'Class;
      Change : Gtk_Filter_Change);

   type Cb_GObject_Gtk_Filter_Change_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Change : Gtk_Filter_Change);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Filter_Record;
       Call  : Cb_Gtk_Filter_Gtk_Filter_Change_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Filter_Record;
       Call  : Cb_GObject_Gtk_Filter_Change_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the filter changed.
   --
   --  Users of the filter should then check items again via
   --  [methodGtk.Filter.match].
   --
   --  `GtkFilterListModel` handles this signal automatically.
   --
   --  Depending on the Change parameter, not all items need to be checked,
   --  but only some. Refer to the [enumGtk.FilterChange] documentation for
   --  details.

end Gtk.Filter;
