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

--  Matches an item when at least one of its filters matches.
--
--  To add filters to a `GtkAnyFilter`, use [methodGtk.MultiFilter.append].

pragma Warnings (Off, "*is already use-visible*");
with Glib;             use Glib;
with Glib.List_Model;  use Glib.List_Model;
with Glib.Object;      use Glib.Object;
with Glib.Types;       use Glib.Types;
with Gtk.Buildable;    use Gtk.Buildable;
with Gtk.Multi_Filter; use Gtk.Multi_Filter;

package Gtk.Any_Filter is

   type Gtk_Any_Filter_Record is new Gtk_Multi_Filter_Record with null record;
   type Gtk_Any_Filter is access all Gtk_Any_Filter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Any_Filter);
   procedure Initialize (Self : not null access Gtk_Any_Filter_Record'Class);
   --  Creates a new empty "any" filter.
   --  Use [methodGtk.MultiFilter.append] to add filters to it.
   --  This filter matches an item if any of the filters added to it matches
   --  the item. In particular, this means that if no filter has been added to
   --  it, the filter matches no item.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Any_Filter_New return Gtk_Any_Filter;
   --  Creates a new empty "any" filter.
   --  Use [methodGtk.MultiFilter.append] to add filters to it.
   --  This filter matches an item if any of the filters added to it matches
   --  the item. In particular, this means that if no filter has been added to
   --  it, the filter matches no item.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_any_filter_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Item_Type
      (Self : not null access Gtk_Any_Filter_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Any_Filter_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Any_Filter_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Any_Filter_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.Buildable"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Any_Filter_Record, Gtk_Any_Filter);
   function "+"
     (Widget : access Gtk_Any_Filter_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Any_Filter
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Any_Filter_Record, Gtk_Any_Filter);
   function "+"
     (Widget : access Gtk_Any_Filter_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Any_Filter
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Any_Filter;
